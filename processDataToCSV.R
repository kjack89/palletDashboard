rm(list = ls())

library(mosaic)
library(dplyr)
library(readr)
library(lubridate)

#edit filepath here
filePath = "C:/Users/kcjac/OneDrive/Documents/R/palletDashboard/amocTelemReport/amocTelemReport/"
fileNames = dir(filePath, pattern='.csv')
amocTelemdata = read_csv(paste0(filePath,"amocTelemetryReport_20191025121436.csv"))
gt1Day = read_csv(paste0(filePath,"modifyPalletLocationTypes_butDontChangeFileName.csv")) %>%
  select(Address,locName,locType,error,desc) %>%
  rename(error_miles='error',coordinateDescription = 'desc',locationType='locType',locationName='locName')

#cleans data and prepares it for the algorithm that determines how long the pallet stays in certain location
amocTelemdata3 = amocTelemdata %>% 
  distinct() %>%
  rename(DeviceID = 'Device ID',AssetName='Asset Name',ReportedDate='Reported Date',GeofenceName='Geofence Name',Humidity_Percent='Humidity (%)',InGeofence='In Geofence',Light_LX='Light (lx)',LocationSource='Location Source',StationaryTime_hours='Stationary Time (hours)',Temperature='Temperature (?F)') %>%
  arrange(DeviceID,mdy_hms(ReportedDate)) %>%
  mutate(startDate = 0) %>%
  mutate(endDate = 0) %>%
  mutate(delete = ' ') %>%
  mutate(distance = 0) %>%
  filter(!grepl('Durham',Address)) %>%
  filter(!grepl('Carrboro',Address)) %>%
  filter(!grepl('Chapel Hill',Address)) %>%
  filter(!grepl('No Location Data',Address)) %>%
  select(ReportedDate,AssetName,Latitude,Longitude,Address,InGeofence,LocationSource,startDate,endDate,delete,distance)

#initializing variables for algorithm
prevLong = 0
prevLat = 0
minDate = amocTelemdata3$ReportedDate[1]
prevDate = '1/1/2000 12:00:01 PM UTC'
prevAssetName = amocTelemdata3$AssetName[1]

#this is the algorithm that determines how long a pallet stays in a certain location
#How it works:
#1) from the above data cleaning, the data has been grouped by the Asset Name, and then ordered by Reported Date
#2) each entry is visited, and that entry's distance gets compared to the previous entry's distance. This is done using the entry latitude/longitude
#3) if the distance is less than 10 miles from the previous entry, then the previous entry is marked to be deleted, and the current entry takes on the start date of the previous entry
#4) this "chaining" of the start date keeps propogating forward until the distance becomes greater than 10 miles, in which that entry does not get marked for deletion, and is kept.
#5) the process restarts when the chain is broken, eg the distance becomes greater than 10 miles and se we feel confident that the pallet has moved to a new location 
for (i in 1:length(amocTelemdata3$ReportedDate)) {
  
  currDate = amocTelemdata3$ReportedDate[i]
  
  dlat = (prevLat*pi)/180 - ((amocTelemdata3$Latitude[i])*pi)/180
  dlong = (prevLong*pi)/180 - ((amocTelemdata3$Longitude[i])*pi)/180
  a = (sin(dlat/2))^2 + cos((prevLat*pi)/180) * cos(((amocTelemdata3$Latitude[i])*pi)/180) * (sin(dlong/2))^2
  distance = 3961*2*(atan2(sqrt(a),sqrt(1-a)))
  
  if (distance < 10 & prevAssetName == amocTelemdata3$AssetName[i]) {
    amocTelemdata3$startDate[i] = minDate
    amocTelemdata3$endDate[i] = currDate
    amocTelemdata3$delete[i-1] = 'delete'
    amocTelemdata3$distance[i] = distance
  } else {
    amocTelemdata3$startDate[i] = currDate
    amocTelemdata3$endDate[i] = currDate
    amocTelemdata3$distance[i] = distance
    minDate = currDate
  }
  
  prevDate = currDate
  prevLat = amocTelemdata3$Latitude[i]
  prevLong = amocTelemdata3$Longitude[i]
  prevAssetName = amocTelemdata3$AssetName[i]
}

#gets rid of the entries marked for deletion. now we have a date associated to the time each pallet entry started to stay at that particular location
amocTelemdata4 = amocTelemdata3 %>%
  filter(delete != 'delete') %>%
  mutate(aggregateTime = round(difftime(mdy_hms(endDate),mdy_hms(startDate))/3600,1)) %>%
  arrange(ReportedDate) %>%
  mutate(distance = round(distance,1)) %>%
  arrange(AssetName,mdy_hms(ReportedDate))
amocTelemdata4$aggregateTime = as.numeric(amocTelemdata4$aggregateTime)

#filters out all of the times that have had exactly 0 time elapsed between locations. these are basically single entries that represent pallets in transit. 
#also, the table for new address entries is added on here into the final table, finalAmocTelemData. unneeded columns are trimmed off.
noZeroTime = amocTelemdata4 %>% filter(aggregateTime>0)
finalAmocTelemData = noZeroTime %>%
  left_join(gt1Day,by='Address') %>%
  mutate(aggregateTime_days=round(aggregateTime/24,3)) %>%
  select(-delete,-distance,-ReportedDate,-aggregateTime,-InGeofence,-LocationSource)
finalAmocTelemData$startDate = mdy_hms(finalAmocTelemData$startDate)
finalAmocTelemData$endDate = mdy_hms(finalAmocTelemData$endDate)

top10 = finalAmocTelemData %>%
  group_by(Address) %>%
  summarise(Longitude = mean(Longitude), Latitude = mean(Latitude),
            dwellTime = sum(aggregateTime_days), distinctPallets = n_distinct(AssetName)) %>%
  arrange(desc(dwellTime)) %>%
  slice(1:10) %>%
  filter(distinctPallets > 1)

assets = finalAmocTelemData %>% select(AssetName) %>% distinct()

write.csv(finalAmocTelemData,file = 'C:/Users/kcjac/OneDrive/Documents/R/pallets/processedReports/el_chupacabra.csv')
write.csv(top10, file = 'C:/Users/kcjac/OneDrive/Documents/R/pallets/processedReports/top10.csv')
write.csv(assets, file = 'C:/Users/kcjac/OneDrive/Documents/R/pallets/processedReports/assetNames.csv')