rm(list = ls())

library(mosaic)
library(dplyr)
library(readr)
library(lubridate)
library(shiny)
library(leaflet)
library(htmltools)

filePath = "filepath/" #edit filepath here
fileNames = dir(filePath, pattern='.csv')
amocTelemdata = read_csv(paste0(filePath,"amocTelemetryReport_20191025121436.csv"))
gt1Day = read_csv(paste0(filePath,"modifyPalletLocationTypes_butDontChangeFileName.csv")) %>%
  select(Address,locName,locType,error,desc) %>%
  rename(error_miles='error',coordinateDescription = 'desc',locationType='locType',locationName='locName')


download.file(
  'https://raw.githubusercontent.com/bbecquet/Leaflet.PolylineDecorator/master/dist/leaflet.polylineDecorator.js', 
  'leaflet.polylineDecorator.js')

polylineDecoratorPlugin <- htmlDependency('Leaflet.PolylineDecorator',
                                          '1.6.0',
                                          src = normalizePath('.'), 
                                          script = 'leaflet.polylineDecorator.js')

registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}

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

latlonDistance <- function(prevLong, prevLat, minDate, prevDate, prevAssetName) {
  dlat = (prevLat*pi)/180 - ((amocTelemdata3$Latitude[i])*pi)/180
  dlong = (prevLong*pi)/180 - ((amocTelemdata3$Longitude[i])*pi)/180
  a = (sin(dlat/2))^2 + cos((prevLat*pi)/180) * cos(((amocTelemdata3$Latitude[i])*pi)/180) * (sin(dlong/2))^2
  distance = 3961*2*(atan2(sqrt(a),sqrt(1-a)))
  return(distance)
}

#this is the algorithm that determines how long a pallet stays in a certain location
#How it works:
#1) from the above data cleaning, the data has been grouped by the Asset Name, and then ordered by Reported Date
#2) each entry is visited, and that entry's distance gets compared to the previous entry's distance. This is done using the entry latitude/longitude
#3) if the distance is less than 10 miles from the previous entry, then the previous entry is marked to be deleted, and the current entry takes on the start date of the previous entry
#4) this "chaining" of the start date keeps propogating forward until the distance becomes greater than 10 miles, in which that entry does not get marked for deletion, and is kept.
#5) the process restarts when the chain is broken, eg the distance becomes greater than 10 miles and so we feel confident that the pallet has actually moved to a new location 
for (i in 1:length(amocTelemdata3$ReportedDate)) {
  
  currDate = amocTelemdata3$ReportedDate[i]
  distance = latlonDistance(prevLong, prevLat, minDate, prevDate, prevAssetName)
  
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

#gets rid of the entries marked for deletion. 
#now we have a date associated to the time each pallet entry started to stay at that particular location
amocTelemdata4 = amocTelemdata3 %>%
  filter(delete != 'delete') %>%
  mutate(aggregateTime = round(difftime(mdy_hms(endDate),mdy_hms(startDate))/3600,1)) %>%
  arrange(ReportedDate) %>%
  mutate(distance = round(distance,1)) %>%
  arrange(AssetName,mdy_hms(ReportedDate))
amocTelemdata4$aggregateTime = as.numeric(amocTelemdata4$aggregateTime)

#filters out all of the times that have had exactly 0 time elapsed between locations. 
#these are basically single entries that represent pallets in transit. 
noZeroTime = amocTelemdata4 %>% filter(aggregateTime>0)
finalAmocTelemData = noZeroTime %>%
  left_join(gt1Day,by='Address') %>%
  mutate(aggregateTime_days=round(aggregateTime/24,3)) %>%
  select(-delete,-distance,-ReportedDate,-aggregateTime,-InGeofence,-LocationSource)
finalAmocTelemData$startDate = mdy_hms(finalAmocTelemData$startDate)
finalAmocTelemData$endDate = mdy_hms(finalAmocTelemData$endDate)

shinyApp(

  ui <- fluidPage(
    tabsetPanel (
      tabPanel('Pallet Location Dashboard', fluid = TRUE,
               titlePanel("Pallet Tracking Data"),
               sidebarLayout(
                 sidebarPanel(
                   radioButtons("togglePathing",
                               "Toggle pathing",
                               choices = list('On' = 'On','Off' = 'Off')),
                   selectInput("setHighTraffic",
                               "Set traffic threshold (red dots signify high traffic locations)",
                               c(0,5,10,20,30,40)),
                   selectInput("assetName",
                               "Select Asset Name",
                               c('All',select(finalAmocTelemData,AssetName) %>% arrange(AssetName))),
                   selectInput('elapsedTime',
                               'Filter by elapsed days',
                               c(0,.5,1,2,3)),
                   sliderInput("dateFilter",
                               "Filter by start date",
                               min = floor_date(finalAmocTelemData$startDate,'day') %>% min(),
                               max = floor_date(finalAmocTelemData$endDate,'day') %>% max(),
                               value = c(floor_date(finalAmocTelemData$startDate,'day') %>% 
                                           min(),floor_date(finalAmocTelemData$endDate,'day') %>% max()),
                               timeFormat = '%m/%d/%Y'
                   ),
                   textOutput('median'),
                   textOutput('mean'),
                   textOutput('sd'),
                   textOutput('count'),
                   plotOutput('rainbowplot'),
                   br(),
                   br()
                 ),
                 
                 mainPanel(
                   leafletOutput("palletMapping"),
                   dataTableOutput('palletloctable')
                 )
               )
      )
    )
  ),
  
  server <- function(input, output) {
    
    dfLogic = reactive ({
      if (input$assetName == 'All') {
        return(finalAmocTelemData %>%
                 filter(aggregateTime_days >= input$elapsedTime &
                          startDate >= input$dateFilter[1] & startDate <= input$dateFilter[2]) %>%
                 select(locationType,locationName,Address,Latitude,Longitude,aggregateTime_days,startDate,endDate,error_miles,coordinateDescription,AssetName) %>%
                 arrange(startDate))
      } else {
        return(finalAmocTelemData %>% filter(AssetName == input$assetName &
                                      aggregateTime_days >= input$elapsedTime &
                                      startDate >= input$dateFilter[1] & startDate <= input$dateFilter[2]) %>%
                 select(locationType,locationName,Address,Latitude,Longitude,aggregateTime_days,startDate,endDate,error_miles,coordinateDescription,AssetName))
      }
    })
    
    dfLogic2 = reactive ({
      return(finalAmocTelemData %>%
               anti_join(gt1Day,by='Address') %>%
               filter(aggregateTime_days >= input$elapsedTime2))
    })
    
    noModificationsDF = reactive ({
      return(finalAmocTelemData)
    })
    
    trafficSetDF = reactive ({
      return(finalAmocTelemData %>%
               group_by(Address) %>%
               summarise(Longitude = mean(Longitude), Latitude = mean(Latitude),
                         dwellTime = sum(aggregateTime_days), distinctPallets = n_distinct(AssetName), traffic = n()) %>%
               arrange(desc(traffic)) %>%
               filter(traffic >= input$setHighTraffic))
    })
    
    output$newLocations <- renderDataTable({
      dfLogic2() %>% arrange(startDate)
    })
    
    output$palletMapping <- renderLeaflet({
      if (input$togglePathing == 'Off') {
        leaflet(data = dfLogic(), options = leafletOptions(minZoom = 0, maxZoom = 12)) %>%
          addTiles() %>%
          addCircles(~Longitude, ~Latitude, radius = 100, popup = ~paste(as.character(Address),'\tdwelled for ',as.character(aggregateTime_days),' days'), labelOptions = (noHide = TRUE), weight = 3, color = 'black', opacity = 1.0, fillOpacity = 1.0) %>%
          addCircles(data = trafficSetDF(), ~Longitude, ~Latitude, radius = 100, popup = ~as.character(Address), labelOptions = (noHide = TRUE),color = 'red',weight = 5, fill = TRUE, fillColor = 'red', fillOpacity = 1.0)
      } else {
        if (input$setHighTraffic == 0) {
          leaflet(data = dfLogic(), options = leafletOptions(minZoom = 0, maxZoom = 12)) %>%
            registerPlugin(polylineDecoratorPlugin) %>%
            addTiles() %>%
            addCircles(~Longitude, ~Latitude, radius = 100, popup = ~paste(as.character(Address),'\tdwelled for ',as.character(aggregateTime_days),' days'), labelOptions = (noHide = TRUE), weight = 3, color = 'black', opacity = 1.0, fillOpacity = 1.0) %>%
            htmlwidgets::onRender(
              "function(el,x,data) {
          var myMap = this;

          // I have to wrap the decoration addition code with map.on() function
          // wait for polyline layer to be added before i add decorator    
          myMap.on('layeradd',
            function(e) {
              var lyr = e.layer;
              // among whatever layers added to the map, i look for
              // 'need_decorator' property which i tell myself to add as an options
              // when adding polyline
              if ('need_decorator' in lyr.options) {

                var dec = L.polylineDecorator(lyr, {
                  patterns: [
                    {offset: 0, repeat: 100, symbol: L.Symbol.arrowHead({pixelSize:5, polygon: false, pathOptions:{stroke:true}})}
                  ]
                }).addTo(myMap);
              }
            }
          );
        }")
        } else {
          leaflet(data = dfLogic(), options = leafletOptions(minZoom = 0, maxZoom = 12)) %>%
            registerPlugin(polylineDecoratorPlugin) %>%
            addTiles() %>%
            addCircles(~Longitude, ~Latitude, radius = 100, popup = ~paste(as.character(Address),'\tdwelled for ',as.character(aggregateTime_days),' days'), labelOptions = (noHide = TRUE), weight = 3, color = 'black', opacity = 1.0, fillOpacity = 1.0) %>%
            htmlwidgets::onRender(
              "function(el,x,data) {
          var myMap = this;

          // I have to wrap the decoration addition code with map.on() function
          // wait for polyline layer to be added before i add decorator    
          myMap.on('layeradd',
            function(e) {
              var lyr = e.layer;
              // among whatever layers added to the map, i look for
              // 'need_decorator' property which i tell myself to add as an options
              // when adding polyline
              if ('need_decorator' in lyr.options) {

                var dec = L.polylineDecorator(lyr, {
                  patterns: [
                    {offset: 0, repeat: 100, symbol: L.Symbol.arrowHead({pixelSize:5, polygon: false, pathOptions:{stroke:true}})}
                  ]
                }).addTo(myMap);
              }
            }
          );
        }",
              data=dfLogic()) %>%
            addCircles(data = trafficSetDF(), ~Longitude, ~Latitude,
                       radius = 15, popup = ~as.character(Address), 
                       labelOptions = (noHide = TRUE), color = 'red', 
                       fillColor = 'red', weight = 5, fill = TRUE, fillOpacity = 1.0)
        }
      }
    })
    
    observe({
      if (input$togglePathing == 'Off') {
        
      } else {
        #unable to loop/use apply() function because the pipe operator's attribute to allow composition is 
        #necessary for addPolylines to be implemented correctly here. So here we have this atrocious thing before us.
        leafletProxy('palletMapping') %>%
          addPolylines(data = dfLogic() %>% filter(AssetName == "Pallet AA01"),lng = ~Longitude,lat = ~Latitude, color = '#03F', weight = 1, options = list(need_decorator = T)) %>%
          addPolylines(data = dfLogic() %>% filter(AssetName == "Pallet AA02"),lng = ~Longitude,lat = ~Latitude, color = '#03F', weight = 1, options = list(need_decorator = T)) %>%
          addPolylines(data = dfLogic() %>% filter(AssetName == "Pallet AA03"),lng = ~Longitude,lat = ~Latitude, color = '#03F', weight = 1, options = list(need_decorator = T)) %>%
          addPolylines(data = dfLogic() %>% filter(AssetName == "Pallet AA04"),lng = ~Longitude,lat = ~Latitude, color = '#03F', weight = 1, options = list(need_decorator = T)) %>%
          addPolylines(data = dfLogic() %>% filter(AssetName == "Pallet AA05"),lng = ~Longitude,lat = ~Latitude, color = '#03F', weight = 1, options = list(need_decorator = T)) %>%
          addPolylines(data = dfLogic() %>% filter(AssetName == "Pallet AA06"),lng = ~Longitude,lat = ~Latitude, color = '#03F', weight = 1, options = list(need_decorator = T)) %>%
          addPolylines(data = dfLogic() %>% filter(AssetName == "Pallet AA07"),lng = ~Longitude,lat = ~Latitude, color = '#03F', weight = 1, options = list(need_decorator = T)) %>%
          addPolylines(data = dfLogic() %>% filter(AssetName == "Pallet AA09"),lng = ~Longitude,lat = ~Latitude, color = '#03F', weight = 1, options = list(need_decorator = T)) %>%
          addPolylines(data = dfLogic() %>% filter(AssetName == "Pallet AA10 (ACCEL1)"),lng = ~Longitude,lat = ~Latitude, color = '#03F', weight = 1, options = list(need_decorator = T)) %>%
          addPolylines(data = dfLogic() %>% filter(AssetName == "Pallet AA11"),lng = ~Longitude,lat = ~Latitude, color = '#03F', weight = 1, options = list(need_decorator = T)) %>%
          addPolylines(data = dfLogic() %>% filter(AssetName == "Pallet AA12"),lng = ~Longitude,lat = ~Latitude, color = '#03F', weight = 1, options = list(need_decorator = T)) %>%
          addPolylines(data = dfLogic() %>% filter(AssetName == "Pallet AA13"),lng = ~Longitude,lat = ~Latitude, color = '#03F', weight = 1, options = list(need_decorator = T)) %>%
          addPolylines(data = dfLogic() %>% filter(AssetName == "Pallet AA14"),lng = ~Longitude,lat = ~Latitude, color = '#03F', weight = 1, options = list(need_decorator = T)) %>%
          addPolylines(data = dfLogic() %>% filter(AssetName == "Pallet AA15"),lng = ~Longitude,lat = ~Latitude, color = '#03F', weight = 1, options = list(need_decorator = T)) %>%
          addPolylines(data = dfLogic() %>% filter(AssetName == "Pallet AA16"),lng = ~Longitude,lat = ~Latitude, color = '#03F', weight = 1, options = list(need_decorator = T)) %>%
          addPolylines(data = dfLogic() %>% filter(AssetName == "Pallet AA17"),lng = ~Longitude,lat = ~Latitude, color = '#03F', weight = 1, options = list(need_decorator = T)) %>%
          addPolylines(data = dfLogic() %>% filter(AssetName == "Pallet AA18"),lng = ~Longitude,lat = ~Latitude, color = '#03F', weight = 1, options = list(need_decorator = T)) %>%
          addPolylines(data = dfLogic() %>% filter(AssetName == "Pallet AA19"),lng = ~Longitude,lat = ~Latitude, color = '#03F', weight = 1, options = list(need_decorator = T)) %>%
          addPolylines(data = dfLogic() %>% filter(AssetName == "Pallet AA20"),lng = ~Longitude,lat = ~Latitude, color = '#03F', weight = 1, options = list(need_decorator = T)) %>%
          addPolylines(data = dfLogic() %>% filter(AssetName == "Pallet AA21"),lng = ~Longitude,lat = ~Latitude, color = '#03F', weight = 1, options = list(need_decorator = T)) %>%
          addPolylines(data = dfLogic() %>% filter(AssetName == "Pallet AA22"),lng = ~Longitude,lat = ~Latitude, color = '#03F', weight = 1, options = list(need_decorator = T)) %>%
          addPolylines(data = dfLogic() %>% filter(AssetName == "Pallet AA23"),lng = ~Longitude,lat = ~Latitude, color = '#03F', weight = 1, options = list(need_decorator = T)) %>%
          addPolylines(data = dfLogic() %>% filter(AssetName == "Pallet AA24"),lng = ~Longitude,lat = ~Latitude, color = '#03F', weight = 1, options = list(need_decorator = T)) %>%
          addPolylines(data = dfLogic() %>% filter(AssetName == "Pallet AA25"),lng = ~Longitude,lat = ~Latitude, color = '#03F', weight = 1, options = list(need_decorator = T)) %>%
          addPolylines(data = dfLogic() %>% filter(AssetName == "Pallet AA26"),lng = ~Longitude,lat = ~Latitude, color = '#03F', weight = 1, options = list(need_decorator = T)) %>%
          addPolylines(data = dfLogic() %>% filter(AssetName == "Pallet AA27"),lng = ~Longitude,lat = ~Latitude, color = '#03F', weight = 1, options = list(need_decorator = T)) %>%
          addPolylines(data = dfLogic() %>% filter(AssetName == "Pallet AA28"),lng = ~Longitude,lat = ~Latitude, color = '#03F', weight = 1, options = list(need_decorator = T)) %>%
          addPolylines(data = dfLogic() %>% filter(AssetName == "Pallet AA29"),lng = ~Longitude,lat = ~Latitude, color = '#03F', weight = 1, options = list(need_decorator = T)) %>%
          addPolylines(data = dfLogic() %>% filter(AssetName == "Pallet AA30"),lng = ~Longitude,lat = ~Latitude, color = '#03F', weight = 1, options = list(need_decorator = T)) %>%
          addPolylines(data = dfLogic() %>% filter(AssetName == "Pallet BB01"),lng = ~Longitude,lat = ~Latitude, color = '#03F', weight = 1, options = list(need_decorator = T)) %>%
          addPolylines(data = dfLogic() %>% filter(AssetName == "Pallet BB02"),lng = ~Longitude,lat = ~Latitude, color = '#03F', weight = 1, options = list(need_decorator = T)) %>%
          addPolylines(data = dfLogic() %>% filter(AssetName == "Pallet BB04"),lng = ~Longitude,lat = ~Latitude, color = '#03F', weight = 1, options = list(need_decorator = T)) %>%
          addPolylines(data = dfLogic() %>% filter(AssetName == "Pallet BB05"),lng = ~Longitude,lat = ~Latitude, color = '#03F', weight = 1, options = list(need_decorator = T)) %>%
          addPolylines(data = dfLogic() %>% filter(AssetName == "Pallet BB06"),lng = ~Longitude,lat = ~Latitude, color = '#03F', weight = 1, options = list(need_decorator = T)) %>%
          addPolylines(data = dfLogic() %>% filter(AssetName == "Pallet BB09"),lng = ~Longitude,lat = ~Latitude, color = '#03F', weight = 1, options = list(need_decorator = T)) %>%
          addPolylines(data = dfLogic() %>% filter(AssetName == "Pallet BB10"),lng = ~Longitude,lat = ~Latitude, color = '#03F', weight = 1, options = list(need_decorator = T)) %>%
          addPolylines(data = dfLogic() %>% filter(AssetName == "Pallet BB11"),lng = ~Longitude,lat = ~Latitude, color = '#03F', weight = 1, options = list(need_decorator = T)) %>%
          addPolylines(data = dfLogic() %>% filter(AssetName == "Pallet BB12"),lng = ~Longitude,lat = ~Latitude, color = '#03F', weight = 1, options = list(need_decorator = T)) %>%
          addPolylines(data = dfLogic() %>% filter(AssetName == "Pallet BB13"),lng = ~Longitude,lat = ~Latitude, color = '#03F', weight = 1, options = list(need_decorator = T)) %>%
          addPolylines(data = dfLogic() %>% filter(AssetName == "Pallet BB14"),lng = ~Longitude,lat = ~Latitude, color = '#03F', weight = 1, options = list(need_decorator = T)) %>%
          addPolylines(data = dfLogic() %>% filter(AssetName == "Pallet BB15 (JH)"),lng = ~Longitude,lat = ~Latitude, color = '#03F', weight = 1, options = list(need_decorator = T)) %>%
          addPolylines(data = dfLogic() %>% filter(AssetName == "Pallet BB16"),lng = ~Longitude,lat = ~Latitude, color = '#03F', weight = 1, options = list(need_decorator = T)) %>%
          addPolylines(data = dfLogic() %>% filter(AssetName == "Pallet BB17"),lng = ~Longitude,lat = ~Latitude, color = '#03F', weight = 1, options = list(need_decorator = T)) %>%
          addPolylines(data = dfLogic() %>% filter(AssetName == "Pallet BB18"),lng = ~Longitude,lat = ~Latitude, color = '#03F', weight = 1, options = list(need_decorator = T)) %>%
          addPolylines(data = dfLogic() %>% filter(AssetName == "Pallet BB19"),lng = ~Longitude,lat = ~Latitude, color = '#03F', weight = 1, options = list(need_decorator = T)) %>%
          addPolylines(data = dfLogic() %>% filter(AssetName == "Pallet D (2) (custom ant.) - geofence"),lng = ~Longitude,lat = ~Latitude, color = '#03F', weight = 1, options = list(need_decorator = T))
        
      }
    })
    
    output$rainbowplot <- renderPlot({
      gg_b = ggplot_build(ggplot(dfLogic()) +
                            geom_histogram(aes(x=aggregateTime_days)))
      nu_bins <- dim(gg_b$data[[1]])[1]
      ggplot(dfLogic()) +
        geom_histogram(aes(x=aggregateTime_days),fill = rainbow(nu_bins)) +
        xlab('Elapsed Time (days)') +
        ylab('Count')+
        ggtitle('Dwell time frequency') +
        theme_dark() +
        theme(axis.title = element_text(face='bold',size=14),
              axis.text = element_text(face = 'bold'),
              plot.title = element_text(face='bold',size=16))
    })
    
    output$mean <- renderText({
      paste(input$assetName,'overall',summary(dfLogic() %>% select(aggregateTime_days) %>%
                                                filter(!is.na(aggregateTime_days)))[3],' days')
    })
    
    output$median <- renderText({
      paste(input$assetName,'overall',summary(dfLogic() %>% select(aggregateTime_days) %>%
                                                filter(!is.na(aggregateTime_days)))[4],' days')
    })
    
    output$sd <- renderText({
      sd = round(sqrt(var(dfLogic() %>% select(aggregateTime_days) %>% filter(!is.na(aggregateTime_days)))),2)
      paste(input$assetName,'overall Standard Deviation: ',sd,' days')
    })
    
    output$count <- renderText({
      count = count(dfLogic() %>% filter(!is.na(aggregateTime_days)))
      paste(input$assetName,'overall count: ',count,' entries')
    })
    
    output$palletloctable <- renderDataTable({
      dfLogic() %>% arrange(startDate) %>% rename(elapsedTime_days='aggregateTime_days',err_mi='error_miles') %>%
        mutate(startDate = as.Date(startDate,format = '%m/%d/%Y')) %>% 
        mutate(endDate = as.Date(endDate,format = '%m/%d/%Y'))
    })
    
  })
