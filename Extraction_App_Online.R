require(ncdf4)
require(ggplot2)
require(reshape2)
require(leaflet)

moving_sum <- function(x, n) {
  temp <- as.data.frame(matrix(NA,nrow = (nrow(x)-n+1), ncol = 2))
  for (irow in 1:nrow(temp)) {
    temp[irow,2] <- sum(x[irow:(irow+n-1),2])
  }
  temp[,1] <- x[1:(nrow(x)-n+1),1]
  colnames(temp) <- colnames(x)
  temp
}

num_threshold <- function(x,p) {
  x$Year <- format(x$Date,format="%Y")
  x$threshold <- as.integer(x$Value>quantile(x$Value,probs=p))
  temp <- aggregate(x[,4], list(x$Year), sum)
  colnames(temp)[1] <- c("Year")
  temp
}

server <- function(input, output) {
  observe({
    output$mymap <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        fitBounds(-124.7666, 49.4000, -67.0583, 25.0666)%>%
        setView(-95.91245, 37.2333, zoom = 3)
    })
    
    click = input$mymap_click
    if(is.null(click))
      return()
    
    leafletProxy('mymap')%>%addMarkers(lng = click$lng,
                                       lat = click$lat)%>%
      setView(lng = click$lng,
              lat = click$lat, zoom =7)
    
    df <- reactive({
      lat <- click$lat
      lat1 <- lat*1.00001
      lon <- click$lng
      lon1 <- lon*1.00001
      
      url <- paste0("http://thredds.northwestknowledge.net:8080/thredds/ncss/",
                    "agg_met_pet_1979_CurrentYear_CONUS.nc?",
                    "var=daily_mean_reference_evapotranspiration_grass&",
                    "north=",lat1,"&west=",lon1,"&east=",lon,"&south=",lat,
                    "&disableProjSubset=on&horizStride=1&",
                    "time_start=",input$dates[1],"T00%3A00%3A00Z&",
                    "time_end=",input$dates[2],"T00%",
                    "3A00%3A00Z&timeStride=1&accept=netcdf");
      download.file(url,"temp.nc", mode = 'wb');
      
      file_nc <- nc_open("temp.nc");
      nc_array <- ncvar_get(file_nc,
                            "daily_mean_reference_evapotranspiration_grass");
      pet_df <- as.data.frame(matrix(0,ncol = 2,
                                     nrow = dim(nc_array)));
      colnames(pet_df) <- c("Date","Value");
      pet_df$Date <- as.Date(ncvar_get(file_nc, "day"),origin="1900-01-01");
      pet_df$Value <- as.numeric(nc_array)
      return(pet_df)
    })
    
    df1 <- reactive({
      pet_df <- df();
      mov_df <- moving_sum(pet_df,n=input$ndays)
      return(mov_df)
    })
    
    df2 <- reactive({
      mov_df <- df1();
      threshold_df <- num_threshold(mov_df,p=input$threshold);
      colnames(threshold_df)[2] <- paste0(input$threshold*100,
                                          " th Percentile - ",
                                          sprintf(quantile(mov_df$Value,
                                                           input$threshold),
                                                  fmt = '%#.2f'),
                                          " mm");
      threshold_melt_df <- melt(threshold_df)
      return(threshold_melt_df)
    })
    
    output$nevents <- renderPlot({
      pet_df <- df()
      mov_df <- df1()
      threshold_melt_df <- df2()
      ggplot(threshold_melt_df, aes(x=Year, y=value,
                                    fill = variable))+ 
        geom_bar(stat = "identity", position = 'dodge') +
        xlab("Year") + ylab("Number of Events in an Year")+
        ggtitle(paste0("Number of Events - ",input$ndays,
                       " Days Accumulated Evaporative Demand - ",
                       input$sname)) +
        theme(plot.title = element_text(hjust = 0.5),
              legend.position = "bottom",
              axis.text.x = element_text(angle = 90),
              legend.title = element_blank())
    })
    
    output$accumulated <- renderPlot({
      pet_df <- df()
      mov_df <- df1()
      ggplot(mov_df, aes(x=Date,y=Value)) + 
        geom_line() +
        xlab("Time") + ylab("Evaporative Demand in mm")+
        ggtitle(paste0("Time Series - ",input$ndays,
                       " Days Accumulated Evaporative Demand - ",
                       input$sname)) +
        theme(plot.title = element_text(hjust = 0.5))
    })
    
    output$downloadNCData <- downloadHandler(
      filename = function() {
        paste0(input$sname,"_",input$dates[1],
               "_",input$dates[2],"_pet_daily.nc")
      },
      content = function(file) {
        file.copy("temp.nc",file)
      })
    
    output$downloadTSData <- downloadHandler(
      filename = function() {
        paste0("Time_Series_",input$ndays,"_Days_",
               input$sname,"_",input$dates[1],
               "_",input$dates[2],"_accumulated.csv")
      },
      content = function(file) {
        pet_df <- df()
        mov_df <- df1()
        write.csv(mov_df, file, row.names = FALSE)
      })
    output$downloadNEData <- downloadHandler(
      filename = function() {
        paste0("Number_of_Events_Accumulated_",input$ndays,"_Days_",
               input$threshold*100,"_Threshold_",
               input$sname,"_",input$dates[1],
               "_",input$dates[2],".csv")
      },
      content = function(file) {
        pet_df <- df()
        mov_df <- df1()
        threshold_melt_df <- df2()
        write.csv(threshold_melt_df, file, row.names = FALSE)
      })
  })
}

ui <- fluidPage(navbarPage("NC CASC Extremes Application",
                           tabPanel("App",
                                    sidebarLayout(
                                      sidebarPanel(
                                        leafletOutput("mymap"),
                                        textInput("sname",
                                                  "Enter Name of Location:",
                                                  "Location"),
                                        dateRangeInput("dates", "Enter Period of Record",
                                                       start = "1979-01-01", end = "2020-12-31",
                                                       min = "1979-01-01",max = "2020-12-31",
                                                       format = "MM dd, yyyy"),
                                        numericInput(
                                          "ndays",
                                          "Enter Number of Days for Accumulation",
                                          14,
                                          min = 1,
                                          max = 360),
                                        numericInput(
                                          "threshold",
                                          "Enter Threshold as decimal",
                                          0.999,
                                          min = 0,
                                          max = 1
                                        ),
                                        downloadButton("downloadNCData",
                                        "Click here to Download the .nc file")
                                      ),
                                      mainPanel(plotOutput("accumulated"),
                                                downloadButton("downloadTSData",
          "Click here to Download the Accumulated PET Time Series as .csv"),
          plotOutput("nevents"),
                downloadButton("downloadNEData",
          "Click here to Download the Number of Events per year as .csv")
                                      )))))

shinyApp(ui = ui, server = server)