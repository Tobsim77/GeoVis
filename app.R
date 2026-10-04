# GeoVis Tobias Simbürger Winter 2024

if (!require(tidyr)) {install.packages("tidyr"); library(tidyr)}
if (!require(dplyr)) {install.packages("dplyr"); library(dplyr)}
if (!require(tidyverse)) {install.packages("tidyverse"); library(tidyverse)}
if (!require(lubridate)) {install.packages("lubridate"); library(lubridate)}
if (!require(readxl)) {install.packages("readxl"); library(readxl)}
if (!require(shiny)) {install.packages("shiny"); library(shiny)}
if (!require(jsonlite)) {install.packages("jsonlite"); library(jsonlite)}
if (!require(plotly)) {install.packages("plotly"); library(plotly)}
if (!require(scales)) {install.packages("scales"); library(scales)}
if (!require(reshape2)) {install.packages("reshape2"); library(reshape2)}
if (!require(bslib)) {install.packages("bslib"); library(bslib)}
if (!require(rsconnect)) {install.packages("rsconnect"); library(rsconnect)}
if (!require(stringi)) {install.packages("stringi"); library(stringi)}
if (!require(sf)) {install.packages("sf"); library(sf)}
if (!require(patchwork)) {install.packages("patchwork"); library(patchwork)}
if (!require(terra)) {install.packages("terra"); library(terra)}
if (!require(mgcv)) {install.packages("mgcv"); library(mgcv)}


# metadata_10min <- fromJSON("https://dataset.api.hub.geosphere.at/v1/station/historical/klima-v2-10min/metadata")
# metadata_hourly <- fromJSON("https://dataset.api.hub.geosphere.at/v1/station/historical/klima-v2-1h/metadata")
# metadata_daily <- fromJSON("https://dataset.api.hub.geosphere.at/v1/station/historical/klima-v2-1d/metadata")
# metadata_monthly <- fromJSON("https://dataset.api.hub.geosphere.at/v1/station/historical/klima-v2-1m/metadata")
# metadata_km <- fromJSON("https://dataset.api.hub.geosphere.at/v1/grid/historical/spartacus-v3-1m-1km/metadata")


if (file.exists("metadata_10min.json")) {
  metadata_10min <- fromJSON("metadata_10min.json")
} else {
  metadata_10min <- fromJSON("https://dataset.api.hub.geosphere.at/v1/station/historical/klima-v2-10min/metadata")
  write_json(metadata_10min,"metadata_10min.json")
}

if (file.exists("metadata_hourly.json")) {
  metadata_hourly <- fromJSON("metadata_hourly.json")
} else {
  metadata_hourly <- fromJSON("https://dataset.api.hub.geosphere.at/v1/station/historical/klima-v2-1h/metadata")
  write_json(metadata_hourly,"metadata_hourly.json")
}

if (file.exists("metadata_daily.json")) {
  metadata_daily <- fromJSON("metadata_daily.json")
} else {
  metadata_daily <- fromJSON("https://dataset.api.hub.geosphere.at/v1/station/historical/klima-v2-1d/metadata")
  write_json(metadata_daily,"metadata_daily.json")
}

if (file.exists("metadata_monthly.json")) {
  metadata_monthly <- fromJSON("metadata_monthly.json")
} else {
  metadata_monthly <- fromJSON("https://dataset.api.hub.geosphere.at/v1/station/historical/klima-v2-1m/metadata")
  write_json(metadata_monthly,"metadata_monthly.json")
}

if (file.exists("metadata_km.json")) {
  metadata_grid <- fromJSON("metadata_km.json")
} else {
  metadata_grid <- fromJSON("https://dataset.api.hub.geosphere.at/v1/grid/historical/spartacus-v3-1m-1km/metadata")
  write_json(metadata_grid,"metadata_km.json")
}


metadata_hourly$stations<-metadata_hourly$stations |> filter(type == "COMBINED")
metadata_10min$stations<-metadata_10min$stations |> filter(type == "COMBINED")

avail_stations<- rbind(metadata_10min$stations|>mutate(origin = "10min"),metadata_hourly$stations|>mutate(origin = "hour"),metadata_daily$stations|>mutate(origin = "daily"),metadata_monthly$stations|>mutate(origin = "montly"))|> 
  group_by(name)|> summarise(amount = n_distinct(origin))|> filter(amount>3)|>select(name)|> unlist()|>array()|>sort()



ui <- navbarPage(
  title = "GeoVis",
  useBusyIndicators(),
  navbar_options = navbar_options(
    bg = "white",
    underline = T),
  nav_panel("Current",
            
              
              plotlyOutput(outputId = "plot_aktuell"),
              selectInput(inputId = "location",
                          label = "Location",
                          choices = c("ÖSTERREICH", avail_stations|> sort()),
                          selected = "Salzburg Freisaal"
              ),
              selectInput(inputId  = "variables_hourly",
                          label = "Variables",
                          choices = metadata_hourly$parameters |> dplyr::pull(long_name)|> sort(),
                          selected = "Lufttemperatur 2m"
              ),
              
            
            
            selectInput("switch", "Switch Visualisation", choices = c("Tiles","Smooth")),
            plotlyOutput(outputId = "plot_history"),
            selectInput(inputId  = "variables_daily",
                        label = "Variables",
                        choices = metadata_daily$parameters |> dplyr::pull(long_name)|> sort(),
                        selected = "Lufttemperatur 2m Mittelwert"),

            
            
  ),
  tabPanel("Trends",
           nav_panel("Task 2",
                     sidebarLayout(
                       sidebarPanel(
                         
                         sliderInput(
                           inputId = "year_slider",
                           label = "Years",
                           min = 1990,
                           max = year(Sys.Date()),
                           value = c(1990, year(Sys.Date())),
                           dragRange = TRUE,
                           sep = ""
                         )
                       ),
                       mainPanel(
                         navset_tab(
                           nav_panel("Trend",
                                     selectInput(inputId  = "variables2",
                                                 label = "Variable",
                                                 choices = metadata_daily$parameters |>
                                                   filter(!(unit %in% c("Code (Synop)", NA, "Code", "", "°"))) |> dplyr::select(long_name) |> filter(!grepl("Beobachtungstermin", long_name)) |> 
                                                   c("Gesamtschneehöhe zum Beobachtungstermin I"),
                                                 selected = "Lufttemperatur 2m Mittelwert"),
                                     textOutput("text2"),
                                     plotlyOutput("plot1")
                           ),
                           nav_panel("Hitzewellen",
                                     textOutput("text_heatwave"),
                                     plotlyOutput(outputId = "heatwave_plot"),
                                     textOutput("text_heatwave_def")
                           )
                         )
                       )
                     )
           )
  ),
  tabPanel("Slopes",
           navset_tab(
             nav_panel("500m",
                       textOutput("slope_stats"),
                       plotOutput("plot_per_500m",width = "100%",height = "80vh")
             ),
             nav_panel("Altitude Relation",
                       plotOutput("plot_altitude_slope",width = "100%",height = "80vh")
             ),
             nav_panel("Nonlinear Effect",
                       textOutput("slope_findings"),
                       plotOutput("plot_altitude_gam",width = "100%",height = "80vh")
             ),
             nav_panel("Spatial Influence",
                       plotOutput("plot_altitude_effect",width = "100%",height = "45vh"),
                       plotOutput("plot_residual_map",width = "100%",height = "75vh")
             )
           )
  ),
  tabPanel("3D",
           plotlyOutput("plot3d",width = "100%",height = "90vh")
  )
)

if (file.exists("slopes.rds")) {
  
  message("Loading slopes.rds...")
  slopes <- readRDS("slopes.rds")
  message("slopes.rds loaded.")
  
} else {
  
  bbox <- paste(metadata_grid$bbox_outer, collapse = ",")
  
  link_grid <- "https://dataset.api.hub.geosphere.at/v1/grid/historical/spartacus-v3-1m-1km"
  
  date0 <- seq(
    as.Date(metadata_grid$start_time),
    as.Date(metadata_grid$end_time),
    by = "1 month"
  )
  
  links <- paste0(
    link_grid,
    "?parameters=TM",
    "&start=", format(date0, "%Y-%m-%dT00:00"),
    "&end=", format(date0, "%Y-%m-%dT00:00"),
    "&bbox=", bbox,
    "&output_format=netcdf"
  )
  
  
  message("Calculating monthly slopes...")
  
  pb <- txtProgressBar(
    min = 0,
    max = length(links),
    style = 3
  )
  
  grid <- NULL
  
  n <- NULL
  sum_month <- NULL
  sum_TM <- NULL
  sum_month2 <- NULL
  sum_month_TM <- NULL
  
  
  for (i in seq_along(links)) {
    
    month_index <- (
      lubridate::year(date0[i]) -
        lubridate::year(date0[1])
    ) * 12 +
      lubridate::month(date0[i]) -
      lubridate::month(date0[1])
    
    
    message(
      "\nLoading ",
      format(date0[i], "%Y-%m"),
      "..."
    )
    
    
    tmp <- tempfile(fileext = ".nc")
    
    download.file(
      links[i],
      tmp,
      mode = "wb",
      quiet = TRUE
    )
    
    r <- terra::rast(tmp)
    
    
    if (terra::nlyr(r) > 1) {
      r <- r[[1]]
    }
    
    
    TM <- terra::values(
      r,
      mat = FALSE
    )
    
    
    if (is.null(grid)) {
      
      coords <- terra::xyFromCell(
        r,
        seq_len(terra::ncell(r))
      )
      
      grid <- tibble(
        X = coords[, 1],
        Y = coords[, 2]
      )
      
      n_cells <- nrow(grid)
      
      n <- integer(n_cells)
      sum_month <- numeric(n_cells)
      sum_TM <- numeric(n_cells)
      sum_month2 <- numeric(n_cells)
      sum_month_TM <- numeric(n_cells)
    }
    
    
    if (length(TM) != nrow(grid)) {
      stop(
        paste(
          "Grid size changed in",
          format(date0[i], "%Y-%m")
        )
      )
    }
    
    
    valid <- is.finite(TM)
    
    
    n[valid] <-
      n[valid] + 1
    
    sum_month[valid] <-
      sum_month[valid] + month_index
    
    sum_TM[valid] <-
      sum_TM[valid] + TM[valid]
    
    sum_month2[valid] <-
      sum_month2[valid] + month_index^2
    
    sum_month_TM[valid] <-
      sum_month_TM[valid] + month_index * TM[valid]
    
    
    rm(r, TM)
    unlink(tmp)
    gc()
    
    
    setTxtProgressBar(pb, i)
  }
  
  
  close(pb)
  
  
  message("\nCalculating final monthly slopes...")
  
  
  denominator <-
    n * sum_month2 -
    sum_month^2
  
  
  slope <- rep(
    NA_real_,
    length(n)
  )
  
  
  valid_slope <-
    n >= 2 &
    denominator != 0
  
  
  slope[valid_slope] <- (
    n[valid_slope] *
      sum_month_TM[valid_slope] -
      sum_month[valid_slope] *
      sum_TM[valid_slope]
  ) /
    denominator[valid_slope]
  
  
  slopes <- grid |>
    mutate(
      slope = slope
    ) |>
    filter(
      is.finite(slope)
    ) |>
    select(
      X,
      Y,
      slope
    )
  
  
  saveRDS(
    slopes,
    "slopes.rds"
  )
  
  
  rm(
    grid,
    n,
    sum_month,
    sum_TM,
    sum_month2,
    sum_month_TM,
    denominator,
    slope,
    valid_slope
  )
  
  gc()
  
  message("slopes.rds saved.")
}





server <- function(input, output, session) {
  busyIndicatorOptions(spinner_type = "dots")
  location_data <- reactive({
    if (input$location != "ÖSTERREICH") {
      location_server <- metadata_hourly$stations |> filter(name %in% input$location & name %in% avail_stations) |> dplyr::select(id) |> toString()
      location_server <- paste0("station_ids=", location_server)
      min_date1 <- metadata_hourly$stations |> filter(id == location_server) |> pull(valid_from) |> as.Date()
      max_date1 <- metadata_hourly$stations |> filter(id == location_server) |> pull(valid_to) |> as.Date()
    } else {
      location_ids1 <- metadata_10min$stations|> filter(name %in% avail_stations)|> select(id)|>unlist()|>array()|> sample(length(avail_stations)/2) #this is for performance reasons. Can cause missinformation!!!!
      min_date1 <- metadata_hourly$stations |> filter(id %in% location_ids1) |> pull(valid_from) |> max() |> date()
      max_date1 <- Sys.Date() - 1
      location_server <- paste0("station_ids=", location_ids1, collapse = "&")
    }
    
    
    list(location_server = location_server)
  })
  
  hour_data <- reactive({
    location_server <- location_data()$location_server
    varsRaw <- metadata_hourly$parameters |> filter(long_name %in% input$variables_hourly) |> dplyr::select(name) 
    
    varsRaw<- varsRaw[,1]
    
    vars <- paste0("parameters=", varsRaw, collapse = "&")
    start_date <- format(Sys.time() - days(14), format = "%Y-%m-%dT%H%%3A%M")
    end_date <- format(Sys.time(),format = "%Y-%m-%dT%H%%3A%M")
    link_hourly <- paste0("https://dataset.api.hub.geosphere.at/v1/station/historical/klima-v2-1h?", vars, "&start=", start_date, "&end=", end_date, "&", location_server, "&output_format=csv")
    
    df_hour <- read_csv(link_hourly, show_col_types = FALSE)
    names(df_hour)[3]<-"var"
    df_hour <- df_hour |> mutate(
      datetime = as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      date = date(time), 
      year = year(date),
      month = month(date),
      yearday = yday(date),
      day = day(date),
      hour = hour(datetime)
    )
    
    
    
    
    list(df_hour = df_hour, varsRaw = varsRaw)
  })
  
  output$plot_aktuell <- renderPlotly({
    ggplotly(
      hour_data()$df_hour|> 
        ggplot() +
        # geom_area(aes(x = time, y = tlmax), fill = "red",alpha =.2) + 
        # geom_area(aes(x = time, y = tl), fill = "white")+
        # geom_area(aes(x = time, y = tl), fill = "blue", alpha =.6) +
        # geom_area(aes(x = time, y = tlmin), fill = "white")+
        
        geom_line(aes(x = time, y = var), col = "black") +
        # geom_line(aes(x = time, y = tlmax), col = "red") + 
        # geom_line(aes(x = time, y = tlmin), col = "blue") +
        
        geom_smooth(aes(x = time, y = var), col = "orange", linetype = "dashed", size =.5, se = F)+
        labs(x = "Datum", y = hour_data()$varsRaw) +
        scale_x_datetime(
          date_breaks = "1 day",
          date_labels = "%d.%m"
        ) +
        theme_bw()
    )
  })
  
  
  
  
  
  daily_data <- reactive({
    varsRaw_daily <- metadata_daily$parameters |> filter(long_name %in% input$variables_daily) |> dplyr::select(name) 
    
    varsRaw_daily<- varsRaw_daily[,1]
    
    vars_daily <- paste0("parameters=", varsRaw_daily, collapse = "&")
    
    
    
    link_daily <- paste0("https://dataset.api.hub.geosphere.at/v1/station/historical/klima-v2-1d?", vars_daily, "&start=",format(ymd_hms("1985-01-01T00:00:00"),format = "%Y-%m-%dT%H%%3A%M"), "&end=", format(Sys.time(),format = "%Y-%m-%dT%H%%3A%M"), "&", location_data()$location_server, "&output_format=csv")
    
    df_daily <- read_csv(link_daily, show_col_types = FALSE)
    names(df_daily)[3]<-"var"
    
    df_daily <- df_daily |> mutate(
      datetime = as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      date = date(time), 
      year = year(date),
      month = month(date),
      yearday = yday(date),
      day = day(date),
      hour = hour(datetime)
    )
    
    
    
    df_daily
  })
  
  observe({
    if (input$switch == "Smooth") {
      output$plot_history <- renderPlotly({
        ggplotly(
          daily_data() |>mutate(is_currentyear= ifelse(year==year(Sys.time()),year(Sys.time()),paste(min(year),max(year),sep ="-")))|> 
            filter(is.finite(var)) |> 
            ggplot(aes(x = yearday, y = var, colour = factor(year),group = year), size = .2) +
            scale_colour_manual(values = c("grey","red"))+
            geom_line(aes(col=is_currentyear ),method = "loess", se = FALSE, na.rm = TRUE) + 
            #labs(x = "Yearday", y = input$variables_daily, color = "Year") +
            theme_bw()
        )
      })
    } else {
      output$plot_history <- renderPlotly({
        daily_data() |>group_by(year,yearday)|>
          summarise(var = mean(var,na.rm=T))|> 
          plot_ly(x = ~yearday, y = ~factor(year), z = ~var, type = "heatmap", colorscale = "Plasma")
      })
    }
    
    
    
  })
  
  trend_data <- reactive({
    if (input$location != "ÖSTERREICH") {
      location2 <- metadata_daily$stations |> filter(type == "COMBINED") |> filter(name == input$location) |> pull(id) |> min()
      varsRaw2 <- metadata_daily$parameters |> filter(long_name %in% input$variables2) |> dplyr::select(name) |> unlist()
      vars2 <- paste0("parameters=", varsRaw2, collapse = "&")
      min_date <- metadata_daily$stations |> filter(id == location2) |> pull(valid_from) |> as.Date()
      max_date <- metadata_daily$stations |> filter(id == location2) |> pull(valid_to) |> as.Date()
      max_date <- min(max_date, Sys.Date() - 1)
      
      updateSliderInput(
        inputId = "year_slider",
        min = year(min_date),
        max = year(max_date)
      )
      
      link2 <- paste0("https://dataset.api.hub.geosphere.at/v1/station/historical/klima-v2-1d?", vars2, "&start=", min_date, "&end=", max_date, "&station_ids=", location2, "&output_format=csv")
      df2 <- read_csv(link2)
      
      names(df2)[3] <- "var"
      
      start_date2 <- paste0(input$year_slider[1], "-01-01")
      end_date2 <- paste0(input$year_slider[2], "-01-01")
      
      link_heatwave <- paste0("https://dataset.api.hub.geosphere.at/v1/station/historical/klima-v2-1d?parameters=tlmax&start=", start_date2, "&end=", end_date2, "&station_ids=", location2, "&output_format=csv")
      df_heatwave <- read_csv(link_heatwave)
      
      df_heatwave <- df_heatwave |>
        mutate(over_30 = tlmax > 30, date = as.Date(time)) |>
        arrange(date) |>
        mutate(
          is_consecutive = (date - lag(date, default = first(date))) == 1 & over_30
        ) |>
        mutate(
          streak_group = cumsum(!is_consecutive | is.na(is_consecutive))
        ) |>
        group_by(streak_group) |>
        filter(over_30) |>
        mutate(streak_length = n()) |>
        ungroup() |>
        filter(streak_length >= 3) |>
        mutate(year = year(date)) |>
        group_by(year) |>
        reframe(heatwaves = n_distinct(streak_group))
      
    } else {
      location_ids <-  metadata_10min$stations|> filter(name %in% avail_stations)|> select(id)|>unlist()|>array()|> sample(length(avail_stations)/2) #this is for performance reasons. Can cause missinformation!!!!
      location_names <- metadata_daily$stations |> filter(id %in% location_ids) |> pull(name)
      min_date <- metadata_daily$stations |> filter(id %in% location_ids) |> pull(valid_from) |> max() |> date()
      max_date <- Sys.Date() - 1
      
      updateSliderInput(
        inputId = "year_slider",
        min = year(min_date),
        max = year(max_date)
      )
      
      start_date2 <- paste0(input$year_slider[1], "-01-01")
      end_date2 <- paste0(input$year_slider[2], "-01-01")
      
      varsRaw2 <- metadata_daily$parameters |> filter(long_name %in% input$variables2) |> dplyr::select(name) |> unlist()
      vars2 <- paste0("parameters=", varsRaw2, collapse = "&")
      location2 <- paste0("station_ids=", location_ids, collapse = "&")
      
      link2 <- paste0("https://dataset.api.hub.geosphere.at/v1/station/historical/klima-v2-1d?", vars2, "&start=", min_date, "&end=", max_date, "&", location2, "&output_format=csv")
      df2 <- read_csv(link2)
      
      names(df2)[3] <- "var"
      
      df2 <- df2 |> group_by(time) |> reframe(var = mean(var, na.rm = TRUE))
      
      link_heatwave <- paste0("https://dataset.api.hub.geosphere.at/v1/station/historical/klima-v2-1d?parameters=tlmax&start=", start_date2, "&end=", end_date2, "&", location2, "&output_format=csv")
      df_heatwave <- read_csv(link_heatwave)
      
      df_heatwave <- df_heatwave |>
        mutate(over_30 = tlmax > 30, date = as.Date(time)) |>
        mutate(
          is_consecutive = (date - lag(date, default = first(date))) == 1 & over_30
        ) |>
        mutate(
          streak_group = cumsum(!is_consecutive | is.na(is_consecutive))
        ) |>
        group_by(streak_group) |>
        filter(over_30) |>
        mutate(streak_length = n()) |>
        ungroup() |>
        filter(streak_length >= 3) |>
        mutate(year = year(date)) |>
        group_by(year) |>
        reframe(heatwaves = n_distinct(streak_group))
    }
    
    df2 <- df2 |> mutate(date = date(time),
                         yearday = yday(date),
                         year = year(date),
                         month = month(date),
                         day = day(date))
    
    df_mean_temps <- df2 |> group_by(year) |> reframe(mean_temps = mean(var, na.rm = TRUE))
    df_mean_temps <- df_mean_temps |> mutate(mean_temps = mean_temps - mean(mean_temps, na.rm = TRUE))
    
    
    list(df_mean_temps = df_mean_temps, df_heatwave = df_heatwave, start_date2 = start_date2, end_date2 = end_date2)
  })
  
  output$plot1 <- renderPlotly({
    ggplotly(trend_data()$df_mean_temps |> select(year,mean_temps)|> filter(year >= year(as.Date(trend_data()$start_date2)) & year <= year(as.Date(trend_data()$end_date2))) |> ggplot() +
               geom_col(aes(x = year, y = mean_temps, fill = mean_temps)) + 
               scale_fill_gradient(low = "blue", high = "red") +
               labs(y = "Abweichung vom Durchschnitt", fill = "Abweichung vom Durchschnitt") +
               scale_x_continuous(breaks = seq(min(trend_data()$df_mean_temps$year), max(trend_data()$df_mean_temps$year), by = 5), labels = as.character(seq(min(trend_data()$df_mean_temps$year), max(trend_data()$df_mean_temps$year), by = 5))) +
               geom_smooth(aes(x = year, y = mean_temps), colour = "black", linewidth = 0.5, method = "lm", se = FALSE, na.rm = TRUE) +
               theme_bw())
  })
  
  output$text2 <- renderText({ c("Abweichung des Parameters ", input$variables2, " vom Gesamtdurchschnitt in ", input$location) })
  output$text_heatwave <- renderText({ "Anzahl der Hitzewelle pro Jahr" })
  output$text_heatwave_def <- renderText({ "Eine Hitzewelle wird festgestellt, sobald an mindestens drei Tagen in Folge die Maximaltemperatur 30 °C überschritten wird" })
  
  output$heatwave_plot <- renderPlotly({
    ggplotly(trend_data()$df_heatwave |> select(year,heatwaves)|> ggplot(aes(x = year, y = heatwaves, fill = heatwaves)) +
               geom_col() + 
               scale_fill_continuous(low = "yellow", high = "red") +
               labs(y = "Anzahl der Hitzewellen", x = "Year", fill = "Anzahl") +
               scale_x_continuous(breaks = seq(min(trend_data()$df_heatwave$year), max(trend_data()$df_heatwave$year), by = 5), labels = as.character(seq(min(trend_data()$df_heatwave$year), max(trend_data()$df_heatwave$year), by = 5))) +
               scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
               theme_bw())
  })
  
  
  output$plot_missing <- renderPlotly({
    ggplotly(df_daily |>distinct()|> mutate(name = reorder(name, .data[[input$sort_missing_by_variable]])) |> ggplot()+ 
               geom_tile(aes(x= time, y= name , fill = var))+
               scale_fill_gradientn(colors = hcl.colors(20, "ag_Sunset")))
  })
  
  
  three_data <- reactive({
    df_elevation <- read_rds("df_elevation.rds") |>
      mutate(
        X = round((X - 500) / 1000) * 1000 + 500,
        Y = round((Y - 500) / 1000) * 1000 + 500
      )
    
    
    df_3d <- slopes |>
      mutate(
        X = round((X - 500) / 1000) * 1000 + 500,
        Y = round((Y - 500) / 1000) * 1000 + 500
      ) |>
      select(X, Y, slope) |>
      left_join(
        df_elevation |>
          select(X, Y, altitude),
        by = c("X", "Y")
      ) |>
      drop_na(slope, altitude) |>
      group_by(X, Y) |>
      summarise(
        altitude = mean(altitude, na.rm = TRUE),
        slope = mean(slope, na.rm = TRUE),
        .groups = "drop"
      )
    
    
    model1 <- lm(slope ~ altitude, data = df_3d)
    model2 <- lm(slope ~ altitude + X + Y, data = df_3d)
    
    model_spatial <- mgcv::bam(
      slope ~ s(altitude, k = 10) + s(X, Y, k = 30),
      data = df_3d,
      method = "fREML",
      discrete = TRUE
    )
    
    
    covariance <- cov(df_3d$altitude, df_3d$slope, use = "complete.obs")
    correlation <- cor(df_3d$altitude, df_3d$slope, use = "complete.obs")
    
    effect_raw <- coef(model1)["altitude"] * 1000
    effect_spatial <- coef(model2)["altitude"] * 1000
    
    reduction <- (1 - abs(effect_spatial / effect_raw)) * 100
    
    
    slope_stats <- paste0(
      "Covariance: ", round(covariance, 4),
      " | Correlation: ", round(correlation, 3),
      " | R² Altitude: ", round(summary(model1)$r.squared, 3),
      " | R² Altitude + X/Y: ", round(summary(model2)$r.squared, 3),
      " | GAM explained deviance: ", round(summary(model_spatial)$dev.expl * 100, 1), "%"
    )
    
    slope_findings <- paste0(
      "The raw altitude effect is ", round(effect_raw, 5),
      " °C/year per 1000m. After controlling for X and Y it is ",
      round(effect_spatial, 5), " °C/year per 1000m, a reduction of ",
      round(reduction, 1), "%. The GAM shows that the remaining altitude relationship is nonlinear."
    )
    
    
    df_3d <- df_3d |>
      mutate(
        residual_altitude = residuals(model1)
      )
    
    
    altitude_pred <- tibble(
      altitude = seq(
        min(df_3d$altitude),
        max(df_3d$altitude),
        length.out = 300
      ),
      X = median(df_3d$X),
      Y = median(df_3d$Y)
    )
    
    pred <- predict(
      model_spatial,
      newdata = altitude_pred,
      type = "terms",
      terms = "s(altitude)",
      se.fit = TRUE
    )
    
    altitude_pred <- altitude_pred |>
      mutate(
        fit = as.numeric(pred$fit),
        se = as.numeric(pred$se.fit),
        lower = fit - 1.96 * se,
        upper = fit + 1.96 * se
      )
    
    
    effect_df <- tibble(
      model = c("Altitude only", "Altitude + X/Y"),
      effect = c(effect_raw, effect_spatial)
    )
    
    
    plot_500m <- df_3d |>
      mutate(
        altitude_group = floor(altitude / 500) * 500
      ) |>
      group_by(altitude_group) |>
      summarise(
        mean_slope = mean(slope, na.rm = TRUE),
        median_slope = median(slope, na.rm = TRUE),
        n = n(),
        .groups = "drop"
      ) |>
      ggplot(aes(altitude_group, mean_slope)) +
      geom_line() +
      geom_point(aes(size = n)) +
      scale_size_continuous(name = "Raster cells") +
      labs(
        x = "Altitude [m]",
        y = "Mean slope"
      ) +
      theme_bw()
    
    
    plot_altitude_slope <- df_3d |>
      ggplot(aes(altitude, slope)) +
      stat_bin_2d(bins = 70) +
      geom_smooth(
        method = "lm",
        se = FALSE,
        colour = "red",
        linewidth = .7
      ) +
      labs(
        x = "Altitude [m]",
        y = "Temperature slope",
        fill = "Raster cells"
      ) +
      theme_bw()
    
    
    plot_altitude_gam <- altitude_pred |>
      ggplot(aes(altitude, fit)) +
      geom_ribbon(
        aes(ymin = lower, ymax = upper),
        fill = "grey",
        alpha = .4
      ) +
      geom_line() +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(
        x = "Altitude [m]",
        y = "Partial effect on temperature slope"
      ) +
      theme_bw()
    
    
    plot_altitude_effect <- effect_df |>
      ggplot(aes(model, effect, fill = model)) +
      geom_col() +
      geom_hline(yintercept = 0) +
      labs(
        x = "",
        y = "Slope effect per 1000m"
      ) +
      theme_bw() +
      theme(legend.position = "none")
    
    
    plot_residual_map <- df_3d |>
      ggplot(aes(X, Y, fill = residual_altitude)) +
      geom_tile(width = 1000, height = 1000) +
      coord_equal() +
      scale_fill_gradient2(
        low = "blue",
        mid = "white",
        high = "red",
        midpoint = 0
      ) +
      labs(
        fill = "Residual",
        x = "X",
        y = "Y"
      ) +
      theme_bw()
    
    
    x <- sort(unique(df_3d$X))
    y <- sort(unique(df_3d$Y))
    
    z_altitude <- matrix(
      NA_real_,
      nrow = length(y),
      ncol = length(x)
    )
    
    z_slope <- matrix(
      NA_real_,
      nrow = length(y),
      ncol = length(x)
    )
    
    ix <- match(df_3d$X, x)
    iy <- match(df_3d$Y, y)
    
    z_altitude[cbind(iy, ix)] <- df_3d$altitude
    z_slope[cbind(iy, ix)] <- df_3d$slope
    
    
    colours <- c(
      "#3C0217",
      "#550C26",
      "#6B1631",
      "#84203E",
      "#9B294C",
      "#AA4C4D",
      "#B76D4C",
      "#BB8553",
      "#BA9860",
      "#BEA875",
      "#A8A57C",
      "#859783",
      "#618886",
      "#417C8B",
      "#287390",
      "#276587",
      "#285A7E",
      "#254E75",
      "#26426E",
      "#2F4774",
      "#415B85",
      "#88A2CA",
      "#91AED3",
      "#C4D3EC",
      "#DCE8F7"
    ) |> rev()
    
    
    colorscale <- Map(
      function(pos, col) {
        list(pos, col)
      },
      seq(0, 1, length.out = length(colours)),
      colours
    )
    
    
    p3d <- plot_ly(
      x = x,
      y = y,
      z = z_altitude,
      surfacecolor = z_slope,
      type = "surface",
      colorscale = colorscale,
      colorbar = list(
        title = "Temperature slope"
      ),
      hovertemplate = paste(
        "X: %{x}<br>",
        "Y: %{y}<br>",
        "Altitude: %{z} m<br>",
        "Slope: %{surfacecolor}<br>",
        "<extra></extra>"
      )
    ) |>
      layout(
        scene = list(
          xaxis = list(
            title = "X"
          ),
          yaxis = list(
            title = "Y"
          ),
          zaxis = list(
            title = "Altitude [m]"
          ),
          aspectmode = "manual",
          aspectratio = list(
            x = 2,
            y = 1,
            z = .1
          )
        ),
        margin = list(
          l = 0,
          r = 0,
          b = 0,
          t = 30
        )
      )
    
    
    rm(model1, model2, model_spatial, pred)
    gc()
    
    
    list(
      plot_500m = plot_500m,
      plot_altitude_slope = plot_altitude_slope,
      plot_altitude_gam = plot_altitude_gam,
      plot_altitude_effect = plot_altitude_effect,
      plot_residual_map = plot_residual_map,
      slope_stats = slope_stats,
      slope_findings = slope_findings,
      p3d = p3d
    )
  })
  
  output$plot_per_500m <- renderPlot({
    three_data()$plot_500m
  })
  
  output$plot_altitude_slope <- renderPlot({
    three_data()$plot_altitude_slope
  })
  
  output$plot_altitude_gam <- renderPlot({
    three_data()$plot_altitude_gam
  })
  
  output$plot_altitude_effect <- renderPlot({
    three_data()$plot_altitude_effect
  })
  
  output$plot_residual_map <- renderPlot({
    three_data()$plot_residual_map
  })
  
  output$slope_stats <- renderText({
    three_data()$slope_stats
  })
  
  output$slope_findings <- renderText({
    three_data()$slope_findings
  })
  
  output$plot3d <- renderPlotly({
    three_data()$p3d
  })
  
  
  
}

shinyApp(ui = ui, server = server)
