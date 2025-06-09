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

if (!require(terra)) {install.packages("terra"); library(terra)}




metadata <- fromJSON("https://dataset.api.hub.geosphere.at/v1/station/historical/synop-v1-1h/metadata")
metadata2 <- fromJSON("https://dataset.api.hub.geosphere.at/v1/station/historical/klima-v1-1d/metadata")
metadata_monthly <- fromJSON("https://dataset.api.hub.geosphere.at/v1/station/historical/klima-v2-1m/metadata")

## custom functions for workaround of shiny because it cant load terra or raster
#' Return the xmin value from a bounding-box data frame
xmin <- function(df, row = 1) {
  df[row, "xmin"][[1]]
}

#' Return the xmax value from a bounding-box data frame
xmax <- function(df, row = 1) {
  df[row, "xmax"][[1]]
}

#' Return the ymin value from a bounding-box data frame
ymin <- function(df, row = 1) {
  df[row, "ymin"][[1]]
}

#' Return the ymax value from a bounding-box data frame
ymax <- function(df, row = 1) {
  df[row, "ymax"][[1]]
}

ui <- navbarPage(
  title = "GeoVis",
  tabPanel("Current",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               selectInput(inputId = "location",
                           label = "Location",
                           choices = c("ÖSTERREICH", fromJSON("https://dataset.api.hub.geosphere.at/v1/station/historical/synop-v1-1h/metadata")$stations |> 
                                         filter(is_active == "TRUE" & name != "MAYRHOFEN") |> pull(name)),
                           selected = "SALZBURG-FLUGHAFEN"
               ),
               selectInput(inputId  = "variables",
                           label = "Variables",
                           choices = fromJSON("https://dataset.api.hub.geosphere.at/v1/station/historical/synop-v1-1h/metadata")$parameters |> 
                             filter(!(unit %in% c("Code (Synop)", NA, "Code", ""))) |> dplyr::select(long_name) |>
                             c("Höhe der tiefsten Wolken", "Neuschneehöhe"),
                           selected = "Lufttemperatur"
               )
             ),
             mainPanel( 
               navset_tab(
                 nav_panel("Aktuell",
                           plotlyOutput(outputId = "plot_aktuell"),
                           tableOutput(outputId = "table_aktuell")
                 ),
                 nav_panel("History",
                           selectInput("switch", "Switch Visualisation", choices = c("Smooth", "Tiles")),
                           selectInput("start_date_history", "Start Date", selected = 2000, choices = seq(1900, 2024)),
                           plotlyOutput(outputId = "plot_history"),
                           textOutput("text_Location1"),
                           tableOutput(outputId = "table"),
                           textOutput("text")
                 )
               )
             )
           )
  ),
  tabPanel("Trends",
           nav_panel("Task 2",
                     sidebarLayout(
                       sidebarPanel(
                         selectInput(inputId = "location2",
                                     label = "Location",
                                     choices = c("ÖSTERREICH", sort(fromJSON("https://dataset.api.hub.geosphere.at/v1/station/historical/klima-v1-1d/metadata")$station |> 
                                                                      filter(type == "COMBINED") |> pull(name))),
                                     selected = fromJSON("https://dataset.api.hub.geosphere.at/v1/station/historical/klima-v1-1d/metadata")$station |> 
                                       filter(type == "COMBINED") |> dplyr::select(name) |> head(1)
                         ),
                         sliderInput(
                           inputId = "year_slider",
                           label = "Years",
                           min = 1990,
                           max = 2024,
                           value = c(1990, 2024),
                           dragRange = TRUE,
                           sep = ""
                         )
                       ),
                       mainPanel(
                         navset_tab(
                           nav_panel("Trend",
                                     selectInput(inputId  = "variables2",
                                                 label = "Variable",
                                                 choices = fromJSON("https://dataset.api.hub.geosphere.at/v1/station/historical/klima-v1-1d/metadata")$parameters |>
                                                   filter(!(unit %in% c("Code (Synop)", NA, "Code", "", "°"))) |> dplyr::select(long_name) |> filter(!grepl("Beobachtungstermin", long_name)) |> 
                                                   c("Gesamtschneehöhe zum Beobachtungstermin I"),
                                                 selected = "Lufttemperaturmittel 2m"),
                                     textOutput("text2"),
                                     plotOutput("plot1")
                           ),
                           nav_panel("Hitzewellen",
                                     textOutput("text_heatwave"),
                                     plotOutput(outputId = "heatwave_plot"),
                                     textOutput("text_heatwave_def")
                           )
                         )
                       )
                     )
           )
  ),
  tabPanel("Slopes",
           navset_tab(
             nav_panel("Slopes",
                       div({ plotlyOutput("plot_slopes") })
             ),
             nav_panel("Slopes Comparison",
                       fluidRow(
                         column(
                           width = 3,
                           sliderInput("time_multiplyer", "Time:", min = 1, max = 10, value = 1),
                           checkboxInput("weighted_switch", label = "Weighted", value = FALSE)
                         ),
                         column(
                           width = 9,
                           plotlyOutput("plot_lin_reg")
                         )
                       )
             ),
             nav_panel("Slopes LinReg",     
                       checkboxInput("weighted_switch2", label = "Weighted", value = FALSE),
                       fluidRow({ plotlyOutput("slope_wsmooth") })
             )
           )
  ),
  tabPanel("Seasons",
           navset_tab(
             nav_panel("Jahreszeiten",
                       div({ plotlyOutput("plot_wiso") })
             ),
             nav_panel("Seasonal Slopes",
                       div({ plotlyOutput("seasonal_slopes") })
             ),
             nav_panel("Seasonal Slopes Weighted",
                       div({ plotlyOutput("seasonal_slopes_weighted") })
             )
           )
  ),
  tabPanel("3D",
           fluidRow(checkboxInput("weighted_switch3", label = "Weighted", value = FALSE)),
           fluidRow({ plotlyOutput("map_with_slopes") })
  )
)

server <- function(input, output, session) {
  
  observe({
    
    if (input$location != "ÖSTERREICH") {
      location_server <- metadata$stations |> filter(name %in% input$location) |> dplyr::select(id) |> toString()
      location_server <- paste0("station_ids=", location_server)
      min_date1 <- metadata$stations |> filter(id == location_server) |> pull(valid_from) |> as.Date()
      max_date1 <- metadata$stations |> filter(id == location_server) |> pull(valid_to) |> as.Date()
    } else {
      location_ids1 <- c(11010, 11036, 11101, 11120, 11150, 11175, 11185, 11231)
      min_date1 <- metadata$stations |> filter(id %in% location_ids1) |> pull(valid_from) |> max() |> date()
      max_date1 <- Sys.Date() - 1
      location_server <- paste0("station_ids=", location_ids1, collapse = "&")
    }
    
    varsRaw <- metadata$parameters |> filter(long_name %in% input$variables) |> dplyr::select(name) |> unlist()
    vars <- paste0("parameters=", varsRaw, collapse = "&")
    start_date <- paste0(input$start_date_history, "-01-01")
    end_date <- Sys.Date() - 1
    link <- paste0("https://dataset.api.hub.geosphere.at/v1/station/historical/synop-v1-1h?", vars, "&start=", start_date, "&end=", end_date, "&", location_server, "&output_format=csv")
    
    df <- read_csv(link, show_col_types = FALSE)
    names(df)[3] <- "var" 
    df <- df |> group_by(time) |> reframe(var = mean(var, na.rm = TRUE))
    df <- df |> mutate(
      datetime = as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      date = date(time), 
      year = year(date),
      month = month(date),
      yearday = yday(date),
      day = day(date),
      hour = hour(datetime)
    )
    
    df_plot_aktuell <- df |> filter(date >= Sys.Date() - days(14)) |> group_by(date) |> reframe(
      max_var = max(var, na.rm = TRUE), 
      min_var = min(var, na.rm = TRUE), 
      mean_var = mean(var, na.rm = TRUE)
    )
    
    output$plot_aktuell <- renderPlotly({
      ggplotly(
        df_plot_aktuell |> ggplot() + 
          geom_line(aes(x = date, y = mean_var), col = "black") +
          geom_line(aes(x = date, y = max_var), col = "red") + 
          geom_line(aes(x = date, y = min_var), col = "blue") +
          scale_x_date(breaks = seq(min(df_plot_aktuell$date), max(df_plot_aktuell$date), by = "day"), date_labels = "%d %b") +
          labs(x = "Date", y = input$variables) +
          theme_bw()
      )
    })
    
    output$table_aktuell <- renderTable({
      df_plot_aktuell |> 
        pivot_longer(cols = c(max_var, mean_var, min_var), names_to = "variable", values_to = "value") |>
        pivot_wider(names_from = date, values_from = value)
    })
    
    df_sorted <- df |> mutate(time = format(time, "%d-%m-%Y")) |> 
      dplyr::select(c(time, var)) |> rename("date" = time) |> 
      arrange(-var)
    
    output$table <- renderTable({
      head(df_sorted)
    })
    
    output$text <- renderText({ names(df[[varsRaw]]) })
    output$text_Location1 <- renderText({ input$location })
    
    if (input$switch == "Smooth") {
      output$plot_history <- renderPlotly({
        ggplotly(
          df |> filter(is.finite(var)) |> ggplot(aes(x = yearday, y = var, colour = factor(year))) + 
            scale_colour_viridis_d(option = "plasma", direction = -1) +
            geom_smooth(method = "loess", se = FALSE, na.rm = TRUE) + 
            labs(x = "Yearday", y = input$variables, color = "Year") + 
            theme_bw()
        )
      })
    } else {
      output$plot_history <- renderPlotly({
        df |> plot_ly(x = ~yearday, y = ~factor(year), z = ~var, type = "heatmap", colorscale = "Plasma")
      })
    }
    
  })
  
  # Task 2
  observe({
    
    if (input$location2 != "ÖSTERREICH") {
      location2 <- metadata2$stations |> filter(type == "COMBINED") |> filter(name == input$location2) |> pull(id) |> min()
      varsRaw2 <- metadata2$parameters |> filter(long_name %in% input$variables2) |> dplyr::select(name) |> unlist()
      vars2 <- paste0("parameters=", varsRaw2, collapse = "&")
      min_date <- metadata2$stations |> filter(id == location2) |> pull(valid_from) |> as.Date()
      max_date <- metadata2$stations |> filter(id == location2) |> pull(valid_to) |> as.Date()
      max_date <- min(max_date, Sys.Date() - 1)
      
      updateSliderInput(
        inputId = "year_slider",
        min = year(min_date),
        max = year(max_date)
      )
      
      link2 <- paste0("https://dataset.api.hub.geosphere.at/v1/station/historical/klima-v1-1d?", vars2, "&start=", min_date, "&end=", max_date, "&station_ids=", location2, "&output_format=csv")
      df2 <- read_csv(link2)
      
      names(df2)[3] <- "var"
      
      start_date2 <- paste0(input$year_slider[1], "-01-01")
      end_date2 <- paste0(input$year_slider[2], "-01-01")
      
      link_heatwave <- paste0("https://dataset.api.hub.geosphere.at/v1/station/historical/klima-v1-1d?parameters=tmax&start=", start_date2, "&end=", end_date2, "&station_ids=", location2, "&output_format=csv")
      df_heatwave <- read_csv(link_heatwave)
      
      df_heatwave <- df_heatwave |>
        mutate(over_30 = tmax > 30, date = as.Date(time)) |>
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
      location_ids <- c(15, 39, 131, 100, 26, 166, 105, 154)
      location_names <- metadata2$stations |> filter(id %in% location_ids) |> pull(name)
      min_date <- metadata2$stations |> filter(id %in% location_ids) |> pull(valid_from) |> max() |> date()
      max_date <- Sys.Date() - 1
      
      updateSliderInput(
        inputId = "year_slider",
        min = year(min_date),
        max = year(max_date)
      )
      
      start_date2 <- paste0(input$year_slider[1], "-01-01")
      end_date2 <- paste0(input$year_slider[2], "-01-01")
      
      varsRaw2 <- metadata2$parameters |> filter(long_name %in% input$variables2) |> dplyr::select(name) |> unlist()
      vars2 <- paste0("parameters=", varsRaw2, collapse = "&")
      location2 <- paste0("station_ids=", location_ids, collapse = "&")
      
      link2 <- paste0("https://dataset.api.hub.geosphere.at/v1/station/historical/klima-v1-1d?", vars2, "&start=", min_date, "&end=", max_date, "&", location2, "&output_format=csv")
      df2 <- read_csv(link2)
      
      names(df2)[3] <- "var"
      
      df2 <- df2 |> group_by(time) |> reframe(var = mean(var, na.rm = TRUE))
      
      link_heatwave <- paste0("https://dataset.api.hub.geosphere.at/v1/station/historical/klima-v1-1d?parameters=tmax&start=", start_date2, "&end=", end_date2, "&", location2, "&output_format=csv")
      df_heatwave <- read_csv(link_heatwave)
      
      df_heatwave <- df_heatwave |>
        mutate(over_30 = tmax > 30, date = as.Date(time)) |>
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
    
    output$plot1 <- renderPlot({
      df_mean_temps |> filter(year >= year(as.Date(start_date2)) & year <= year(as.Date(end_date2))) |> ggplot() +
        geom_col(aes(x = year, y = mean_temps, fill = mean_temps)) + 
        scale_fill_gradient(low = "blue", high = "red") +
        labs(y = "Abweichung vom Durchschnitt", fill = "Abweichung vom Durchschnitt") +
        scale_x_continuous(breaks = seq(min(df_mean_temps$year), max(df_mean_temps$year), by = 5), labels = as.character(seq(min(df_mean_temps$year), max(df_mean_temps$year), by = 5))) +
        geom_smooth(aes(x = year, y = mean_temps), colour = "black", linewidth = 0.5, method = "lm", se = FALSE, na.rm = TRUE) +
        theme_bw()
    })
    
    output$text2 <- renderText({ c("Abweichung des Parameters ", input$variables2, " vom Gesamtdurchschnitt in ", input$location2) })
    output$text_heatwave <- renderText({ "Anzahl der Hitzewelle pro Jahr" })
    output$text_heatwave_def <- renderText({ "Eine Hitzewelle wird festgestellt, sobald an mindestens drei Tagen in Folge die Maximaltemperatur 30 °C überschritten wird" })
    
    output$heatwave_plot <- renderPlot({
      df_heatwave |> ggplot(aes(x = year, y = heatwaves, fill = heatwaves)) +
        geom_col() + 
        scale_fill_continuous(low = "yellow", high = "red") +
        labs(y = "Anzahl der Hitzewellen", x = "Year", fill = "Anzahl") +
        scale_x_continuous(breaks = seq(min(df_heatwave$year), max(df_heatwave$year), by = 5), labels = as.character(seq(min(df_heatwave$year), max(df_heatwave$year), by = 5))) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
        theme_bw()
    })
  })
  
  ## Task 4
  observe({
    varsRaw <- c("tl_mittel")
    locations <- metadata_monthly$stations |> filter(type == "COMBINED") |> pull(id)
    start_date <- "1900-01-01"
    end_date <- Sys.Date() - days(1)
    location_server_2 <- paste0("station_ids=", locations, collapse = "&")
    vars <- paste0("parameters=", varsRaw, collapse = "&")
    link <- paste0("https://dataset.api.hub.geosphere.at/v1/station/historical/klima-v2-1m?", vars, "&start=", start_date, "&end=", end_date, "&", location_server_2, "&output_format=csv")
    
    df1 <- read_csv(link)
    df <- df1
    df <- df |> rename("id" = station) 
    df <- left_join(df, metadata_monthly$stations |> filter(is_active == "TRUE" & type == "COMBINED"))
    names(df)[3] <- "var"
    mean_var <- mean(df$var, na.rm = TRUE)
    
    df <- df |> mutate(datetime = as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                       date = date(time), 
                       year = year(date),
                       month = month(date),
                       yearday = yday(date),
                       day = day(date),
                       hour = hour(datetime))
    
    df <- df |> arrange(altitude) |>
      mutate(ordered_name = factor(paste(name, altitude), levels = unique(paste(name, altitude))))
    
    get_slope <- function(x) {
      x <- x |> mutate(min_time = x |> filter(!is.na(var)) |> pull(time) |> min(),
                       time_numeric = as.numeric(difftime(time, min_time, units = "days")) / 365.25)
      coef(lm(var ~ time_numeric, data = drop_na(x, var)))["time_numeric"]
    }
    
    get_intercept <- function(x) {
      x <- x |> mutate(time_numeric = as.numeric(difftime(time, min(time), units = "days")) / 365.25)
      coef(lm(var ~ time_numeric, data = drop_na(x, var)))["(Intercept)"]
    }
    
    non_na_count <- df |> group_by(id) |> reframe(nonNacount = sum(!is.na(var))) |> as.data.frame()
    df_first_non_na <- df |> 
      filter(!is.na(var)) |>              
      group_by(id) |>                          
      reframe(first_date = min(time), last_date = max(time)) |>     
      ungroup()
    
    df <- left_join(df, non_na_count)
    df <- left_join(df, df_first_non_na)
    
    max_time_range <- df |> 
      group_by(id) |> 
      reframe(time_range_years = as.double((max(time) - first_date) / 365)) |> 
      pull(time_range_years) |> 
      max(na.rm = TRUE)
    
    timesteps <- n_distinct(df$time)[1]
    
    df_linreg <- df |> group_by(id) |> reframe(
      time_range_months = as.double((max(time) - first_date) / 12),
      time_range_years = as.double((max(time) - first_date) / 365),
      slope = as.double(get_slope(pick(everything()))),
      slope_weighted = slope * (nonNacount / timesteps)
    ) |> unique()
    
    df_linreg <- left_join(df, df_linreg)
    
    output$plot_slopes <- renderPlotly({
      ggplotly(df |> ggplot(aes(x = time, y = (altitude + var), colour = ordered_name)) +
                 geom_smooth(method = "lm", se = FALSE, na.rm = TRUE))
    })
    
    output$plot_lin_reg <- renderPlotly({
      if (!input$weighted_switch) {
        df_linreg |> dplyr::select(altitude, name, slope) |> unique() |>
          plot_ly(x = ~altitude, y = ~slope * input$time_multiplyer, text = ~name, hovertext = ~name, hoverinfo = ~name, type = "bar", marker = list(color = "orange")) |>
          layout(title = "Change per 10-Years in °C")
      } else {        
        df_linreg |> dplyr::select(altitude, name, slope_weighted) |> unique() |>
          plot_ly(x = ~altitude, y = ~slope_weighted * input$time_multiplyer, text = ~name, hovertext = ~name, hoverinfo = ~name, type = "bar", marker = list(color = "orange")) |>
          layout(title = "Change per 10-Years in °C")
      }
    })
    
    output$slope_wsmooth <- renderPlotly({
      slope_col <- if (!input$weighted_switch2) "slope" else "slope_weighted"
      ggplotly(df_linreg |> dplyr::select(altitude, name, slope, slope_weighted) |> unique() |>
                 ggplot(aes(x = altitude, y = .data[[slope_col]])) +
                 geom_col(colour = "orange") +
                 geom_smooth(method = "lm", se = FALSE, na.rm = TRUE))
    })
    
    df_wiso <- df1
    df_wiso <- df_wiso |> rename("id" = station)
    df_wiso <- left_join(df_wiso, metadata_monthly$stations |> filter(is_active == "TRUE" & type == "COMBINED"))
    names(df_wiso)[3] <- "var"
    
    df_wiso <- df_wiso |> mutate(datetime = as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                 date = date(time), 
                                 year = year(date),
                                 month = month(date),
                                 yearday = yday(date),
                                 day = day(date),
                                 hour = hour(datetime))
    
    df_wiso <- df_wiso |> filter(!is.na(var)) |>
      mutate(season = case_when(month %in% c(12, 1, 2)  ~ "winter",
                                month %in% 3:5          ~ "spring",
                                month %in% 6:8          ~ "summer",
                                month %in% 9:11         ~ "fall"))
    
    output$plot_wiso <- renderPlotly({
      df_wiso |> dplyr::select(time, var, id, season) |> unique() |>
        ggplot(aes(x = time, y = var, col = season)) +
        geom_line() +
        facet_wrap(~season)
    })
    
    non_na_count_wiso <- df_wiso |> group_by(id, season) |> reframe(nonNacount = sum(!is.na(var))) |> as.data.frame()
    df_first_non_na_wiso <- df_wiso |> 
      filter(!is.na(var)) |>              
      group_by(id, season) |>                          
      reframe(first_date = min(time)) |>     
      ungroup()
    
    df_wiso <- left_join(df_wiso, non_na_count_wiso)
    df_wiso <- left_join(df_wiso, df_first_non_na_wiso)
    
    timesteps_wiso <- n_distinct(df_linreg$time)[1]
    
    df_linreg_wiso <- df_wiso |> group_by(season, id) |> reframe(
      time_range_months = as.double((max(time) - first_date) / 12),
      time_range_years = as.double((max(time) - first_date) / 365),
      slope = as.double(get_slope(pick(everything()))),
      slope_weighted = slope * (nonNacount / timesteps_wiso) * 12
    ) |> unique()
    
    df_linreg_wiso <- left_join(df_wiso, df_linreg_wiso)
    
    output$seasonal_slopes <- renderPlotly({
      ggplotly(df_linreg_wiso |> dplyr::select(altitude, name, slope, season) |> unique() |>
                 ggplot(aes(x = altitude, y = slope, colour = season)) +
                 geom_col() +
                 geom_smooth(method = "lm", se = FALSE, colour = "black", linewidth = 0.2, na.rm = TRUE) +
                 facet_wrap(~season) +
                 theme_dark())
    })
    
    output$seasonal_slopes_weighted <- renderPlotly({
      ggplotly(df_linreg_wiso |> dplyr::select(altitude, name, slope_weighted, season) |> unique() |>
                 ggplot(aes(x = altitude, y = slope_weighted, colour = season)) +
                 geom_col() +
                 geom_smooth(method = "lm", se = FALSE, colour = "black", linewidth = 0.2, na.rm = TRUE) +
                 facet_wrap(~season) +
                 theme_dark())
    })
    
    data <- df_linreg %>%
      dplyr::select(lat, lon, altitude, tot = slope) %>%
      unique() %>%
      na.omit()
    
    elevation_matrix <- readRDS("elevation_matrix.rds")
    
    output$austria3d <- renderPlotly({
      plot_ly(z = ~(elevation_matrix), type = "surface") |>
        layout(
          scene = list(
            aspectmode = "manual",
            aspectratio = list(x = 10, y = 5, z = 0.5),
            camera = list(
              eye = list(x = 0, y = -6, z = 5),
              center = list(x = 0, y = 0, z = 0),
              up = list(x = 0, y = 0, z = 1)
            )
          )
        )
    })
    
    coords_map <- which(elevation_matrix == 3547, arr.ind = TRUE)
    coords_df <- df_linreg |> filter(altitude == max(altitude, na.rm = TRUE)) |> dplyr::select(lat, lon) |> unique() |> ungroup()
    x_scaler <- coords_map[1] / coords_df[2]
    y_scaler <- coords_map[2] / coords_df[1]
    
    raster_extent <- data.frame(
      xmin = 9.4,
      xmax = 17.3,
      ymin = 46.2,
      ymax = 49.2
    )
    
    points_df <- data.frame(
      lon = df_linreg$lon |> na.omit(),
      lat = df_linreg$lat |> na.omit()
    )
    
    within_extent <- points_df$lon >= xmin(raster_extent) & points_df$lon <= xmax(raster_extent) &
      points_df$lat >= ymin(raster_extent) & points_df$lat <= ymax(raster_extent)
    ### zusatz 05-06-2025
    
    srtm_austria <-rast("elevation/AUT_elv_msk.tif")
    #AUT_elv_msk.tif
    srtm_austria_cropped <- srtm_austria
    
    points_vect <- vect(points_df, geom = c("lon", "lat"), crs = crs(srtm_austria_cropped))
    
    elevation_values <- extract(srtm_austria_cropped, points_vect)
    ### zusatz Ende
    #elevation_values <- readRDS("elevation_values.rds")
    
    
    
    points_with_elevation <- cbind(points_df, elevation = elevation_values[,2])
    
    res_x <- 0.008333333
    res_y <- 0.008333333
    origin_x <- xmin(raster_extent)
    origin_y <- ymax(raster_extent)
    points_df$col_index <- floor((points_df$lon - origin_x) / res_x) + 1
    points_df$row_index <- floor((origin_y - points_df$lat) / res_y) + 1
    nrows <- nrow(elevation_matrix)
    points_df$matrix_row_index <- nrows - points_df$row_index + 1
    points_df$matrix_col_index <- points_df$col_index
    
    valid_indices <- points_df$matrix_row_index >= 1 & points_df$matrix_row_index <= nrows &
      points_df$matrix_col_index >= 1 & points_df$matrix_col_index <= ncol(elevation_matrix)
    
    elevation_from_matrix <- mapply(function(row, col) {
      if (row >= 1 && row <= nrows && col >= 1 && col <= ncol(elevation_matrix)) {
        elevation_matrix[row, col]
      } else {
        NA
      }
    }, points_df$matrix_row_index, points_df$matrix_col_index)
    
    points_df$elevation_matrix <- elevation_from_matrix
    
    points_df <- points_df |> left_join(df_linreg |> dplyr::select(lat, lon, id, slope, slope_weighted, altitude, name) |> na.omit() |> unique())
    
    custom_colorscale <- list(
      c(0, "cyan"),
      c(0.3, "yellow"),
      c(0.6, "orange"),
      c(1, "red")
    )
    
    plot_map <- plot_ly(z = ~(elevation_matrix), type = "surface", showscale = FALSE) |>
      layout(
        scene = list(
          aspectmode = "manual",
          aspectratio = list(x = 10, y = 5, z = 0.5),
          camera = list(
            eye = list(x = 0, y = -6, z = 5),
            center = list(x = 0, y = 0, z = 0),
            up = list(x = 0, y = 0, z = 1)
          )
        )
      )
    
    observeEvent(input$weighted_switch3, {
      if (!input$weighted_switch3) {
        plot_slopes <- points_df %>%
          dplyr::select(matrix_col_index, matrix_row_index, altitude, slope, slope_weighted, name, elevation_matrix) %>%
          unique() %>%
          plot_ly(
            x = ~matrix_col_index,
            y = ~matrix_row_index,
            z = ~elevation_matrix + 250,
            hovertext = ~paste("name:", name, "\nslope:", slope),
            type = "scatter3d",
            mode = "markers",
            marker = list(
              size = 3,
              color = ~slope * 10,
              colorscale = custom_colorscale,
              colorbar = list(title = "Slope")
            )
          )
      } else {
        plot_slopes <- points_df %>%
          dplyr::select(matrix_col_index, matrix_row_index, altitude, slope, slope_weighted, name, elevation_matrix) %>%
          unique() %>%
          plot_ly(
            x = ~matrix_col_index,
            y = ~matrix_row_index,
            z = ~elevation_matrix + 250,
            hovertext = ~paste("name:", name, "\nslope:", slope),
            type = "scatter3d",
            mode = "markers",
            marker = list(
              size = 3,
              color = ~slope_weighted * 10,
              colorscale = custom_colorscale,
              colorbar = list(title = "Slope")
            )
          )
      }
      
      output$map_with_slopes <- renderPlotly({
        subplot(plot_slopes, plot_map) |> layout(scene = list(
          xaxis = list(title = ""),
          yaxis = list(title = ""),
          zaxis = list(title = ""),
          showlegend = FALSE
        ))
      })
    })
  })
}

shinyApp(ui = ui, server = server)
