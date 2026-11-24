library(shiny)
library(dplyr)
library(tidyr)
library(readr)
library(bslib)
library(lubridate)
library(jsonlite)
library(ggplot2)
library(plotly)
library(scales)

options(shiny.maxRequestSize = 20 * 1024^2)

metadata_from <- function(filename, url) {
  if (file.exists(filename)) {
    jsonlite::fromJSON(filename)
  } else {
    jsonlite::fromJSON(url)
  }
}

metadata_10min <- metadata_from(
  "metadata_10min.json",
  "https://dataset.api.hub.geosphere.at/v1/station/historical/klima-v2-10min/metadata"
)
metadata_hourly <- metadata_from(
  "metadata_hourly.json",
  "https://dataset.api.hub.geosphere.at/v1/station/historical/klima-v2-1h/metadata"
)
metadata_daily <- metadata_from(
  "metadata_daily.json",
  "https://dataset.api.hub.geosphere.at/v1/station/historical/klima-v2-1d/metadata"
)
metadata_monthly <- metadata_from(
  "metadata_monthly.json",
  "https://dataset.api.hub.geosphere.at/v1/station/historical/klima-v2-1m/metadata"
)

metadata_hourly$stations <- metadata_hourly$stations |> filter(type == "COMBINED")
metadata_10min$stations <- metadata_10min$stations |> filter(type == "COMBINED")

avail_stations <- bind_rows(
  metadata_10min$stations |> mutate(origin = "10min"),
  metadata_hourly$stations |> mutate(origin = "hour"),
  metadata_daily$stations |> mutate(origin = "daily"),
  metadata_monthly$stations |> mutate(origin = "monthly")
) |>
  group_by(name) |>
  summarise(amount = n_distinct(origin), .groups = "drop") |>
  filter(amount == 4) |>
  pull(name) |>
  sort()

selected_station <- if ("Salzburg Freisaal" %in% avail_stations) {
  "Salzburg Freisaal"
} else if (length(avail_stations) > 0L) {
  avail_stations[1]
} else {
  "ÖSTERREICH"
}

national_ids <- metadata_10min$stations |>
  filter(name %in% avail_stations) |>
  arrange(name, id) |>
  distinct(name, .keep_all = TRUE) |>
  pull(id)
if (length(national_ids) > 0L) {
  national_ids <- national_ids[seq(1L, length(national_ids), by = 2L)]
}

prepared_file <- file.path("data", "geovis_precomputed.rds")

read_geo_csv <- function(url) {
  dat <- readr::read_csv(url, show_col_types = FALSE, progress = FALSE)
  if (ncol(dat) < 3L) stop("Die GeoSphere-API hat weniger als drei Spalten geliefert.")
  names(dat)[3] <- "var"
  dat
}

station_ids <- function(meta, station_name, national = FALSE) {
  if (national) return(national_ids)
  
  ids <- meta$stations |>
    filter(
      .data$name == .env$station_name,
      type == "COMBINED"
    ) |>
    pull(id)
  
  if (length(ids) == 0L) {
    stop("Keine kombinierte Stations-ID gefunden.")
  }
  
  ids[1]
}

id_query <- function(ids) {
  paste(paste0("station_ids=", ids), collapse = "&")
}

station_bounds <- function(station_name) {
  if (station_name == "ÖSTERREICH") {
    candidates <- metadata_daily$stations |>
      filter(id %in% national_ids)
    
    first_date <- suppressWarnings(
      max(as.Date(candidates$valid_from), na.rm = TRUE)
    )
  } else {
    candidates <- metadata_daily$stations |>
      filter(
        .data$name == .env$station_name,
        type == "COMBINED"
      )
    
    first_date <- suppressWarnings(
      min(as.Date(candidates$valid_from), na.rm = TRUE)
    )
  }
  
  last_date <- suppressWarnings(
    min(as.Date(candidates$valid_to), na.rm = TRUE)
  )
  
  if (!is.finite(as.numeric(first_date))) {
    first_date <- as.Date("1990-01-01")
  }
  
  if (!is.finite(as.numeric(last_date))) {
    last_date <- Sys.Date() - 1
  }
  
  last_date <- min(last_date, Sys.Date() - 1)
  
  list(from = first_date, to = last_date)
}

ui <- navbarPage(
  title = "GeoVis",
  id = "main_tab",
  bg = "#3F4C61",
  selected = "Current",
  fillable = FALSE,
  header = shiny::useBusyIndicators(),
  nav_panel(
    "Current",
    plotlyOutput("plot_aktuell"),
    selectInput("location", "Location",
                choices = c("ÖSTERREICH", avail_stations),
                selected = selected_station
    ),
    selectInput("variables_hourly", "Variables",
                choices = sort(metadata_hourly$parameters$long_name),
                selected = "Lufttemperatur 2m"
    ),
    selectInput("switch", "Switch Visualisation", choices = c("Tiles", "Smooth")),
    plotlyOutput("plot_history"),
    selectInput("variables_daily", "Variables",
                choices = sort(metadata_daily$parameters$long_name),
                selected = "Lufttemperatur 2m Mittelwert"
    )
  ),
  nav_panel(
    "Trends",
        sliderInput("year_slider", "Years",
                    min = 1990,
                    max = lubridate::year(Sys.Date()),
                    value = c(1990, lubridate::year(Sys.Date())),
                    dragRange = TRUE,
                    sep = ""
        ),
        navset_tab(
          id = "trend_subtab",
          nav_panel(
            "Trend",
            selectInput("variables2", "Variable",
                        choices = c(
                          metadata_daily$parameters |>
                            filter(!is.na(unit), !(unit %in% c("Code (Synop)", "Code", "", "°"))) |>
                            filter(!grepl("Beobachtungstermin", long_name)) |>
                            pull(long_name),
                          "Gesamtschneehöhe zum Beobachtungstermin I"
                        ),
                        selected = "Lufttemperatur 2m Mittelwert"
            ),
            textOutput("text2"),
            plotlyOutput("plot1")
          ),
          nav_panel(
            "Hitzewellen",
            textOutput("text_heatwave"),
            plotlyOutput("heatwave_plot"),
            textOutput("text_heatwave_def")
          )
        )
      
    
  ),
  nav_panel(
    "Slopes",
    navset_tab(
      id = "slope_subtab",
      nav_panel(
        "500m",
        textOutput("slope_stats"),
        plotOutput("plot_per_500m", width = "100%", height = "80vh")
      ),
      nav_panel(
        "Altitude Relation",
        plotOutput("plot_altitude_slope", width = "100%", height = "80vh")
      ),
      nav_panel(
        "Nonlinear Effect",
        textOutput("slope_findings"),
        plotOutput("plot_altitude_gam", width = "100%", height = "80vh")
      ),
      nav_panel(
        "Spatial Influence",
        plotOutput("plot_altitude_effect", width = "100%", height = "45vh"),
        plotOutput("plot_residual_map", width = "100%", height = "75vh")
      )
    )
  ),
  nav_panel(
    "3D",
    plotlyOutput("plot3d", width = "100%", height = "90vh")
  )
)

server <- function(input, output, session) {
  hour_cache <- reactiveVal(NULL)
  daily_cache <- reactiveVal(NULL)
  trend_cache <- reactiveVal(NULL)
  heat_cache <- reactiveVal(NULL)
  prepared_cache <- reactiveVal(NULL)
  
  observeEvent(input$main_tab, {
    hour_cache(NULL)
    daily_cache(NULL)
    trend_cache(NULL)
    heat_cache(NULL)
    prepared_cache(NULL)
    gc(verbose = FALSE)
  }, ignoreInit = TRUE, priority = 100)
  
  observeEvent(input$slope_subtab, {
    if (identical(input$main_tab, "Slopes")) {
      prepared_cache(NULL)
      gc(verbose = FALSE)
    }
  }, ignoreInit = TRUE, priority = 100)
  
  observeEvent(input$location, {
    bounds <- station_bounds(input$location)
    min_y <- lubridate::year(bounds$from)
    max_y <- lubridate::year(bounds$to)
    if (!is.finite(min_y) || !is.finite(max_y) || min_y > max_y) return()
    lo <- max(min_y, 1990L)
    if (lo > max_y) lo <- min_y
    updateSliderInput(session, "year_slider",
                      min = min_y,
                      max = max_y,
                      value = c(lo, max_y)
    )
  }, ignoreInit = FALSE)
  
  current_ids <- reactive({
    req(input$location)
    if (identical(input$location, "ÖSTERREICH")) {
      national_ids
    } else {
      station_ids(metadata_hourly, input$location)
    }
  })
  
  hour_data <- reactive({
    req(input$main_tab == "Current", input$variables_hourly)
    key <- paste(input$location, input$variables_hourly, Sys.Date(), sep = "|")
    old <- isolate(hour_cache())
    if (!is.null(old) && identical(old$key, key)) return(old$data)
    
    v <- metadata_hourly$parameters |>
      filter(long_name == input$variables_hourly) |>
      pull(name)
    req(length(v) > 0L)
    start_date <- format(Sys.time() - days(14), "%Y-%m-%dT%H%%3A%M")
    end_date <- format(Sys.time(), "%Y-%m-%dT%H%%3A%M")
    link <- paste0(
      "https://dataset.api.hub.geosphere.at/v1/station/historical/klima-v2-1h?",
      "parameters=", v[1], "&start=", start_date,
      "&end=", end_date, "&", id_query(current_ids()),
      "&output_format=csv"
    )
    dat <- read_geo_csv(link)
    if (input$location == "ÖSTERREICH") {
      dat <- dat |>
        group_by(time) |>
        summarise(var = mean(var, na.rm = TRUE), .groups = "drop")
    }
    dat <- dat |>
      mutate(datetime = as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
    result <- list(df_hour = dat, varsRaw = v[1])
    hour_cache(list(key = key, data = result))
    result
  })
  
  daily_data <- reactive({
    req(input$main_tab == "Current", input$variables_daily)
    key <- paste(input$location, input$variables_daily, Sys.Date(), sep = "|")
    old <- isolate(daily_cache())
    if (!is.null(old) && identical(old$key, key)) return(old$data)
    
    v <- metadata_daily$parameters |>
      filter(long_name == input$variables_daily) |>
      pull(name)
    req(length(v) > 0L)
    start_date <- format(as.POSIXct("1985-01-01", tz = "UTC"), "%Y-%m-%dT%H%%3A%M")
    end_date <- format(Sys.time(), "%Y-%m-%dT%H%%3A%M")
    link <- paste0(
      "https://dataset.api.hub.geosphere.at/v1/station/historical/klima-v2-1d?",
      "parameters=", v[1], "&start=", start_date,
      "&end=", end_date, "&", id_query(current_ids()),
      "&output_format=csv"
    )
    dat <- read_geo_csv(link)
    if (input$location == "ÖSTERREICH") {
      dat <- dat |>
        group_by(time) |>
        summarise(var = mean(var, na.rm = TRUE), .groups = "drop")
    }
    dat <- dat |>
      transmute(
        time, var,
        year = lubridate::year(as.Date(time)),
        yearday = lubridate::yday(as.Date(time))
      )
    daily_cache(list(key = key, data = dat))
    dat
  })
  
  output$plot_aktuell <- renderPlotly({
    req(input$main_tab == "Current")
    d <- hour_data()
    p <- ggplot(d$df_hour, aes(datetime, var)) +
      geom_line(colour = "black") +
      geom_smooth(colour = "orange", linetype = "dashed", linewidth = 0.5, se = FALSE) +
      labs(x = "Datum", y = d$varsRaw) +
      scale_x_datetime(date_breaks = "1 day", date_labels = "%d.%m") +
      theme_bw()
    ggplotly(p)
  })
  
  output$plot_history <- renderPlotly({
    req(input$main_tab == "Current")
    dat <- daily_data() |> filter(is.finite(var))
    
    if (input$switch == "Smooth") {
      dat <- dat |>
        mutate(is_currentyear = if_else(
          year == lubridate::year(Sys.Date()),
          "Current", "Historical"
        ))
      
      p <- ggplot(dat, aes(yearday, var, group = year)) +
        geom_line(
          data = filter(dat, is_currentyear == "Historical"),
          aes(colour = is_currentyear),
          linewidth = 0.25,
          na.rm = TRUE
        ) +
        geom_line(
          data = filter(dat, is_currentyear == "Current"),
          aes(colour = is_currentyear),
          linewidth = 0.6,
          na.rm = TRUE
        ) +
        scale_colour_manual(values = c(
          "Historical" = "grey",
          "Current" = "red"
        )) +
        labs(x = "Yearday", y = input$variables_daily, colour = "Year") +
        theme_bw() 
      
      ggplotly(p)
      
    } else {
      heat <- dat |>
        group_by(year, yearday) |>
        summarise(var = mean(var, na.rm = TRUE), .groups = "drop")
      
      plot_ly(heat,
              x = ~yearday, y = ~factor(year), z = ~var,
              type = "heatmap", colorscale = "Plasma"
      )
    }
  })
  
  trend_data <- reactive({
    req(input$main_tab == "Trends", input$trend_subtab == "Trend", input$variables2)
    key <- paste(input$location, input$variables2, sep = "|")
    old <- isolate(trend_cache())
    if (!is.null(old) && identical(old$key, key)) return(old$data)
    
    v <- metadata_daily$parameters |>
      filter(long_name == input$variables2) |>
      pull(name)
    req(length(v) > 0L)
    ids <- if (input$location == "ÖSTERREICH") {
      national_ids
    } else {
      station_ids(metadata_daily, input$location)
    }
    bounds <- station_bounds(input$location)
    link <- paste0(
      "https://dataset.api.hub.geosphere.at/v1/station/historical/klima-v2-1d?",
      "parameters=", v[1],
      "&start=", bounds$from,
      "&end=", bounds$to,
      "&", id_query(ids),
      "&output_format=csv"
    )
    df <- read_geo_csv(link)
    if (input$location == "ÖSTERREICH") {
      df <- df |>
        group_by(time) |>
        summarise(var = mean(var, na.rm = TRUE), .groups = "drop")
    }
    result <- df |>
      mutate(year = lubridate::year(as.Date(time))) |>
      group_by(year) |>
      summarise(mean_temps = mean(var, na.rm = TRUE), .groups = "drop") |>
      mutate(mean_temps = mean_temps - mean(mean_temps, na.rm = TRUE))
    trend_cache(list(key = key, data = result))
    result
  })
  
  heatwave_data <- reactive({
    req(input$main_tab == "Trends", input$trend_subtab == "Hitzewellen")
    req(length(input$year_slider) == 2L)
    
    key <- paste(
      input$location,
      paste(input$year_slider, collapse = "-"),
      sep = "|"
    )
    
    old <- isolate(heat_cache())
    if (!is.null(old) && identical(old$key, key)) {
      return(old$data)
    }
    
    ids <- if (input$location == "ÖSTERREICH") {
      national_ids
    } else {
      station_ids(metadata_daily, input$location)
    }
    
    start_date <- paste0(input$year_slider[1], "-01-01")
    end_date <- paste0(input$year_slider[2], "-12-31")
    
    link <- paste0(
      "https://dataset.api.hub.geosphere.at/v1/station/historical/klima-v2-1d?",
      "parameters=tlmax&start=", start_date,
      "&end=", end_date,
      "&", id_query(ids),
      "&output_format=csv"
    )
    
    df <- readr::read_csv(
      link,
      show_col_types = FALSE,
      progress = FALSE
    )
    
    if (input$location == "ÖSTERREICH") {
      df <- df |>
        group_by(time) |>
        summarise(
          tlmax = mean(tlmax, na.rm = TRUE),
          .groups = "drop"
        )
    }
    
    hot_days <- df |>
      mutate(date = as.Date(time)) |>
      group_by(date) |>
      summarise(
        tlmax = mean(tlmax, na.rm = TRUE),
        .groups = "drop"
      ) |>
      filter(!is.na(date), is.finite(tlmax), tlmax > 30) |>
      arrange(date) |>
      mutate(
        new_wave = is.na(lag(date)) |
          as.numeric(date - lag(date)) != 1,
        wave_id = cumsum(new_wave)
      )
    
    waves <- hot_days |>
      group_by(wave_id) |>
      summarise(
        start = min(date),
        duration = n(),
        .groups = "drop"
      ) |>
      filter(duration >= 3) |>
      mutate(year = lubridate::year(start)) |>
      count(year, name = "heatwaves")
    
    result <- tibble(
      year = seq(input$year_slider[1], input$year_slider[2])
    ) |>
      left_join(waves, by = "year") |>
      mutate(heatwaves = coalesce(heatwaves, 0L))
    
    heat_cache(list(key = key, data = result))
    
    result
    
  })
  
  
  output$heatwave_plot <- renderPlotly({
    req(input$main_tab == "Trends", input$trend_subtab == "Hitzewellen")
    
    df <- heatwave_data()
    
    plot_ly(
      data = df,
      x = ~year,
      y = ~heatwaves,
      type = "bar",
      marker = list(
        color = df$heatwaves,
        colorscale = list(
          list(0, "yellow"),
          list(1, "red")
        ),
        showscale = TRUE,
        colorbar = list(title = "Anzahl")
      )
    ) |>
      layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "Anzahl der Hitzewellen"),
        margin = list(l = 60, r = 80, b = 50, t = 30)
      )
  })
  
  output$plot1 <- renderPlotly({
    req(input$main_tab == "Trends", input$trend_subtab == "Trend")
    df <- trend_data() |>
      filter(year >= input$year_slider[1], year <= input$year_slider[2])
    shiny::validate(need(nrow(df) > 0, "Keine Daten im ausgewählten Zeitraum."))
    p <- ggplot(df, aes(year, mean_temps)) +
      geom_col(aes(fill = mean_temps)) +
      scale_fill_gradient(low = "blue", high = "red") +
      labs(y = "Abweichung vom Durchschnitt", fill = "Abweichung vom Durchschnitt") +
      geom_smooth(colour = "black", linewidth = 0.5, method = "lm", se = FALSE, na.rm = TRUE) +
      theme_bw()+ theme(legend.position = "top")
    ggplotly(p)
  })
  
  output$text2 <- renderText({
    req(input$main_tab == "Trends", input$trend_subtab == "Trend")
    paste("Abweichung des Parameters", input$variables2, "vom Gesamtdurchschnitt in", input$location)
  })
  
  output$text_heatwave <- renderText({
    req(input$main_tab == "Trends", input$trend_subtab == "Hitzewellen")
    "Anzahl der Hitzewellen pro Jahr"
  })
  
  output$text_heatwave_def <- renderText({
    req(input$main_tab == "Trends", input$trend_subtab == "Hitzewellen")
    "Eine Hitzewelle wird festgestellt, sobald an mindestens drei Tagen in Folge die Maximaltemperatur 30 °C überschreitet."
  })
  
  prepared <- reactive({
    req(input$main_tab %in% c("Slopes", "3D"))
    shiny::validate(need(file.exists(prepared_file),
                         "Die vorberechneten Daten fehlen. Bitte prepare_geovis.R lokal ausführen und data/geovis_precomputed.rds mit deployen."
    ))
    old <- isolate(prepared_cache())
    if (!is.null(old)) return(old)
    dat <- readRDS(prepared_file)
    prepared_cache(dat)
    dat
  })
  
  output$slope_stats <- renderText({
    req(input$main_tab == "Slopes", input$slope_subtab == "500m")
    prepared()$slope_stats
  })
  
  output$plot_per_500m <- renderPlot({
    req(input$main_tab == "Slopes", input$slope_subtab == "500m")
    ggplot(prepared()$altitude_500m, aes(altitude_group, mean_slope)) +
      geom_line() +
      geom_point(aes(size = n)) +
      scale_size_continuous(name = "Raster cells") +
      labs(x = "Altitude [m]", y = "Mean slope [°C/year]") +
      theme_bw()
  })
  
  output$plot_altitude_slope <- renderPlot({
    req(input$main_tab == "Slopes", input$slope_subtab == "Altitude Relation")
    ggplot(prepared()$grid, aes(altitude, slope)) +
      stat_bin_2d(bins = 70) +
      geom_smooth(method = "lm", se = FALSE, colour = "red", linewidth = 0.7) +
      labs(x = "Altitude [m]", y = "Temperature slope [°C/year]", fill = "Raster cells") +
      theme_bw()
  })
  
  output$slope_findings <- renderText({
    req(input$main_tab == "Slopes", input$slope_subtab == "Nonlinear Effect")
    prepared()$slope_findings
  })
  
  output$plot_altitude_gam <- renderPlot({
    req(input$main_tab == "Slopes", input$slope_subtab == "Nonlinear Effect")
    ggplot(prepared()$gam_prediction, aes(altitude, fit)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey", alpha = 0.4) +
      geom_line() +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(x = "Altitude [m]", y = "Partial effect on temperature slope [°C/year]") +
      theme_bw()
  })
  
  output$plot_altitude_effect <- renderPlot({
    req(input$main_tab == "Slopes", input$slope_subtab == "Spatial Influence")
    ggplot(prepared()$altitude_effect, aes(model, effect, fill = model)) +
      geom_col() +
      geom_hline(yintercept = 0) +
      labs(x = "", y = "Slope effect [°C/year per 1000m]") +
      theme_bw() +
      theme(legend.position = "none")
  })
  
  output$plot_residual_map <- renderPlot({
    req(input$main_tab == "Slopes", input$slope_subtab == "Spatial Influence")
    ggplot(prepared()$grid, aes(X, Y, fill = residual_altitude)) +
      geom_tile(width = 1000, height = 1000) +
      coord_equal() +
      scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
      labs(fill = "Residual", x = "X", y = "Y") +
      theme_bw()
  })
  
  output$plot3d <- renderPlotly({
    req(input$main_tab == "3D")
    surf <- prepared()$surface
    colours <- rev(c(
      "#3C0217", "#550C26", "#6B1631", "#84203E", "#9B294C", "#AA4C4D",
      "#B76D4C", "#BB8553", "#BA9860", "#BEA875", "#A8A57C", "#859783",
      "#618886", "#417C8B", "#287390", "#276587", "#254E75", "#26426E",
      "#2F4774", "#415B85", "#88A2CA", "#91AED3", "#C4D3EC", "#DCE8F7"
    ))
    colorscale <- Map(
      function(pos, col) list(pos, col),
      seq(0, 1, length.out = length(colours)), colours
    )
    hover_text <- matrix(
      sprintf(
        "Altitude: %.0f m<br>Slope: %.4f °C/year",
        as.vector(surf$altitude),
        as.vector(surf$slope)
      ),
      nrow = nrow(surf$altitude),
      ncol = ncol(surf$altitude)
    )
    
    plot_ly(
      x = surf$x,
      z = surf$altitude,
      surfacecolor = surf$slope,
      type = "surface",
      colorscale = colorscale,
      colorbar = list(
        title = list(text = "Temperature slope [°C/year]", side = "top"),
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = 1.02,
        yanchor = "bottom",
        len = 0.55,
        thickness = 12,
        tickfont = list(size = 10)
      ),
      text = hover_text
    ) |>
      layout(
        scene = list(
          xaxis = list(title = "X"),
          yaxis = list(title = "Y"),
          zaxis = list(title = "Altitude [m]"),
          aspectmode = "manual",
          aspectratio = list(x = 2, y = 1, z = 0.1)
        ),
        margin = list(l = 0, r = 0, b = 0, t = 80)
      )|>config(
        scrollZoom = TRUE,
        displayModeBar = TRUE,
        displaylogo = FALSE,
        responsive = TRUE
      )
  })
}

shinyApp(ui, server)
