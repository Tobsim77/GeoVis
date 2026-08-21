app_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

source(file.path(app_dir, "R", "helpers.R"), local = TRUE)
source(file.path(app_dir, "R", "analysis.R"), local = TRUE)

load_packages()

cache_dir <- file.path(app_dir, "cache")
data_dir <- locate_data_root(app_dir)
metadata_bundle <- load_metadata_bundle(cache_dir)

overview_location_choices <- available_synop_locations(metadata_bundle$synop)
overview_variable_choices <- available_synop_variables(metadata_bundle$synop)
trend_location_choices <- available_daily_locations(metadata_bundle$daily)
trend_variable_choices <- available_daily_variables(metadata_bundle$daily)

default_overview <- if ("SALZBURG-FLUGHAFEN" %in% overview_location_choices) "SALZBURG-FLUGHAFEN" else overview_location_choices[[1]]
default_trend <- if ("SALZBURG-FLUGHAFEN" %in% trend_location_choices) "SALZBURG-FLUGHAFEN" else trend_location_choices[[max(1, min(2, length(trend_location_choices)))]]
default_overview_variable <- if ("Lufttemperatur" %in% overview_variable_choices) "Lufttemperatur" else overview_variable_choices[[1]]
default_trend_variable <- if ("Lufttemperaturmittel 2m" %in% trend_variable_choices) "Lufttemperaturmittel 2m" else trend_variable_choices[[1]]
default_overview_info <- resolve_synop_selection(metadata_bundle$synop, default_overview)
default_trend_info <- resolve_daily_selection(metadata_bundle$daily, default_trend)

ui <- shiny::tagList(
  tags$head(tags$style(HTML(app_css()))),
  navbarPage(
    title = "GeoVis Optimized",
    id = "main_nav",
    theme = app_theme(),
    windowTitle = "GeoVis Optimized",
    header = div(
      class = "app-shell",
      div(
        class = "section-intro",
        "A cleaner GeoSphere dashboard copy with cached API calls, less redundant recomputation, and a more focused layout for quick climate exploration."
      )
    ),
    tabPanel(
      "Overview",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4("Current & History"),
          p("This view reuses one hourly dataset for both the short-term and long-term panels, so changing the chart style no longer triggers another API request."),
          selectInput("overview_location", "Location", choices = overview_location_choices, selected = default_overview),
          selectInput("overview_variable", "Variable", choices = overview_variable_choices, selected = default_overview_variable),
          sliderInput(
            "overview_start_year",
            "Start year",
            min = lubridate::year(default_overview_info$available_start),
            max = lubridate::year(default_overview_info$available_end),
            value = max(lubridate::year(default_overview_info$available_start), 2000),
            step = 1,
            sep = ""
          ),
          radioButtons(
            "overview_history_style",
            "History style",
            choices = c("Smooth", "Tiles"),
            selected = "Smooth",
            inline = TRUE
          )
        ),
        mainPanel(
          width = 9,
          uiOutput("overview_metrics"),
          textOutput("overview_caption"),
          tabsetPanel(
            id = "overview_tabs",
            tabPanel("Recent", plotlyOutput("overview_recent_plot", height = "360px"), tableOutput("overview_recent_table")),
            tabPanel("History", plotlyOutput("overview_history_plot", height = "520px"), tableOutput("overview_ranked_table"))
          )
        )
      )
    ),
    tabPanel(
      "Trends",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4("Daily Climate Trends"),
          p("Yearly anomalies are computed from one cached daily series per selection. The heatwave chart uses a separate, small tmax request scoped to your year range."),
          selectInput("trend_location", "Location", choices = trend_location_choices, selected = default_trend),
          selectInput("trend_variable", "Variable", choices = trend_variable_choices, selected = default_trend_variable),
          sliderInput(
            "trend_years",
            "Year range",
            min = lubridate::year(default_trend_info$available_start),
            max = lubridate::year(default_trend_info$available_end),
            value = c(max(lubridate::year(default_trend_info$available_start), 1990), lubridate::year(default_trend_info$available_end)),
            step = 1,
            sep = ""
          )
        ),
        mainPanel(
          width = 9,
          uiOutput("trend_metrics"),
          textOutput("trend_caption"),
          tabsetPanel(
            tabPanel("Anomalies", plotlyOutput("trend_anomaly_plot", height = "420px")),
            tabPanel("Heatwaves", plotlyOutput("trend_heatwave_plot", height = "420px"), textOutput("trend_heatwave_note"))
          )
        )
      )
    ),
    tabPanel(
      "Altitude & Terrain",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4("Monthly Elevation Analysis"),
          p("This section is prepared lazily and cached for a week, because the monthly all-station dataset is the most expensive part of the app."),
          selectInput(
            "terrain_sort_by",
            "Sort missing-data heatmap by",
            choices = c("Coverage count" = "non_na_count", "Altitude" = "altitude", "Time range" = "time_range_years", "Name" = "name"),
            selected = "time_range_years"
          ),
          sliderInput("terrain_year_multiplier", "Show change per n years", min = 1, max = 10, value = 1),
          checkboxInput("terrain_weighted", "Use weighted slope", value = FALSE),
          checkboxInput("terrain_map_weighted", "Use weighted slope in 3D map", value = FALSE)
        ),
        mainPanel(
          width = 9,
          uiOutput("terrain_metrics"),
          tabsetPanel(
            tabPanel("Coverage", plotOutput("terrain_missing_plot", height = "620px")),
            tabPanel("Station slopes", plotlyOutput("terrain_slope_plot", height = "460px"), plotOutput("terrain_profile_plot", height = "360px")),
            tabPanel("Seasonal slopes", plotOutput("terrain_seasonal_plot", height = "560px")),
            tabPanel("3D map", plotOutput("terrain_map_plot", height = "700px"))
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$overview_location, {
    info <- resolve_synop_selection(metadata_bundle$synop, input$overview_location)
    current_value <- input$overview_start_year %||% lubridate::year(info$available_start)
    current_value <- max(min(current_value, lubridate::year(info$available_end)), lubridate::year(info$available_start))

    updateSliderInput(
      session,
      "overview_start_year",
      min = lubridate::year(info$available_start),
      max = lubridate::year(info$available_end),
      value = current_value
    )
  }, ignoreInit = TRUE)

  observeEvent(input$trend_location, {
    info <- resolve_daily_selection(metadata_bundle$daily, input$trend_location)
    current_value <- input$trend_years %||% c(lubridate::year(info$available_start), lubridate::year(info$available_end))
    current_value[1] <- max(current_value[1], lubridate::year(info$available_start))
    current_value[2] <- min(current_value[2], lubridate::year(info$available_end))

    updateSliderInput(
      session,
      "trend_years",
      min = lubridate::year(info$available_start),
      max = lubridate::year(info$available_end),
      value = current_value
    )
  }, ignoreInit = TRUE)

  overview_dataset <- eventReactive(
    list(input$overview_location, input$overview_variable, input$overview_start_year),
    {
      withProgress(message = "Loading hourly observations", value = 0.2, {
        info <- resolve_synop_selection(metadata_bundle$synop, input$overview_location)
        parameter_name <- parameter_name_from_label(metadata_bundle$synop, input$overview_variable)
        start_date <- as.Date(sprintf("%s-01-01", input$overview_start_year))
        start_date <- max(start_date, info$available_start)

        incProgress(0.4)
        raw <- fetch_synop_history(
          cache_dir = cache_dir,
          parameter_name = parameter_name,
          station_ids = info$station_ids,
          start_date = start_date,
          end_date = info$available_end
        )

        incProgress(0.3)
        history <- prepare_observation_data(raw, aggregate_multiple = info$aggregate)

        list(
          info = info,
          history = history,
          recent = compute_recent_summary(history),
          ranked = compute_ranked_days(history),
          start_date = min(history$date, na.rm = TRUE),
          end_date = max(history$date, na.rm = TRUE)
        )
      })
    },
    ignoreInit = FALSE
  )

  output$overview_metrics <- renderUI({
    dataset <- overview_dataset()
    history <- dataset$history

    tags$div(
      class = "metric-grid",
      metric_card("Selection", dataset$info$label, if (dataset$info$aggregate) paste(dataset$info$station_count, "stations averaged") else "Single station"),
      metric_card("Loaded period", format(dataset$start_date, "%Y"), pretty_span(dataset$start_date, dataset$end_date)),
      metric_card("Observations", scales::comma(nrow(history)), paste("Latest day:", format(max(history$date, na.rm = TRUE), "%d %b %Y")))
    )
  })

  output$overview_caption <- renderText({
    dataset <- overview_dataset()
    paste(input$overview_variable, "for", dataset$info$label, "- one cached dataset powers both tabs.")
  })

  output$overview_recent_plot <- renderPlotly({
    recent <- overview_dataset()$recent
    validate(need(nrow(recent) > 0, "No recent data available for this selection."))

    p <- ggplot2::ggplot(recent, ggplot2::aes(x = date)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = min_value, ymax = max_value), fill = "#D9E8F2", alpha = 0.9) +
      ggplot2::geom_line(ggplot2::aes(y = mean_value), colour = "#0B5D7A", linewidth = 1) +
      ggplot2::geom_line(ggplot2::aes(y = max_value), colour = "#B33A3A", linewidth = 0.55) +
      ggplot2::geom_line(ggplot2::aes(y = min_value), colour = "#335C67", linewidth = 0.55) +
      ggplot2::scale_x_date(date_labels = "%d %b") +
      ggplot2::labs(x = NULL, y = input$overview_variable) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

    ggplotly(p, tooltip = c("x", "y"))
  })

  output$overview_recent_table <- renderTable({
    recent <- overview_dataset()$recent
    recent |>
      dplyr::mutate(
        max_value = round(max_value, 2),
        mean_value = round(mean_value, 2),
        min_value = round(min_value, 2)
      )
  })

  output$overview_history_plot <- renderPlotly({
    history <- overview_dataset()$history
    validate(need(nrow(history) > 0, "No history data available for this selection."))

    if (identical(input$overview_history_style, "Smooth")) {
      p <- ggplot2::ggplot(history, ggplot2::aes(x = yearday, y = value, colour = year, group = year)) +
        ggplot2::geom_smooth(se = FALSE, method = "loess", linewidth = 0.55, alpha = 0.75) +
        ggplot2::scale_colour_gradientn(colours = hcl.colors(8, "YlGnBu"), guide = "none") +
        ggplot2::labs(x = "Day of year", y = input$overview_variable) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

      return(ggplotly(p, tooltip = c("x", "y")))
    }

    plot_ly(
      data = history,
      x = ~yearday,
      y = ~year,
      z = ~value,
      type = "heatmap",
      colorscale = "YlGnBu",
      hovertemplate = "Day %{x}<br>Year %{y}<br>Value %{z:.2f}<extra></extra>"
    ) |>
      layout(
        xaxis = list(title = "Day of year"),
        yaxis = list(title = "Year")
      )
  })

  output$overview_ranked_table <- renderTable({
    overview_dataset()$ranked
  })

  trend_base_dataset <- eventReactive(
    list(input$trend_location, input$trend_variable),
    {
      withProgress(message = "Loading daily trend series", value = 0.2, {
        info <- resolve_daily_selection(metadata_bundle$daily, input$trend_location)
        parameter_name <- parameter_name_from_label(metadata_bundle$daily, input$trend_variable)

        incProgress(0.4)
        raw <- fetch_daily_history(
          cache_dir = cache_dir,
          parameter_name = parameter_name,
          station_ids = info$station_ids,
          start_date = info$available_start,
          end_date = info$available_end
        )

        incProgress(0.3)
        history <- prepare_observation_data(raw, aggregate_multiple = info$aggregate)
        yearly <- compute_yearly_anomalies(history)

        list(info = info, history = history, yearly = yearly)
      })
    },
    ignoreInit = FALSE
  )

  trend_heatwave_dataset <- eventReactive(
    list(input$trend_location, input$trend_years),
    {
      withProgress(message = "Loading heatwave data", value = 0.35, {
        info <- resolve_daily_selection(metadata_bundle$daily, input$trend_location)
        year_range <- input$trend_years

        raw <- fetch_daily_history(
          cache_dir = cache_dir,
          parameter_name = "tmax",
          station_ids = info$station_ids,
          start_date = as.Date(sprintf("%s-01-01", year_range[[1]])),
          end_date = min(as.Date(sprintf("%s-12-31", year_range[[2]])), info$available_end)
        )

        compute_heatwave_counts(raw)
      })
    },
    ignoreInit = FALSE
  )

  output$trend_metrics <- renderUI({
    dataset <- trend_base_dataset()
    selected_years <- input$trend_years
    filtered <- dataset$yearly |>
      dplyr::filter(year >= selected_years[[1]], year <= selected_years[[2]])

    if (nrow(filtered) == 0) {
      return(tags$div(
        class = "metric-grid",
        metric_card("Selection", dataset$info$label, "No annual values in the selected range"),
        metric_card("Shown years", paste(selected_years[[1]], "-", selected_years[[2]]), "0 annual values"),
        metric_card("Largest anomaly", "n/a", "Pick a wider range")
      ))
    }

    hottest <- filtered |>
      dplyr::slice_max(anomaly, n = 1, with_ties = FALSE)

    tags$div(
      class = "metric-grid",
      metric_card("Selection", dataset$info$label, if (dataset$info$aggregate) paste(dataset$info$station_count, "stations averaged") else "Single station"),
      metric_card("Shown years", paste(selected_years[[1]], "-", selected_years[[2]]), paste(nrow(filtered), "annual values")),
      metric_card("Largest anomaly", scales::number(hottest$anomaly[[1]], accuracy = 0.01), paste("Year", hottest$year[[1]]))
    )
  })

  output$trend_caption <- renderText({
    paste(
      "Yearly anomaly for", input$trend_variable,
      "in", input$trend_location,
      "relative to the full loaded station mean."
    )
  })

  output$trend_anomaly_plot <- renderPlotly({
    dataset <- trend_base_dataset()
    selected_years <- input$trend_years

    filtered <- dataset$yearly |>
      dplyr::filter(year >= selected_years[[1]], year <= selected_years[[2]])

    validate(need(nrow(filtered) > 0, "No anomaly data available for the selected years."))

    p <- ggplot2::ggplot(filtered, ggplot2::aes(x = year, y = anomaly)) +
      ggplot2::geom_col(ggplot2::aes(fill = anomaly)) +
      ggplot2::geom_smooth(
        ggplot2::aes(group = 1),
        method = "lm",
        se = FALSE,
        colour = "#1D2730",
        linewidth = 0.7,
        inherit.aes = TRUE
      ) +
      ggplot2::scale_fill_gradient2(low = "#335C67", mid = "#F6F3EC", high = "#B33A3A", midpoint = 0) +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(10)) +
      ggplot2::labs(x = NULL, y = "Deviation from long-term mean", fill = "Deviation") +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

    ggplotly(p, tooltip = c("x", "y"))
  })

  output$trend_heatwave_plot <- renderPlotly({
    heatwaves <- trend_heatwave_dataset()
    validate(need(nrow(heatwaves) > 0, "No heatwaves detected in the selected year range."))

    p <- ggplot2::ggplot(heatwaves, ggplot2::aes(x = year, y = heatwaves, fill = heatwaves)) +
      ggplot2::geom_col() +
      ggplot2::scale_fill_gradient(low = "#F2C14E", high = "#B33A3A") +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(10)) +
      ggplot2::labs(x = NULL, y = "Heatwaves", fill = "Count") +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

    ggplotly(p, tooltip = c("x", "y"))
  })

  output$trend_heatwave_note <- renderText({
    "Heatwave definition: at least three consecutive days with tmax above 30 C."
  })

  terrain_dataset <- reactive({
    withProgress(message = "Preparing monthly altitude analytics", value = 0.2, {
      get_monthly_analysis(cache_dir = cache_dir, data_dir = data_dir, metadata = metadata_bundle$monthly)
    })
  })

  output$terrain_metrics <- renderUI({
    dataset <- terrain_dataset()

    tags$div(
      class = "metric-grid",
      metric_card("Stations", dplyr::n_distinct(dataset$slope_summary$id), "Combined monthly stations"),
      metric_card("Date span", paste(format(min(dataset$monthly$date), "%Y"), "-", format(max(dataset$monthly$date), "%Y")), "Loaded once and cached"),
      metric_card("Map points", nrow(dataset$map_points), "Ready for the 3D terrain overlay")
    )
  })

  output$terrain_missing_plot <- renderPlot({
    dataset <- terrain_dataset()
    missing_data <- dataset$missing_data

    if (identical(input$terrain_sort_by, "name")) {
      missing_data <- missing_data |>
        dplyr::mutate(label_ordered = reorder(label, label))
    } else {
      missing_data <- missing_data |>
        dplyr::mutate(label_ordered = reorder(label, .data[[input$terrain_sort_by]]))
    }

    p <- ggplot2::ggplot(missing_data, ggplot2::aes(x = time, y = label_ordered, fill = has_data)) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_manual(values = c("TRUE" = "#0B5D7A", "FALSE" = "#E7EEF3")) +
      ggplot2::labs(x = NULL, y = NULL, fill = "Data available") +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(panel.grid = ggplot2::element_blank())

    print(p)
  }, res = 110)

  output$terrain_slope_plot <- renderPlotly({
    dataset <- terrain_dataset()
    slope_col <- if (isTRUE(input$terrain_weighted)) "slope_weighted" else "slope"

    plot_data <- dataset$slope_summary |>
      dplyr::mutate(display_slope = .data[[slope_col]] * input$terrain_year_multiplier)

    plot_ly(
      data = plot_data,
      x = ~altitude,
      y = ~display_slope,
      type = "scatter",
      mode = "markers",
      color = ~display_slope,
      colors = c("#335C67", "#F6F3EC", "#B33A3A"),
      text = ~paste0(
        label,
        "<br>Altitude: ", round(altitude), " m",
        "<br>Change: ", scales::number(display_slope, accuracy = 0.001)
      ),
      hoverinfo = "text"
    ) |>
      layout(
        xaxis = list(title = "Altitude (m)"),
        yaxis = list(title = paste("Change per", input$terrain_year_multiplier, "years"))
      )
  })

  output$terrain_profile_plot <- renderPlot({
    dataset <- terrain_dataset()
    slope_col <- if (isTRUE(input$terrain_weighted)) "slope_weighted" else "slope"

    p <- ggplot2::ggplot(dataset$slope_summary, ggplot2::aes(x = altitude, y = .data[[slope_col]])) +
      ggplot2::geom_point(colour = "#0B5D7A", size = 2.6) +
      ggplot2::geom_smooth(method = "lm", se = FALSE, colour = "#B33A3A", linewidth = 0.9) +
      ggplot2::labs(x = "Altitude (m)", y = "Slope") +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

    print(p)
  }, res = 110)

  output$terrain_seasonal_plot <- renderPlot({
    dataset <- terrain_dataset()
    slope_col <- if (isTRUE(input$terrain_weighted)) "slope_weighted" else "slope"

    p <- ggplot2::ggplot(dataset$seasonal_summary, ggplot2::aes(x = altitude, y = .data[[slope_col]])) +
      ggplot2::geom_col(ggplot2::aes(fill = season)) +
      ggplot2::geom_smooth(
        ggplot2::aes(group = 1),
        method = "lm",
        se = FALSE,
        colour = "#1D2730",
        linewidth = 0.35,
        inherit.aes = TRUE
      ) +
      ggplot2::facet_wrap(~season, scales = "free_y") +
      ggplot2::scale_fill_manual(values = c("Winter" = "#335C67", "Spring" = "#6C9A8B", "Summer" = "#C07A00", "Autumn" = "#B85C38")) +
      ggplot2::labs(x = "Altitude (m)", y = "Slope", fill = "Season") +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

    print(p)
  }, res = 110)

  output$terrain_map_plot <- renderPlot({
    dataset <- terrain_dataset()
    surface_data <- downsample_surface_data(dataset$elevation_matrix, dataset$map_points, max_dim = 220)
    points <- surface_data$map_points
    slope_col <- if (isTRUE(input$terrain_map_weighted)) "slope_weighted" else "slope"

    if (nrow(points) == 0) {
      plot.new()
      text(0.5, 0.5, "No mappable stations found.", col = "#546471", cex = 1.2)
      return(invisible(NULL))
    }

    z_matrix_raw <- surface_data$elevation_matrix
    z_floor <- min(z_matrix_raw, na.rm = TRUE)
    z_matrix <- z_matrix_raw
    z_matrix[!is.finite(z_matrix)] <- z_floor
    x_axis <- seq_len(nrow(z_matrix))
    y_axis <- seq_len(ncol(z_matrix))

    terrain_palette <- grDevices::colorRampPalette(
      c("#18471B", "#90B77D", "#D9C7A3", "#8B6F47", "#F5F5F5")
    )(140)
    terrain_cuts <- cut(
      as.vector(z_matrix[-1, -1, drop = FALSE]),
      breaks = length(terrain_palette),
      include.lowest = TRUE
    )
    terrain_cols <- matrix(
      terrain_palette[terrain_cuts],
      nrow = nrow(z_matrix) - 1,
      ncol = ncol(z_matrix) - 1
    )

    point_palette <- grDevices::colorRampPalette(
      c("#1E88E5", "#F3E9C8", "#C0392B")
    )(200)
    point_index <- round(scales::rescale_mid(points[[slope_col]], to = c(1, 200), mid = 0))
    point_index <- pmin(pmax(point_index, 1), 200)
    point_cols <- point_palette[point_index]
    point_cex <- scales::rescale(abs(points[[slope_col]]), to = c(0.55, 1.4))

    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par), add = TRUE)
    par(mar = c(1.5, 1.5, 2.5, 8), xpd = NA)

    perspective <- graphics::persp(
      x = x_axis,
      y = y_axis,
      z = z_matrix,
      col = terrain_cols,
      border = NA,
      shade = 0.25,
      theta = 132,
      phi = 36,
      expand = 0.18,
      ltheta = 115,
      ticktype = "detailed",
      xlab = "",
      ylab = "",
      zlab = "Elevation",
      main = if (isTRUE(input$terrain_map_weighted)) {
        "Terrain with weighted station slopes"
      } else {
        "Terrain with station slopes"
      }
    )

    point_projection <- graphics::trans3d(
      x = points$matrix_row_index_small,
      y = points$matrix_col_index_small,
      z = ifelse(is.finite(points$surface_height), points$surface_height, z_floor) + max(z_matrix, na.rm = TRUE) * 0.04,
      pmat = perspective
    )

    graphics::points(
      point_projection$x,
      point_projection$y,
      pch = 16,
      cex = point_cex,
      col = point_cols
    )

    legend_values <- pretty(range(points[[slope_col]], finite = TRUE), n = 5)
    legend_cols <- point_palette[round(scales::rescale_mid(legend_values, to = c(1, 200), mid = 0))]

    graphics::legend(
      "topright",
      inset = c(-0.18, 0),
      title = if (isTRUE(input$terrain_map_weighted)) "Weighted slope" else "Slope",
      legend = format(round(legend_values, 3), nsmall = 3),
      fill = legend_cols,
      border = NA,
      bty = "n",
      cex = 0.85
    )
  }, res = 110)
}

shinyApp(ui = ui, server = server)
