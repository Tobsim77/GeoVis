`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) {
    y
  } else {
    x
  }
}

load_packages <- function() {
  required <- c(
    "bslib",
    "dplyr",
    "ggplot2",
    "htmltools",
    "jsonlite",
    "lubridate",
    "plotly",
    "readr",
    "scales",
    "shiny",
    "tidyr"
  )

  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop(
      paste(
        "Missing R packages:",
        paste(missing, collapse = ", "),
        "\nInstall them once before starting the optimized app."
      ),
      call. = FALSE
    )
  }

  invisible(lapply(required, function(pkg) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }))
}

app_theme <- function() {
  bslib::bs_theme(
    version = 5,
    primary = "#0B5D7A",
    secondary = "#E7EEF3",
    success = "#1D6F5E",
    info = "#335C67",
    warning = "#C07A00",
    danger = "#B33A3A",
    bg = "#F6F3EC",
    fg = "#1D2730"
  )
}

app_css <- function() {
  paste(
    ".navbar{background:linear-gradient(135deg,#12324a 0%,#0b5d7a 55%,#2c7a7b 100%);border:none;}",
    ".navbar-default .navbar-brand,.navbar-default .navbar-nav>li>a{color:#f6f3ec!important;}",
    ".navbar-default .navbar-nav>.active>a,.navbar-default .navbar-nav>.active>a:focus,.navbar-default .navbar-nav>.active>a:hover{background:rgba(255,255,255,.12)!important;color:#ffffff!important;}",
    "body{font-family:'Segoe UI Variable Text','Segoe UI','Trebuchet MS',sans-serif;background:radial-gradient(circle at top left,#fffaf0 0%,#f6f3ec 42%,#ecf2f5 100%);}",
    ".app-shell{padding:18px 8px 30px 8px;}",
    ".panel,.well{border:none;border-radius:18px;box-shadow:0 18px 40px rgba(13,35,51,.08);background:rgba(255,255,255,.86);backdrop-filter:blur(6px);}",
    ".well{padding:20px;}",
    ".metric-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(180px,1fr));gap:14px;margin-bottom:18px;}",
    ".metric-card{background:linear-gradient(180deg,rgba(255,255,255,.98) 0%,rgba(240,245,248,.98) 100%);border:1px solid rgba(11,93,122,.10);border-radius:20px;padding:16px 18px;min-height:132px;box-shadow:0 12px 28px rgba(17,37,54,.08);}",
    ".metric-label{font-size:12px;letter-spacing:.12em;text-transform:uppercase;color:#5d7281;margin-bottom:10px;}",
    ".metric-value{font-family:'Trebuchet MS','Segoe UI',sans-serif;font-size:30px;line-height:1.1;font-weight:700;color:#12324a;}",
    ".metric-subtitle{margin-top:8px;color:#546471;font-size:13px;}",
    ".section-intro{margin:2px 0 18px 0;color:#4d606d;max-width:72ch;}",
    ".tabbable>.nav>li>a{border:none;border-radius:999px!important;margin-right:8px;background:rgba(11,93,122,.06);color:#24485c;font-weight:600;}",
    ".tabbable>.nav>li.active>a{background:#12324a!important;color:#fff!important;}",
    ".control-label{font-size:12px;letter-spacing:.08em;text-transform:uppercase;color:#5d7281;}",
    ".form-control,.selectize-input{border-radius:14px!important;border:1px solid rgba(18,50,74,.12)!important;box-shadow:none!important;}",
    ".form-group{margin-bottom:18px;}",
    ".table{background:rgba(255,255,255,.94);}",
    ".shiny-output-error-validation{color:#6a4c00;font-weight:600;}",
    sep = "\n"
  )
}

metric_card <- function(title, value, subtitle = NULL) {
  htmltools::tags$div(
    class = "metric-card",
    htmltools::tags$div(class = "metric-label", title),
    htmltools::tags$div(class = "metric-value", value),
    htmltools::tags$div(class = "metric-subtitle", subtitle %||% "")
  )
}

locate_data_root <- function(app_dir) {
  local_files <- c("elevation_matrix.rds", "raster_extent.rds")
  parent_dir <- normalizePath(file.path(app_dir, ".."), winslash = "/", mustWork = TRUE)

  if (all(file.exists(file.path(app_dir, local_files)))) {
    normalizePath(app_dir, winslash = "/", mustWork = TRUE)
  } else if (all(file.exists(file.path(parent_dir, local_files)))) {
    parent_dir
  } else {
    stop("Could not locate the GeoVis data files next to the optimized app or its parent folder.", call. = FALSE)
  }
}

build_query <- function(params) {
  pieces <- unlist(Map(function(key, value) {
    if (length(value) == 0 || all(is.na(value))) {
      return(character())
    }

    value <- as.character(value)
    stats::setNames(
      paste0(utils::URLencode(key, reserved = TRUE), "=", utils::URLencode(value, reserved = TRUE)),
      NULL
    )
  }, names(params), params), use.names = FALSE)

  paste(pieces, collapse = "&")
}

.geovis_cache <- new.env(parent = emptyenv())

hash_string <- function(x) {
  text <- enc2utf8(paste(x, collapse = "|"))
  ints <- utf8ToInt(text)

  hash_a <- 0
  hash_b <- 0

  for (i in seq_along(ints)) {
    hash_a <- (hash_a * 131 + ints[[i]]) %% 2147483647
    hash_b <- (hash_b * 137 + ints[[i]] + i) %% 2147483647
  }

  paste0(
    sprintf("%08x", as.integer(hash_a)),
    sprintf("%08x", as.integer(hash_b))
  )
}

sanitize_cache_key <- function(key) {
  readable <- gsub("[^A-Za-z0-9]+", "_", key)
  readable <- gsub("^_+|_+$", "", readable)
  readable <- substr(readable, 1, 48)

  if (!nzchar(readable)) {
    readable <- "cache"
  }

  paste0(readable, "_", hash_string(key))
}

read_cached_value <- function(cache_dir, prefix, key, max_age_seconds, loader) {
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  cache_key <- paste(prefix, key, sep = "::")
  now_time <- Sys.time()

  if (exists(cache_key, envir = .geovis_cache, inherits = FALSE)) {
    cached <- get(cache_key, envir = .geovis_cache, inherits = FALSE)
    age <- as.numeric(difftime(now_time, cached$cached_at, units = "secs"))
    if (is.finite(age) && age <= max_age_seconds) {
      return(cached$value)
    }
  }

  cache_file <- file.path(cache_dir, paste0(prefix, "_", sanitize_cache_key(key), ".rds"))
  if (file.exists(cache_file)) {
    age <- as.numeric(difftime(now_time, file.info(cache_file)$mtime, units = "secs"))
    if (is.finite(age) && age <= max_age_seconds) {
      value <- readRDS(cache_file)
      assign(cache_key, list(value = value, cached_at = now_time), envir = .geovis_cache)
      return(value)
    }
  }

  value <- loader()
  saveRDS(value, cache_file)
  assign(cache_key, list(value = value, cached_at = now_time), envir = .geovis_cache)
  value
}

fetch_json_cached <- function(cache_dir, url, ttl_seconds = 6 * 3600) {
  read_cached_value(
    cache_dir = cache_dir,
    prefix = "json",
    key = url,
    max_age_seconds = ttl_seconds,
    loader = function() jsonlite::fromJSON(url)
  )
}

fetch_csv_cached <- function(cache_dir, endpoint, params, ttl_seconds = 24 * 3600, show_col_types = FALSE) {
  query <- build_query(params)
  url <- paste0(endpoint, "?", query)

  read_cached_value(
    cache_dir = cache_dir,
    prefix = "csv",
    key = url,
    max_age_seconds = ttl_seconds,
    loader = function() {
      readr::read_csv(url, show_col_types = show_col_types, progress = FALSE)
    }
  )
}

station_table <- function(metadata) {
  key <- intersect(c("stations", "station"), names(metadata))[1]
  if (is.na(key) || length(key) == 0) {
    stop("Station metadata not found.", call. = FALSE)
  }
  dplyr::as_tibble(metadata[[key]])
}

parameter_table <- function(metadata) {
  key <- intersect(c("parameters", "parameter"), names(metadata))[1]
  if (is.na(key) || length(key) == 0) {
    stop("Parameter metadata not found.", call. = FALSE)
  }
  dplyr::as_tibble(metadata[[key]])
}

parameter_name_from_label <- function(metadata, label) {
  params <- parameter_table(metadata)
  match <- params |>
    dplyr::filter(long_name == label) |>
    dplyr::pull(name)

  if (length(match) == 0) {
    stop(paste("No parameter found for label:", label), call. = FALSE)
  }

  match[[1]]
}

rename_value_column <- function(df) {
  df <- dplyr::as_tibble(df)

  if ("value" %in% names(df)) {
    return(df)
  }

  value_cols <- setdiff(names(df), c("station", "time"))
  if (length(value_cols) == 0) {
    stop("No observation column found in API response.", call. = FALSE)
  }

  names(df)[names(df) == value_cols[[1]]] <- "value"
  df
}

safe_mean <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    NA_real_
  } else {
    mean(x, na.rm = TRUE)
  }
}

safe_min <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    NA_real_
  } else {
    min(x, na.rm = TRUE)
  }
}

safe_max <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    NA_real_
  } else {
    max(x, na.rm = TRUE)
  }
}

add_time_columns <- function(df) {
  df |>
    dplyr::mutate(
      datetime = as.POSIXct(time, tz = "UTC"),
      date = as.Date(time),
      year = lubridate::year(date),
      month = lubridate::month(date),
      yearday = lubridate::yday(date),
      day = lubridate::day(date),
      hour = lubridate::hour(datetime)
    )
}

compute_recent_summary <- function(df, days_back = 14) {
  df |>
    dplyr::filter(date >= (Sys.Date() - days_back)) |>
    dplyr::group_by(date) |>
    dplyr::summarise(
      max_value = safe_max(value),
      min_value = safe_min(value),
      mean_value = safe_mean(value),
      .groups = "drop"
    )
}

compute_ranked_days <- function(df, top_n = 12) {
  df |>
    dplyr::filter(is.finite(value)) |>
    dplyr::transmute(date = as.Date(time), value = round(value, 2)) |>
    dplyr::arrange(dplyr::desc(value)) |>
    head(top_n)
}

coerce_valid_to <- function(x) {
  value <- as.Date(x)
  if (length(value) == 0 || is.na(value)) {
    Sys.Date() - 1
  } else {
    min(value, Sys.Date() - 1)
  }
}

pretty_span <- function(start_date, end_date) {
  paste(format(as.Date(start_date), "%d %b %Y"), "to", format(as.Date(end_date), "%d %b %Y"))
}

plotly_empty <- function(message = "No data available for the current selection.") {
  plotly::plot_ly() |>
    plotly::layout(
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      annotations = list(
        list(
          text = message,
          xref = "paper",
          yref = "paper",
          x = 0.5,
          y = 0.5,
          showarrow = FALSE,
          font = list(size = 16, color = "#546471")
        )
      )
    )
}

downsample_surface_data <- function(elevation_matrix, points_df, max_dim = 220) {
  n_rows <- nrow(elevation_matrix)
  n_cols <- ncol(elevation_matrix)

  row_step <- max(1, ceiling(n_rows / max_dim))
  col_step <- max(1, ceiling(n_cols / max_dim))

  row_index <- unique(c(seq(1, n_rows, by = row_step), n_rows))
  col_index <- unique(c(seq(1, n_cols, by = col_step), n_cols))

  matrix_small <- elevation_matrix[row_index, col_index, drop = FALSE]

  point_rows <- pmin(
    pmax(findInterval(points_df$matrix_row_index, row_index), 1),
    length(row_index)
  )
  point_cols <- pmin(
    pmax(findInterval(points_df$matrix_col_index, col_index), 1),
    length(col_index)
  )

  points_small <- points_df |>
    dplyr::mutate(
      matrix_row_index_small = point_rows,
      matrix_col_index_small = point_cols
    )

  list(
    elevation_matrix = matrix_small,
    map_points = points_small
  )
}

load_metadata_bundle <- function(cache_dir) {
  list(
    synop = fetch_json_cached(
      cache_dir,
      "https://dataset.api.hub.geosphere.at/v1/station/historical/synop-v1-1h/metadata",
      ttl_seconds = 24 * 3600
    ),
    daily = fetch_json_cached(
      cache_dir,
      "https://dataset.api.hub.geosphere.at/v1/station/historical/klima-v1-1d/metadata",
      ttl_seconds = 24 * 3600
    ),
    monthly = fetch_json_cached(
      cache_dir,
      "https://dataset.api.hub.geosphere.at/v1/station/historical/klima-v2-1m/metadata",
      ttl_seconds = 24 * 3600
    )
  )
}
