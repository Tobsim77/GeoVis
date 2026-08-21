synop_region_ids <- c(11010, 11036, 11101, 11120, 11150, 11175, 11185, 11231)
daily_region_ids <- c(15, 39, 131, 100, 26, 166, 105, 154)

available_synop_locations <- function(metadata) {
  stations <- station_table(metadata)
  active_names <- stations |>
    dplyr::filter(is_active == "TRUE", name != "MAYRHOFEN") |>
    dplyr::arrange(name) |>
    dplyr::pull(name) |>
    unique()

  c("Oesterreich", active_names)
}

available_daily_locations <- function(metadata) {
  stations <- station_table(metadata)
  if ("type" %in% names(stations)) {
    stations <- dplyr::filter(stations, type == "COMBINED")
  }

  c("Oesterreich", sort(unique(stations$name)))
}

available_synop_variables <- function(metadata) {
  parameter_table(metadata) |>
    dplyr::filter(!(unit %in% c("Code (Synop)", NA_character_, "Code", ""))) |>
    dplyr::pull(long_name) |>
    unique() |>
    sort()
}

available_daily_variables <- function(metadata) {
  parameter_table(metadata) |>
    dplyr::filter(!(unit %in% c("Code (Synop)", NA_character_, "Code", "", "°"))) |>
    dplyr::filter(!grepl("Beobachtungstermin", long_name, fixed = TRUE)) |>
    dplyr::pull(long_name) |>
    unique() |>
    sort()
}

resolve_synop_selection <- function(metadata, location_label) {
  stations <- station_table(metadata)

  if (identical(location_label, "Oesterreich")) {
    selected <- stations |>
      dplyr::filter(id %in% synop_region_ids)

    return(list(
      label = location_label,
      station_ids = synop_region_ids,
      aggregate = TRUE,
      available_start = max(as.Date(selected$valid_from), na.rm = TRUE),
      available_end = min(as.Date(selected$valid_to), Sys.Date() - 1, na.rm = TRUE),
      station_count = length(synop_region_ids)
    ))
  }

  selected <- stations |>
    dplyr::filter(name == location_label) |>
    dplyr::arrange(dplyr::desc(is_active == "TRUE"), id)

  if (nrow(selected) == 0) {
    stop(paste("No SYNOP station found for", location_label), call. = FALSE)
  }

  selected <- selected[1, ]

  list(
    label = selected$name[[1]],
    station_ids = selected$id[[1]],
    aggregate = FALSE,
    available_start = as.Date(selected$valid_from[[1]]),
    available_end = coerce_valid_to(selected$valid_to[[1]]),
    station_count = 1
  )
}

resolve_daily_selection <- function(metadata, location_label) {
  stations <- station_table(metadata)
  if ("type" %in% names(stations)) {
    stations <- dplyr::filter(stations, type == "COMBINED")
  }

  if (identical(location_label, "Oesterreich")) {
    selected <- stations |>
      dplyr::filter(id %in% daily_region_ids)

    return(list(
      label = location_label,
      station_ids = daily_region_ids,
      aggregate = TRUE,
      available_start = max(as.Date(selected$valid_from), na.rm = TRUE),
      available_end = min(as.Date(selected$valid_to), Sys.Date() - 1, na.rm = TRUE),
      station_count = length(daily_region_ids)
    ))
  }

  selected <- stations |>
    dplyr::filter(name == location_label) |>
    dplyr::arrange(id)

  if (nrow(selected) == 0) {
    stop(paste("No daily climate station found for", location_label), call. = FALSE)
  }

  selected <- selected[1, ]

  list(
    label = selected$name[[1]],
    station_ids = selected$id[[1]],
    aggregate = FALSE,
    available_start = as.Date(selected$valid_from[[1]]),
    available_end = coerce_valid_to(selected$valid_to[[1]]),
    station_count = 1
  )
}

fetch_synop_history <- function(cache_dir, parameter_name, station_ids, start_date, end_date) {
  fetch_csv_cached(
    cache_dir = cache_dir,
    endpoint = "https://dataset.api.hub.geosphere.at/v1/station/historical/synop-v1-1h",
    params = list(
      parameters = parameter_name,
      start = as.Date(start_date),
      end = as.Date(end_date),
      station_ids = station_ids,
      output_format = "csv"
    ),
    ttl_seconds = 24 * 3600
  )
}

fetch_daily_history <- function(cache_dir, parameter_name, station_ids, start_date, end_date) {
  fetch_csv_cached(
    cache_dir = cache_dir,
    endpoint = "https://dataset.api.hub.geosphere.at/v1/station/historical/klima-v1-1d",
    params = list(
      parameters = parameter_name,
      start = as.Date(start_date),
      end = as.Date(end_date),
      station_ids = station_ids,
      output_format = "csv"
    ),
    ttl_seconds = 24 * 3600
  )
}

fetch_monthly_history <- function(cache_dir, parameter_name, station_ids, start_date, end_date) {
  fetch_csv_cached(
    cache_dir = cache_dir,
    endpoint = "https://dataset.api.hub.geosphere.at/v1/station/historical/klima-v2-1m",
    params = list(
      parameters = parameter_name,
      start = as.Date(start_date),
      end = as.Date(end_date),
      station_ids = station_ids,
      output_format = "csv"
    ),
    ttl_seconds = 7 * 24 * 3600
  )
}

prepare_observation_data <- function(df, aggregate_multiple = FALSE) {
  df <- rename_value_column(df)
  df <- add_time_columns(df)

  if (aggregate_multiple && "station" %in% names(df)) {
    df <- df |>
      dplyr::group_by(time, datetime, date, year, month, yearday, day, hour) |>
      dplyr::summarise(value = safe_mean(value), .groups = "drop")
  }

  df |>
    dplyr::arrange(datetime)
}

compute_yearly_anomalies <- function(df) {
  yearly <- df |>
    dplyr::group_by(year) |>
    dplyr::summarise(mean_value = safe_mean(value), .groups = "drop")

  baseline <- safe_mean(yearly$mean_value)
  yearly |>
    dplyr::mutate(anomaly = mean_value - baseline)
}

compute_heatwave_counts <- function(df) {
  df <- rename_value_column(df) |>
    dplyr::mutate(date = as.Date(time))

  if ("station" %in% names(df) && dplyr::n_distinct(df$station) > 1) {
    df <- df |>
      dplyr::group_by(date) |>
      dplyr::summarise(value = safe_mean(value), .groups = "drop")
  } else {
    df <- df |>
      dplyr::transmute(date, value)
  }

  if (nrow(df) == 0) {
    return(dplyr::tibble(year = integer(), heatwaves = integer()))
  }

  df |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      over_30 = value > 30,
      is_consecutive = (date - dplyr::lag(date, default = first(date))) == 1 & over_30,
      streak_group = cumsum(!is_consecutive | is.na(is_consecutive))
    ) |>
    dplyr::group_by(streak_group) |>
    dplyr::filter(over_30) |>
    dplyr::mutate(streak_length = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::filter(streak_length >= 3) |>
    dplyr::mutate(year = lubridate::year(date)) |>
    dplyr::group_by(year) |>
    dplyr::summarise(heatwaves = dplyr::n_distinct(streak_group), .groups = "drop")
}

fit_series_trend <- function(df) {
  clean <- df |>
    dplyr::filter(is.finite(value), !is.na(date)) |>
    dplyr::arrange(date)

  if (nrow(clean) < 3) {
    return(dplyr::tibble(slope = NA_real_, intercept = NA_real_, std_error = NA_real_))
  }

  clean <- clean |>
    dplyr::mutate(time_numeric = as.numeric(difftime(date, min(date), units = "days")) / 365.25)

  fit <- stats::lm(value ~ time_numeric, data = clean)
  coefs <- coef(summary(fit))

  dplyr::tibble(
    slope = unname(coefs["time_numeric", "Estimate"]),
    intercept = unname(coefs["(Intercept)", "Estimate"]),
    std_error = unname(coefs["time_numeric", "Std. Error"])
  )
}

season_from_month <- function(month) {
  dplyr::case_when(
    month %in% c(12, 1, 2) ~ "Winter",
    month %in% 3:5 ~ "Spring",
    month %in% 6:8 ~ "Summer",
    TRUE ~ "Autumn"
  )
}

compute_map_positions <- function(df_linreg, data_dir) {
  elevation_matrix <- readRDS(file.path(data_dir, "elevation_matrix.rds"))
  raster_extent_raw <- readRDS(file.path(data_dir, "raster_extent.rds"))

  raster_extent <- if (is.data.frame(raster_extent_raw)) {
    raster_extent_raw[1, c("xmin", "xmax", "ymin", "ymax")]
  } else {
    data.frame(xmin = 9.4, xmax = 17.3, ymin = 46.2, ymax = 49.2)
  }

  res_x <- (raster_extent$xmax - raster_extent$xmin) / (ncol(elevation_matrix) - 1)
  res_y <- (raster_extent$ymax - raster_extent$ymin) / (nrow(elevation_matrix) - 1)

  points_df <- df_linreg |>
    dplyr::filter(!is.na(lon), !is.na(lat)) |>
    dplyr::mutate(
      matrix_col_index = pmin(pmax(round((lon - raster_extent$xmin) / res_x) + 1, 1), ncol(elevation_matrix)),
      matrix_row_index = pmin(pmax(round((raster_extent$ymax - lat) / res_y) + 1, 1), nrow(elevation_matrix))
    )

  points_df$surface_height <- mapply(function(row_index, col_index) {
    elevation_matrix[row_index, col_index]
  }, points_df$matrix_row_index, points_df$matrix_col_index)

  list(elevation_matrix = elevation_matrix, map_points = points_df)
}

get_monthly_analysis <- function(cache_dir, data_dir, metadata) {
  read_cached_value(
    cache_dir = cache_dir,
    prefix = "monthly_analysis",
    key = "v2",
    max_age_seconds = 7 * 24 * 3600,
    loader = function() {
      stations <- station_table(metadata)
      if ("type" %in% names(stations)) {
        stations <- dplyr::filter(stations, type == "COMBINED")
      }

      raw <- fetch_monthly_history(
        cache_dir = cache_dir,
        parameter_name = "tl_mittel",
        station_ids = stations$id,
        start_date = as.Date("1900-01-01"),
        end_date = Sys.Date() - 1
      )

      monthly <- rename_value_column(raw) |>
        dplyr::rename(id = station) |>
        dplyr::left_join(stations, by = "id") |>
        add_time_columns() |>
        dplyr::mutate(
          label = paste0(name, " (", round(altitude), " m)"),
          season = season_from_month(month)
        )

      coverage <- monthly |>
        dplyr::filter(!is.na(value)) |>
        dplyr::group_by(id) |>
        dplyr::summarise(
          first_date = min(date),
          last_date = max(date),
          non_na_count = dplyr::n(),
          .groups = "drop"
        )

      max_count <- max(coverage$non_na_count, na.rm = TRUE)
      max_range <- max(as.numeric(difftime(coverage$last_date, coverage$first_date, units = "days")) / 365.25, na.rm = TRUE)

      slope_summary <- monthly |>
        dplyr::left_join(coverage, by = "id") |>
        dplyr::group_by(id, name, label, altitude, lat, lon, first_date, last_date, non_na_count) |>
        dplyr::group_modify(function(.x, .y) fit_series_trend(.x)) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          time_range_years = as.numeric(difftime(last_date, first_date, units = "days")) / 365.25,
          precision_weight = 1 / (std_error ^ 2),
          coverage_share = non_na_count / max_count,
          time_share = time_range_years / max_range,
          weighted_factor = precision_weight * coverage_share * time_share,
          slope_weighted = slope * weighted_factor
        )

      seasonal_coverage <- monthly |>
        dplyr::filter(!is.na(value)) |>
        dplyr::group_by(id, season) |>
        dplyr::summarise(
          first_date = min(date),
          last_date = max(date),
          non_na_count = dplyr::n(),
          .groups = "drop"
        )

      seasonal_summary <- monthly |>
        dplyr::left_join(seasonal_coverage, by = c("id", "season")) |>
        dplyr::group_by(id, name, label, altitude, season, first_date, last_date, non_na_count) |>
        dplyr::group_modify(function(.x, .y) fit_series_trend(.x)) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          time_range_years = as.numeric(difftime(last_date, first_date, units = "days")) / 365.25,
          precision_weight = 1 / (std_error ^ 2),
          coverage_share = non_na_count / max_count,
          time_share = time_range_years / max_range,
          weighted_factor = precision_weight * coverage_share * time_share,
          slope_weighted = slope * weighted_factor
        )

      missing_data <- monthly |>
        dplyr::left_join(coverage, by = "id") |>
        dplyr::transmute(
          time = date,
          label,
          name,
          altitude,
          non_na_count,
          time_range_years = as.numeric(difftime(last_date, first_date, units = "days")) / 365.25,
          value,
          has_data = !is.na(value)
        )

      map_info <- compute_map_positions(slope_summary, data_dir)

      list(
        monthly = monthly,
        missing_data = missing_data,
        slope_summary = slope_summary,
        seasonal_summary = seasonal_summary,
        elevation_matrix = map_info$elevation_matrix,
        map_points = map_info$map_points
      )
    }
  )
}
