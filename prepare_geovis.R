library(dplyr)
library(tidyr)
library(lubridate)
library(jsonlite)
library(terra)
library(mgcv)

options(timeout = max(300L, getOption("timeout")))

create_slopes <- function(path = "slopes.rds") {
  if (file.exists(path)) {
    message("Vorhandene slopes.rds wird verwendet; ihre Zeiteinheit muss geprueft sein.")
    return(invisible(path))
  }
  
  metadata <- if (file.exists("metadata_km.json")) {
    jsonlite::fromJSON("metadata_km.json")
  } else {
    jsonlite::fromJSON(
      "https://dataset.api.hub.geosphere.at/v1/grid/historical/spartacus-v3-1m-1km/metadata"
    )
  }
  
  bbox <- paste(metadata$bbox_outer, collapse = ",")
  dates <- seq(as.Date(substr(metadata$start_time, 1, 10)),
               as.Date(substr(metadata$end_time, 1, 10)), by = "1 month")
  dates <- dates[lubridate::year(dates) < lubridate::year(Sys.Date())]
  endpoint <- "https://dataset.api.hub.geosphere.at/v1/grid/historical/spartacus-v3-1m-1km"
  
  grid <- NULL
  n <- NULL
  sum_t <- NULL
  sum_y <- NULL
  sum_t2 <- NULL
  sum_ty <- NULL
  
  for (i in seq_along(dates)) {
    t <- as.numeric(dates[i] - dates[1]) / 365.2425
    dt <- format(dates[i], "%Y-%m-%dT00:00")
    url <- paste0(endpoint,
                  "?parameters=TM&start=", dt,
                  "&end=", dt,
                  "&bbox=", bbox,
                  "&output_format=netcdf"
    )
    tmp <- tempfile(fileext = ".nc")
    download.file(url, tmp, mode = "wb", quiet = TRUE)
    r <- terra::rast(tmp)
    if (terra::nlyr(r) > 1L) r <- r[[1]]
    tm <- terra::values(r, mat = FALSE)
    
    if (is.null(grid)) {
      xy <- terra::xyFromCell(r, seq_len(terra::ncell(r)))
      grid <- tibble(X = xy[, 1], Y = xy[, 2])
      k <- nrow(grid)
      n <- integer(k)
      sum_t <- numeric(k)
      sum_y <- numeric(k)
      sum_t2 <- numeric(k)
      sum_ty <- numeric(k)
    }
    if (length(tm) != nrow(grid)) stop("Die Rastergröße hat sich geändert: ", dt)
    
    valid <- is.finite(tm)
    n[valid] <- n[valid] + 1L
    sum_t[valid] <- sum_t[valid] + t
    sum_y[valid] <- sum_y[valid] + tm[valid]
    sum_t2[valid] <- sum_t2[valid] + t^2
    sum_ty[valid] <- sum_ty[valid] + t * tm[valid]
    rm(r, tm)
    unlink(tmp)
    if (i %% 12L == 0L || i == length(dates)) {
      message("Monatsdaten verarbeitet: ", i, "/", length(dates))
      gc(verbose = FALSE)
    }
  }
  
  den <- n * sum_t2 - sum_t^2
  slope <- rep(NA_real_, length(n))
  valid <- n >= 2L & den != 0
  slope[valid] <- (n[valid] * sum_ty[valid] - sum_t[valid] * sum_y[valid]) / den[valid]
  result <- grid |> mutate(slope = slope) |> filter(is.finite(slope))
  attr(result, "slope_unit") <- "degC/year"
  attr(result, "period") <- paste0(lubridate::year(dates[1]), "-", lubridate::year(tail(dates, 1)))
  saveRDS(result, path, compress = "xz")
  invisible(path)
}

aggregate_grid <- function(slopes, elevation, resolution_m) {
  bin_xy <- function(df, value_name) {
    df |>
      transmute(
        X = round((X - 500) / resolution_m) * resolution_m + 500,
        Y = round((Y - 500) / resolution_m) * resolution_m + 500,
        value = .data[[value_name]]
      ) |>
      filter(is.finite(value)) |>
      group_by(X, Y) |>
      summarise(value = mean(value), .groups = "drop")
  }
  s <- bin_xy(slopes, "slope") |> rename(slope = value)
  e <- bin_xy(elevation, "altitude") |> rename(altitude = value)
  inner_join(s, e, by = c("X", "Y"))
}

prepare <- function() {
  if (!file.exists("df_elevation.rds")) {
    stop("df_elevation.rds fehlt. Bitte die vorhandene Datei in den Projektordner kopieren.")
  }
  
  create_slopes("slopes.rds")
  message("Lese vorbereitete SPARTACUS-Steigungen und Höhen ein...")
  slopes <- readRDS("slopes.rds")
  elevation <- readRDS("df_elevation.rds")
  if (!all(c("X", "Y", "slope") %in% names(slopes))) {
    stop("slopes.rds benötigt die Spalten X, Y und slope.")
  }
  if (!all(c("X", "Y", "altitude") %in% names(elevation))) {
    stop("df_elevation.rds benötigt die Spalten X, Y und altitude.")
  }
  
  if (!is.null(attr(slopes, "slope_unit")) &&
      !identical(attr(slopes, "slope_unit"), "degC/year")) {
    stop("Die gespeicherten Slopes sind nicht in degC/year.")
  }
  if (median(abs(slopes$slope), na.rm = TRUE) > 0.2) {
    stop("Auffaellig hohe Slopes: Bitte die Zeiteinheit der Eingabedaten kontrollieren.")
  }
  slopes <- slopes |> select(X, Y, slope)
  elevation <- elevation |> select(X, Y, altitude)
  
  grid <- aggregate_grid(slopes, elevation, 1000L)
  rm(slopes, elevation)
  gc(verbose = FALSE)
  if (nrow(grid) < 100L) stop("Zu wenige Rasterzellen nach dem Zusammenführen.")
  
  message("Berechne lineare Modelle...")
  model1 <- lm(slope ~ altitude, data = grid)
  model2 <- lm(slope ~ altitude + X + Y, data = grid)
  effect_raw <- unname(coef(model1)["altitude"] * 1000)
  effect_spatial <- unname(coef(model2)["altitude"] * 1000)
  reduction <- if (is.finite(effect_raw) && effect_raw != 0) {
    (1 - abs(effect_spatial / effect_raw)) * 100
  } else NA_real_
  
  grid$residual_altitude <- unname(residuals(model1))
  slope_stats <- paste0(
    "Covariance: ", round(cov(grid$altitude, grid$slope), 4),
    " | Correlation: ", round(cor(grid$altitude, grid$slope), 3),
    " | R² Altitude: ", round(summary(model1)$r.squared, 3),
    " | R² Altitude + X/Y: ", round(summary(model2)$r.squared, 3)
  )
  rm(model1, model2)
  gc(verbose = FALSE)
  
  message("Berechne das räumliche GAM lokal...")
  model_gam <- mgcv::bam(
    slope ~ s(altitude, k = 10) + s(X, Y, k = 30),
    data = grid,
    method = "fREML",
    discrete = TRUE
  )
  altitude_pred <- tibble(
    altitude = seq(min(grid$altitude), max(grid$altitude), length.out = 300),
    X = median(grid$X),
    Y = median(grid$Y)
  )
  pred <- predict(model_gam,
                  newdata = altitude_pred,
                  type = "terms",
                  terms = "s(altitude)",
                  se.fit = TRUE
  )
  gam_prediction <- altitude_pred |>
    mutate(
      fit = as.numeric(pred$fit),
      se = as.numeric(pred$se.fit),
      lower = fit - 1.96 * se,
      upper = fit + 1.96 * se
    )
  slope_stats <- paste0(slope_stats,
                        " | GAM explained deviance: ",
                        round(summary(model_gam)$dev.expl * 100, 1), "%"
  )
  slope_findings <- paste0(
    "The raw altitude effect is ", round(effect_raw, 5),
    " °C/year per 1000m. After controlling for X and Y it is ",
    round(effect_spatial, 5), " °C/year per 1000m, a change of ",
    round(reduction, 1), "% in absolute magnitude. The GAM provides a flexible nonlinear estimate."
  )
  rm(model_gam, pred)
  gc(verbose = FALSE)
  
  altitude_500m <- grid |>
    mutate(altitude_group = floor(altitude / 500) * 500) |>
    group_by(altitude_group) |>
    summarise(
      mean_slope = mean(slope),
      median_slope = median(slope),
      n = n(),
      .groups = "drop"
    )
  altitude_effect <- tibble(
    model = c("Altitude only", "Altitude + X/Y"),
    effect = c(effect_raw, effect_spatial)
  )
  
  message("Erzeuge ein reduziertes 3D-Raster...")
  surface_grid <- grid |>
    transmute(
      X = round((X - 500) / 1000) * 1000 + 500,
      Y = round((Y - 500) / 1000) * 1000 + 500,
      altitude,
      slope
    ) |>
    group_by(X, Y) |>
    summarise(
      altitude = mean(altitude),
      slope = mean(slope),
      .groups = "drop"
    )
  x <- sort(unique(surface_grid$X))
  y <- sort(unique(surface_grid$Y))
  z_altitude <- matrix(NA_real_, nrow = length(y), ncol = length(x))
  z_slope <- matrix(NA_real_, nrow = length(y), ncol = length(x))
  ix <- match(surface_grid$X, x)
  iy <- match(surface_grid$Y, y)
  z_altitude[cbind(iy, ix)] <- surface_grid$altitude
  z_slope[cbind(iy, ix)] <- surface_grid$slope
  
  dir.create("data", showWarnings = FALSE, recursive = TRUE)
  saveRDS(list(
    grid = grid,
    altitude_500m = altitude_500m,
    altitude_effect = altitude_effect,
    gam_prediction = gam_prediction,
    slope_stats = slope_stats,
    slope_findings = slope_findings,
    surface = list(x = x, y = y, altitude = z_altitude, slope = z_slope)
  ), file.path("data", "geovis_precomputed.rds"), compress = "xz")
  message("Fertig: data/geovis_precomputed.rds")
}

prepare()

