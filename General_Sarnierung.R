
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


metadata <- fromJSON("https://dataset.api.hub.geosphere.at/v1/station/historical/synop-v1-1h/metadata")
metadata2 <- fromJSON("https://dataset.api.hub.geosphere.at/v1/station/historical/klima-v2-1d/metadata")
metadata_monthly <- fromJSON("https://dataset.api.hub.geosphere.at/v1/station/historical/klima-v2-1m/metadata")

metadata_grid <- fromJSON("https://dataset.api.hub.geosphere.at/v1/grid/historical/spartacus-v2-1y-1km/metadata")
bbox <- paste(metadata_grid$bbox_outer, collapse = ",")



link_grid <- "https://dataset.api.hub.geosphere.at/v1/grid/historical/spartacus-v2-1y-1km"


date0 <- seq(as.Date(metadata_grid$start_time), as.Date(metadata_grid$end_time), by = "1 year")


df <- tibble(Date = date0, link = link_grid)|> mutate(link = paste0(link,
                                                                    "?parameters=TM&start=", format(date0, "%Y-%m-%dT00:00"),
                                                                    "&end=",   format(date0, "%Y-%m-%dT00:00") ,
                                                                    "&bbox=", bbox,
                                                                    "&output_format=geojson"))

library(sf)

datasets <- purrr::map(df$link, sf::read_sf)
df$dataset <- I(datasets)


saveRDS(df, "~/historical_grid_data.rds")


df<-readRDS("~/historical_grid_data.rds")

df|>head(1)


sf_geo <- df_$dataset[[1]] |>
  mutate(
    TM = map_dbl(parameters, function(x) {
      val <- fromJSON(x)$TM$data
      
      if (length(val) == 0 || all(is.na(val))) {
        NA_real_
      } else {
        as.numeric(val[1])
      }
    })
  )

sf_geo <- sf_geo |> st_transform(3416)

df_geo <- bind_cols(
  st_drop_geometry(sf_geo),
  as_tibble(st_coordinates(sf_geo))
)

temp_pal <- c(
  "#3C0217", "#550C26", "#6B1631", "#84203E", "#9B294C",
  "#AA4C4D", "#B76D4C", "#BB8553", "#BA9860", "#BEA875",
  "#A8A57C", "#859783", "#618886", "#417C8B", "#287390",
  "#276587", "#285A7E", "#254E75", "#26426E", "#2F4774",
  "#38507D", "#415B85", "#4C648F", "#546F99", "#5D77A0",
  "#718CB3", "#7E99C0", "#88A2CA", "#91AED3", "#96B1D6",
  "#A1B9DB", "#A9C0DF", "#B1C6E3", "#BDD0EA", "#C4D3EC",
  "#CCDAEF", "#D5E1F3", "#DCE8F7"
)

temp_F <- c(
  135,
  seq(117.5, -57.5, by = -5),
  -80
)

temp_C <- (temp_F - 32) * 5/9

temp_scale <- scale_fill_gradientn(
  colours = rev(temp_pal),
  values = scales::rescale(
    rev(temp_C),
    from = range(temp_C)
  ),
  limits = range(temp_C),
  oob = scales::squish,
  name = "Temperature [°C]"
)

df_geo |>
  filter(!is.na(TM)) |>
  ggplot(aes(X, Y, fill = TM)) +
  geom_tile(width = 1000, height = 1000) +
  coord_equal()+ temp_scale
