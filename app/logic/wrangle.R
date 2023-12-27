# app/logic/wrangle

box::use(
  spData,
  sf,
  dplyr,
  purrr,
  leaflet
)

box::use(
  app/logic/helper[fmt, disp_time]
)

# By state -----------------------------
#' @export
state_clean <- function(df_activities) {
  # Get STATE spatial data
  states <- spData$us_states |> sf$st_transform(crs = "EPSG:4326")

  # Figure out the ride starting State based on start lat/lng
  state <- df_activities |>
    dplyr$select(start_latlng) |>
    dplyr$mutate(
      lat = purrr$map_dbl(start_latlng, ~ if (length(.x) > 0) as.numeric(.x[1]) else NA_real_),
      lng = purrr$map_dbl(start_latlng, ~ if (length(.x) > 0) as.numeric(.x[2]) else NA_real_)
    ) |>
    dplyr$filter(!is.na(lat) | !is.na(lng)) |>
    sf$st_as_sf(coords = c("lng","lat"), crs = "EPSG:4326") |>
    sf$st_join(states) |>
    sf$st_drop_geometry() |>
    dplyr$summarise(
      .by = "NAME",
      rides = dplyr$n()
    ) |>
    dplyr$filter(!is.na(NAME))

  states <- states |>
    dplyr$left_join(state, by = "NAME")

  return(states)
}

#' @export
state_map <- function(df_states) {
  pal <- leaflet$colorNumeric(c("#00204D","#FFEA46"), domain = df_states$rides)

  leaflet$leaflet(df_states) |>
    leaflet$addTiles() |>
    leaflet$addPolygons(
      fillColor = ~pal(rides),
      fillOpacity = 0.7,
      color = "#BDBDC3",
      weight = 1,
      smoothFactor = 0.5,
      label = ~paste0(NAME, " - ", rides)
    ) |>
    leaflet$addLegend("bottomright", pal = pal, values = ~rides,
                       title = "Total Activities",
                       opacity = 0.7)
}

# Card summaries -------------------------
# trainer rides
#' @export
trainer <- function(df_activities) {
  df_activities |>
    dplyr$filter(trainer | type == "VirtualRide") |>
    dplyr$summarise(
      n = dplyr$n(),
      distance = fmt(sum(distance, na.rm = TRUE)),
      moving_time = disp_time(sum(moving_time, na.rm = TRUE))
    )
}


### Distance
#' @export
distance <- function(df_activities){
  df_activities |>
    dplyr$summarise(
      distance_total = fmt(sum(distance)),
      distance_avg = fmt(round(mean(distance), 2)),
      distance_max  = fmt(max(distance)),
      distance_max_id = id[which.max(distance)],
      distance_max_name = name[which.max(distance)]
    )
}
### Elevation
#' @export
elevation <- function(df_activities){
  df_activities |>
    dplyr$mutate(
      elevation_mile = total_elevation_gain / distance
    ) |>
    dplyr$summarise(
      elevation = fmt(sum(total_elevation_gain)),
      elevation_max  = fmt(max(total_elevation_gain)),
      elevation_max_id = id[which.max(total_elevation_gain)],
      elevation_max_name = name[which.max(total_elevation_gain)],

      elevation_mile_max = fmt(round(max(elevation_mile))),
      elevation_mile_id = id[which.max(elevation_mile)],
      elevation_mile_name = name[which.max(elevation_mile)],
      elevation_mile = fmt(round(sum(total_elevation_gain) / sum(distance))),
    )
}

### Time
#' @export
time <- function(df_activities){
  df_activities |>
    dplyr$summarise(
      time_total = disp_time(sum(moving_time)),
      time_avg = disp_time(mean(moving_time)),
      time_max  = disp_time(max(moving_time)),
      time_max_id = id[which.max(moving_time)],
      time_max_name = name[which.max(moving_time)]
    )
}

## Fun
#' @export
fun <- function(df_activities){
  df_activities |>
    dplyr$summarise(
      kudo = sum(kudos_count, na.rm = TRUE),
      kudo_max = max(kudos_count),
      kudo_id = id[which.max(kudos_count)],
      kudo_name = name[which.max(kudos_count)],

      comment = sum(comment_count, na.rm = TRUE),
      comment_max = max(comment_count),
      comment_id = id[which.max(comment_count)],
      comment_name = name[which.max(comment_count)],

      self_kudo = sum(has_kudoed),

      photo = sum(total_photo_count, na.rm = TRUE),
      photo_max = max(total_photo_count),
      photo_id = id[which.max(total_photo_count)],
      photo_name = name[which.max(total_photo_count)]
    )
}
