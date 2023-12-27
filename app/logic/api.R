# app/logic/api

box::use(
  httr2,
  tibble,
  dplyr,
  lubridate,
  glue[glue]
)

box::use(
  app/logic/helper[disp_time]
)

base <- "https://www.strava.com/api/v3"

#' @export
base_req <- function(token) {
  httr2$request(base) |>
    httr2$req_auth_bearer_token(token = token)
}

# Helpers ------------------------
epoch_timestamp <- function(date) {
  as.numeric(as.POSIXct(date))
}
m_mi <- function(x) {
  x *  0.00062137
}
m_ft <- function(x) {
  x * 3.2808399
}
mps_miph <- function(x) {
  x * 2.23693629
}

#' Get summary of all activities in a given year
#' @export
get_activities <- function(year, token) {
  start <-  epoch_timestamp(glue("{year}-01-01"))
  end <- epoch_timestamp(glue("{year}-12-31"))

  i <- 1
  lst_act <- vector(mode = "list")
  df_act <- tibble$tibble(init = "init")

  repeat {
    resp <- base_req(token) |>
      httr2$req_url_path_append("athlete/activities") |>
      httr2$req_url_query(before = end, after = start, page = i) |>
      httr2$req_perform()

    df_act <- resp |>
      httr2$resp_body_json(simplifyVector = TRUE) |>
      tibble$as_tibble()

    if(nrow(df_act) == 0) break

    lst_act[[i]] <- df_act
    i <- i + 1
  }


  vars <- c(
    'name', 'distance', 'moving_time', 'elapsed_time', 'total_elevation_gain', 'type',
    'sport_type', 'id', 'start_date_local', 'start_latlng', 'end_latlng', 'achievement_count',
    'kudos_count', 'comment_count', 'athlete_count', 'average_speed',
    'max_speed', 'average_cadence', 'total_photo_count', 'has_kudoed', 'trainer'
  )

  x <- lst_act |>
    dplyr$bind_rows() |>
    dplyr$select(dplyr$any_of(vars)) |>
    dplyr$mutate(
      distance = round(m_mi(distance), digits = 2),
      moving_time_disp = disp_time(moving_time),
      elapsed_time_disp = disp_time(elapsed_time),
      total_elevation_gain = round(m_ft(total_elevation_gain)),
      date = lubridate$ymd_hms(start_date_local),
      average_speed = round(mps_miph(average_speed), digits = 1),
      max_speed = round(mps_miph(max_speed), digits = 1)
    )
}



