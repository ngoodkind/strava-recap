# app/logic/helper

box::use(
  lubridate[...],
  glue[glue],
  dplyr
)


# Function to get the current year and the previous four years
#' @export
year_choices <- function(inputYear) {
  # Validate input
  if(!is.numeric(inputYear) || length(inputYear) != 1) {
    stop("Input must be a single numeric year")
  }

  # Get the current year
  currentYear <- year(today())

  # Create a vector of years from the input year to the current year
  yearsVector <- seq(from = inputYear, to = currentYear, by = 1)

  return(rev(yearsVector))
}

#' @export
disp_time <- function(x) {
  h <- x %/% 3600
  m <- (x %% 3600) %/% 60
  s <- x %% 60

  glue("{h}h {m}m {round(s)}s")
}

#' @export
fmt <- function(x) {
  format(x, big.mark = ",", decimal.mark = ".")
}

#' @export
link_act <- function(id) {
  glue("https://www.strava.com/activities/{id}")
}

