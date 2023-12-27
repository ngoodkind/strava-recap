box::use(
  shiny[...],
  bslib[...],
  waiter,
  dplyr,
  lubridate
)

box::use(
  app/logic/oauth,
  app/logic/api,
  app/logic/helper
)

box::use(
  app/view/sign_in_btn,
  app/view/athlete_info,
  app/view/distance,
  app/view/ride_locations,
  app/view/elevation,
  app/view/time,
  app/view/fun,
  app/view/trainer
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_fluid(
    tags$head(tags$style('.selectize-dropdown {z-index: 10000}')),
    window_title = "Strava Stats",
    theme = bs_theme(version = 5, primary = '#fc5200'),
    waiter$useWaiter(),
    layout_columns(
      athlete_info$ui(ns("info")),
      selectInput(ns("year"), "Choose Year",
                  choices = NULL),
      sign_in_btn$ui(ns("signin"))
    ),
    ride_locations$ui(ns("loc")),
    distance$ui(ns("distance")),
    elevation$ui(ns("el")),
    time$ui(ns("time")),
    fun$ui(ns("fun")),
    trainer$ui(ns("trainer"))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

    rv <- reactiveValues()

    # Log in details ----------------------------------------
    # Check if user has logged in and given correct scope.
    parsed_url <- isolate(parseQueryString(session$clientData$url_search))

    if(length(parsed_url) > 0) {
      waiter$waiter_show(html = tagList(waiter$spin_pulsar(), tags$h4("Authenticating")),
             color = "black")

      auth_code <- parsed_url$code
      scope <- parsed_url$scope

      if(is.null(auth_code) || scope != "read,activity:read_all") {
        showNotification("Unable to login. Access not granted or not enough scope", type = "error")
      }else{
        auth_response <- oauth$token_exchange(auth_code)

        # Get token info
        rv$token  <- auth_response$access_token
        rv$athlete <- auth_response$athlete
        rv$athlete_id <- auth_response$athlete$id
      }
      waiter$waiter_hide()
    }

    observeEvent(rv$athlete, {
      x <- req(rv$athlete)
      year <- lubridate$year(lubridate$ymd_hms(x$created_at))
      choices <- helper$year_choices(year)
      updateSelectInput(inputId = "year", choices = choices, selected = choices[1])
    })

    rv_activity <- reactive({
      token <- req(rv$token)

      waiter$waiter_show(html = tagList(waiter$spin_pulsar(), tags$h4("Loading data from Strava...")),
                         color = "black")

      x <- api$get_activities(year = input$year, token) |>
        dplyr$filter(grepl("Ride", type))

      waiter$waiter_hide()
      x
    }) |>
      bindCache(input$year)

    rv_activity_filter <- reactive({
      req(df <- rv_activity())
      df |>
        dplyr$filter(!(trainer | type == "VirtualRide"))
    })


  # servers --------------------------
  sign_in_btn$server("signin", rv)
  athlete_info$server("info", rv)
  distance$server("distance", rv_activity_filter)
  ride_locations$server("loc", rv_activity_filter)
  elevation$server("el", rv_activity_filter)
  time$server("time", rv_activity_filter)
  fun$server("fun", rv_activity_filter)
  trainer$server("trainer", rv_activity)
  })
}



