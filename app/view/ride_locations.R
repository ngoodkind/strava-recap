# app/view/ride_locations

box::use(
  shiny[...],
  bslib[...],
  leaflet[...]
)

box::use(
  app/logic/wrangle
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("map"))
  )
}

#' @export
server <- function(id, rv_d) {
  moduleServer(id, function(input, output, session) {
    rv_states <- reactive({
      df <- req(rv_d())
      wrangle$state_clean(df)
    })

    output$map <- renderLeaflet({
      wrangle$state_map(rv_states())
    })

  })
}
