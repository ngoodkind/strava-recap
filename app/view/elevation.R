# app/view/elevation

box::use(
  shiny[...],
  bslib[...],
  glue[glue],
  bsicons[bs_icon]
)

box::use(
  app/logic/wrangle,
  app/logic/helper[link_act]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      uiOutput(ns("elevation")),
      uiOutput(ns("elevation_mile"))
    )

  )
}

#' @export
server <- function(id, rv_d) {
  moduleServer(id, function(input, output, session) {

    rv_elevation <- reactive({
      df <- req(rv_d())
      wrangle$elevation(df)
    })

    output$elevation <- renderUI({
      elevation <- rv_elevation()

      value_box(
        title = "Elevation",
        showcase = icon("mountain"),
        value = glue("Total Elevation: {elevation$elevation} ft"),
        tags$p("Most Elevation Gain: ",
               tags$a(elevation$elevation_max_name, href = link_act(elevation$elevation_max_id), target = "_blank"),
               glue(" - {elevation$elevation_max} ft"))
      )

    })

    output$elevation_mile <- renderUI({
      elevation <- rv_elevation()

      value_box(
        title = "Elevation per Mile",
        showcase = icon("mountain"),
        value = glue("Avg Elevation per Mile: {elevation$elevation_mile} ft"),
        tags$p("Most Elevation per Mile: ",
               tags$a(elevation$elevation_mile_name, href = link_act(elevation$elevation_mile_id), target = "_blank"),
               glue(" - {elevation$elevation_mile_max} ft/mile"))
      )

    })


  })
}
