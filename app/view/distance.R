# app/view/distance

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
      uiOutput(ns("distance"))
    )

  )
}

#' @export
server <- function(id, rv_d) {
  moduleServer(id, function(input, output, session) {

    output$distance <- renderUI({
      df <- req(rv_d())
      distance <- wrangle$distance(df)

      value_box(
        title = "Distance",
        showcase = bs_icon("bicycle"),
        value = glue("Total Distance: {distance$distance_total} mi"),
        tags$p(glue("Average Distance per Ride: {distance$distance_avg} mi/ride")),
        tags$p("Longest Ride: ",
               tags$a(distance$distance_max_name, href = link_act(distance$distance_max_id), target = "_blank"),
               glue(" - {distance$distance_max} mi"))
      )

    })

  })
}
