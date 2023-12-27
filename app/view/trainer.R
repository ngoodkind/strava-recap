# app/view/time

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
      uiOutput(ns("trainer"))
    )
  )
}

#' @export
server <- function(id, rv_d) {
  moduleServer(id, function(input, output, session) {

    output$trainer <- renderUI({
      df <- req(rv_d())
      trainer <- wrangle$trainer(df)
      req(nrow(trainer) > 0)

      value_box(
        title = "Indoor Trainer",
        showcase = bs_icon("house"),
        value = glue("Total Indoor Distance: {trainer$distance} mi"),
        tags$p(glue("Total Indoor Rides: {trainer$n}")),
        tags$p(glue("Indoor Moving Time: {trainer$moving_time}")),
        tags$p(tags$i("Indoor rides are only considered here. All stats shown
                      above filter out indoor rides"))
      )

    })

  })
}
