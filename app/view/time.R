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
      uiOutput(ns("time"))
    )
  )
}

#' @export
server <- function(id, rv_d) {
  moduleServer(id, function(input, output, session) {

    output$time <- renderUI({
      df <- req(rv_d())
      time <- wrangle$time(df)

      value_box(
        title = "Time",
        showcase = bs_icon("clock"),
        value = glue("Total Moving Time: {time$time_total}"),
        tags$p(glue("Average Moving Time per Ride: {time$time_avg}")),
        tags$p("Longest Ride: ",
               tags$a(time$time_max_name, href = link_act(time$time_max_id), target = "_blank"),
               glue(" - {time$time_max}"))
      )

    })

  })
}
