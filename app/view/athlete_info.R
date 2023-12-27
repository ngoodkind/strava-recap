# app/view/athlete_info

box::use(
  shiny[...],
  bslib[...]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      uiOutput(ns("photo")),
      uiOutput(ns("name"))
    )
  )
}

#' @export
server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    output$photo <- renderUI({
      req(url <- rv$athlete$profile)
      tags$img(src = url)
    })

    output$name <- renderUI({
      req( a <- rv$athlete)

      name <- paste(a$firstname, a$lastname)
      tags$h3(name)
    })
  })
}
