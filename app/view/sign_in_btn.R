# app/view/sign_in_btn

box::use(
  shiny[...],
  bslib[...]
)

box::use(
  app/logic/oauth
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("sign_in"), style = "top: 20px; position: relative;")
  )
}

#' @export
server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    output$sign_in <- renderUI({

      req(is.null(rv$token))

      tags$a(
        "Sign in",
        href = oauth$create_auth_url(), class = "btn btn-default"
      )
    })
  })
}
