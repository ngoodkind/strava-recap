# app/view/fun

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
      uiOutput(ns("kudo")),
      uiOutput(ns("comment")),
      uiOutput(ns("photo"))
    )

  )
}

#' @export
server <- function(id, rv_d) {
  moduleServer(id, function(input, output, session) {

    rv_fun <- reactive({
      df <- req(rv_d())
      wrangle$fun(df)
    })

    output$kudo <- renderUI({
      fun <- rv_fun()

      value_box(
        title = "Kudos",
        showcase = bs_icon("hand-thumbs-up"),
        value = glue("Total Kudos: {fun$kudo}"),
        tags$p("Most Kudos: ",
               tags$a(fun$kudo_name, href = link_act(fun$kudo_id), target = "_blank"),
               glue(" - {fun$kudo_max} kudos"))#,
        #tags$p(glue("Self Love: {fun$self_kudo} kudos given to your own rides"))
      )
    })

    output$comment <- renderUI({
      fun <- rv_fun()

      value_box(
        title = "Comments",
        showcase = bs_icon("chat-left-text"),
        value = glue("Total Comments: {fun$comment}"),
        tags$p("Most Comments: ",
               tags$a(fun$comment_name, href = link_act(fun$comment_id), target = "_blank"),
               glue(" - {fun$comment_max} comments"))
      )
    })

    output$photo <- renderUI({
      fun <- rv_fun()

      value_box(
        title = "Photos",
        showcase = bs_icon("camera"),
        value = glue("Total Photos: {fun$photo}"),
        tags$p("Most Photos: ",
               tags$a(fun$photo_name, href = link_act(fun$photo_id), target = "_blank"),
               glue(" - {fun$photo_max} photos"))
      )
    })

  })
}
