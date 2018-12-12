library(shiny)

fluidPage(

  title = 'mcvis',

  h1('mcvis'),

  fluidRow(
    column(6, DT::dataTableOutput('x1')),
    column(6, plotOutput('x2', height = 500))
  ),

  hr()
)
