fluidPage(
  h1('mcvis shiny app'),
  hr(),

  mainPanel(
    h1("Summary statistics of your input data"),
    shiny::dataTableOutput('variableTables'),
    hr(),
    plotOutput('ggplotOutput', height = 400)
  ) ## End mainPanel
)
