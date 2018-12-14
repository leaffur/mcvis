library(shiny)

fluidPage(

  title = 'mcvis',

  h1('mcvis'),


  hr(),

  # App title ----
  titlePanel("Shiny Text"),

  # Sidebar layout with a input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(
      # sliderInput(inputId = "sizeCategory5",
      #             label = "Size of line",
      #             min = 0, max = 1, value = 1, step = 0.1)


      h2("Some inputs if needed")
    ),

    # Main panel for displaying outputs ----
    mainPanel(
      DT::dataTableOutput('variableTables'),
      hr(),
      plotOutput('ggplotOutput', height = 500)
    ) ## End mainPanel
  )
)
