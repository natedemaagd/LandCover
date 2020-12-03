library(shiny)


# ui ----
ui <- fluidPage(

  # App title ----
  titlePanel("Land Cover Simulation"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input

      # Download button

    ),

    # Main panel for displaying outputs
    mainPanel(



    )

  )

)


# server ----
server <- function(input, output) {



}


# run app ----
shinyApp(ui, server)
