library(shiny)


# ui ----
ui <- fluidPage(

  # App title ----
  titlePanel("Land Cover Simulation"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(h2("User input"), width = 4,

      br(),

      # Input

          # data
          h4("Data files"),
          fluidRow(
            column(4, fileInput("data",    "Regression data file\n(must be type .xlsx)", accept = '.xlsx', width = '1000px')),
            column(4, fileInput("shp_reg", "Raster - regression region\n(e.g., a .tif file)", width = '1000px')),
            column(4, fileInput("shp_app", "Raster - simulation application region\n(e.g., a .tif file)", width = '1000px'))
          ),

          # variable names
          h4("Variable names"),
          fluidRow(
            column(4, textInput("landcover_varname", "Landcover variable name")),
            column(4, textInput("dep_varname", "Dependent variable name"))),
          fluidRow(
            column(4, textInput("x_coords_varname", "X-coordinate variable name")),
            column(4, textInput("y_coords_varname", "Y-coordinate variable name"))
          ),

          # regression equation
          h4('Regression equation'),
          fluidRow(
            column(6, textInput("reg_formula", "y ~ x1 + x2 + ..."))
          ),

          # values
          h4("Values"),
          fluidRow(
            column(3, textInput("landcover_invasive", "Invasive landcover ID (only one)")),
            column(3, textInput("landcover_susceptible", "Landcovers susceptible to invasion (if more than one, comma delimited)", "1,2,3"))
          ),
          fluidRow(
            column(3, numericInput("spread_rate", "Invasion spread rate (%/100)", value = 0.05, min = 0, max = 1, step = 0.01)),
            column(3, numericInput("birdcell", "Random invasion (%/100)", value = 0.05, min = 0, max = 1, step = 0.01)),
            column(3, numericInput("simlength", "Simulation length (years)", value = 50, min = 0, max = NA, step = 1))
          ),
          fluidRow(
            column(3, numericInput("simulation_count", "Number of simulations", value = 100, min = 0, max = NA, step = 100)),
            column(3, numericInput("dep_var_modifier", "Dependent variable modifier", value = 0.5, min = NA, max = NA, step = 0.10)),
            column(3, numericInput("num_cores", "Number of cores (max 5)", value = 5, min = 0, max = 5, step = 1))
          ),

      # Submit and download buttons
      br(),
      br(),
      fluidRow(
        column(1, submitButton(text = 'Submit')),
        column(1, downloadButton('download', "Download"))
      )


    ),

    # Main panel for displaying outputs
    mainPanel(h2("Output preview"), width = 2,

      NULL

    )

  )

)


# server ----
server <- function(input, output) {



}


# run app ----
shinyApp(ui, server)
