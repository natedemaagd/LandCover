library(shiny)


# ui ----
ui <- fluidPage(

  # App title ----
  titlePanel("Land Cover Simulation"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(h2("User input"), width = 5,

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
            column(4, textInput("x_coords_varname", "X-coordinate variable name")),
            column(4, textInput("y_coords_varname", "Y-coordinate variable name"))
          ),

          # regression equation
          h4('Regression equation'),
          fluidRow(
            column(12, textInput("reg_formula", "y ~ x1 + x2 + ..."))
          ),

          # values
          h4("Values"),
          fluidRow(
            column(4, textInput("landcover_invasive", "Invasive landcover ID (only one)")),
            column(5, textInput("landcover_susceptible", "Susceptible landcovers (comma-delimited)", "1,2,3")),
            column(3, numericInput("spread_rate", "Invasion spread rate (%/100)", value = 0.05, min = 0, max = 1, step = 0.01))
          ),
          fluidRow(
            column(4, numericInput("birdcell", "Random invasion (%/100)", value = 0.05, min = 0, max = 1, step = 0.01)),
            column(4, numericInput("simlength", "Simulation length (years)", value = 50, min = 0, max = NA, step = 1)),
            column(4, numericInput("simulation_count", "Number of simulations", value = 100, min = 0, max = NA, step = 100))
          ),
          fluidRow(
            column(4, numericInput("dep_var_modifier", "Dependent variable modifier", value = 0.5, min = NA, max = NA, step = 0.10)),
            column(4, numericInput("sample", "Regression obs sample size", value = 5000, min = 1, step = 1)),
            column(4, numericInput("num_cores", "Number of cores (max 5)", value = 5, min = 0, max = 5, step = 1))
          ),
          fluidRow(
            column(12, textInput("covar_adjustment", "Covariate adjustment"))
          ),

      # Submit and download buttons
      br(),
      br(),
      fluidRow(
        column(2, submitButton(text = 'Submit')),
        column(2, downloadButton('download', "Download"))
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
