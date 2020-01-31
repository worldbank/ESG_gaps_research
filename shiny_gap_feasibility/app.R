library(shiny)
setwd("..")
source("_common.R")
source("R/load_data.R")
source("R/esg_filling_gap_feasibility.R")
setwd("shiny_gap_feasibility")

# Define UI for application
ui <- fluidPage(

  # Application title
  titlePanel("Change in number of indicators suitable for extrapolation"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("th_cg",
                  label = "Coverage threshold",
                  choices = seq(0, 1, .1),
                  selected = .5
                  ),
      selectInput("th_cv",
                  label = "Coefficient of variation threshold",
                  choices = seq(0, 1, .1),
                  selected = .3
                  ),
      selectInput("tg_year",
                  label = "Target year",
                  choices = c(2015:2019),
                  selected = 2018
                  ),
      selectInput("mx_year",
                  label = "Max year",
                  choices = c(2015:2019),
                  selected = 2018
                  ),
      selectInput("bs_year",
                  label = "Base year",
                  choices = c(1980:2005),
                  selected = 2000
                  )
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("bars",
                   height = "600px",
                   width = "800px")
    )
  )
)

# Define server logic
server <- function(input, output) {

  output$bars <- renderPlotly({

    th_cg   <- as.numeric(input$th_cg  )
    th_cv   <- as.numeric(input$th_cv  )
    tg_year <- as.numeric(input$tg_year)
    mx_year <- as.numeric(input$mx_year)
    bs_year <- as.numeric(input$bs_year)


    #--------- Benchmark
    y <- fn_lstrank(x = x,
                    th_cg   = th_cg    ,
                    th_cv   = th_cv    ,
                    tg_year = tg_year  ,
                    mx_year = mx_year  ,
                    bs_year = bs_year)

    ialy <- y[["ialy"]]
    ialy2 <- y[["ialy2"]]

    p_irank  <- ggplotly(chbar(df = ialy))
    p_irank2 <- ggplotly(chbar(df = ialy2))
    plotly::subplot(p_irank, p_irank2)
  }) # end of output$bars
} # end of server

# Run the application
shinyApp(ui = ui, server = server)
