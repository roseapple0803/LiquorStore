library(shiny)

# shinyUI(fluidPage(theme = "bootstrap.css", 
shinyUI(fluidPage(
     tags$head(
      tags$style(HTML("
      @import url('https://fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
            "))
                  ),           
  
  titlePanel(
    h1("Welcome to BC Liquor Store", style = "font-family: 'Lobster', cursive;align = 'center';
          font-weight: 500;color: steelblue"), 
      windowTitle = "Liquor Price"),
     
  sidebarLayout( 
    sidebarPanel(  
      sliderInput("price", "Choose your price range:", min=1, max=100, c(50,70), pre="$"), 
      br(),
      uiOutput("productUI"),
      uiOutput("countryUI"),
      tableOutput("subtypeList"),
      
      tags$div(class = "header", checked = NA,
                tags$a(href = "https://github.com/roseapple0803/LiquorStore", "Click Here to read my codes!")
              )
                
    ),

    
    mainPanel(
      strong("Your search criteria"),
     
      uiOutput("echo"),
      br(),
      br(),
      tabsetPanel(type = "tabs", 
                        tabPanel("Plot", br(), br(), plotOutput("liquorplot")), 
                        tabPanel("Tally", tableOutput("summary")), 
                        tabPanel("TallyChart", plotOutput("summaryView")), 
                        tabPanel("ProductList", DT::dataTableOutput("results")), 
                        tabPanel("Help", uiOutput("helpguide"))
      )

    )
  )
  
))
