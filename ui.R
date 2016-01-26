library(shiny)

shinyUI(fluidPage(
  titlePanel("Welcome to BC Liquor Store",windowTitle = "Liquor Price"),
 
  sidebarLayout(
    sidebarPanel(  
     
      sliderInput("price", "Choose your price range:", min=1, max=100, c(5,40), pre="$"), 
      br(),
      uiOutput("productUI"),
      uiOutput("countryUI"),
      tableOutput("subtypeList")
    ),
    
    
    mainPanel(
      h4("Price Range:", textOutput("captionPrice", container = span), style="color:steelblue"),
      h4("Product: ", textOutput("captionProduct", container = span), style="color:steelblue"),
      br(),
      br(),
            tabsetPanel(type = "tabs", 
                        tabPanel("Plot", br(), br(), plotOutput("liquorplot")), 
                        tabPanel("Tally", tableOutput("summary")), 
                        tabPanel("List", DT::dataTableOutput("results")), 
                        tabPanel("Help", uiOutput("helpguide"))
            )
      
    )
  )
  
))
