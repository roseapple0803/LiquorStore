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
      textOutput("captionPrice"),
      textOutput("captionProduct"),
      
      h4("Price Range:", textOutput("captionPrice", container = span), style="color:lightblue"),
      h4("Product Chosen: ", textOutput("captionProduct", container = span), style="color:lightblue"),
      br
     
 
    )
  )
  
))
