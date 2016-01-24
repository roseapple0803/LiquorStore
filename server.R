library(shiny)
library(ggplot2)
library(plyr)
library(dplyr)
library(DT)

mydf <- read.csv("bcl-data.csv", stringsAsFactors=FALSE)

hasData <- function(x)
{
  if (nrow(x) == 0){
    
    return (FALSE)
  }    
  else
    return (TRUE)  
}



shinyServer(function(input, output, session) {
  
  output$captionPrice <- renderText({
    paste("Price range: ", input$price[1], "to ", input$price[2], " dollars")
  })
  
  output$captionProduct <- renderText({
    paste("Product: ", input$type, " from ", input$countryInput)
  })
  
  
  
  bigdf <- reactive({
    mydf %>% filter(Price >= input$price[1], Price <= input$price[2], 
                    Type == input$type)
  })
  
  
  theDF <- reactive({    
    dataset <- bigdf()
    
    if (nrow(dataset) == 0){
      return ()
    }
    
    dataset %>%
      filter(
        #Subtype == input$subtype,
        
        Country == input$countryInput
      )
  })
  
  
  
  dfTally <- reactive({    
    ddply(theDF(), c("Country", "Type", "Subtype"), summarise, 
          AVG_PRICE=mean(Price, na.rm=TRUE), MIN_PRICE=min(Price, na.rm=TRUE), 
          MAX_PRICE=max(Price, na.rm=TRUE), AVG_ALCOHOL_CONTENT=mean(Alcohol_Content), TOTAL=length(Price))
    
  })
  
  
  ###########################################################  
  
  dfSubtype <- reactive({
    #     if (is.null(theDF())){
    #       return (NULL)  
    #     }
    
    dataset <- theDF() %>% filter(Type == input$type) %>% select(Subtype) %>% distinct()
    
    
  })
  
  ###########################################################  
  
  
  output$captionPrice <- renderText({
    paste(input$price[1], "to ", input$price[2], " dollars")
  })
  
  output$captionProduct <- renderText({ 
    if (hasData(currDataset())){
      paste(input$type, " from ", input$countryInput)
    }
    else
      paste("NONE available")
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$productUI <- renderUI({
    radioButtons("type", "Product type:", unique(mydf$Type), selected = "WINE")  
  })
  
  
  output$countryUI <- renderUI({
    if (is.null(bigdf())){
      return (NULL)  
    }
    
    
    dataset <- bigdf()
    if (nrow(dataset) == 0){
      return ()
    }
    
    selectInput("countryInput", "Country of Origin",
                sort(unique(dataset$Country)),
                selected = dataset$Country[1])
  })
  
  
  output$subtypeUI <- renderUI({
    # radioButtons("subtype", "Available in:", unique(dfSubtype()$Subtype), selected = dfSubtype()$Subtype[1])
  })
  
  ########################################################### 
  #  
  #  output$subproductUI <- renderUI({
  #    radioButtons("subtype", "Sub Product type:", unique(mydf$subType), selected = "Red WINE")      
  #  })
  #
  
  
  
  output$subtypeList <- renderTable({
    dataset <- dfSubtype()
    names(dataset) <- c("Available in")
    dataset
  })
  
  
  
  
  output$liquorplot <- renderPlot({
    if (is.null(theDF())) {
      print("theDF is not ready")
      return()
    }
    
    
    ggplot(theDF(), aes(Alcohol_Content)) +
      geom_histogram(colour="grey", fill="lightblue", bins=25) 
    
  })
  
  
  #output$results <- renderTable({
  output$results <- DT::renderDataTable({
    dataset <- theDF()  
    dataset[,c("Subtype", "Name", "Alcohol_Content", "Price", "Sweetness")]
  })
  
  
  
  
  
  ###########################################################  
  output$summary <- renderTable({
    dataset <- dfTally()
    dataset[,c("Subtype", "AVG_PRICE", "MIN_PRICE", "MAX_PRICE", "AVG_ALCOHOL_CONTENT", "TOTAL")]
  })
  
  
  ###########################################################  
  
  observe({   
    print(input$price)
  })
  ########################################################### 
  
})


# selectInput("countryInput", "Country of Origin",
#             sort(unique(mydf$Country)),
#             selected = "CANDA")


# theDF <- reactive({
#       if (is.null(input$countryInput)){
#         return (NULL)
#       }
#  
#   mydf %>%
#     filter(Price >= input$price[1],
#            Price <= input$price[2],
#            Type == input$type,
#            
#            #Subtype == input$subtype,
#            
#            Country == input$countryInput
#     )
# })
