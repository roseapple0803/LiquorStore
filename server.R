library(shiny)
library(plyr)
library(dplyr)
library(ggplot2)
library(DT)


mydf <- read.csv("bcl-data.csv", stringsAsFactors=FALSE)


isAcceptable <- function(x)
{
  if (is.null(x)){
    
    return (FALSE)
  }
  else if (nrow(x) == 0){
    
    return (FALSE)
  }    
  else
    return (TRUE)  
}



################################################################## 


shinyServer(function(input, output) {
  
  currDataset <- reactive({
    mydf %>% filter(Price >= input$price[1], Price <= input$price[2])
  })
  
  
  
  bigDF <- reactive({
    currDataset() %>% filter(Type == input$type, Country == input$countryInput)
  })
  
  
 
  output$captionPrice <- renderText({
    paste(input$price[1], "to ", input$price[2], " dollars")
  })
  
  output$captionProduct <- renderText({ 
    if (isAcceptable(currDataset())){
      paste(input$type, " from ", input$countryInput)
    }
    else
      paste("NONE available")
  })
  
  
  output$productUI <- renderUI({
    if (isAcceptable(currDataset())){
      df <- currDataset()
      radioButtons("type", "Product type:", unique(df$Type), selected = unique(df$Type)[1])
    }
    
    else
      (NULL)
  })
  
  
  output$countryUI <- renderUI({
    if (isAcceptable(currDataset())){
      thedf <- currDataset() %>% filter(Type == input$type)
      selectInput("countryInput", "Country of Origin", sort(unique(thedf$Country)), 
                  selected = thedf$Country[1])  
    }
    
    else
      (NULL)
  })


  
  
  output$subtypeList <- renderTable({
    if (isAcceptable(currDataset())){
      thedf <- currDataset() %>% filter(Type == input$type, Country == input$countryInput) %>% select(Subtype) %>% distinct()
      names(thedf) <- c("Available in")
      thedf 
    }
    
    else
      (NULL)
  })
  
  
  output$liquorplot <- renderPlot({
    if (isAcceptable(currDataset())){
      ggplot(bigDF(), aes(Alcohol_Content)) +
        geom_histogram(fill="lightblue", colour ="slateblue4", bins=25) 
    }
    
  })


  

  output$summary <- renderTable({
    if (isAcceptable(currDataset())){
      thedf <- ddply(bigDF(), c("Country", "Type", "Subtype"), summarise, 
                     AVG_PRICE=mean(Price, na.rm=TRUE), MIN_PRICE=min(Price, na.rm=TRUE), 
                     MAX_PRICE=max(Price, na.rm=TRUE), AVG_ALCOHOL_CONTENT=mean(Alcohol_Content), TOTAL_LABELS=length(Price))
  
      thedf %>% select(Subtype, AVG_PRICE, MIN_PRICE, MAX_PRICE, AVG_ALCOHOL_CONTENT, TOTAL_LABELS)
    }
  })
  
  
  
  output$results <- DT::renderDataTable({
    if (isAcceptable(currDataset())){
      thedf <- bigDF()
      thedf[,c("Subtype", "Name", "Alcohol_Content", "Price", "Sweetness")]
    }
  })
 
})
