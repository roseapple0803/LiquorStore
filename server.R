library(shiny)
library(plyr)
library(dplyr)
library(ggplot2)
library(DT)


 
#mydf <- read.csv("bcl-data.csv", stringsAsFactors=FALSE)
mydf <- readRDS("alcohol.rds")


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




shinyServer(function(input, output) {
  
  currDataset <- reactive({
    mydf %>% filter(Price >= input$price[1], Price <= input$price[2])
  })
  
  
  
  bigDF <- reactive({
    if (is.null(input$type)){
      return (NULL)
    }
    
    else if (is.null(input$countryInput)){
      return (NULL)
    }
    
    else
      currDataset() %>% filter(Type == input$type, Country == input$countryInput)
  })
  
  
 #################################
 
 output$echo <- renderUI({
   
#    if (isAcceptable(currDataset())){
#      echoProduct <- paste(input$type, " from ", input$countryInput)
#    }
#    else
#      echoProduct <- "NONE available"
   
#   
   if (isAcceptable(currDataset())){
     list("Price Range: ", span(input$price[1], style="color:salmon"), "to ", span(input$price[2], style="color:salmon"), 
          " Canadian Dollars", br(), "Product: ", span(input$type, style="color:salmon"), "from ", 
          span(input$countryInput, style='color:salmon'))
   }
   
   else{
        list("Price Range: ", span(input$price[1], style="color:salmon"), "to ", span(input$price[2], style="color:salmon"), 
             " Canadian Dollars", br(), "Product: ", span("NONE available", stype="color:salmon"))
      }
 
 })
 
 
 
 #################################


  
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
      
      if (!is.null(input$type)){
        thedf <- currDataset() %>% filter(Type == input$type)
        selectInput("countryInput", "Country of Origin", sort(unique(thedf$Country)), 
                    selected = thedf$Country[1]) 
      }
    }
    
    else
      (NULL)
  })
  
  
  
  
  output$subtypeList <- renderTable({
    if (isAcceptable(currDataset())){
      
      if (!is.null(input$countryInput)){
        thedf <- currDataset() %>% filter(Type == input$type, Country == input$countryInput) %>% select(Subtype) %>% distinct()
        names(thedf) <- c("Available in")
        
        if (isAcceptable(thedf)){
          thedf 
        }
      }
      
    }
    
    else
      (NULL)
  })
  
  

  
  output$liquorplot <- renderPlot({
    if (isAcceptable(currDataset())){
      
      thedf <- bigDF()
      if (!is.null(thedf))
      { 
        # remove all the rows with NA Alcohol_Content 
        thedf <- thedf[!is.na(thedf$Alcohol_Content),]
        
        thestart <- min(thedf$Alcohol_Content)
        theend <- max(thedf$Alcohol_Content)
        thestep <- ifelse((theend - thestart) >= 2, 2, 0.1)
         
        if (!is.null(thedf$Alcohol_Content)){
          avgMsg <- paste("Average alcohol content within your search criteria is ", round(mean(thedf$Alcohol_Content),2))
        
          ggplot(thedf, aes(Alcohol_Content)) +
            geom_histogram(fill="lightblue", colour ="slateblue4", bins=25) + 
 
            ggtitle("Histogram of Alcohol Content") + ylab("Count") +
            theme(plot.title=element_text(size=rel(1.5),face="bold")) + 
        
            scale_x_continuous(breaks=seq(thestart, theend, thestep)) +
           
          
            theme(axis.text=element_text(size=12),
                  axis.title=element_text(size=14,face="bold")) +
            geom_vline(xintercept= mean(thedf$Alcohol_Content), col="salmon", size=1) + 
            annotate("text", x=-Inf, y=Inf, label=avgMsg, hjust=-.2, vjust=3, label=avgMsg)
        }

        else{
          print(thedf$Alcohol_Content)        
          return()
        }

      }
    }
    
  })
  

 
  
    
    output$results <- DT::renderDataTable({
      if (isAcceptable(currDataset())){
  
        thedf <- bigDF()
        if (!is.null(thedf))
        {
          thedf[,c("Subtype", "Name", "Alcohol_Content", "Price", "Sweetness")]
        }
      }
    })
  
  
    output$summary <- renderTable({
      if (isAcceptable(currDataset())){
        
        thedf <- bigDF()
        if (!is.null(thedf)){
          df <- ddply(thedf, c("Country", "Type", "Subtype"), summarise, 
                       AVG_PRICE=mean(Price, na.rm=TRUE), MIN_PRICE=min(Price, na.rm=TRUE), 
                       MAX_PRICE=max(Price, na.rm=TRUE), AVG_ALCOHOL_CONTENT=mean(Alcohol_Content), TOTAL_LABELS=length(Price))
        
          # make sure $AVG_PRICE is there
          if (!is.null(df$AVG_PRICE)){
            df %>% select(Subtype, AVG_PRICE, MIN_PRICE, MAX_PRICE, AVG_ALCOHOL_CONTENT, TOTAL_LABELS)
          }
          else{
            #print(df$AVG_PRICE)
            return()
          }
        }
       
          
      }
    })



  output$summaryView <- renderPlot({
    if (isAcceptable(currDataset())){
      
      thedf <- bigDF()
      if (!is.null(thedf)){
        df <- ddply(thedf, c("Country", "Type", "Subtype"), summarise, 
                    AVG_PRICE=mean(Price, na.rm=TRUE), MIN_PRICE=min(Price, na.rm=TRUE), 
                    MAX_PRICE=max(Price, na.rm=TRUE), AVG_ALCOHOL_CONTENT=mean(Alcohol_Content), TOTAL_LABELS=length(Price))
        
        # make sure $AVG_PRICE is there
        if (!is.null(df$AVG_PRICE)){
          filtered <- df %>% select(Subtype, AVG_PRICE, MIN_PRICE, MAX_PRICE, AVG_ALCOHOL_CONTENT, TOTAL_LABELS)
          
          ggplot(data=filtered, aes(x=reorder(Subtype, TOTAL_LABELS), y=TOTAL_LABELS)) + 
            geom_bar(stat = "identity", colour="red") + coord_flip()
        }
        else{
          #print(df$AVG_PRICE)
          return()
        }
      }
      
      
    }
    
  })

    
    output$helpguide <- renderUI({
#       HTML(paste("This small Shiny application demonstrates Shiny's automatic UI updates.", 
#                  "Move the Number of bins slider and notice how the renderPlot expression is automatically 
#                   re-evaluated when its dependant, input$bins,changes,causing a histogram with 
#                  a new number of bins to be rendered.", "Hello", "World", sep="<br/><br/>"))
      
      
#       list("This samll ",  span("Shiny application", style='color:salmon'), "demonstrates Shiny's automatic UI updates.", br(),br(), 
#            "Move the Number of bins slider and notice how the renderPlot expression is automatically 
#                   re-evaluated when its dependant, input$bins,changes,causing a histogram with 
#                  a new number of bins to be rendered.", br(),br(),
#            strong("Hello"), br(),"World")
      
      list("This small ",  strong("Shiny application"), "demonstrates Shiny's automatic UI updates.", br(),br(), 
           "To start using the app, ", br(), "* decide on your price range by moving the slider or ", br(), 
           "* pick your product type or ", br(), 
           "* choose country of origin from the drop down list", br(),br(),

          "Notice how all the related data are automatically re-evaluated and refreshed.",
           br(),br(),
           "Also click a tab such as ", strong("Plot"), "or ", strong("Tally"), "to see corresponding histogram plot and data display based on your search criteria.")

    })


})
