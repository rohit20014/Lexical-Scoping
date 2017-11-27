library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(leaflet)
library(DT)

#Available themes- cerulean, cosmo, cyborg, darkly, flatly, journal, lumen, paper, readable, sandstone, simplex, 
#slate, spacelab, superhero, united, yeti

prop <- read.csv("R:\\Berkadia\\data_merged_shiny1.csv", nrows = 490, stringsAsFactors = FALSE)

salePrice_max <- as.numeric(max(prop$PriceAmt));
ui <- fluidPage(
  theme = shinytheme("sandstone"),
  titlePanel("Property Sales Estimator"),
  hr(),
  navbarPage("", id="completeSection", footer="Footer for this page",
  tabPanel("Summary", value="summaryPanel",  
  sidebarLayout(
    sidebarPanel(tags$head(tags$script("
                                       Shiny.addCustomMessageHandler('myCallbackHandler',
                                       function(typeMessage) {console.log(typeMessage)
                                       if(typeMessage == 1){
                                       console.log('got here');
                                       $('a:contains(Summary)').click();
                                       }
                                       if(typeMessage == 2){
                                       $('a:contains(Property Info)').click();
                                       }
                                       })
                                       ")),
      headerPanel(h1("Berkadia",style = "font-family: 'Lobster', 
                     cursive; font-weight: 500; line-height: 1.1;color: #4d3a7d;")),
      sliderInput("PriceAmtInput", "Price", 0, salePrice_max, c(25, 29495308.31), pre = "$"),
      radioButtons("propertyTypeInput", "Property Type",
                   choices = c("Multifamily", "Retail", "Industrial", "Other"),
                   selected = "Multifamily"),
      radioButtons("TransactionTypeInput", "Transaction Type",
                   choices = c("Sale", "Refinance"),
                   selected = "Sale"),
      uiOutput("stateOutput"),
      uiOutput("NumFloorsOutput"),
      downloadButton("downloadData", label = "Download", class = "btn-primary", width = '32%'),
      br(), br(),
      tags$div(class="header", checked=NA,
               tags$p("Downloaded file extension is CSV")),
      br(), br(),
      hr(),
      width = 3
      #plotOutput("coolplot") #plot graph at right top
    ),
    mainPanel(
      plotOutput("coolplot"),
      br(), br(),
      DT::dataTableOutput("results")
   )
  )
 ),
#Second tab
tabPanel("Property Info", value = "customerInfoPanel",
         sidebarLayout(sidebarPanel(fluidRow(column(8,  textInput(inputId = "propertyID",placeholder = "Property Id", label = "Property Id"))),
                                    fluidRow(actionButton("getData", "Get Details", class = "btn-primary", width = '40%')),
                                    verbatimTextOutput('x5')
         ),
         mainPanel(
           leafletOutput("houseMap", height = "700px")
         )
         )
)
#Second tab ends
)
)

# Function for the property filter
final_property_data <- prop;
get_property_data <- function(propertyId){
  if(propertyId != '' & !is.na(propertyId) & !is.null(propertyId)){
    final_property_data <- subset(final_property_data, PropertyId == propertyId);
    return(final_property_data)
  }
}

info <-  '';
server <- function(input, output, session) {
  output$stateOutput <- renderUI({
    selectInput("stateInput", "Property State",
                sort(unique(prop$PropertyState)),
                selected = "CA")
  })
    output$NumFloorsOutput <- renderUI({
      selectInput("NumFloorsInput", "Number of Floors",
                  sort(unique(prop$NumFloors)),
                  selected = "2")
    })

  
  filtered <- reactive({
    if (is.null(input$stateInput)) {
      return(NULL)
    }
    if (is.null(input$TransactionTypeInput)) {
      return(NULL)
    }
    prop %>%
      filter(PriceAmt >= input$PriceAmtInput[1],
             PriceAmt <= input$PriceAmtInput[2],
             PropertyType == input$propertyTypeInput,
             TransactionType == input$TransactionTypeInput,
             PropertyState == input$stateInput,
             NumFloors == input$NumFloorsInput
      )
  })
  
  

  output$coolplot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    windowsFonts(Arial=windowsFont("TT Arial"))
    ggplot(filtered(), aes(NumFloors)) +
      geom_histogram()+
      ggtitle("No. of data Vs No. of Floors") +
      labs(x="Floors",y="No. of data")+
      theme(plot.title = element_text(family = "Arial", color="#666666", face="bold", size=32, hjust=0)) +
      theme(axis.title = element_text(family = "Arial", color="#666666", face="bold", size=22))

  })

  
  output$results <- renderDataTable({
    datatable(subset(filtered(),select = c(PropertyId,TransactionType,OwnerName,PropertyName,PropertyType,
                                           PriceAmt,PredictedSalesAmount,TotSqrFt,PropertyState,
                                           PropertyAddress)),escape=FALSE, style = "bootstrap",
              options = list(searching = T, pageLength = 10, filter =FALSE), rownames = FALSE,
              selection = list(mode = 'single', target = 'cell'))%>%
    formatStyle('PropertyId', fontWeight = styleInterval(5, c('normal', 'bold')))%>%
      formatStyle(
        'TotSqrFt',
        background = styleColorBar(prop$TotSqrFt, 'steelblue'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })

  #Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() { paste('data-', Sys.Date(), '.csv', sep='') },
    content = function(file) {
      write.csv(filtered(), file, row.names = FALSE)
    }
  )

  observeEvent(input$results_cell_clicked, {
    info = input$results_cell_clicked
    if(!is.null(info$row)){
      if(info$col==0){
        updateTextInput(session, inputId = "propertyID", value =info$value );
        session$sendCustomMessage("myCallbackHandler", "2")
      }
    }
  })
  
  observeEvent(input$getData, {
    data <- get_property_data(as.numeric(input$propertyID));
    label <- paste("<p style=\"font-family: verdana\"; \"font-size:3\">", "<b>", data[1,24], "<b>","</p>", sep = "<br/>")
    output$x5 = renderPrint({
      cat("=============== Property Details ==================\n\n")
      cat("Property Name:", as.character(data[1,23]), "\n\n")
      cat("Property Address:", as.character(data[1,24]),",", as.character(data[1,25]),",", as.character(data[1,26]),",",as.character(data[1,26]), "\n\n")
      cat("Property Type:", as.character(data[1,10]), "\n\n")
      cat("Number of Rooms:", as.character(data[1,13]), "\n\n")
      cat("Area: ", as.character(data[1,17]), "\n\n")
      cat("=============== Owner Details =====================\n\n")
      cat("Owner Name:", as.character(data[1,29]), "\n\n")
      cat("Owner Address:", as.character(data[1,30]),",", as.character(data[1,31]),",", as.character(data[1,32]), "," , as.character(data[1,33]), "\n\n")
    })
    
    
    output$houseMap <- renderLeaflet({
      title <- "Berkadia Property on Map"
      leaflet() %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addMarkers(lng=data[1,27], lat=data[1,28], popup=label)
    })
    
  })
}
shinyApp(ui = ui, server = server)

