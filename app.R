#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(RefManageR)
library(shinyjs)

#Interface
ui <- fluidPage(
  
  useShinyjs(),
  
  fluidRow(
    
    column(1,
           NULL
    ),
    column(10,
           titlePanel("DOI Drop"), 
           
           actionButton("appHelp", label = "Help"),
           
           tags$hr(style="border-color: black;"),
           
           #Input box for DOI
           textInput("doi", label = h3("Enter a DOI"), value = ""),
           
           #Lookup DOI that was dropped in input box
           actionButton("getrefButton","Get reference"),
           
           hr(),
           
           #Display the reference in the UI
           fluidRow(column(10, textOutput("reference"))),
           
           tags$hr(style="border-color: black;"),
           
           
           #Input box for user corrections to displayed reference
           textInput("notes", label = h3("Note corrections, if any"), value = ""),
           
           tags$hr(style="border-color: black;"),
           #hr(),
           
           h3("Submit reference + corrections"),
           
           #Write the new reference to file, adding a notes field with
           #user-supplied corrections
           actionButton("submitButton","Submit"), 
           
           hr(),
           
           #Output field for console messages and errors
           fluidRow(column(10, textOutput("console"))),
           
           tags$hr(style="border-color: black;"),
           #hr(),
           
           #h3("Reset all fields"),
           
           #Reset the app to default state
           actionButton("reset", "Reset all fields"),
           
           tags$hr(style="border-color: black;")
    ),
    column(1,
           NULL
    )
  )
)


#Server
server <- function(input, output) {
  
  observeEvent(input$appHelp, {
    showModal(modalDialog(
      title = "DOI Drop help", 
      fluidPage(includeMarkdown("help.md")),
      size = "m",
      easyClose = TRUE
    ))
  })
  
  
  
  #Create a reactive variable with slots associated
  #with the get reference and submit output sections.
  #This is necessary to later clear these outputs
  #when the Reset button is clicked.
  v <- reactiveValues(ref=NULL, con=NULL)
  
  #Creates the output$console variable I need
  #for capturing console messages/errors.
  insertUI("#console", where = "beforeEnd", ui = NULL)  
  
  #Attempt DOI lookup only on "Get reference" button click
  observeEvent(input$getrefButton, {
    
    #Set some ref formatting options
    #From RefManageR
    BibOptions(max.names=200, bib.style="authoryear", no.print.fields=c("editor", "month", "url", "publisher"))
    
    #Look up the DOI and return the ref
    #Takes DOI from the input box on webapp
    #From RefManageR
    v$ref <- GetBibEntryWithDOI( rev(unlist(strsplit(input$doi, split="https://doi.org/")))[1] )
    
  })
  
  #Actions to perform when user hits the submit botton
  observeEvent(input$submitButton, {
    
    #Check if input field has something in it, and if yes
    #write that input to the notes field of the reference.
    #This will automatically create the notes field if it
    #doesn't already exist.
    if(input$notes != ""){v$ref$notes <- input$notes}
    
    #Check if the output file already exists and
    #create it if not.
    if(!file.exists("CEC_Refs.bib")){file.create("CEC_Refs.bib")}
    
    #Read in existing datafile
    #Contents will be harvested periodically
    #From RefManageR
    bib <- ReadBib("CEC_Refs.bib", check=F)
    
    #Capture messages and errors in a file
    z <- file("errors.Rout", open="wt")
    sink(z, type="message")
    
    #Merge the refs already in the file with the new ref added
    #From RefManageR
    try(bibnew <- bib + v$ref)
    
    #Write the updated dataset back to file
    #From RefManageR
    try(WriteBib(bibnew, file="CEC_Refs.bib"))
    
    #Close the connection
    sink(type="message")
    close(z)
    
    #Save the contents of the log file to a reactive variable
    #For later printing to the UI. Again, this variable can
    #be reset later when necessary.
    v$con <- readLines("errors.Rout")
    
    #Delete the log file
    file.remove("errors.Rout")
    
  })
  
  #This clears the input fields if the user hits the reset button
  #Also clears the ref and console output fields
  observeEvent(input$reset, {
    
    #From shinyjs package
    reset("doi")
    reset("notes")
    
    #These variables can be erased in code, unlike output$reference and output$console
    v$ref <- NULL
    v$con <- NULL
    
  })
  
  #Prints the reference if/when its there and nothing otherwise.
  #Default behavior was to print "NULL" to the output field
  #when there was no ref, which was annoying.
  output$reference <- renderPrint({ if(is.null(v$ref)){invisible(v$ref)}else{v$ref} })
  
  #As above, but sends console messages to the UI. Prints nothing
  #if there are no messages.
  output$console <- renderPrint({ if(is.null(v$con)){ invisible(v$con) }else{ v$con }  })
  
}


shinyApp(ui=ui, server=server)
