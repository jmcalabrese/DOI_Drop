#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rcrossref)
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
  v <- reactiveValues(doi=NULL, dsp=NULL, ref=NULL, con=NULL)
  
  #Creates the output$console variable I need
  #for capturing console messages/errors.
  insertUI("#console", where = "beforeEnd", ui = NULL)  
  
  #Attempt DOI lookup only on "Get reference" button click
  observeEvent(input$getrefButton, {
    
    #Strip off anything preceeding the actual DOI (e.g., https://doi.org/)
    v$doi <- sub("^.*(?=10[.])", "", input$doi, perl=T)
    
    #Get the raw display ref, formatted appropriately
    dspRaw <- cr_cn(v$doi, format="text", style="springer-basic-brackets-no-et-al")
    
    #Clip off the leading number and space
    dspFr <- sub("^[0-9]\\.\\s","", x=dspRaw)
    
    #Drop the doi from the end of the ref
    dspClean <- sub("\\sdoi:\\s.*","", x=dspFr)
    
    #Save the cleaned display ref for output
    v$dsp <- dspClean
    
  })
  
  #Actions to perform when user hits the submit botton
  observeEvent(input$submitButton, {
    
    #Get the ref in RIS format
    v$ref <- cr_cn(v$doi, format="ris")
    
    #Capture the note from the input field
    notes <- paste0("\n", "N1  - ", input$notes, "\n")
    
    #This finds the carriage return immediately before
    #the ER field and replaces it with "notes". 
    #Therefore "notes" needs to both begin and end
    #with a carriage return
    v$ref <- sub("\n(?=ER)", notes, v$ref, perl=T)
    
    #Check if the output file already exists and
    #create it if not.
    if(!file.exists("CEC_Refs.ris")){file.create("CEC_Refs.ris")}
    
    #Read in existing datafile
    ris <- readLines("CEC_Refs.ris")
    
    #Capture messages and errors in a file
    z <- file("errors.Rout", open="wt")
    sink(z, type="message")
    
    #Write the updated dataset back to file
    try(write(c(ris, v$ref), "CEC_Refs.ris"))
    
    #Close the connection
    sink(type="message")
    close(z)
    
    #Save the contents of the log file to a reactive variable
    #For later printing to the UI. Again, this variable can
    #be reset later when necessary.
    v$con <- readLines("errors.Rout")
    
    #If no warning/error messages are returned, print "Success!"
    if(identical(v$con, character(0))){v$con <- "Sucess!"}
    
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
    v$dsp <- NULL
    v$ref <- NULL
    v$con <- NULL
    
  })
  
  #Prints the reference if/when its there and nothing otherwise.
  output$reference <- renderPrint({ if(is.null(v$dsp)){invisible(v$dsp)}else{ cat(v$dsp) } })
  
  #As above, but sends console messages to the UI. Prints nothing
  #if there are no messages.
  output$console <- renderPrint({ if(is.null(v$con)){ invisible(v$con) }else{ cat(v$con) }  })
  
  
}


shinyApp(ui=ui, server=server)
