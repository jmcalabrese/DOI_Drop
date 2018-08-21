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

#Functions
articleRIS <- function(ref){
  
  #lookup article DOI
  #ref <- cr_cn(article.doi, format="citeproc-json")
  
  #ref type
  typ <- "TY  - JOUR\n"
  
  #article title
  mtl <- paste0("TI  - ", ref$title, "\n")
  
  #authors
  nAut <- dim(ref$author)[1]
  fName <- ref$author$given
  lName <- ref$author$family
  for(i in 1:nAut){
    atiRaw <- paste(lName[i], fName[i], sep=", ")
    ati <- paste0("AU  - ", atiRaw, "\n")
    if(i==1){
      ats <- ati
    } else {
      ats <- paste0(ats, ati)
    }
  }
  
  #publication year
  pyr <- paste0("PY  - ", ref$`published-print`$`date-parts`[1], "\n")
  
  #volume number
  avl <- paste0("VL  - ", ref$volume, "\n")
  
  #journal title
  jnm <- paste0("T2  - ", ref$`container-title`, "\n")
  
  #article page range or number
  apr <- ref$page
  ano <- ref$`article-number`
  
  if(!is.null(apr)){
    asp <- paste0("SP  - ", apr, "\n")
  }else if(!is.null(ano)){
    asp <- paste0("SP  - ", ano, "\n")
  }else{
    asp <- NULL
  }
  
  
  #doi
  doi <- paste0("DO  - ", ref$DOI, "\n")
  
  #end of record
  enr <- "ER  - \n"
  
  #create the RIS file
  ris <- paste0(typ, mtl, ats, pyr, avl, asp, jnm, doi, enr)
  
  return(ris)
  
}


chapterRIS <- function(ref){
  
  #Parse chapter.doi into book.doi
  #book.doi <- sub("[_.][0-9]?[0-9]?[0-9]$", "", chapter.doi)
  
  #Simultaneously lookup book and chapter DOIs
  #ref <- cr_cn(chapter.doi, format="citeproc-json")
  book.isbn <- ref$ISBN[1]
  book <- cr_works(filter=c(type="book", isbn=book.isbn), limit=1)
  book.doi <- book$data$doi
  ref2 <- cr_cn(book.doi, format="citeproc-json")
  
  #ref type
  typ <- "TY  - CHAP\n"
  
  
  #book title
  mtl <- ref2$title
  stl <- ref2$subtitle
  if(length(stl) > 0){
    btl <- paste0("T2  - ", mtl, ": ", stl, "\n")
  } else{
    btl <- paste0("T2  - ", mtl, "\n")
  }
  
  #book editors
  nEds <- dim(ref2$editor)[1]
  for(i in 1:nEds){
    ediRaw <- paste(ref2$editor[i, 2], ref2$editor[i, 1], sep=", ")
    edi <- paste0("A2  - ", ediRaw, "\n")
    if(i==1){
      eds <- edi
    } else {
      eds <- paste0(eds, edi)
    }
  }
  
  #chapter title
  ctl <- paste0("TI  - ", ref$title, "\n")
  
  #chapter authors
  nAut <- dim(ref$author)[1]
  for(i in 1:nAut){
    atiRaw <- paste(ref$author[i, 2], ref$author[i, 1], sep=", ")
    ati <- paste0("AU  - ", atiRaw, "\n")
    if(i==1){
      ats <- ati
    } else {
      ats <- paste0(ats, ati)
    }
  }
  
  #publication year
  pyr <- paste0("PY  - ", ref2$`published-print`$`date-parts`[1], "\n")
  
  #chapter start page
  spg <- paste0("SP  - ", sub("-?[0-9]?[0-9]?[0-9]?[0-9]$", "", ref$page), "\n")
  
  #chapter end page
  epg <- paste0("EP  - ", sub("^?[0-9]?[0-9]?[0-9]?[0-9]-", "", ref$page), "\n")
  
  #publisher location
  pcy <- paste0("CY  - ", ref2$`publisher-location`, "\n")
  
  #publisher name
  pnm <- paste0("PB  - ", ref2$publisher, "\n")
  
  #doi
  doi <- paste0("DO  - ", ref$DOI, "\n")
  
  #end of record
  enr <- "ER  - \n"
  
  #create the RIS file
  ris <- paste0(typ, btl, eds, ctl, ats, pyr, spg, epg, pcy, pnm, doi, enr)
  
  return(ris)
  
}

bookRIS <- function(ref){
  
  #Parse chapter.doi into book.doi
  #book.doi <- sub("[_.][0-9]?[0-9]?[0-9]$", "", chapter.doi)
  
  #Simultaneously lookup book and chapter DOIs
  #ref <- cr_cn(book.doi, format="citeproc-json")
  
  #ref type
  typ <- "TY  - BOOK\n"
  
  #book title
  mtl <- ref$title
  stl <- ref$subtitle
  if(length(stl) > 0){
    btl <- paste0("TI  - ", mtl, ": ", stl, "\n")
  } else{
    btl <- paste0("TI  - ", mtl, "\n")
  }
  
  
  #authors
  nAut <- dim(ref$author)[1]
  if(length(nAut)>0){
    for(i in 1:nAut){
      atiRaw <- paste(ref$author[i, 2], ref$author[i, 1], sep=", ")
      ati <- paste0("AU  - ", atiRaw, "\n")
      if(i==1){
        ats <- ati
      } else {
        ats <- paste0(ats, ati)
      }
    }
  } else{
    ats <- NULL
  }
  
  #book editors
  nEds <- dim(ref$editor)[1]
  if(length(nEds)>0){
    for(i in 1:nEds){
      ediRaw <- paste(ref$editor[i, 2], ref$editor[i, 1], sep=", ")
      edi <- paste0("A2  - ", ediRaw, "\n")
      if(i==1){
        eds <- edi
      } else {
        eds <- paste0(eds, edi)
      }
    }
  } else{
    eds <- NULL
  }
  
  #publication year
  pyr <- paste0("PY  - ", ref$`published-print`$`date-parts`[1], "\n")
  
  #publisher location
  pcy <- paste0("CY  - ", ref$`publisher-location`, "\n")
  
  #publisher name
  pnm <- paste0("PB  - ", ref$publisher, "\n")
  
  #doi
  doi <- paste0("DO  - ", ref$DOI, "\n")
  
  #end of record
  enr <- "ER  - \n"

  #create the RIS file
  if(!is.null(ats) & is.null(eds)){
    ris <- paste0(typ, btl, ats, pyr, pcy, pnm, doi, enr)
  } else if(is.null(ats) & !is.null(eds)){
    ris <- paste0(typ, btl, eds, pyr, pcy, pnm, doi, enr)
  }
  
  return(ris)
  
}


formatRIS <- function(ris){
  
  #break the ris file into rows
  risRW <- strsplit(ris, '\n')
  
  #loop through rows and break each row in to a vector with field code
  #and corresponding string.
  #assemble in a 2X(N-1) matirx, where N is the number of rows in the ris file
  res <- NULL
  #note that this drops the "end record" row, hence N-1 rows in the output
  for(i in 1:(length(risRW[[1]])-1)){
    ts <- risRW[[1]][[i]]
    ps <- unlist(strsplit(ts, '  - '))
    if(length(ps)==2){
      res <- rbind(res, c(ps[1], ps[2]))
    }
  }
  
  #get a list of all the unique field codes in the focal RIS file
  tgs <- unique(res[,1])
  
  #create appropriately formated strings for each field
  #idea is to paste these together with only a blank space between each
  
  #article, book, or chapter authors
  if("AU" %in% tgs){
    atsV <- res[which(res[,1]=="AU"), 2]
    ats <- paste0(paste(atsV, collapse="; "), " ")
  }else{
    ats <- NULL
  }
  
  #publication year
  if("PY" %in% tgs){
    pyr <- paste0("(", res[which(res[,1]=="PY"), 2], "). ")
  }else{
    pyr <- ""
  }
  
  #primary title
  if("TI" %in% tgs){
    ptl <- paste0(res[which(res[,1]=="TI"), 2], ". ")
  }else{
    ptl <- ""
  }
  
  #editors for edited volume or book chapter in an edited volume
  if("A2" %in% tgs){
    edsV <- res[which(res[,1]=="A2"), 2]
    eds <- paste0(paste0(paste(edsV, collapse="; ")), " (Eds.), ")
  }else{
    eds <- NULL
  }
  
  #in edited volume
  iev <- "In "
  
  #either book title for a chapter or journal name for a journal article
  if("T2" %in% tgs){
    stl <- res[which(res[,1]=="T2"), 2]
    btl <- paste0(stl, " ")
    jnm <- paste0(stl, ", ")
  }else{
    btl <- ""
    jnm <- ""
  }
  
  #either the start page for a book chapter or the article page range or number for a journal article
  if("SP" %in% tgs){
    spg <- res[which(res[,1]=="SP"), 2]
    apg <- paste0(spg, ". ")
  }else{
    spg <- ""
    apg <- ""
  }
  
  #end page for a book chapter
  if("EP" %in% tgs){
    epg <- res[which(res[,1]=="EP"), 2]
  }else{
    epg <- ""
  }
  
  #prepare the page range/article number for text display
  if(("SP" %in% tgs) & ("EP" %in% tgs)){
    cpg <- paste0("(pp. ", spg, "-", epg, "). ")
  }else if(("SP" %in% tgs) & !("EP" %in% tgs)){
    cpg <- paste0("(pp. ", spg, "). ")
  }else{
    cpg <- ""
  }
  
  #publisher name
  if("PB" %in% tgs){  
    pnm <- paste0(res[which(res[,1]=="PB"), 2], ". ")
  }else{
    pnm <- ""
  }
  
  #volume number for journal article
  if("VL" %in% tgs){  
    avl <- paste0(res[which(res[,1]=="VL"), 2], ":")
  }else{
    avl <- ""
  }
  
  #doi
  if("DO" %in% tgs){
    doi <- paste0("DOI:", res[which(res[,1]=="DO"), 2], ".")
  }else{
    doi <- ""
  }
  
  
  #get the reference type
  typ <- res[which(res[,1]=="TY"), 2]
  
  #assemble plain text citation appropriate for ref type
  if(typ=="JOUR"){
    txt <- paste0(ats, pyr, ptl, jnm, avl, apg, doi)
  }else if(typ=="BOOK" & !is.null(ats) & is.null(eds)){
    txt <- paste0(ats, pyr, ptl, pnm, doi)
  }else if(typ=="BOOK" & is.null(ats) & !is.null(eds)){
    txt <- paste0(eds, pyr, ptl, pnm, doi)
  }else if(typ=="CHAP"){
    txt <- paste0(ats, pyr, ptl, iev, eds, btl, cpg, pnm, doi)
  }else{
    txt <- "Can't handle this type of reference"
  }
  
  return(txt)
}


#Interface
ui <- fluidPage(
  
  useShinyjs(),
  
  fluidRow(
    
    column(1,
           NULL
    ),
    column(10,
           titlePanel("DOI Drop"), 
           
           tags$hr(style="border-color: black;"),
    
           #input box for DOI
           textInput("doi", label = h3("Enter a DOI"), value = "", width='500px'),
          
           #lookup DOI that was dropped in input box and app help
           div(style="display: inline-block;vertical-align:top; width: 100px;",actionButton("getrefButton","Get reference")),
           div(style="display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")),
           div(style="display: inline-block;vertical-align:top; width: 100px;",actionButton("appHelp", label = "Help")),
           
           hr(),
           
           #display the reference in the UI
           fluidRow(column(10, textOutput("reference"))),
           
           tags$hr(style="border-color: black;"),
           
           
           #input box for user corrections to displayed reference
           textInput("notes", label = h3("Note corrections, if any"), value = "", width='500px'),
           
           tags$hr(style="border-color: black;"),
           
           #write ref to file, adding a notes field with user-supplied corrections
           h3("Submit reference + corrections"),
           actionButton("submitButton","Submit"), 
           
           hr(),
           
           #output field for console messages and errors
           fluidRow(column(10, textOutput("console"))),
           
           tags$hr(style="border-color: black;"),
           
           #reset the app to default state
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
  
  
  
  #create a reactive variable with slots associated
  #with the get reference and submit output sections.
  #this is necessary to later clear these outputs
  #when the Reset button is clicked.
  v <- reactiveValues(doi=NULL, dsp=NULL, ref=NULL, con=NULL)
  
  #creates the output$console variable
  #for capturing console messages/errors.
  insertUI("#console", where = "beforeEnd", ui = NULL)  
  
  #attempt DOI lookup only on "Get reference" button click
  observeEvent(input$getrefButton, {
    
    #strip off anything preceeding the actual DOI (e.g., https://doi.org/)
    v$doi <- sub("^.*(?=10[.])", "", input$doi, perl=T)
    
    #get the ref in full detail
    ref <- cr_cn(v$doi, format="citeproc-json")
    
    #create an RIS file from the full detail ref
    if(ref$type=="article-journal"){
        v$ref <- articleRIS(ref)
      } else if(ref$type=="chapter"){
        v$ref <- chapterRIS(ref)
      } else if(ref$type=="book" | ref$type=="monograph"){
        v$ref <- bookRIS(ref)
      }
    
    #create and save a formatted text ref from the RIS file
    v$dsp <- formatRIS(v$ref)
    
  })
  
  #actions to perform when user hits the submit botton
  observeEvent(input$submitButton, {
  
    #capture the note from the input field
    notes <- paste0("\n", "N1  - ", input$notes, "\n")
    
    #this finds the carriage return immediately before
    #the ER field and replaces it with "notes". 
    #"notes" therefore needs to begin and end
    #with a carriage return
    v$ref <- sub("\n(?=ER)", notes, v$ref, perl=T)
    
    #check if the output file already exists and
    #create it if not.
    if(!file.exists("CEC_Refs.ris")){file.create("CEC_Refs.ris")}
    
    #read in existing datafile
    ris <- readLines("CEC_Refs.ris")
    
    #capture messages and errors in a file
    z <- file("errors.Rout", open="wt")
    sink(z, type="message")
    
    #write the updated dataset back to file
    try(write(c(ris, v$ref), "CEC_Refs.ris"))
    
    #close the connection
    sink(type="message")
    close(z)
    
    #save the contents of the log file to a reactive variable
    #for later printing to the UI. this variable can
    #be reset later when necessary.
    v$con <- readLines("errors.Rout")
    
    #if no warning/error messages are returned, print "Success!"
    if(identical(v$con, character(0))){v$con <- "Success!"}
    
    #Delete the log file
    file.remove("errors.Rout")
    
  })
  
  #this clears the input fields and ref and console if 
  #console output fields when user hits the reset button
  observeEvent(input$reset, {
    
    #from shinyjs package
    reset("doi")
    reset("notes")
    
    #these variables can be erased in code, unlike output$reference and output$console
    v$dsp <- NULL
    v$ref <- NULL
    v$con <- NULL
    
  })
  
  #prints the reference if/when it's there and nothing otherwise.
  output$reference <- renderPrint({ if(is.null(v$dsp)){invisible(v$dsp)}else{ cat(v$dsp) } })
  
  #as above, but sends console messages to the UI. 
  #prints nothing if there are no messages.
  output$console <- renderPrint({ if(is.null(v$con)){ invisible(v$con)}else{ cat(v$con) }  })
  
  
}


shinyApp(ui=ui, server=server)
