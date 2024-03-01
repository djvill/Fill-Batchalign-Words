## Fill-Batchalign-Words Shiny app

library(shiny)
library(dplyr)
library(purrr)
library(magrittr)
library(xml2)

##Batchalign-processing function
source("process-batchalign.R")
procBatch <- partial(processBatchalign, 
                     noWor=FALSE, formatText=FALSE, mergeMax=Inf, 
                     eafUtils="github")

##ELAN utilities
source("elan-utils.R")

# Parameters ------------------------------------------------------------------

##Automatically use given files to expedite testing; set to NULL to force drag-n-drop
testFileAISeg <- NULL
testFileBA <- NULL
# testFileAISeg <- dir("Test-Files/AI-Segmented/", full.names=T)[c(2, 3)]
# testFileBA <- dir("Test-Files/Batchalign/", full.names=T)[c(2:4)]

##Debugging
##Show additional UI element(s) at top of main panel for debugging?
showDebug <- FALSE


# UI ----------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Fill Batchalign Words"),
  h4("Dan Villarreal (University of Pittsburgh)"), 
  sidebarLayout(
    sidebarPanel(
      ##Functionality selection
      selectInput("task",
                  "Choose a task",
                  list("Fill Batchalign transcription into AI segmentation" = "fill",
                       "Format Batchalign transcription" = "format"),
                  "fill"),
      
      ##File upload (drag-n-drop) box(es)
      uiOutput("upload"),
      
      ##Configuration settings
      uiOutput("options")
    ),
    
    mainPanel(uiOutput("debug"),
              uiOutput("outputButton"),
							uiOutput("output"))
  ),
  p("App code on ", a("GitHub", href="https://github.com/djvill/Fill-Batchalign-Words", target="_blank"), 
    class="footer")
)


# Server ------------------------------------------------------------------

##Convenience functions to display/undisplay HTML elements
display <- function(x) {
  if (!("shiny.tag" %in% class(x))) {
    stop("display() only works with shiny.tag objects, not ", 
         paste(class(x), collapse="/"), " objects.")
  }
  
  if (is.null(x$attribs$style)) {
    x$attribs$style <- "display: inherit;"
  } else {
    x$attribs$style <- x$attribs$style %>% 
      gsub("display: \\w+;", "", .) %>%
      paste0("display: inherit;")
  }
  
  x
}
undisplay <- function(x) {
  if (!("shiny.tag" %in% class(x))) {
    stop("undisplay() only works with shiny.tag objects, not ", 
         paste(class(x), collapse="/"), " objects.")
  }
  
  if (is.null(x$attribs$style)) {
    x$attribs$style <- "display: none;"
  } else {
    x$attribs$style <- x$attribs$style %>% 
      gsub("display: \\w+;", "", .) %>%
      paste0("display: none;")
  }
  
  x
}


server <- function(input, output, session) {
  ##Read files
  if (is.null(testFileAISeg) || !all(file.exists(testFileAISeg))) {
    ##If no test file specified, file must be uploaded via drag-n-drop
    AISeg <- eventReactive(input$fileAISeg, {
      filePaths <- set_names(input$fileAISeg$datapath, input$fileAISeg$name)
      
      if (!all(endsWith(filePaths, ".eaf"))) {
        stop("AI-segmented file must be an .eaf file")
      }
      filePaths %>% 
        imap_dfr(~ procBatch(.x) %>% 
                   ##Override temp path (datapath) with actual name (name)
                   mutate(File = .y))
    })
  } else if (all(file.exists(testFileAISeg))) {
    ##If test file exists, use it
    AISeg <- eventReactive(TRUE, {
      testFileAISeg %>% 
        map_dfr(procBatch)
    })
  }
  if (is.null(testFileBA) || !all(file.exists(testFileBA))) {
    ##If no test file specified, file must be uploaded via drag-n-drop
    BA <- eventReactive(input$fileBA, {
      filePaths <- set_names(input$fileBA$datapath, input$fileBA$name)
      
      if (!all(endsWith(filePaths, ".eaf"))) {
        stop("AI-segmented file must be an .eaf file")
      }
      if (input$task=="fill") {
        filePaths %>% 
          imap_dfr(~ procBatch(.x) %>% 
                     ##Override temp path (datapath) with actual name (name)
                     mutate(File = .y))
      } else {
        filePaths %>% 
          imap_dfr(~ processBatchalign(.x) %>% 
                     ##Override temp path (datapath) with actual name (name)
                     mutate(File = .y))
      }
    })
  } else if (all(file.exists(testFileBA))) {
    ##If test file exists, use it
    BA <- eventReactive(TRUE, {
      testFileBA %>% 
        map_dfr(procBatch)
    })
  }
  
	
  ##Debug wrapper
  output$debug <- renderUI({
    out <- verbatimTextOutput("debugContent")
    
    ##Optionally display or undisplay
    if (showDebug) {
      display(out)
    } else {
      undisplay(out)
    }
  })
  
  ##Verbatim debugging text (works best if a list() of objects with names from
  ##  environment, to 'peek into' environment)
  output$debugContent <- renderPrint({
    list(
      `inFiles()` = inFiles(),
      `formattedFiles()` = formattedFiles()
		)
  })
  
  ##File upload (drag-n-drop) box(es)
  output$upload <- renderUI({
    uploadAISeg <- fileInput("fileAISeg",
                             label="Drag and drop the AI-segmented file(s) into the box below",
                             buttonLabel="Browse...",
                             placeholder="Your file here",
                             accept=".eaf",
                             multiple=TRUE)
    uploadBA <- fileInput("fileBA",
                          label="Drag and drop the Batchalign-transcribed file(s) into the box below",
                          buttonLabel="Browse...",
                          placeholder="Your file here",
                          accept=".eaf",
                          multiple=TRUE)
    if (input$task=="fill") {
      tagList(uploadAISeg, uploadBA)
    } else {
      tagList(uploadBA)
    }
  })
  
  ##Config options
  ##Column selection
  output$options <- renderUI({
    ##Only show once both files have been uploaded
    req(AISeg(), BA())
    
    if (input$task=="fill") {
      tagList(
        ##Column selection (will create 3 selection inputs)
        h3("Config options"),
        radioButtons("overlaps", "Text for overlapping turns", 
                     c("On both turns", "On a separate tier")),
        radioButtons("containment", "How to determine when to fill a word",
                     c("Totally contained in turn", "Partially contained in turn")),
        checkboxGroupInput("noMatch", "Separate tier(s) to create for words that don't match any turns",
                           c("Batchalign turn", "Batchalign word"), 
                           c("Batchalign turn", "Batchalign word"))
      )
    }
  })
  
  
  ##Match input files
  inFiles <- reactive({
    if (input$task=="fill") {
      req(AISeg(), BA())
      full_join(nest(AISeg(), `AI-Segmented`=-File),
                nest(BA(), Batchalign=-File),
                "File")
    } else {
      req(BA())
      nest(BA(), Batchalign=-File)
    }
  })
  
  ##File info
  output$fileInfo <- renderTable({
    req(inFiles())
    fileTable <-
      inFiles() %>%
      rowwise() %>%
      mutate(across(-File,
                    ~ ifelse(!is.null(.x),
                             .x %>%
                               filter(!startsWith(Tier, "wor@")) %>%
                               nrow() %>%
                               paste("turns"),
                             "(No file uploaded)")))
    if (input$task=="fill") {
      fileMessage <- 
        fileTable %>% 
        mutate(across(-File, ~ str_remove_all(.x, " turns|\\(|\\)"))) %>% 
        str_glue_data("{File} ({`AI-Segmented`}, {Batchalign})")
      message(paste(c("File(s) uploaded (AI-segmented turns, Batchalign turns):",
                      fileMessage), 
                    collapse="\n  "))
    } else {
      fileMessage <- 
        fileTable %>% 
        mutate(across(-File, ~ str_remove_all(.x, " turns|\\(|\\)"))) %>% 
        str_glue_data("{File} ({Batchalign})")
      message(paste(c("File(s) uploaded (Batchalign turns):",
                      fileMessage), 
                    collapse="\n  "))
      fileTable <- fileTable %>% rename(Length = Batchalign)
    }
    fileTable
  })
  
  ##File info table & "Generate output" button
  output$outputButton <- renderUI({
    if (input$task=="fill") {
      ##Only show once both files have been uploaded
      req(inFiles())
      
      tagList(
        tableOutput("fileInfo"),
        div(actionButton("genOutput", "Generate output"),
            p("(Please be patient—it's slow)",
              style="margin: 0px 5px;"),
            style="display: flex;align-items: center;")
      )
    }
  })
  
  ##Get output files
  outFiles <- reactive({
    ##Translate options to fillWords() args
    overlaps <- if_else(input$overlaps=="On both turns", "duplicate", "separate")
    containment <- if_else(input$overlaps=="Totally contained in turn", "total", "partial")
    if ("Batchalign turn" %in% input$noMatch) {
      if ("Batchalign word" %in% input$noMatch) {
        noMatch <- "both"
      } else {
        noMatch <- "turn"
      }
    } else if ("Batchalign word" %in% input$noMatch) {
      noMatch <- "word"
    } else {
      noMatch <- "none"
    }
    
    ##Only use files that match for both
    matchFiles <- 
      inFiles() %>% 
      rowwise() %>% 
      filter(!if_any(everything(), is.null))
    
    ##Inputs for fillWords()
    segDF <- 
      matchFiles %>% 
      select(File, `AI-Segmented`) %>% 
      unnest(`AI-Segmented`)
    wordDF <- 
      matchFiles %>% 
      select(File, Batchalign) %>% 
      unnest(Batchalign) %>% 
      ##Mimic call requirement
      structure(call=str2lang("processBatchalign(noWor=FALSE, formatText=FALSE, mergeMax=Inf)")) %>% 
      ##Split words
      splitWords() ##From process-batchalign.R
    
    ##Fill words
    filled <- 
      fillWords(segDF, wordDF, 
                overlaps=overlaps, containment=containment, noMatch=noMatch) %>% 
      mutate(across(Text, betterText)) %>% ##betterText(): from process-batchalign.R
      select(-Overlap)
    
    ##Convert output to Elan using built-in methods
    eaflist <- 
      filled %>% 
      nest(df = -File) %>% 
      pull(df, File) %>% 
      imap(~ df_to_elan(.x, mediaFile=gsub("eaf$", "wav", .y)))
    
    ##Add additional tiers
    walk(eaflist, 
         ~ .x %>% 
           add_tier("Noise", "Noise") %>% 
           add_tier("Comment", "Comment") %>% 
           add_tier("Redaction", "Redaction"))
    
    ##Return eaflist
    eaflist
  }) %>% 
    ##Only run when "Generate output" is clicked
    bindEvent(input$genOutput)
  
  formattedFiles <- reactive({
    inFiles() %>% 
      pull(Batchalign, File) %>% 
      imap(~ df_to_elan(.x, mediaFile=gsub("eaf$", "wav", .y)))
  })
	
  ##Main panel
	output$output <- renderUI({
	  if (input$task=="fill") {
	    req(outFiles())
	    
	    ##Download button
	    tagList(
	      hr(),
	      downloadButton("OutputFile", "Download merged file(s)")
	    )
	  } else {
	    req(inFiles())
	    tagList(
	      hr(),
	      downloadButton("OutputFile", "Download formatted file(s)")
	    )
	  }
	})
	
	##Download handler
	output$OutputFile <- downloadHandler(
	  filename=function() {
	    if (input$task=="fill") { 
	      if (length(outFiles())==1) {
	        names(outFiles())
	      } else {
	        "merged_eafs.zip"
	      }
	    } else {
	      if (length(formattedFiles())==1) {
	        names(formattedFiles())
	      } else {
	        "formatted_eafs.zip"
	      }
	    }
	  },
	  content=function(file) {
	    if (input$task=="fill") {
	      if (length(outFiles())==1) {
	        write_xml(outFiles()[[1]], file)
	      } else {
	        outFiles() %>% 
	          iwalk(write_xml)
	        zip(file, names(outFiles()))
	      }
	    } else {
	      if (length(formattedFiles())==1) {
	        write_xml(formattedFiles()[[1]], file)
	      } else {
	        formattedFiles() %>% 
	          iwalk(write_xml)
	        zip(file, names(formattedFiles()))
	      }
	    }
	  }
	)
}


shinyApp(ui, server)
