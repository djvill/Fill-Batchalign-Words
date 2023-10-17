## Fill-Batchalign-Words Shiny app

library(shiny)
library(dplyr)
library(purrr)
library(magrittr)
library(xml2)
library(reticulate)

##Batchalign-processing function
source("process-batchalign.R")
procBatch <- partial(processBatchalign, 
                     noWor=FALSE, formatText=FALSE, mergeMax=Inf, 
                     eafUtils="github")

##Python libraries
pathlib <- import("pathlib")
elan_data <- import("elan_data")

# Parameters ------------------------------------------------------------------

##Automatically use given files to expedite testing; set to NULL to force drag-n-drop
testFileAISeg <- NULL
testFileBA <- NULL
# testFileAISeg <- dir("Test-Files/AI-Segmented/", full.names=T)[c(2, 3)]
# testFileBA <- dir("Test-Files/Batchalign/", full.names=T)[c(2, 4)]

##Debugging
##Show additional UI element(s) at top of main panel for debugging?
showDebug <- FALSE


# UI ----------------------------------------------------------------------

ui <- fluidPage(
  # tags$head(
    # tags$link(rel = "stylesheet", type = "text/css", href = "lvc-plotter.css")
  # ),
  titlePanel("Fill Batchalign Words"),
  h4("Dan Villarreal (University of Pittsburgh)"), 
  sidebarLayout(
    sidebarPanel(
      ##File upload (drag-n-drop) box
      fileInput("fileAISeg",
                label="Drag and drop the AI-segmented file(s) into the box below",
                buttonLabel="Browse...",
                placeholder="Your file here",
                accept=".eaf",
                multiple=TRUE),
			
			##File upload (drag-n-drop) box
      fileInput("fileBA",
                label="Drag and drop the Batchalign file(s) into the box below",
                buttonLabel="Browse...",
                placeholder="Your file here",
                accept=".eaf",
                multiple=TRUE),
      
      ##Configuration settings
      uiOutput("options"),
      uiOutput("outputButton")
    ),
    
    mainPanel(uiOutput("debug"),
              tableOutput("fileInfo"),
							uiOutput("output"))
  ),
  p("App code on ", a("GitHub", href="https://github.com/djvill/Fill-Batchalign-Words"), class="footer")
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
      filePaths %>% 
        imap_dfr(~ procBatch(.x) %>% 
                   ##Override temp path (datapath) with actual name (name)
                   mutate(File = .y))
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
      `names(outFiles())` = names(outFiles()),
      `names(outFiles()[[1]])` = names(outFiles()[[1]]),
      `formals(outFiles()[[1]]$save_ELAN)` = formals(outFiles()[[1]]$save_ELAN)
		)
  })
  
  ##Config options
  ##Column selection
  output$options <- renderUI({
    ##Only show once both files have been uploaded
    req(AISeg(), BA())
    
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
  })
  
  ##"Generate output" button
  output$outputButton <- renderUI({
    ##Only show once both files have been uploaded
    req(AISeg(), BA())
    
    actionButton("genOutput", "Generate output")
  })
  
  ##Match input files
  inFiles <- reactive({
    req(AISeg(), BA())
    full_join(nest(AISeg(), `AI-Segmented`=-File),
              nest(BA(), Batchalign=-File),
              "File")
  })
  
  ##File info
  output$fileInfo <- renderTable({
    req(inFiles())
    inFiles() %>%
      rowwise() %>%
      mutate(across(-File,
                    ~ ifelse(!is.null(.x),
                             .x %>%
                               filter(!startsWith(Tier, "wor@")) %>%
                               nrow() %>%
                               paste("turns"),
                             "(No file uploaded)")))
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
      mutate(across(Text, betterText)) ##betterText(): from process-batchalign.R
    
    ##Convert output to Elan using built-in methods
    timeSlots <-
      filled %>%
      select(File, Start, End) %>% 
      pivot_longer(-File, names_to=NULL, values_to="TIME_SLOT_VALUE") %>% 
      distinct() %>% 
      mutate(TIME_SLOT_ID = paste0("ts", seq_len(n())),
             ##TODO: Double-check that this .by scopes over n(), too
             .by=File, .before=TIME_SLOT_VALUE)
    out <-
      filled %>% 
      mutate(ANNOTATION_ID = paste0("a", seq_len(n())),
             ##TODO: Double-check that this .by scopes over n(), too
             .by=File) %>% 
      left_join(timeSlots %>% 
                  rename(Start = TIME_SLOT_VALUE, 
                         TIME_SLOT_REF1 = TIME_SLOT_ID),
                c("File", "Start")) %>% 
      left_join(timeSlots %>% 
                  rename(End = TIME_SLOT_VALUE, 
                         TIME_SLOT_REF2 = TIME_SLOT_ID),
                c("File", "End")) %>% 
      rename(ANNOTATION_VALUE = Text) %>% 
      select(-c(Start, End, Overlap)) %>% 
      nest(ANNOTATION = -c(File, Tier))# %>% 
      # rename(TIER_ID = Tier) %>% 
      # mutate(PARTICIPANT = TIER_ID,
      #        .after=TIER_ID) %>% 
      # nest(TIER = -File)
    ##Create XML nodes
    
    ##TIME_ORDER nodes 
    nodesTime <-
      timeSlots %>% 
      rowwise() %>% 
      mutate(TIME_SLOT = list(structure(list(), 
                                        TIME_SLOT_ID = TIME_SLOT_ID, 
                                        TIME_SLOT_VALUE = TIME_SLOT_VALUE))) %>% 
      ungroup() %>% 
      nest(TIME_ORDER = -File) %>% 
      mutate(TIME_ORDER = map(TIME_ORDER, 
                              ~ .x$TIME_SLOT %>% 
                                set_names(rep("TIME_SLOT", length(.))) %>% 
                                list(TIME_ORDER = .) %>% 
                                as_xml_document()))
    
    ##TIER nodes
    out %>% 
      ##Create lists of annotation nodes
      mutate(TIER = map(ANNOTATION,
                        ~ .x %>% 
                          rowwise() %>% 
                          mutate(ANNOTATION = list(
                            ANNOTATION = list(
                              ALIGNABLE_ANNOTATION = structure(list(
                                ANNOTATION_VALUE = list(ANNOTATION_VALUE)),
                                ANNOTATION_ID = ANNOTATION_ID, 
                                TIME_SLOT_REF1 = TIME_SLOT_REF1,
                                TIME_SLOT_REF2 = TIME_SLOT_REF2)))) %>% 
               pull(ANNOTATION))) %>%
      ##Add tier-level attributes to lists of annotation nodes
      mutate(TIER = map2(TIER, Tier,
                         ~ set_attributes(.x, list(LINGUISTIC_TYPE_REF="default-lt",
                                                   TIER_ID = .y,
                                                   PARTICIPANT = .y)) %>% 
                           ##Restore node names zapped by set_attributes()
                           set_names(rep("ANNOTATION", length(.x))))) %>%
      ##Extract from dataframe and add node name
      pull(TIER) %>% 
      set_names(rep("TIER", length(.)))# %>% 
      # str(2, list.len=6)
      # list(TIERS = .) %>% 
      # as_xml_document() %>% 
      # write_xml("tmp")
    
    ##Convert output to Elan using https://github.com/AlejandroCiuba/elan_data
    
    ##Get a list of ELAN_Data objects
    elanList <-
      out %>% 
      mutate(across(c(Start, End), as.integer)) %>% 
      select(File, TIER_ID = Tier, START = Start, STOP = End, TEXT = Text) %>%
      nest(df = -File) %>% 
      mutate(file = list(pathlib$Path(File)),
             audio = list(pathlib$Path(str_replace(File, "eaf$", "wav"))),
             init_df=TRUE) %>% 
      select(-File) %>% 
      pmap(elan_data$ELAN_Data$from_dataframe) %>% 
      set_names(unique(out$File))
    
    ##Modify list (modifies in-place because Python)
    elanList %>% 
      walk(~ .x$add_tiers(c("Comment","Noise","Redaction"))) # %>% 
      # walk(~ .x$remove_tiers("default")) %>%  ##doesn't seem to work
      # walk(\(eaf) walk(eaf$tier_names, \(tier) eaf$add_participant(tier, tier))) ##errors out
  }) %>% 
    ##Only run when "Generate output" is clicked
    bindEvent(input$genOutput)
	
  ##Main panel
	output$output <- renderUI({
	  req(outFiles())
	  
	  ##File info 
		downloadButton("OutputFile", "Download merged file(s)")
	})
	
	
	##Download handler
	output$OutputFile <- downloadHandler(
	  filename=function() {
	    if (length(outFiles())==1) {
	      names(outFiles())
	    } else {
	      "merged_eafs.zip"
	    }
	  },
	  content=function(file) {
	    if (length(outFiles())==1) {
	      outFiles()[[1]]$save_ELAN(rename=pathlib$Path(file))
	    } else {
	      outFiles() %>% 
	        walk(~ .x$save_ELAN())
	      zip(file, names(outFiles()))
	    }
	  }
	)
}


shinyApp(ui, server)
