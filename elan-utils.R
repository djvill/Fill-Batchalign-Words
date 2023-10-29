##Convert dataframe to XML document that conforms to ELAN file requirements
df_to_elan <- function(df, mediaFile=NULL) {
  ##Check arg
  if (!is.data.frame(df)) stop("df must be a data.frame")
  reqCols <- c("TierID", "Text", "Start", "End")
  missingCols <- setdiff(reqCols, colnames(df))
  if (length(missingCols) > 0) stop("Missing required columns: ", 
                                    paste(missingCols, collapse=" "))
  if ("File" %in% colnames(df)) stop("df must contain *one file's* worth of annotations")
  
  ##Packages
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(xml2)
  
  ##Get time slots
  timeSlots <- 
    df %>%
    select(Start, End) %>% 
    pivot_longer(everything(), names_to=NULL, values_to="TIME_VALUE") %>% 
    distinct() %>% 
    arrange(TIME_VALUE) %>% 
    mutate(TIME_SLOT_ID = paste0("ts", seq_len(n())),
           .before=TIME_VALUE)
  
  ##Create TIME_ORDER node
  TIME_ORDER <-
    timeSlots %>% 
    rowwise() %>% 
    mutate(TIME_SLOT = list(TIME_SLOT = structure(list(), 
                                                  TIME_SLOT_ID = TIME_SLOT_ID, 
                                                  TIME_VALUE = TIME_VALUE))) %>% 
    pull(TIME_SLOT) %>%
    list(TIME_ORDER = .) %>%
    as_xml_document()
  
  ##Add timeslots to annotation dataframes
  tierDF <-
    df %>%
    mutate(ANNOTATION_ID = paste0("a", seq_len(n()))) %>% 
    left_join(timeSlots %>% 
                rename(Start = TIME_VALUE, 
                       TIME_SLOT_REF1 = TIME_SLOT_ID),
              "Start") %>% 
    left_join(timeSlots %>% 
                rename(End = TIME_VALUE, 
                       TIME_SLOT_REF2 = TIME_SLOT_ID),
              "End") %>% 
    mutate(ANNOTATION_VALUE = replace_na(Text, "")) %>% 
    select(TierID, ANNOTATION_VALUE, ANNOTATION_ID, 
           TIME_SLOT_REF1, TIME_SLOT_REF2) %>% 
    nest(ANNOTATION = -TierID)
  
  ##List of TIER nodes
  TIERs <-
    tierDF %>% 
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
    mutate(TIER = map2(TIER, TierID,
                       ~ set_attributes(.x, list(LINGUISTIC_TYPE_REF="default-lt",
                                                 TIER_ID = .y,
                                                 PARTICIPANT = .y)) %>% 
                         ##Restore node names zapped by set_attributes()
                         set_names(rep("ANNOTATION", length(.x))))) %>%
    ##Extract from dataframe and add node name
    pull(TIER) %>% 
    set_names(rep("TIER", length(.))) %>%
    ##Convert to XML
    lmap(as_xml_document)
  
  ##Get minimal xml
  minXML <- readLines("minimal-elan.xml")
  if (startsWith(minXML[2], "<!--")) {
    minXML <- minXML[-2]
  }
  
  ##Fill in XML
  outXML <- read_xml(paste(minXML, collapse=""))
  xml_replace(xml_find_first(outXML, "//TIME_ORDER"), TIME_ORDER)
  defaultTier <- xml_find_first(outXML, "//TIER[@TIER_ID='default']")
  walk(rev(TIERs),
       ~ xml_add_sibling(defaultTier, .x))
  xml_remove(defaultTier)
  ##Optionally add media file
  if (!is.null(mediaFile)) {
    header <- xml_find_first(outXML, "//HEADER")
    xml_attr(header, "MEDIA_FILE") <- mediaFile
  }
  
  ##Return
  outXML
}

##Add TIER node as a sibling after final TIER node
add_tier <- function(eaf, tierName, participant=tierName) {
  ##Check args
  if (!inherits(eaf, "xml_document")) stop("eaf is not an xml_document")
  if (!is.character(tierName) || length(tierName) != 1) 
    stop("tierName must be a string (length-1 character)")
  if (!is.character(participant) || length(participant) != 1) 
    stop("participant must be a string (length-1 character)")
  
  ##Packages
  library(xml2)
  
  ##Modify eaf by adding tier
  tiers <- xml_find_all(eaf, "//TIER")
  lastTier <- tiers[length(tiers)]
  newTier <- list(TIER = structure(list(),
                                   LINGUISTIC_TYPE_REF = "default-lt",
                                   TIER_ID = tierName))
  if (!is.null(participant)) {
    attr(newTier$TIER, "PARTICIPANT") <- participant
  }
  xml_add_sibling(lastTier, as_xml_document(newTier))
  
  ##Return
  eaf
}

