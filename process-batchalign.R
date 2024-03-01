processBatchalign <- function(eaf, write=FALSE, outDir=".", 
                              eafUtils=c("local","github"), 
                              ##Remove non-blank 'wor@' tiers?
                              ##  (Blank tiers removed regardless)
                              noWor=TRUE,
                              ##Impute missing alignments?
                              imputeMissing=TRUE,
                              ##Format text using betterText()?
                              formatText=TRUE,
                              ##If final turn(s) are un-aligned, duration of 
                              ##  each un-aligned turn (added onto final 
                              ##	aligned turn's end boundary)
                              endBuffer=1000,
                              ##Merge turns on the same tier if they are
                              ##	at most this far apart. (Set to Inf to
                              ##	disable merging.)
                              mergeMax=150) {
  ##Check args
  if (!file.exists(eaf)) {
    stop("File ", eaf, " does not exist.")
  }
  eafUtils <- match.arg(eafUtils)
  
  ##Packages
  library(xml2)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(rlang) ##For call_match()
  
  ##Source eaf-utils.R (for xmllist_to_df())
  localUtils <- "~/Elan-File-Checkers/eaf-utils.R"
  githubUtils <- "https://github.com/djvill/elan-file-checkers/raw/main/eaf-utils.R"
  if (eafUtils=="local" && file.exists(localUtils)) {
    source(localUtils)
  } else {
    source(githubUtils)
  }
  
  ##Read elan file (removing illegal char if needed)
  xml <- tryCatch(read_xml(eaf),
                  error = function(e) {
                    eaf %>% 
                      readLines() %>% 
                      str_remove_all("\u0015") %>% 
                      paste(collapse="\n") %>% 
                      read_xml()
                  })
  
  ##Remove *blank* wor@ tiers
  xml_remove(xml_find_all(xml, "//TIER[@TIER_ID='wor@']"))
  
  ##Optionally remove dependency (wor@) tiers
  if (noWor) {
    xml_remove(xml_find_all(xml, "//TIER[starts-with(@TIER_ID, 'wor@')]"))
  }
  
  ##Convert to DF
  eafName <- basename(eaf)
  df <- 
    list(xml) %>% 
    set_names(eafName) %>% 
    xmllist_to_df(df_nesting="None", tierText=TRUE)
  
  ##Optionally impute boundaries for un-aligned turns:
  if (imputeMissing) {
    ##- For each un-aligned turn that neighbors aligned turns (regardless of 
    ##  tier), set boundaries contiguous to neighbors
    ##- For a group of consecutive un-aligned turns, set outer boundaries
    ##  contiguous to neighboring turns, with equally spaced inner boundaries
    ##- For un-aligned turn(s) at start of transcript, set first turn's start to
    ##  0, with equally spaced inner boundaries until first aligned turn
    ##- For un-aligned turn(s) at end of transcript, set first un-aligned turn's
    ##  start to previous turn's end, and assume a constant duration for turn(s)
    
    ##Set up imputation
    df <- df %>%
      ##Put turns in order by start time, but preserve relative position of turns
      ##  with missing Start
      mutate(TmpStart = na_if(Start, 0)) %>%
      fill(TmpStart, .direction="down") %>%
      ##Restore 0 in first row of each Tier...
      mutate(TmpStart = if_else(is.na(TmpStart) & is.na(lag(Start)), 0, TmpStart),
             .by=Tier) %>%
      ##...and any remaining NA TmpStart (i.e., if >1 un-aligned turn at start)
      replace_na(list(TmpStart = 0)) %>% 
      ##Arrange by TmpStart
      arrange(TmpStart) %>%
      ##Set up temporary columns for imputing un-aligned turns
      mutate(Unaligned = Start==0 & End==0,
             i = row_number(),
             prop = i / n(),
             ##Counters for runs of un-aligned turns
             Run = accumulate(Unaligned, ~ if_else(.y, .x + .y, 0)),
             RevRun = accumulate(Unaligned, ~ if_else(.x, .x + .y, 0), .dir="backward"))
    
    df <- df %>%
      rowwise() %>%
      ##Fix Start for Unaligned
      mutate(Start = case_when(
        ##Impute a 0 for un-aligned first turn
        i==1 && Unaligned ~ 0,
        ##For each un-aligned turn at the end of the transcript, assume endBuffer
        ##  duration (if only one transcript-final un-aligned turn, Start is just
        ##  End[n-1])
        i+RevRun > nrow(df) && Unaligned ~ df$End[max(i-Run, 1)] + endBuffer*(Run-1),
        ##Close gaps: new Start is the ith member of the (Run+RevRun)-long
        ##  sequence between End[i-Run] and Start[i+RevRun]
        ##N.B. lead()/lag() don't work here because of rowwise(), but rowwise()
        ##  is needed to get the row indices (i, Run, RevRun) to work
        Unaligned ~ seq(df$End[max(i-Run, 1)],
                        df$Start[min(i+RevRun, nrow(df))],
                        ##[max(Run, 1)] index needed to avoid error:
                        ##  `true` must have size 1, not size 0
                        length.out=Run+RevRun)[max(Run, 1)],
        .default=Start)) %>%
      ungroup() %>%
      ##New End is much simpler since Start no longer has gaps
      mutate(End = if_else(Unaligned, lead(Start), End),
             ##If needed, do final row separately
             End = if_else(prop==1 & Unaligned, Start+endBuffer, End)) %>% 
      select(-c(TmpStart, Unaligned, i, prop, Run, RevRun))
  }
  
  # ##Start & End: msec -> sec
  # df <- df %>% 
  #   mutate(across(c(Start, End), ~ .x / 1000))
  
  ##Optionally format text using betterText()
  if (formatText) {
    df <- df %>% 
      mutate(across(Text, betterText))
  }
  
  ##Optionally merge consecutive turns with small gaps
  if (is.finite(mergeMax)) {
    df <- df %>%
      ##Establish groups of turns that should be merged
      group_by(Tier) %>%
      mutate(PrevGap = coalesce(Start - lag(End), mergeMax+1),
             GapGroup = cumsum(PrevGap > mergeMax)) %>%
      ##Merge turns
      group_by(Tier, GapGroup) %>%
      summarise(across(Start, min), 
                across(End, max),
                across(Text, ~ paste(.x, collapse=" ")),
                .groups="drop") %>%
      ##Back in Start order
      arrange(Start) %>% 
      select(-GapGroup)
  } else {
    df <- df %>% 
      select(Tier, Start, End, Text)
  }
  
  ##Optionally write
  if (write) {
    df %>% 
      ##Only the columns needed for ELAN_Data
      select(TIER_ID = Tier, START = Start, STOP = End, TEXT = Text) %>%
      ##Write
      write.csv(file.path(outDir, str_replace(eafName, "eaf$", "csv")),
                row.names=FALSE)
  }
  
  ##Add call as attribute
  df <- structure(df, call=call_match(defaults=TRUE))
  
  ##Return df silently
  invisible(df)
}

betterText <- function(x) {
  library(stringr)
  
  ##Format text: restricted mini-lexicon spelling
  x <- x %>% 
    str_replace_all("\\bmm\\b", "mmm") %>%
    str_replace_all("mhm", "mmm hmm") %>%
    str_replace_all("uhhuh", "uh huh") %>% 
    str_replace_all("lemme", "let me") %>% 
    str_replace_all("\\(be\\)cause", "'cause") %>% 
    str_replace_all("\\(un\\)til", "'til")
  
  ##Unwind CHAT format conversions by removing elements:
  x <- x %>% 
    ##&- disfluency markers
    str_remove_all("&-") %>% 
    ##[/] & <> repetition markers
    str_remove_all("\\[/\\]|<|>") %>% 
    ##_ for "linkages"
    str_replace_all("_", " ") %>%
    ##parens for incomplete words
    str_remove_all("[\\(\\)]") %>%
    ##end-of-sentence period
    str_remove_all(" \\.$") %>%
    ##Remove extra space
    str_squish() %>%
    str_trim()
  
  x
}

##Get word alignments
splitWords <- function(df) {
  ##Check arg
  if (!is.data.frame(df)) stop("df must be a data.frame")
  reqCols <- c("File", "Tier", "Text", "Start", "End")
  missingCols <- setdiff(reqCols, colnames(df))
  if (length(missingCols) > 0) stop("Missing required columns: ", 
                                    paste(missingCols, collapse=" "))
  dfCall <- attr(df, "call")
  if (is.null(dfCall)) stop("df missing call attribute--was it run with processBatchalign()?")
  if (dfCall$noWor || dfCall$formatText || is.finite(dfCall$mergeMax)) 
    stop("df must be created with processBatchalign(noWor=FALSE, formatText=FALSE, mergeMax=Inf)")
  
  ##Libraries
  library(dplyr)
  library(stringr)
  library(purrr)
  library(tidyr)
  
  ##Split into words
  df %>% 
    filter(str_detect(Tier, "wor@PAR")) %>% 
    rowwise() %>% 
    mutate(Word = Text %>% 
             ##Set up for word-splitting: each word is Word+Start+End, with spaces in between
             str_remove(" \\.$") %>% 
             str_replace_all("(\\d)_(\\d)", "\\1+\\2") %>%
             str_replace_all("([^\\d]) (\\d)", "\\1+\\2") %>%
             str_replace_all("([^\\d])-(\\d)", "\\1-+\\2") %>% ##Preserve hyphens
             ##Split at spaces *after digits*
             str_split("(?<=\\d) ") %>% 
             ##Make each turn a tibble with columns Word,WordStart,WordEnd
             map_if(~ any(str_detect(.x, "\\d")),
                    ##If there are word alignments
                    ~ tibble(data = .x) %>%
                      separate_wider_delim(data, "+", 
                                           names=c("Word", "WordStart", "WordEnd"),
                                           too_few="align_start") %>% 
                      mutate(across(c(WordStart, WordEnd), as.numeric)),
                    ##If word alignments are missing
                    .else = ~ tibble(Word = str_split_1(.x, " ")) %>% 
                      mutate(WordStart = seq(Start, End, length.out=n()+1)[1:n()],
                             WordEnd = replace_na(lead(WordStart), End)))) %>% 
    unnest(Word) %>% 
    ##Deal with hyphens: Batchalign tokenizes after hyphens, so squish entries together
    mutate(PrevHyphenated = replace_na(lag(endsWith(Word, "-")), FALSE),
           Run = accumulate(PrevHyphenated, ~ .x + !.y)) %>% 
    summarise(across(Word, ~ paste(.x, collapse="")),
              across(WordStart, min),
              across(WordEnd, max),
              .by=c(File, Tier, Run, Start, End)) %>% 
    select(-Run)
}
##Wrapper function for convenience
wordAlignments <- function(eaf, ...) {
  ##Check arg
  if (!file.exists(eaf)) {
    stop("File ", eaf, " does not exist.")
  }
  df <- processBatchalign(eaf, noWor=FALSE, formatText=FALSE, mergeMax=Inf, ...)
  splitWords(df)
}

##Given a dataframe of segmentations and a dataframe of words, fill the latter
##  into the former
fillWords <- function(segDF, wordDF, 
                      overlaps=c("duplicate","separate"),
                      containment=c("total","partial"),
                      noMatch=c("turn","word","both","none")) {
  ##Check args
  stopifnot(is.data.frame(segDF))
  stopifnot(all(c("File", "Tier", "Start", "End") %in% colnames(segDF)))
  stopifnot(is.data.frame(wordDF))
  stopifnot(all(c("File", "Start", "End", "Word", "WordStart", "WordEnd") %in% colnames(wordDF)))
  overlaps <- match.arg(overlaps)
  containment <- match.arg(containment)
  ##Default noMatch
  if (missing(noMatch)) {
    noMatch <- "both"
  } else {
    noMatch <- match.arg(noMatch)
  }
  
  library(dplyr)
  library(stringr)
  library(forcats)
  
  ##Add overlaps to segDF
  segDF <- segDF %>% 
    ##Add Overlap column
    add_count(File, Start, End) %>% 
    mutate(Overlap = n > 1,
           .keep="unused")
  
  ##Get joins
  if (containment=="total") {
    filledJoin <- join_by(File, within(y$WordStart, y$WordEnd, x$Start, x$End))
    noMatchJoin <- join_by(File, within(x$WordStart, x$WordEnd, y$Start, y$End))
  } else {
    filledJoin <- join_by(File, overlaps(y$WordStart, y$WordEnd, x$Start, x$End))
    noMatchJoin <- join_by(File, overlaps(x$WordStart, x$WordEnd, y$Start, y$End))
  }
  
  ##Add words that fall within AI-segmented boundaries (duplicating text for overlaps)
  filledTurns <- 
    segDF %>% 
    left_join(wordDF %>% 
                select(File, starts_with("Word")), 
              filledJoin) %>% 
    summarise(Text = str_c(Word, collapse=" "), ##str_c() preserves NAs
              .by=c(File, Tier, Start, End, Overlap))
  
  ##Separate overlaps if needed
  if (overlaps=="separate") {
    filledTurns <- filledTurns %>% 
      mutate(Text = if_else(Overlap, NA_character_, Text)) %>% 
      bind_rows(filledTurns %>% 
                  filter(Overlap) %>% 
                  distinct(File, Start, End, Text) %>% 
                  mutate(Tier = "Overlap"))
  }
  
  ##Get words that don't fall into any AI-segmented turns
  noMatchWords <- 
    wordDF %>% 
    anti_join(segDF, noMatchJoin)
  noMatchTurns <- noMatchWords %>% 
    summarise(Text = paste(Word, collapse=" "),
              .by=c(File, Start, End)) %>% 
    mutate(Tier = "NoMatch - Turn")
  
  ##Put dataframes together
  allTurns <- filledTurns
  if (noMatch %in% c("turn", "both")) {
    allTurns <- allTurns %>% 
      bind_rows(noMatchTurns) %>% 
      ##Put NoMatch at top
      arrange(File, Tier)
  }
  if (noMatch %in% c("word", "both")) {
    allTurns <- allTurns %>% 
      bind_rows(noMatchWords %>% 
                  select(File, Text = Word, Start = WordStart, End = WordEnd) %>% 
                  mutate(Tier = "NoMatch - Word")) %>%
      ##Put word at top
      mutate(across(Tier, ~ fct_relevel(.x, "NoMatch - Word"))) %>% 
      arrange(File, Tier) %>% 
      mutate(across(Tier, as.character))
  }
  
  allTurns
}
