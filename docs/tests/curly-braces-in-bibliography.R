library(tidyverse)
library(testthat)
library(bib2df)

test_that("BibTeX converts all characters in the title to lowercase. 
          If you want to override this, wrap the character(s) in curly braces", {
            map_dfr(list.files("../data/orig_bib", full.names = TRUE), function(i){
              suppressWarnings({bib2df(i)}) |> 
                select(BIBTEXKEY, TITLE) |>  
                mutate(topic = str_remove(i, "../data/orig_bib/"),
                       topic = str_remove(topic, ".bib$"),
                       source_file = i)
            }) ->
              bibs
            
            map_dfr(list.files("../data/orig_bib_tsv", full.names = TRUE), function(i){
              read_tsv(i, progress = FALSE, show_col_types = FALSE) |> 
                select(BIBTEXKEY, TITLE) |>  
                mutate(topic = str_remove(i, "../data/orig_bib_tsv/"),
                       topic = str_remove(topic, "_bib.tsv$"),
                       source_file_tsv = i)
            }) ->
              tsvs
            
            bibs |> 
              full_join(tsvs) |> 
              mutate(source_file = ifelse(is.na(source_file_tsv), source_file, source_file_tsv),
                     TITLE = str_remove_all(TITLE, "\\{.*?\\}")) |> 
              filter(str_remove(TITLE, "^.") != tolower(str_remove(TITLE, "^."))) ->
              df_missed_curly_braces          
            
            
            if(nrow(df_missed_curly_braces) > 0){
              observed <- str_c("Missing curly braces in the TITLE field of          ", 
                                df_missed_curly_braces$BIBTEXKEY,
                                "          entry in the          ",
                                str_remove(df_missed_curly_braces$source_file, "../data/orig_bib(_tsv)?/"))
              expected <- rep("", nrow(df_missed_curly_braces))
            } else {
              observed <- "everything is ok"
              expected <- "everything is ok"
            }
            write_lines(observed, "../test_logs.txt", append = TRUE)
            expect_equal(observed, expected)    
          })
