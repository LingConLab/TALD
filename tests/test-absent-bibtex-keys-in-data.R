library(tidyverse)
library(testthat)

test_that("Test bibkeys in data:", {
  biblib <- readxl::read_xlsx("../data/biblib.xlsx", 
                              range = readxl::cell_cols("B"))$BIBTEXKEY
  
  map(list.files("../data/orig_table", full.names = TRUE), function(tbl){
    read_tsv(tbl, show_col_types = FALSE, col_select = "source") %>% 
      tibble(refs = .,
             files = str_remove(tbl, "../data/orig_table/"))}) %>% 
    list_rbind() %>% 
    mutate(refs = str_split(refs, "; "))  %>% 
    unnest_longer(refs) %>% 
    filter(str_detect(refs, "\\w*?\\d{4}([a-z])?")) %>% 
    na.omit() %>% 
    distinct() %>% 
    mutate(detected = refs %in% biblib) %>% 
    filter(!detected) ->
    refs_absent_in_biblib
  
  if(nrow(refs_absent_in_biblib) > 0){
    observed <- str_c("The BibTeX entry          ", 
                      refs_absent_in_biblib$refs, 
                      "          present in the          ",
                      refs_absent_in_biblib$files,
                      ", but absent in the biblib.xlsx")
    expected <- rep("", nrow(refs_absent_in_biblib))
  } else {
    observed <- "everything is ok"
    expected <- "everything is ok"
  }
  write_lines(observed, "../test_logs.txt", append = TRUE)
  expect_equal(observed, expected)    
})
