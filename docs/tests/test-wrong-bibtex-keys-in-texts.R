library(tidyverse)
library(testthat)

test_that("Test bibkeys in articles:", {
  map_dfr(list.files("../data/orig_rmd", full.names = TRUE), function(rmd){
    tibble(refs = str_extract_all(read_lines(rmd, progress = FALSE), "@\\w*?\\d{4}([a-z])?"),
           files = str_remove(rmd, "../data/orig_rmd/"))}) %>% 
      unnest_longer(refs) %>% 
      mutate(refs = str_remove_all(refs, "\\@"),
             files = str_remove(files, ".Rmd$")) %>% 
      na.omit() %>% 
      distinct() ->
    refs_in_rmd
  
  map_dbl(seq_along(refs_in_rmd$files), function(i){
    sum(str_detect(read_lines(str_c("../data/orig_bib/", refs_in_rmd$files[i], ".bib"),
                              progress = FALSE),
                   refs_in_rmd$refs[i]))
  }) %>% 
    as.logical() ->
    refs_in_rmd$detected
  
  refs_in_rmd %>% 
    filter(!detected) ->
    refs_absent_in_bib
  
  if(nrow(refs_absent_in_bib) > 0){
    observed <- str_c("The BibTeX entry ", 
                      refs_absent_in_bib$refs, 
                      " present in the .Rmd, but absent in the ",
                      refs_absent_in_bib$files,
                      ".bib")
    expected <- rep("", nrow(refs_absent_in_bib))
  } else {
    observed <- "everything is ok"
    expected <- "everything is ok"
  }
  write_lines(observed, "../test_logs.txt", append = TRUE)
  expect_equal(observed, expected)    
})
