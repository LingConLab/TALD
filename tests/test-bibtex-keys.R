library(tidyverse)
library(testthat)

test_that("Test bibtexkeys in the source field.", {
  
  bib <- RefManageR::ReadBib(file = '../data/bibliography.bib')
  
  map(list.files("../data/orig_table", full.names = TRUE), function(tsv){
    read_tsv(tsv, progress = FALSE, show_col_types = FALSE) |> 
      select(source, page) |> 
      mutate(page = str_replace_all(page, '--', '–'),
             source = str_split(source, '; '),
             page = str_split(page, '; ')) |> 
      unnest_longer(col = c(source, page)) |> 
      mutate(source = if_else(is.na(source), "", source)) |> 
      rowwise() |> 
      mutate(page = ifelse(page == 'NA', NA_character_, page),
             source2 = case_when(str_detect(source, '[Ff]ield [Dd]ata') ~ source,
                                 str_detect(source, 'p.\\s?c.$') ~ source,
                                 TRUE ~ RefManageR::Cite(bib, source, 
                                                         after = ifelse(!is.na(page),  
                                                                        str_c(': ', page),
                                                                        '')))) |> 
      filter(source2 == "") |> 
      select(source) |> 
      mutate(source = if_else(source == "", "<empty>", source),
             file = tsv, 
             file = str_remove(file, "../data/orig_table/"))
  }) |> 
    list_rbind() ->
    wrong_bibtexkey_df
  
  if(nrow(wrong_bibtexkey_df) > 0){
    observed <- str_c("The wrong bibtexkey          ", 
                      wrong_bibtexkey_df$source, 
                      "          in the file          ",
                      wrong_bibtexkey_df$file)
    expected <- rep("", nrow(wrong_bibtexkey_df))
  } else {
    observed <- "everything is ok"
    expected <- "everything is ok"
  }
  write_lines(observed, "../test_logs.txt", append = TRUE)
  expect_equal(observed, expected)
})
