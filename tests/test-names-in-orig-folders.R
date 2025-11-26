library(tidyverse)
library(testthat)

test_that("Test number and file names in orig folders:
I expect all files in orig_table, orig_rmd and orig_bib have the same 
names as in the `../data/contributers.xlsx`", {
  readxl::read_xlsx("../data/contributors.xlsx") |>
    filter(render == 1,
           is.na(major_topic_text)) |>
    pull(filename) |>
    sort() ->
    expected_texts
  
  readxl::read_xlsx("../data/contributors.xlsx") |>
    filter(render == 1,
           !is.na(major_topic_text)) |>
    pull(filename) |>
    sort() ->
    expected_mt
  
  tables <- str_c(expected_texts, ".tsv")[which(!(str_c(expected_texts, ".tsv") %in% list.files("../data/orig_table")))]
  tables <- ifelse(length(tables) == 0, "", tables)
  expect_equal(str_c("There is no file", tables, " in the ../data/orig_table", sep = " ", collapse = "; "),
               "There is no file   in the ../data/orig_table")
  
  bibs <- str_c(c(expected_texts, expected_mt), ".bib")[which(!(str_c(c(expected_texts, expected_mt), ".bib") %in% list.files("../data/orig_bib")))]
  
  
  if(length(bibs) > 0){
    observed <- str_c("There is no file           ", bibs, 
                      "          in the ../data/orig_table", sep = " ", collapse = "; ")
    expected <- rep("", length(bibs))
  } else {
    observed <- "everything is ok"
    expected <- "everything is ok"
  }
  
  write_lines(observed, "../test_logs.txt", append = TRUE)
  expect_equal(observed, expected)
  expect_equal(sort(setdiff(list.files("../data/orig_rmd"),
                            list.files("../data/orig_rmd", pattern = "_map.Rmd"))), 
               sort(str_c(c(expected_texts, expected_mt), ".Rmd")))
})
