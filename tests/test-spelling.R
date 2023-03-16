library(tidyverse)
library(testthat)
library(spelling)

test_that("Test spellingin chapters", {
  
  ignore_spelling <- read_lines("../data/spelling_exceptions.txt")
  files_to_check <- c(list.files("../data/orig_rmd", full.names = TRUE),
                      "../README.md", "../about.Rmd", "../team.Rmd", 
                      "../index.Rmd", "../changes.Rmd")
  
  wrong_spelling <- map_dfr(files_to_check, function(rmd){
    spelling::spell_check_files(rmd, ignore = ignore_spelling)
  })
  
  observed <- str_c("speling of ", wrong_spelling$word, " in ", wrong_spelling$found)
  expected <- character(length = length(observed))
  
  write_lines(observed, "../test_logs.txt", append = TRUE)
  expect_equal(observed, expected)
})
