library(tidyverse)
library(testthat)

test_that("Test hashes in rmarkdown text:
I expect that the h1 header `#` is absent in Rmd files.", {
  
  n_wrong_hashes <- map_dbl(list.files("../data/orig_rmd", full.names = TRUE), function(rmd){
    sum(str_detect(read_lines(rmd, progress = FALSE), "^# ")) 
  })
  
  observed <- str_c(file = list.files("../data/orig_rmd"), 
                    ":          ",
                    n_wrong_hashes = n_wrong_hashes, 
                    "          wrong hashes")
  expected <- str_c(file = list.files("../data/orig_rmd"), 
                    ": 0 wrong hashes")
  
  write_lines(observed, "../test_logs.txt", append = TRUE)
  expect_equal(observed, expected)
})
