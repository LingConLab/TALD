library(tidyverse)
library(testthat)

test_that("Test {-} in the List of glosses header:
I expect that the List of glosses header will end with {-}.", {
  
  map(list.files("../data/orig_rmd", full.names = TRUE), function(rmd){
    read_lines(rmd, progress = FALSE) |> 
      str_subset("^## List of glosses") |> 
      str_detect("\\{-\\}") ->
      result
    
    tibble(filename = rmd,
           wrong = ifelse(isFALSE(result), 
                     "no {-} in the List of glosses section", 
                     "everything is fine"))
  }) |> 
    list_rbind() |> 
    filter(wrong != "everything is fine") ->
    wrong_numbering
  
  if(nrow(wrong_numbering) > 0){
    observed <- str_c("No {-} in the List of glosses section in the file     ",
                      str_remove(wrong_numbering$filename, ".*/"))
    expected <- rep("", nrow(wrong_numbering))
  } else {
    observed <- "everything is ok"
    expected <- "everything is ok"
  }
  
  write_lines(observed, "../test_logs.txt", append = TRUE)
  expect_equal(observed, expected)
})

test_that("Test {-} in the References header:
I expect that the References header will end with {-}.", {
  
  map(list.files("../data/orig_rmd", full.names = TRUE), function(rmd){
    read_lines(rmd, progress = FALSE) |> 
      str_subset("^## Reference") |> 
      str_detect("\\{-\\}") ->
      result
    
    tibble(filename = rmd,
           wrong = ifelse(isFALSE(result), 
                          "no {-} in the List of glosses section", 
                          "everything is fine"))
  }) |> 
    list_rbind() |> 
    filter(wrong != "everything is fine") ->
    wrong_numbering
  
  if(nrow(wrong_numbering) > 0){
    observed <- str_c("No {-} in the Reference section in the file       ",
                      str_remove(wrong_numbering$filename, ".*/"))
    expected <- rep("", nrow(wrong_numbering))
  } else {
    observed <- "everything is ok"
    expected <- "everything is ok"
  }
  
  write_lines(observed, "../test_logs.txt", append = TRUE)
  expect_equal(observed, expected)
})

test_that("Test {-} in the References header:
I expect that the Table header will end with {-}.", {
  
  map(list.files("../data/orig_rmd", full.names = TRUE), function(rmd){
    read_lines(rmd, progress = FALSE) |> 
      str_subset("^#### .*Table") |> 
      str_detect("\\{.*?-\\}") ->
      result
    
    tibble(filename = rmd,
           wrong = ifelse(isFALSE(result), 
                          "no {-} in the List of glosses section", 
                          "everything is fine"))
  }) |> 
    list_rbind() |> 
    filter(wrong != "everything is fine") ->
    wrong_numbering
  
  if(nrow(wrong_numbering) > 0){
    observed <- str_c("No {-} in the Table header in the file       ",
                      str_remove(wrong_numbering$filename, ".*/"))
    expected <- rep("", nrow(wrong_numbering))
  } else {
    observed <- "everything is ok"
    expected <- "everything is ok"
  }
  
  write_lines(observed, "../test_logs.txt", append = TRUE)
  expect_equal(observed, expected)
})
