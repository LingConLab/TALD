library(tidyverse)
library(testthat)

test_that("Test column names and separator in data:
I expect that each file in `../data/orig_table` is in `.tsv` format and have 
columns listed in `expected_columns`", {
  expected_columns <- c("id", 
                        "lang", 
                        "idiom", 
                        "type", 
                        "genlang_point", 
                        "map", 
                        "feature", 
                        "source", 
                        "page", 
                        "value1",
                        "value1_name",
                        "contributor", 
                        "date")
  
  check_col_names <- expand.grid(expected_columns, 
                                 list.files("../data/orig_table", full.names = TRUE),
                                 stringsAsFactors = FALSE) 
  
  check_col_names$observed <- map2_lgl(check_col_names$Var1, check_col_names$Var2, .f = function(var, table){
    var %in% colnames(read_tsv(table, 
                               n_max = 1, 
                               progress = FALSE, 
                               show_col_types = FALSE))
  }) 
  check_col_names %>% 
    mutate(Var2 = str_remove(Var2, "../data/orig_table/")) %>% 
    filter(!observed) ->
    absent_columns
  
  if(nrow(absent_columns) > 0){
    observed <- str_c("The obligatory column ", 
                      absent_columns$Var1, 
                      " is absent in the file ",
                      absent_columns$Var2)
    expected <- rep("", nrow(absent_columns))
  } else {
    observed <- "everything is ok"
    expected <- "everything is ok"
  }
  write_lines(observed, "../test_logs.txt", append = TRUE)
  expect_equal(observed, expected)    
})
