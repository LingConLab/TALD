library(tidyverse)
library(testthat)

test_that("Test duplicated column names in data:
I expect there won't be any duplicated names in .tsv files.", {
  
  list.files("../data/orig_table", full.names = TRUE) |> 
    map(function(tsv){
      read_lines(tsv, n_max = 1) |> 
        str_split("\t") |> 
        unlist() ->
        columns
      tibble(column_name = columns[duplicated(columns)],
             filename = tsv)
    }) |> 
    list_rbind() ->
    wrong_column_names
  
  if(nrow(wrong_column_names) > 0){
    observed <- str_c("Duplicated column name      '",
                      wrong_column_names$column_name,
                      "'      in the      ",
                      str_remove(wrong_column_names$filename, "../data/orig_table/"))
    expected <- rep("", nrow(wrong_column_names))
  } else {
    observed <- "everything is ok"
    expected <- "everything is ok"
  }
  
  write_lines(observed, "../test_logs.txt", append = TRUE)
  expect_equal(observed, expected)
})
