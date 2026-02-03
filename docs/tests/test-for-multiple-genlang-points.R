library(tidyverse)
library(testthat)

test_that("Test for multiple genlang_point:
I expect that for each language there is only one 'yes' in the genlang_point column in `../data/orig_table`", {
  map_dfr(list.files("../data/orig_table", full.names = TRUE), function(tsv){
    read_tsv(tsv,
             progress = FALSE, 
             show_col_types = FALSE) |> 
      filter(genlang_point == "yes") |> 
      count(language) |> 
      mutate(file = tsv, 
             file = str_remove(file, "../data/orig_table/")) |> 
      filter(n > 1)
  }) ->
    wrong_langs_df

  if(nrow(wrong_langs_df) > 0){
    observed <- str_c("Multiple genlang_point for          ", 
                      wrong_langs_df$language, 
                      "          in the file          ",
                      wrong_langs_df$file)
    expected <- rep("", nrow(wrong_langs_df))
  } else {
    observed <- "everything is ok"
    expected <- "everything is ok"
  }
  write_lines(observed, "../test_logs.txt", append = TRUE)
  expect_equal(observed, expected)    
})
