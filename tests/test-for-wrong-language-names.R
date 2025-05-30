library(tidyverse)
library(testthat)

test_that("Test for wrong languages:
I expect that all languages in the `language` field of tables in `../data/orig_table`
match with `../data/tald_villages.csv`", {
  read_tsv("../data/tald_villages.csv",
           progress = FALSE, 
           show_col_types = FALSE) |> 
    pull(language) |> 
    unique() |> 
    sort() ->
    expected_langs
  
  map_dfr(list.files("../data/orig_table", full.names = TRUE), function(tsv){
    read_tsv(tsv,
             progress = FALSE, 
             show_col_types = FALSE) |> 
      distinct(language) |> 
      mutate(file = tsv, 
             file = str_remove(file, "../data/orig_table/")) |> 
      filter(!(language %in% expected_langs))
  }) ->
    wrong_langs_df

  if(nrow(wrong_langs_df) > 0){
    observed <- str_c("The wrong language name          ", 
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


test_that("Test for the village dialect names:
I expect that all village dialect names in the `idiom` field of tables in 
`../data/orig_table` match with `../data/tald_villages.csv`", {
  read_tsv("../data/tald_villages.csv",
           progress = FALSE, 
           show_col_types = FALSE) |> 
    pull(village_dialect) |> 
    unique() |> 
    sort() ->
    expected_villages
  
  map_dfr(list.files("../data/orig_table", full.names = TRUE), function(tsv){
    read_tsv(tsv,
             progress = FALSE, 
             show_col_types = FALSE) |> 
      filter(type == "village") |> 
      distinct(idiom) |> 
      mutate(file = tsv, 
             file = str_remove(file, "../data/orig_table/")) |> 
      filter(!(idiom %in% expected_villages))
  }) ->
    wrong_langs_df
  
  if(nrow(wrong_langs_df) > 0){
    observed <- str_c("The wrong village dialect name ", 
                      wrong_langs_df$idiom, 
                      " in the file ",
                      wrong_langs_df$file)
    expected <- rep("", nrow(wrong_langs_df))
  } else {
    observed <- "everything is ok"
    expected <- "everything is ok"
  }
  write_lines(observed, "../test_logs.txt", append = TRUE)
  expect_equal(observed, expected)    
})

test_that("Test for the non-toplevel 3 dialect names:
I expect that all village non-toplevel 3 dialect names in the `idiom` field of tables in 
`../data/orig_table` match with `../data/tald_villages.csv`", {
  read_tsv("../data/tald_villages.csv",
           progress = FALSE, 
           show_col_types = FALSE) |> 
    pull(dialect_nt3) |> 
    unique() |> 
    sort() ->
    expected_villages
  
  map_dfr(list.files("../data/orig_table", full.names = TRUE), function(tsv){
    read_tsv(tsv,
             progress = FALSE, 
             show_col_types = FALSE) |> 
      filter(type == "dialect_nt3") |> 
      distinct(idiom) |> 
      mutate(file = tsv, 
             file = str_remove(file, "../data/orig_table/")) |> 
      filter(!(idiom %in% expected_villages))
  }) ->
    wrong_langs_df
  
  if(nrow(wrong_langs_df) > 0){
    observed <- str_c("The wrong non-toplevel 3 dialect name ", 
                      wrong_langs_df$idiom, 
                      " in the file ",
                      wrong_langs_df$file)
    expected <- rep("", nrow(wrong_langs_df))
  } else {
    observed <- "everything is ok"
    expected <- "everything is ok"
  }
  write_lines(observed, "../test_logs.txt", append = TRUE)
  expect_equal(observed, expected)    
})

test_that("Test for the non-toplevel 2 dialect names:
I expect that all village non-toplevel 2 dialect names in the `idiom` field of tables in 
`../data/orig_table` match with `../data/tald_villages.csv`", {
  read_tsv("../data/tald_villages.csv",
           progress = FALSE, 
           show_col_types = FALSE) |> 
    pull(dialect_nt2) |> 
    unique() |> 
    sort() ->
    expected_villages
  
  map_dfr(list.files("../data/orig_table", full.names = TRUE), function(tsv){
    read_tsv(tsv,
             progress = FALSE, 
             show_col_types = FALSE) |> 
      filter(type == "dialect_nt2") |> 
      distinct(idiom) |> 
      mutate(file = tsv, 
             file = str_remove(file, "../data/orig_table/")) |> 
      filter(!(idiom %in% expected_villages))
  }) ->
    wrong_langs_df
  
  if(nrow(wrong_langs_df) > 0){
    observed <- str_c("The wrong non-toplevel 2 dialect name ", 
                      wrong_langs_df$idiom, 
                      " in the file ",
                      wrong_langs_df$file)
    expected <- rep("", nrow(wrong_langs_df))
  } else {
    observed <- "everything is ok"
    expected <- "everything is ok"
  }
  write_lines(observed, "../test_logs.txt", append = TRUE)
  expect_equal(observed, expected)    
})

test_that("Test for the non-toplevel 1 dialect names:
I expect that all village non-toplevel 1 dialect names in the `idiom` field of tables in 
`../data/orig_table` match with `../data/tald_villages.csv`", {
  read_tsv("../data/tald_villages.csv",
           progress = FALSE, 
           show_col_types = FALSE) |> 
    pull(dialect_nt1) |> 
    unique() |> 
    sort() ->
    expected_villages
  
  map_dfr(list.files("../data/orig_table", full.names = TRUE), function(tsv){
    read_tsv(tsv,
             progress = FALSE, 
             show_col_types = FALSE) |> 
      filter(type == "dialect_nt1") |> 
      distinct(idiom) |> 
      mutate(file = tsv, 
             file = str_remove(file, "../data/orig_table/")) |> 
      filter(!(idiom %in% expected_villages))
  }) ->
    wrong_langs_df
  
  if(nrow(wrong_langs_df) > 0){
    observed <- str_c("The wrong non-toplevel 1 dialect name ", 
                      wrong_langs_df$idiom, 
                      " in the file ",
                      wrong_langs_df$file)
    expected <- rep("", nrow(wrong_langs_df))
  } else {
    observed <- "everything is ok"
    expected <- "everything is ok"
  }
  write_lines(observed, "../test_logs.txt", append = TRUE)
  expect_equal(observed, expected)    
})

test_that("Test for the toplevel dialect names:
I expect that all toplevel dialect names in the `idiom` field of tables in 
`../data/orig_table` match with `../data/tald_villages.csv`", {
  read_tsv("../data/tald_villages.csv",
           progress = FALSE, 
           show_col_types = FALSE) |> 
    pull(dialect_toplevel) |> 
    unique() |> 
    sort() ->
    expected_villages
  
  map_dfr(list.files("../data/orig_table", full.names = TRUE), function(tsv){
    read_tsv(tsv,
             progress = FALSE, 
             show_col_types = FALSE) |> 
      filter(type == "dialect_toplevel") |> 
      distinct(idiom) |> 
      mutate(file = tsv, 
             file = str_remove(file, "../data/orig_table/")) |> 
      filter(!(idiom %in% expected_villages))
  }) ->
    wrong_langs_df
  
  if(nrow(wrong_langs_df) > 0){
    observed <- str_c("The wrong toplevel dialect name ", 
                      wrong_langs_df$idiom, 
                      " in the file ",
                      wrong_langs_df$file)
    expected <- rep("", nrow(wrong_langs_df))
  } else {
    observed <- "everything is ok"
    expected <- "everything is ok"
  }
  write_lines(observed, "../test_logs.txt", append = TRUE)
  expect_equal(observed, expected)    
})

test_that("Test for the standard language names:
I expect that all standard language names in the `idiom` field of tables in 
`../data/orig_table` match with `../data/tald_villages.csv`", {
  read_tsv("../data/tald_villages.csv",
           progress = FALSE, 
           show_col_types = FALSE) |> 
    pull(standard) |> 
    unique() |> 
    sort() ->
    expected_villages
  
  map_dfr(list.files("../data/orig_table", full.names = TRUE), function(tsv){
    read_tsv(tsv,
             progress = FALSE, 
             show_col_types = FALSE) |> 
      filter(type == "standard") |> 
      distinct(idiom) |> 
      mutate(file = tsv, 
             file = str_remove(file, "../data/orig_table/")) |> 
      filter(!(idiom %in% expected_villages))
  }) ->
    wrong_langs_df
  
  if(nrow(wrong_langs_df) > 0){
    observed <- str_c("The wrong standard language name ", 
                      wrong_langs_df$idiom, 
                      " in the file ",
                      wrong_langs_df$file)
    expected <- rep("", nrow(wrong_langs_df))
  } else {
    observed <- "everything is ok"
    expected <- "everything is ok"
  }
  write_lines(observed, "../test_logs.txt", append = TRUE)
  expect_equal(observed, expected)    
})

