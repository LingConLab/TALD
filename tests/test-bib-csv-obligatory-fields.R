library(tidyverse)
library(testthat)

test_that("Test column names in the fields in bibtex tsvs", {
  map_dfr(list.files("../data/orig_bib_tsv", full.names = TRUE), function(bib_tsv){
    df <- read_tsv(bib_tsv, n_max = 1, 
                   progress = FALSE, 
                   show_col_types = FALSE) |> 
      mutate(YEAR = as.character(YEAR),
             NUMBER = as.character(NUMBER))
    all_fields <-  c('CATEGORY', 
                     'BIBTEXKEY', 
                     'AUTHOR', 
                     'TITLE', 
                     'YEAR', 
                     'TITLE_TRANSLATION', 
                     'PUBLISHER', 
                     'ADDRESS', 
                     'PAGES', 
                     'BOOKTITLE', 
                     'BOOKTITLE_TRANSLATION', 
                     'EDITOR', 
                     'JOURNAL', 
                     'NOTE', 
                     'NUMBER', 
                     'SCHOOL', 
                     'URL', 
                     'VOLUME')
    missed_fields <- all_fields[which(!(all_fields %in% colnames(df)))]
    tibble(missed_fields = missed_fields, 
           file = bib_tsv)
  }) ->
    df_missed_fields
  
    if(nrow(df_missed_fields) > 0){
      observed <- str_c("The obligatory field          ", 
                        df_missed_fields$missed_fields,
                        "          is missing in          ",
                        str_remove(df_missed_fields$file, "../data/orig_bib_tsv/"))
      expected <- rep("", nrow(df_missed_fields))
    } else {
      observed <- "everything is ok"
      expected <- "everything is ok"
    }
    write_lines(observed, "../test_logs.txt", append = TRUE)
    expect_equal(observed, expected)
})

test_that("Test CATEGORY field in bibtex tsvs:
see https://en.wikipedia.org/wiki/BibTeX for the details.", {
  df <- map_dfr(list.files("../data/orig_bib_tsv/", full.names = TRUE), function(i){
    read_tsv(i, show_col_types = FALSE, progress = FALSE) %>% 
      mutate(filename = i,
             CATEGORY = tolower(CATEGORY),
             VOLUME = as.character(VOLUME),
             AUTHOR = as.character(AUTHOR),
             ISSUE = ifelse("ISSUE" %in% colnames(.), as.character(ISSUE), NA),
             YEAR = as.character(YEAR),
             NOTE = as.character(NOTE),
             NUMBER = as.character(NUMBER))})
  bad_category <- df[which(!(df$CATEGORY %in% c("article", "book", "booklet", 
                                                "conference", "inbook", 
                                                "incollection", "inproceedings", 
                                                "manual", "mastersthesis", "misc", 
                                                "phdthesis", "proceedings", 
                                                "techreport", "unpublished"))),]
  bad_category |> 
    mutate(CATEGORY = ifelse(is.na(CATEGORY), "", CATEGORY)) ->
    bad_category
  
  if(nrow(bad_category) > 0){
    observed <- str_c("The BibTeX entry ", 
                      bad_category$BIBTEXKEY, 
                      " from ",
                      str_remove(bad_category$filename, ".*/"), 
                      " has the wrong CATEGORY value: ",
                      bad_category$CATEGORY)
    expected <- rep("", nrow(bad_category))
  } else {
    observed <- "everything is ok"
    expected <- "everything is ok"
  }
  write_lines(observed, "../test_logs.txt", append = TRUE)
  expect_equal(observed, expected)  
})

test_that("Test obligatory fields in bibtex tsvs:
see https://en.wikipedia.org/wiki/BibTeX for the details.", {
  df <- map_dfr(list.files("../data/orig_bib_tsv", full.names = TRUE), function(i){
    read_tsv(i, show_col_types = FALSE, progress = FALSE) |> 
      mutate_all(function(x) as.character(x)) |> 
      mutate(filename = i)})
  
  df |> 
    filter(CATEGORY == "article") |> 
    select(BIBTEXKEY, AUTHOR, TITLE, JOURNAL, YEAR, VOLUME, filename) |> 
    filter(if_any(everything(), is.na)) |> 
    mutate_all(as.character) |> 
    pivot_longer(names_to = "field", values_to = "value", -c(BIBTEXKEY, filename)) |> 
    filter(is.na(value)) ->
    article_fields
  
  df |> 
    filter(CATEGORY == "book") |> 
    select(BIBTEXKEY, TITLE, PUBLISHER, YEAR, filename) |> 
    filter(if_any(everything(), is.na)) |> 
    mutate_all(as.character) |> 
    pivot_longer(names_to = "field", values_to = "value", -c(BIBTEXKEY, filename)) |> 
    filter(is.na(value)) ->
    book_fields
  
  df |> 
    filter(CATEGORY == "booklet") |> 
    select(BIBTEXKEY, TITLE, filename) |> 
    filter(if_any(everything(), is.na)) |> 
    mutate_all(as.character) |> 
    pivot_longer(names_to = "field", values_to = "value", -c(BIBTEXKEY, filename)) |> 
    filter(is.na(value)) ->
    booklet_fields
  
  df |> 
    filter(CATEGORY == "conference") |> 
    select(BIBTEXKEY, AUTHOR, TITLE, BOOKTITLE, YEAR, filename) |> 
    filter(if_any(everything(), is.na)) |> 
    mutate_all(as.character) |> 
    pivot_longer(names_to = "field", values_to = "value", -c(BIBTEXKEY, filename)) |> 
    filter(is.na(value)) ->
    conference_fields
  
  df |> 
    filter(CATEGORY == "inbook") |> 
    select(BIBTEXKEY, TITLE, PUBLISHER, YEAR, filename) |> 
    filter(if_any(everything(), is.na)) |> 
    mutate_all(as.character) |> 
    pivot_longer(names_to = "field", values_to = "value", -c(BIBTEXKEY, filename)) |> 
    filter(is.na(value)) ->
    inbook_fields
  
  df |> 
    filter(CATEGORY == "incollection") |> 
    select(BIBTEXKEY, AUTHOR, TITLE, BOOKTITLE, PUBLISHER, YEAR, filename) |> 
    filter(if_any(everything(), is.na)) |> 
    mutate_all(as.character) |> 
    pivot_longer(names_to = "field", values_to = "value", -c(BIBTEXKEY, filename)) |> 
    filter(is.na(value)) ->
    incollection_fields
  
  df |> 
    filter(CATEGORY == "inproceedings") |> 
    select(BIBTEXKEY, AUTHOR, TITLE, BOOKTITLE, YEAR, filename) |> 
    filter(if_any(everything(), is.na)) |> 
    mutate_all(as.character) |> 
    pivot_longer(names_to = "field", values_to = "value", -c(BIBTEXKEY, filename)) |> 
    filter(is.na(value)) ->
    inproceedings_fields
  
  df |> 
    filter(CATEGORY == "manual") |> 
    select(BIBTEXKEY, TITLE, filename) |> 
    filter(if_any(everything(), is.na)) |> 
    mutate_all(as.character) |> 
    pivot_longer(names_to = "field", values_to = "value", -c(BIBTEXKEY, filename)) |> 
    filter(is.na(value)) ->
    manual_fields
  
  df |> 
    filter(CATEGORY == "mastersthesis") |> 
    select(BIBTEXKEY, AUTHOR, TITLE, SCHOOL, YEAR, filename) |> 
    filter(if_any(everything(), is.na)) |> 
    mutate_all(as.character) |> 
    pivot_longer(names_to = "field", values_to = "value", -c(BIBTEXKEY, filename)) |> 
    filter(is.na(value)) ->
    mastersthesis_fields

  df |> 
    filter(CATEGORY == "phdthesis") |> 
    select(BIBTEXKEY, AUTHOR, TITLE, SCHOOL, YEAR, filename) |> 
    filter(if_any(everything(), is.na)) |> 
    mutate_all(as.character) |> 
    pivot_longer(names_to = "field", values_to = "value", -c(BIBTEXKEY, filename)) |> 
    filter(is.na(value)) ->
    phdthesis_fields

  df |> 
    filter(CATEGORY == "proceedings") |> 
    select(BIBTEXKEY, TITLE, YEAR, filename) |> 
    filter(if_any(everything(), is.na)) |> 
    mutate_all(as.character) |> 
    pivot_longer(names_to = "field", values_to = "value", -c(BIBTEXKEY, filename)) |> 
    filter(is.na(value)) ->
    proceedings_fields
  
  # # for some reason we don't have INSTITUTION in the template
  # 
  # df |>
  #   filter(CATEGORY == "techreport") |>
  #   select(BIBTEXKEY, AUTHOR, TITLE, INSTITUTION, YEAR) |>
  #   filter(if_any(everything(), is.na)) |>
  #   mutate_all(as.character) |> 
  #   pivot_longer(names_to = "field", values_to = "value", -c(BIBTEXKEY, filename)) |> 
  #   filter(is.na(value)) ->
  #   techreport_fields
  
  df |> 
    filter(CATEGORY == "unpublished") |> 
    select(BIBTEXKEY, AUTHOR, TITLE, filename) |> 
    filter(if_any(everything(), is.na)) |> 
    mutate_all(as.character) |> 
    pivot_longer(names_to = "field", values_to = "value", -c(BIBTEXKEY, filename)) |> 
    filter(is.na(value)) ->
    unpublished_fields
  
  bind_rows(article_fields, 
            book_fields, 
            booklet_fields, 
            conference_fields, 
            inbook_fields, 
            incollection_fields, 
            inproceedings_fields, 
            manual_fields, 
            mastersthesis_fields, 
            phdthesis_fields, 
            proceedings_fields, 
            unpublished_fields) ->
    wrong_fields
  
  if(nrow(wrong_fields) > 0){
    observed <- str_c("The BibTeX entry ", 
                      wrong_fields$BIBTEXKEY, 
                      " from the file ",
                      str_remove(wrong_fields$filename, ".*/"), 
                      " has an empty value in the obligatory field ",
                      wrong_fields$field)
    expected <- rep("", nrow(wrong_fields))
  } else {
    observed <- "everything is ok"
    expected <- "everything is ok"
  }
  write_lines(observed, "../test_logs.txt", append = TRUE)
  expect_equal(observed, expected)
})


# test_that("We expect BIBTEXKEYs to be of a structure lastnameyear, e. g. jakovlev1940", {
#   list.files("../data/orig_bib_tsv", full.names = TRUE) |> 
#     map(function(i){
#     read_tsv(i, show_col_types = FALSE, progress = FALSE) %>%
#       mutate(filename = i,
#              AUTHOR = as.character(AUTHOR),
#              ISSUE = ifelse("ISSUE" %in% colnames(.), as.character(ISSUE), NA),
#              VOLUME = as.character(VOLUME),
#              YEAR = as.character(YEAR),
#              NOTE = as.character(NOTE),
#              NUMBER = as.character(NUMBER))}) |> 
#     list_rbind() ->
#     df
#   
#   df  |>  
#     filter(str_detect(BIBTEXKEY, "[^a-z0-9-]")) ->
#     wrong_bibtexkeys
#   
#   if(nrow(wrong_bibtexkeys) > 0){
#     observed <- str_c("Revise the BibTeX entry ", 
#                       wrong_bibtexkeys$BIBTEXKEY, 
#                       " from the file ",
#                       str_remove(wrong_bibtexkeys$filename, ".*/"), 
#                       ".")
#     expected <- rep("", nrow(wrong_bibtexkeys))
#   } else {
#     observed <- "everything is ok"
#     expected <- "everything is ok"
#   }
#   write_lines(observed, "../test_logs.txt", append = TRUE)
#   expect_equal(observed, expected)
# })

