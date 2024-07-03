file.remove("test_logs.txt")
testthat::test_dir("tests", stop_on_failure = FALSE)
suppressPackageStartupMessages(library(tidyverse))
test_logs <- read_lines("test_logs.txt")
read_tsv("data/tald_villages.csv") |> 
  select(dialect_toplevel, dialect_nt1, dialect_nt2, dialect_nt3, village_dialect) |> 
  pivot_longer(names_to = "col", values_to = "value", everything()) |> 
  na.omit() |> 
  distinct(value) |>
  pull(value) |> 
  str_c(collapse = ")|(") ->
  langs_and_dialects

langs_and_dialects <- str_c("(", langs_and_dialects, ")", collapse = "")

str_subset(test_logs, "(everything is ok)|(0          wrong hashes)|[ːʔʢʡʕʜħɬłčβɟɡǧɥχɣλƛüıɨəʁžšşșǝʲæäğА-я]", negate = TRUE) |> 
  str_subset("spelling of          .{2,3} ", negate = TRUE) |> 
  str_subset(str_glue("spelling of          {langs_and_dialects} "), negate = TRUE) |> 
  write_lines("test_logs.txt", append = FALSE)
read_lines("test_logs.txt") %>% cat(sep = "\n")
system("git add -u")
