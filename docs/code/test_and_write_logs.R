file.remove("test_logs.txt")
testthat::set_max_fails(Inf)
testthat::test_dir("tests", stop_on_failure = FALSE)
suppressPackageStartupMessages(library(tidyverse))
test_logs <- read_lines("test_logs.txt")

str_subset(test_logs, "(everything is ok)|(0          wrong hashes)|[ːʔʢʡʕʜħɬłčβɟɡǧɥχɣλƛüıɨəʁžšşșǝʲæäğА-я]", negate = TRUE) |> 
  # str_subset("spelling of          .{2,3} ", negate = TRUE) |> 
  setdiff(read_lines("data/remove_from_logs.txt")) |> 
  write_lines("test_logs.txt", append = FALSE)

read_lines("test_logs.txt") %>% cat(sep = "\n")
system("git add -u")
