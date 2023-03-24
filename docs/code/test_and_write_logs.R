file.remove("test_logs.txt")
testthat::test_dir("tests", stop_on_failure = FALSE)
suppressPackageStartupMessages(library(tidyverse))
test_logs <- read_lines("test_logs.txt")
test_logs[str_detect(test_logs, "(everything is ok)|(0 wrong hashes)", negate = TRUE)] %>% 
write_lines("test_logs.txt", append = FALSE)
read_lines("test_logs.txt") %>% cat(sep = "\n")
system("git add -u")
