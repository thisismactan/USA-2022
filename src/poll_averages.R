library(tidyverse)

# Generic ballot
download.file("https://projects.fivethirtyeight.com/polls/data/generic_ballot_polls.csv",
              "data/generic_ballot_polls.csv")
generic_ballot_polls <- read_csv("data/generic_ballot_polls.csv", lazy = FALSE)
