library(tidyverse)
library(reshape2)

house_results <- read_csv("data/1976-2020-house.csv", lazy = FALSE)

recent_midterm_results <- house_results %>%
  filter(year %in% c(2014, 2018),
         grepl("REPUBLICAN|DEMOCRAT", party)) %>%
  mutate(district = as.numeric(district)) %>%
  group_by(year, state = state_po, district, totalvotes, party) %>%
  summarise(votes = sum(candidatevotes)) %>%
  ungroup() %>%
  dplyr::select(year, state, district, totalvotes, party, votes) %>%
  spread(party, votes, fill = 0) %>%
  mutate(DEM = DEMOCRAT + `DEMOCRATIC-FARMER-LABOR` + `DEMOCRATIC-NONPARTISAN LEAGUE` + `DEMOCRATIC-NPL`,
         REP = REPUBLICAN) %>%
  dplyr::select(year, state, district_number = district, totalvotes, DEM, REP) %>%
  mutate(dem_margin_2p = (DEM - REP) / (DEM + REP),
         pct_other = (totalvotes - (DEM + REP)) / totalvotes)

natl_midterm_results <- recent_midterm_results %>%
  group_by(year) %>%
  summarise(totalvotes = sum(totalvotes),
            DEM = sum(DEM),
            REP = sum(REP)) %>%
  mutate(natl_dem_margin_2p = (DEM - REP) / (DEM + REP),
         natl_pct_other = (totalvotes - (DEM + REP)) / totalvotes)

dk_pres_cd_results <- read_csv("data/daily_kos_pres_cd_results.csv", lazy = FALSE) %>%
  mutate(state = str_split(district, "-") %>% sapply(head, n = 1),
         district_number = str_split(district, "-") %>% sapply(tail, n = 1),
         district_number = ifelse(district_number == "AL", 0, district_number) %>% as.numeric(),
         `2018` = (clinton_2016 - trump_2016) / (clinton_2016 + trump_2016),
         `2014` = (obama_2012 - romney_2012) / (obama_2012 + romney_2012)) %>%
  dplyr::select(district, state, district_number, `2014`, `2018`) %>%
  melt(id.vars = c("district", "state", "district_number"), variable.name = "next_midterm", value.name = "pres_lean") %>%
  mutate(next_midterm = next_midterm %>% as.character() %>% as.numeric()) %>%
  as_tibble()

midterm_results <- recent_midterm_results %>%
  dplyr::select(-DEM, -REP) %>%
  left_join(dk_pres_cd_results, by = c("state" = "state", "district_number" = "district_number", "year" = "next_midterm")) %>%
  left_join(natl_midterm_results %>% dplyr::select(-totalvotes, -DEM, -REP), by = "year")

write_csv(midterm_results, "data/house_midterm_results.csv")
  