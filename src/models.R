library(tidyverse)

house_midterm_results <- read_csv("data/house_midterm_results.csv", lazy = FALSE) %>%
  mutate(house_lean = dem_margin_2p - natl_dem_margin_2p)

house_model <- lm(house_lean ~ pres_lean, 
                  data = house_midterm_results %>% 
                    filter(abs(dem_margin_2p) < 1, year == 2018))
summary(house_model)

