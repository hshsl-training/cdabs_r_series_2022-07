library(tidyverse)
library(lubridate)

#read in dataset
measles_us <- read_csv("data/measles_us.csv",
col_types = cols(PeriodStartDate = col_date(format = "%m/%d/%Y"),
PeriodEndDate = col_date(format = "%m/%d/%Y")))


yearly_count_state <-
  measles_us %>%
  select(
    state = Admin1Name,
    PeriodStartDate,
    PeriodEndDate,
    PartOfCumulativeCountSeries,
    CountValue
  ) %>% 
  filter(state %in% toupper(state.name)) %>% 
  filter(PartOfCumulativeCountSeries==0) %>% 
  mutate(Year = year(PeriodStartDate)) %>% 
  group_by(Year, state) %>%
  summarise(TotalCount = sum(CountValue))

#load csv of populations historical state populations
hist_pop_by_state <-
  read_csv(
    "https://osf.io/download/62cdb7d8779f1710fb070f06/")

#convert to long
hist_pop_long <- hist_pop_by_state %>%
  pivot_longer(ALASKA:WYOMING,
               names_to = "state",
               values_to = "pop1000")

#join with measles data and population data
joined_df <-
  full_join(yearly_count_state, hist_pop_long, by=c("state" = "state", "Year" = "DATE" )) %>% 
  replace_na(list(TotalCount=0, rate=0))

#calculate rates per 1000 persons
measles_year_rates <- joined_df %>% 
  mutate(rate = TotalCount/ pop1000)

#prepare to add regions and divisions with built in R state data
state_data <- data.frame(toupper(state.name), state.division, state.region)

state_data <- state_data %>% rename(state = toupper.state.name.)


#join region and division data to measles and pop data
yearly_rates_ext <- left_join(measles_year_rates, state_data, by="state")



