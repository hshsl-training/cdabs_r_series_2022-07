library(ggplot2)

# annotated line graph
yearly_count %>% 
  ggplot(aes(x=Year, y=TotalCount)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(from = 1900, to=2000, by=10)) +
   geom_vline(xintercept = 1963, color="red", linetype="dashed") +
  annotate(geom = "label", x=1963, y = 800000, label="1963: vaccine introduced") +
  labs(title = "Measles Cases Decrease After Vaccine Introduced", x="Year", y="Total Measles Case Count")

#some additional data transformations and trying things out
avg_yearly_counts <- measles_yearly_rates %>% group_by(Year) %>% summarize(mean_count=mean(TotalCount))

rates_by_region <- yearly_rates_ext %>% 
  group_by(state.region, Year) %>% 
  summarize(rates=mean(rate))

# attempt at grouped line graph
rates_by_region %>% 
  ggplot(aes(x=Year, y=rates, group=state.region)) + 
  geom_line(aes(color=state.region), size=0.75) + 
  scale_color_viridis(discrete=TRUE) +
  scale_x_continuous(breaks = seq(from = 1900, to=2000, by=10)) +
  theme_minimal() +
  facet_wrap(~state.region, nrow=2)

yearly_counts_means <- joined_df %>% 
  group_by(Year) %>% 
  summarize(YearCounts=sum(TotalCount), AvgCount=mean(TotalCount))

weekly_counts <- measles_non_cumulative %>% group_by(PeriodStartDate) %>% summarize(TotalCount=sum(CountValue))

weekly_counts %>% ggplot(aes(x=PeriodStartDate, y=TotalCount)) + geom_line()

measles_yearly_rates %>% 
  filter(State=="MARYLAND") %>% 
  ggplot(aes(x=Year, y=TotalCount)) +
  geom_line() + 
  geom_line(data = yearly_counts_means)


# simple bar plot
measles_yearly_rates %>%  
  filter(Year==1963) %>% 
  ggplot(aes(x=State, y=rate)) +
  geom_bar(stat = "identity")

# fixing the order and fixing the axis text
yearly_rates_ext %>%  
  filter(Year==1963) %>% 
  ggplot(aes(x=reorder(State, -rate), y=rate)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle=90))

#Using coord_flip
yearly_rates_ext %>%  
  filter(Year==1963) %>% 
  ggplot(aes(x=reorder(State, rate), y=rate)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_classic(base_size = 8)

#adding a reference line
measles_yearly_rates %>%  
  filter(Year==1963) %>% 
  ggplot(aes(x=reorder(State, rate), y=rate)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = mean(measles_yearly_rates$rate, na.rm = TRUE), color="red") +
  coord_flip() +
  theme_minimal(base_size = 8)


measles_pre_vacc <- measles_yearly_rates %>% 
  filter(between(Year, 1958, 1963))

measles_pre_vacc_rate <- measles_pre_vacc %>% 
  group_by(Year) %>% 
  summarize(mean_rate = mean(rate))

measles_pre_vacc %>%
  ggplot(aes(x=State, y=rate)) + geom_bar(stat="identity") + facet_wrap(~Year, ncol=2) + theme_classic() + theme(axis.text.x = element_text(angle=90, vjust = 0.7, size = 7))


#maps
library(usmap)
plot_usmap(region="states")  

measles1963df <- yearly_rates_ext %>% 
  filter(Year==1963)

#playing with color palettes
plot_usmap(data = measles1963df, values = "rate") +
  scale_fill_viridis(option = "rocket", direction = -1)

plot_usmap(data = measles1963df, values = "TotalCount") +
  scale_fill_viridis(option = "D")

plot_usmap(data = measles1963df, values = "rate") + 
  scale_fill_continuous(type = "gradient", low= "#EFF3FF", high= "#08519C")


#or put the dplyr stuff right in data
plot_usmap(data = yearly_rates_ext %>% rename(state=State) %>% 
             filter(Year==1963), values = "rate", color="grey") 

#small multiples
measles_pre_vacc <- yearly_rates_ext %>% 
  rename(state=State) %>% 
  filter(between(Year, 1958, 1963))

measles_pre_vacc$rate <- replace_na(measles_pre_vacc$rate, 0)

sum(is.na(measles_pre_vacc$rate))
sum(is.na(yearly_rates_ext))

plot_usmap(data = measles_pre_vacc, values = "rate") +
  facet_wrap(~Year) +
  scale_fill_viridis()
  
plot_usmap(data = measles_pre_vacc, values = "rate") +
  facet_wrap(~Year) +
  scale_fill_continuous(type = "gradient", low= "#EFF3FF", high= "#08519C")

plot_usmap(data = measles_pre_vacc, values = "rate") +
  facet_wrap(~Year) +
  scale_fill_viridis(option = "rocket", direction = -1)


#highlighting

regional_rates <- yearly_rates_ext %>% filter(state.division=="South Atlantic" & between(Year, 1950, 1980))

regional_rates %>% ungroup() %>% count(state)

tmp <- regional_rates %>%
  mutate(state2=state)

tmp %>%
  ggplot(aes(x=Year, y=rate)) +
  geom_line(data=tmp %>% dplyr::select(-state), aes(group=state2), color="grey", size=0.5, alpha=0.5) +
  geom_line(aes(color=state), color="#69b3a2", size=1.2 ) +
  scale_x_continuous(breaks=seq(from=1950, to=1980, by=5)) +
  scale_color_viridis() +
  theme_minimal() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14),
    panel.grid = element_blank()
  ) +
  ggtitle("A comparison of measles cases in the South Atlantic Region") +
  facet_wrap(~state, ncol = 2)
