library(dplyr)
library(reactable)
# data
results = read.csv("../eurovision_results.csv")
events = read.csv("../events_info.csv")

# data frame: country, first attended eurovision, last attended eurovision
df_country_first_last = data.frame(Country = unique(results$Country))

# first attended eurovision year
min_years <- results %>%
  group_by(Country) %>%
  summarise(first_attended = min(Year, na.rm = TRUE)) 

#last attended eurovision year
max_years <- results %>%
  group_by(Country) %>%
  summarise(last_attended = max(Year, na.rm = TRUE)) 

# how many times a country has attended eurovision
times_attended <- results %>%
                  group_by(Country) %>%
                  summarise(times_attended = n())


# nr of times being in the top 5
freq_top_5 <- results %>%
              filter(suppressWarnings(as.integer(Grand.Final.Place)) <= 5) %>%
              group_by(Country) %>%
              summarise(freq_top_5 = n())

freq_top_5

# best scored place
best_scored_place <- results %>%
                     mutate(Grand.Final.Place = ifelse(Grand.Final.Place %in% c("N/A", "DQ", "NQ"), "NULL", Grand.Final.Place))%>%
                     group_by(Country) %>% 
                     summarise(best_scored_place = as.integer(min(suppressWarnings(as.integer(Grand.Final.Place)), na.rm = TRUE)))
best_scored_place


# best song (top place if ties -> most points)
best_songs <- results
best_songs <- best_songs %>%
              mutate(
                Grand.Final.Points = ifelse(Grand.Final.Points %in% c("N/A", "DQ", "NQ"), "0", Grand.Final.Points),
                Grand.Final.Place = ifelse(Grand.Final.Place %in% c("N/A", "DQ", "NQ"), "100", Grand.Final.Place)
              ) %>%
              group_by(Country) %>%
              filter(as.integer(Grand.Final.Place) == min(as.integer(Grand.Final.Place))) %>% 
              filter(as.integer(Grand.Final.Points) == max(as.integer(Grand.Final.Points))) %>%
              slice(1)

best_songs <- best_songs %>%
              mutate(Grand.Final.Points = ifelse(as.integer(Grand.Final.Points) == 0, "N/A", Grand.Final.Points),
                    Grand.Final.Place = ifelse(as.integer(Grand.Final.Place) == 100, "N/A", Grand.Final.Place))

df_best_song_country <- data.frame(Country = best_songs$Country,
                                   Best_Song = paste(best_songs$Song ,"by", best_songs$Artist))
df_best_song_country

df_country_first_last <- df_country_first_last %>%
  left_join(min_years, by = "Country") %>%
  left_join(max_years, by = "Country") %>%
  left_join(times_attended, by = "Country") %>%
  left_join(freq_top_5, by = "Country") %>%
  left_join(best_scored_place, by = "Country") %>%
  mutate(freq_top_5 = ifelse(is.na(freq_top_5), 0, freq_top_5),
         best_scored_place = ifelse(is.infinite(best_scored_place), "N/A", best_scored_place))  %>%
  left_join(df_best_song_country, by = "Country")

names(df_country_first_last) = c("Country", "First Eurovision", "Last Eurovision", 
                                 "Times Attended", "Top 5 Placements", "Best Place Scored", "Best Song")
reactable(df_country_first_last)

















