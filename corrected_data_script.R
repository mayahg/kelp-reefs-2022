green_summary <- green_data_raw%>%
  group_by(transect_id) %>% 
  summarize(individual_count = n(), 
            stipe_count = sum(stipe_count, na.rm = T)) %>% 
  mutate(individual_count = ifelse(stipe_count == 0, 0, individual_count))


merritt_summary <- merritt_data_raw%>%
  group_by(transect_id) %>% 
  summarize(individual_count = n(), 
            stipe_count = sum(stipe_count, na.rm = T)) %>% 
  mutate(individual_count = ifelse(stipe_count == 0, 0, individual_count))

joined_summary <- left_join(merritt_summary, green_summary, by = "transect_id") %>% 
  rename("merritt_individual_count" = "individual_count.x", 
         "merritt_stipe_count" = "stipe_count.x",
         "green_individual_count" = "individual_count.y",
         "green_stipe_count" = "stipe_count.y")
dive_transect_metadata <- read.csv("csv_files/dive_transect_metadata - data.csv")
field_vs_merritt_counts <- read.csv("csv_files/comp_kelp_stipe_counts_ - metadata.csv") %>% 
  select(-swath_m)

merritt_green_field_summary <- left_join(field_vs_merritt_counts, 
                                         green_summary, by = "transect_id") %>% 
  rename("merrit_individual_count" = "individual_count_video",
         "merritt_stipe_count" = "stipe_count_video", 
         "green_individual_count" = "individual_count",
         "green_stipe_count" = "stipe_count")

elahi_merritt_green_joined <- left_join(merritt_green_field_summary, dive_transect_metadata, 
                                        by = "transect_id") %>% 
  gather(key = "observer", value = "video_count", merritt_stipe_count, green_stipe_count) %>% 
  mutate(swath_width = as.character(swath_m))

ggplot(data = elahi_merritt_green_joined, aes(x = stipe_count_field, y = video_count, color = observer)) +
  geom_point() +
  geom_smooth(method = lm, se=F)+
  geom_abline(intercept =0, slope=1)+
  facet_wrap(~observer)

ggplot(data = elahi_merritt_green_joined, aes(x = stipe_count_field, y = video_count, color = swath_width)) +
  geom_point() +
  geom_smooth(method = lm, se=F)+
  geom_abline(intercept =0, slope=1)+
  facet_wrap(~observer)


#data for certainty plots

maya_kelp_counts_high_certainty <- green_data_raw %>%
  mutate(stipe_count2 = ifelse(certainty_in_swath == "low", 0.01, stipe_count)) %>% 
  group_by(transect_id) %>% 
  summarize(individual_count = n(), 
            stipe_count = sum(stipe_count2, na.rm = T)) %>%
  mutate(stipe_count_green_high = ifelse(stipe_count < 1, 0, floor(stipe_count))) %>% 
  mutate(difference_stipe_count = stipe_count - stipe_count_green_high) %>% 
  mutate(times_100 = difference_stipe_count*100) %>% 
  mutate(actual_individual_count = individual_count - times_100) %>% 
  mutate(individual_count_green_high = ifelse(stipe_count == 0, 0, actual_individual_count)) %>% 
  select(transect_id, stipe_count_green_high, individual_count_green_high)

merritt_kelp_counts_high_certainty <- merritt_data_raw %>% 
  mutate(stipe_count2 = ifelse(certainty_in_swath == "low", 0.01, stipe_count)) %>% 
  group_by(transect_id) %>% 
  summarize(individual_count = n(), 
            stipe_count = sum(stipe_count2, na.rm = T)) %>%
  mutate(stipe_count_merritt_high = ifelse(stipe_count < 1, 0, floor(stipe_count))) %>% 
  mutate(difference_stipe_count = stipe_count - stipe_count_merritt_high) %>% 
  mutate(times_100 = difference_stipe_count*100) %>% 
  mutate(actual_individual_count = individual_count - times_100) %>% 
  mutate(individual_count_merritt_high = ifelse(stipe_count == 0, 0, actual_individual_count)) %>% 
  select(transect_id, stipe_count_merritt_high, individual_count_merritt_high)


bound_high <- left_join(merritt_kelp_counts_high_certainty, maya_kelp_counts_high_certainty,
                        by = "transect_id")

high_stipe_df <- left_join(bound_high, field_vs_merritt_counts, by = "transect_id") %>% 
  select(-c(individual_count_video, stipe_count_video, individual_count_field, 
            individual_count_green_high, individual_count_merritt_high))

high_stipe_df %>% 
  ggplot(aes(x = stipe_count_field, y = stipe_count_green_high))+
  geom_point()+ 
  geom_smooth(method = lm, se = F)+
  geom_abline(intercept = 0, slope = 1)+
  labs(title = "Green High Certainty vs. Field Counts")

high_stipe_df %>% 
  ggplot(aes(x = stipe_count_field, y = stipe_count_merritt_high))+
  geom_point()+ 
  geom_smooth(method = lm, se = F)+
  geom_abline(intercept = 0, slope = 1)+
  labs(title = "Merritt High Certainty vs. Field Counts")


by_observer <- high_stipe_df %>% 
  gather(key = "observer", value = "high_counts", 
         stipe_count_merritt_high:stipe_count_green_high) 


by_observer %>% 
  ggplot(aes(x = stipe_count_field, y = high_counts, color = observer))+
  geom_point()+
  geom_smooth(method = lm, se = F)+
  geom_abline(intercept = 0, slope = 1)+
  labs(title = "Field vs. High certainty stipe counts by observer")


by_observer %>% 
  ggplot(aes(x = stipe_count_field, y = high_counts, color = observer))+
  geom_point()+
  geom_smooth(method = lm, se = F)+
  geom_abline(intercept = 0, slope = 1)+
  labs(title = "Field vs. High certainty stipe counts by observer") +
  xlab("Stipes Observed in Situ") +
  ylab("High Certainty Video Stipe Counts")+
  facet_wrap(~observer)

by_observer$observer <- ifelse(by_observer$observer == "stipe_count_merritt_high", 
                               "Merritt", by_observer$observer)
by_observer$observer<- ifelse(by_observer$observer == "stipe_count_green_high", 
                              "Green", by_observer$observer)

by_observer <- by_observer %>% 
  select(transect_id, field_count, observer, high_certainty_video_count)

