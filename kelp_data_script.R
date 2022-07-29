first_transect <- slice(by_transect, 1:17) %>% 
count(first_transect, "transect_id")
count(by_transect, "stipe_count")
first_transect <- slice(by_transect, 1:17) 


maya_kelp_counts <- stipe_data %>%
  #filter(!is.na(stipe_count)) %>% 
  group_by(transect_id) %>% 
  summarize(individual_count = n(), 
            stipe_count = sum(stipe_count, na.rm = T)) %>% 
  
  mutate(individual_count = ifelse(stipe_count == 0, 0, individual_count))

getwd()
green_data_raw <- read.csv("kelp_stipes_from_video_220620 - raw_data.csv")

maya_kelp_counts_certainty <- stipe_data %>%
  group_by(transect_id, certainty_in_swath, video_recorder) %>% 
  summarize(individual_count = n(), 
            stipe_count = sum(stipe_count, na.rm = T)) %>% 
  mutate(individual_count = ifelse(stipe_count == 0, 0, individual_count))



maya_kelp_counts_certainty$certainty_in_swath <- sub("^$", "high", maya_kelp_counts_certainty$certainty_in_swath)



merritt_kelp_counts_certainty <- merritt_data_raw%>%
  group_by(transect_id, certainty_in_swath, video_recorder) %>% 
  summarize(individual_count = n(), 
            stipe_count = sum(stipe_count, na.rm = T)) %>% 
  mutate(individual_count = ifelse(stipe_count == 0, 0, individual_count))


merritt_data_raw <- read.csv("merritt_kelp_stipe_counts_from_video_220421 - raw_data.csv")

merritt_data_raw$stipe_count <- as.numeric(merritt_data_raw$stipe_count)
merritt_data_raw$distance_m <- as.numeric(merritt_data_raw$distance_m)
merritt_data_raw$swath_m <- as.numeric(merritt_data_raw$swath_m)

merritt_kelp_counts <- merritt_data_raw %>% 
  select(transect_id, stipe_count) %>%
  group_by(transect_id) %>% 
  summarize(individual_count = n(), 
            stipe_count = sum(stipe_count, na.rm = T)) %>% 
  mutate(individual_count = ifelse(stipe_count == 0, 0, individual_count))
  
comparison <- read_csv("comp_kelp_stipe_counts_ - metadata.csv") 
comparison <- rename(comparison, individual_count_merritt = individual_count_video)
comparison <- rename(comparison, stipe_count_merritt = stipe_count_video)  
  
comparison <- select(comparison, -swath_m)

library(tidyverse)
all_data <- read_csv("Copy of kelp_stipe_counts_comparison - metadata.csv")

indv_count_green_vs_field <- ggplot(data = all_data, aes(x = individual_count_field, y = individual_count_green)) +
  geom_point() +
  geom_smooth(method = lm)

indv_count_green_vs_merritt <- ggplot(data = all_data, aes(x = individual_count_merritt, y = individual_count_green)) +
  geom_point() +
  geom_smooth(method = lm)

ggplot(data = all_data, aes(x = stipe_count_field, y = stipe_count_green, color = swath_m)) +
  geom_point() +
  geom_smooth(method = lm)

ggplot(data = all_data, aes(x = stipe_count_field, y = stipe_count_merritt)) +
  geom_point() +
  geom_smooth(method = lm)

ggplot(data = all_data, aes(x = stipe_count_green, y = stipe_count_merritt)) +
  geom_point() +
  geom_smooth(method = lm)

dive_transect_metadata <- read_csv("dive_transect_metadata - data.csv")
dive_transect_metadata <- select(dive_transect_metadata, -c(kelp_video_id, kelp_video_id_pasted, transect_id_pasted))

joined_transect_data <- left_join(dive_transect_metadata, all_data, by = "transect_id") %>% 
  filter(!is.na(kelp_video)) %>% 
  mutate(swath_width = as.character(swath_m))

#left_join was an interesting and probably wrong choice for this
joined_certainty <- left_join(maya_kelp_counts_certainty, merritt_kelp_counts_certainty, 
                              by = "transect_id") %>% 
  rename(certainty_in_swath_green = certainty_in_swath.x )


maya_kelp_counts_certainty <- stipe_data %>%
  group_by(transect_id, certainty_in_swath) %>% 
  summarize(individual_count = n(),   
            stipe_count = sum(stipe_count, na.rm = T)) %>% 
  mutate(individual_count = ifelse(stipe_count == 0, 0, individual_count))


#trying bind_rows
install.packages(dplyr)
library(dplyr)

bound_rows_certainty <- bind_rows(maya_kelp_counts_certainty, merritt_kelp_counts_certainty)

certainty_stipe_df <- left_join(clean_stipe_df, bound_rows_certainty, by = "transect_id")


joined_certainty <- left_join(maya_kelp_counts_certainty, merritt_kelp_counts_certainty, 
                              by = "transect_id") %>% 
  group_by(transect_id) %>% 
  summarize()
  rename(certainty_in_swath_green = certainty_in_swath.x )



green_raw_no_low <- green_data_raw %>%  
  filter(certainty_in_swath == "high")

merritt_raw_no_low <- merritt_data_raw %>%  
  filter(certainty_in_swath == "high")

joint_raw <- read_csv("joint_kelp_stipes_from_video_220707 - raw_data.csv")

green_kelp_counts_nolow <- green_raw_no_low %>% 
  select(video_recorder, stipe_count, certainty_in_swath, certainty_stipe_count, transect_id) %>% 
  group_by(transect_id) %>% 
  summarize(individual_count = n(), 
            stipe_count = sum(stipe_count, na.rm = T)) %>% 
  mutate(individual_count = ifelse(stipe_count == 0, 0, individual_count))

merritt_kelp_counts_nolow <- merritt_raw_no_low %>% 
  select(video_recorder, stipe_count, certainty_in_swath, certainty_stipe_count, transect_id) %>% 
  group_by(transect_id) %>% 
  summarize(individual_count = n(), 
            stipe_count = sum(stipe_count, na.rm = T)) %>% 
  mutate(individual_count = ifelse(stipe_count == 0, 0, individual_count))


ggplot(data = joined_transect_data, aes(x = stipe_count_field, y = stipe_count_green, color = swath_width)) +
  geom_point() +
  geom_smooth(method = lm, se=F)+
  geom_abline(intercept =0, slope=1)


stipe_df <- joined_transect_data %>% 
  select(-c(individual_count_field, individual_count_merritt, individual_count_green)) %>% 
  gather(key = "observer", value = "video_count", stipe_count_merritt:stipe_count_green) %>% 
  mutate(camera_type = substr(kelp_video, 1, 1))


ggplot(data = stipe_df, aes(x = stipe_count_field, y = video_count, color = swath_width)) +
  geom_point() +
  geom_smooth(method = lm, se=F)+
  geom_abline(intercept =0, slope=1)+
  facet_wrap(~observer)

ggplot(data = stipe_df, aes(x = stipe_count_field, y = video_count, color = swath_width)) +
  geom_point() +
  geom_smooth(method = lm, se=F)+
  geom_abline(intercept =0, slope=1)+
  facet_wrap(~observer)

ggplot(data = stipe_df, aes(x = stipe_count_field, y = video_count, color = camera_type)) +
  geom_point() +
  geom_smooth(method = lm, se=F)+
  geom_abline(intercept =0, slope=1)+
  facet_wrap(~observer)

stipe_df %>%
  filter(camera_type != "P" | swath_width != "1") %>% 
  ggplot(aes(x = stipe_count_field, y = video_count)) +
  geom_point() +
  geom_smooth(method = lm, se=F)+
  geom_abline(intercept =0, slope=1)+
  facet_wrap(~observer)


joint_no_low <- joint_raw %>%
  filter(certainty_in_swath == "high") %>% 
  select(video_recorder, transect_id, certainty_in_swath, certainty_stipe_count, swath_m, stipe_count) %>% 
  group_by(video_recorder, transect_id, certainty_in_swath) %>% 
  summarize(individual_count = n(), 
            stipe_count = sum(stipe_count, na.rm = T)) %>% 
  mutate(individual_count = ifelse(stipe_count == 0, 0, individual_count))

joint_no_high <- joint_raw %>%
  filter(certainty_in_swath == "low") %>% 
  select(video_recorder, transect_id, certainty_in_swath, certainty_stipe_count, swath_m, stipe_count) %>% 
  group_by(video_recorder, transect_id, certainty_in_swath) %>% 
  summarize(individual_count = n(), 
            stipe_count = sum(stipe_count, na.rm = T)) %>% 
  mutate(individual_count = ifelse(stipe_count == 0, 0, individual_count))


clean_stipe_df <- stipe_df %>% 
  select(-c(transect, heading, depth_start_ft, depth_end_ft, benthic_video, kelp_video_id_pasted, 
            transect_id_pasted, comment))

joint_no_low %>% 
  ggplot(aes(x = transect_id, y = stipe_count, color = video_recorder)) +
  geom_point() +
  geom_smooth(method = lm)+
  geom_abline(intercept =0, slope=1)+
  theme(axis.text.x = element_text(size = 6, angle = 90))

joint_raw <- read_csv("correct_joint_kelp_stipes_from_video_220707 - raw_data.csv")

joint_no_high %>% 
  ggplot(aes(x = individual_count, y = stipe_count, color = video_recorder)) +
  geom_point() +
  geom_smooth(method = lm, se = F)+
  labs(title = "Low Certainty in Swath")


high_certainty_plot <- joint_no_low %>% 
  ggplot(aes(x = video_recorder, y = stipe_count, color = transect_id)) +
  geom_point() +
  geom_smooth(method = lm)

joint_no_low %>% 
  ggplot(aes(x = individual_count, y = stipe_count, color = video_recorder)) +
  geom_point() +
  geom_smooth(method = lm, se = F) +
  labs(title = "High Certainty in Swath")

joint_no_low %>% 
  ggplot(aes(x = individual_count, y = stipe_count, color = transect_id)) +
  geom_point() +
  labs(title = "High Certainty in Swath by Transect")

joint_no_low %>% 
  ggplot(aes(x = individual_count, y = stipe_count, color = transect_id)) +
  geom_point() +
  labs(title = "High Certainty in Swath by Transect")+
  xlim(0,4) +
  ylim(0, 40)
  

joint_no_high %>% 
  ggplot(aes(x = individual_count, y = stipe_count, color = transect_id)) +
  geom_point() +
  labs(title = "Low Certainty in Swath by Transect") +
  facet_wrap(~video_recorder)


#is certainty_stipe_df even correct? 
certainty_stipe_df %>% 
  ggplot(aes(x = stipe_count_field, y = video_count, color = certainty_in_swath)) +
  geom_point()+
  facet_wrap(~observer)+
  geom_smooth(method = lm, se =F)+
  geom_abline(intercept =0, slope=1)+
  labs(title = "Stipe vs. Video Counts by Certainty in Swath")

certainty_stipe_df %>% 
  ggplot(aes(x = stipe_count_field, y = video_count, color = certainty_in_swath)) +
  geom_point()+
  geom_smooth(method = lm, se =F)+
  geom_abline(intercept =0, slope=1)


group_by(transect_id) %>% 
  summarize(individual_count = n(), 
            stipe_count = sum(stipe_count, na.rm = T)) %>% 
  mutate(stipe_count2 = ifelse(swath_certainty == "low", 0.01, stipe_count)) %>% 
  mutate(individual_count = ifelse(stipe_count == 0, 0, individual_count))

maya_kelp_counts_high_certainty <- stipe_data %>%
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

  




#mutate(stipe_count2 = ifelse(swath_certainty == "low", 0.01, stipe_count))
#summarize(individual_counts = n()
  #stipe_count = sum(stipe_count))
#ifelse(stipe_count < 1, 0, floor(stipe_count))



