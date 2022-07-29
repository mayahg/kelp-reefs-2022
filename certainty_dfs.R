#create maya high certainty data frame
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

#create merritt high certainty data frame
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


bound_high <- left_join(merritt_kelp_counts_high_certainty, maya_kelp_counts_high_certainty)

high_stipe_df <- left_join(bound_high, clean_stipe_df, by = "transect_id")

for_highc_plots <- high_stipe_df %>% 
  select(transect_id, stipe_count_merritt_high, stipe_count_green_high, stipe_count_field)


for_highc_plots %>% 
  ggplot(aes(x = stipe_count_field, y = stipe_count_green_high))+
  geom_point()+ 
  geom_smooth(method = lm, se = F)+
  geom_abline(intercept = 0, slope = 1)+
  ylim(0, 275)+
  labs(title = "Green High Certainty vs. Field Counts")


#adding equation of line of best fit and r^2 value
eq <- function(x,y) {
  m <- lm(y ~ x)
  as.character(
    as.expression(
      substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                 list(a = format(coef(m)[1], digits = 4),
                      b = format(coef(m)[2], digits = 4),
                      r2 = format(summary(m)$r.squared, digits = 3)))
    )
  )
}


for_highc_plots %>% 
  ggplot(aes(x = stipe_count_field, y = stipe_count_green_high))+
  geom_point()+ 
  geom_smooth(method = lm, se = F)+
  geom_abline(intercept = 0, slope = 1)+
  ylim(0, 275)+
  labs(title = "Green High Certainty vs. Field Counts")+
  geom_text(x = 0, y = 200, label = eq(for_highc_plots$stipe_count_field, for_highc_plots$stipe_count_green_high), 
            parse = T)



for_highc_plots %>% 
  ggplot(aes(x = stipe_count_field, y = stipe_count_merritt_high))+
  geom_point()+ 
  geom_smooth(method = lm, se = F)+
  geom_abline(intercept = 0, slope = 1)+
  labs(title = "Merritt High Certainty vs. Field Counts")+
  geom_text(x = 0, y = 200, label = eq(for_highc_plots$stipe_count_field, for_highc_plots$stipe_count_merritt_high), 
            parse = T)


by_observer <- for_highc_plots %>% 
  gather(key = "observer", value = "high_counts", 
         stipe_count_merritt_high:stipe_count_green_high) 

high_stipe_df <- high_stipe_df %>% 
  gather(key = "observer2", value = "high_certainty_vid_counts", 
         stipe_count_merritt_high, stipe_count_green_high) 

  
high_stipe_and_indiv_df <- high_stipe_df %>%  
gather(key = "individual_observer", value = "high_certainty_indiv_counts",
         individual_count_merritt_high, individual_count_green_high)
 



by_observer %>% 
  ggplot(aes(x = stipe_count_field, y = high_counts, color = observer))+
  geom_point()+
  geom_smooth(method = lm, se = F)+
  geom_abline(intercept = 0, slope = 1)+
  labs(title = "Field vs. High certainty stipe counts by observer")

by_observer %>% 
  ggplot(aes(x = field_count, y = high_certainty_counts, color = observer))+
  geom_point()+
  geom_smooth(method = lm, se = F)+
  geom_abline(intercept = 0, slope = 1)+
  labs(title = "Field vs. High certainty stipe counts by observer") +
  xlab("Stipes Observed in Situ") +
  ylab("High Certainty Video Stipe Counts")+
  facet_wrap(~observer)

#change value names within column "observer"
by_observer$observer <- ifelse(by_observer$observer == "stipe_count_merritt_high", 
                               "Merritt", by_observer$observer)
by_observer$observer<- ifelse(by_observer$observer == "stipe_count_green_high", 
                              "Green", by_observer$observer)

by_observer <- by_observer %>% 
  select(transect_id, observer, stipe_count_field, high_counts) %>% 
  rename("field_count" = "stipe_count_field", "high_certainty_video_count" = "high_counts")

high_stipe_df %>% 
  ggplot(aes(x = stipe_count_field, y = high_certainty_vid_counts, color = camera_type))+
  geom_point()+
  geom_smooth(method = lm, se = F)+
  geom_abline(intercept = 0, slope = 1)
 


combined_data_raw <- read.csv("mg_kelp_stipe_counts_from_video_220714 - raw_data.csv")
green_data_raw <- read.csv("corrected_kelp_stipes_from_video_220713 - raw_data.csv")
merritt_data_raw <- read.csv("kelp_stipe_counts_from_video_220421 - raw_data.csv")



