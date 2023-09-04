library(tidyverse)
library(fivethirtyeight)
library(scales)

glimpse(college_recent_grads)

# college_recent_grads %>% 
#   select(rank, major, unemployment_rate) %>%
#   arrange(unemployment_rate)

#majors with the lowest unemployment rate
college_recent_grads %>%
  arrange(unemployment_rate) %>%
  relocate(rank, major, unemployment_rate)

#majors with the highest unemployment rate
college_recent_grads %>%
  arrange(desc(unemployment_rate)) %>%
  relocate(rank, major, unemployment_rate)

# top 3 majors with highest women proportions
college_recent_grads %>% 
  arrange(desc(women)) %>% 
  relocate(major, women, total, sharewomen) %>% 
  top_n(3)

# viz median income accross majors
ggplot(data=college_recent_grads)+
  geom_histogram(mapping = aes(x = median), binwidth = 5000)+
  labs(
    x = "Median earnings, in $",
    y = "Frequency",
    title = "Distribution of median earnings for college majors"
  )

# descriptive statistics
college_recent_grads %>% 
  summarise(
    min = min(median),
    max = max(median),
    mean = mean(median),
    med = median(median),
    sd = sd(median), 
    q1 = quantile(median, probs = 0.25),
    q3 = quantile(median, probs = 0.75)
  )

# viz faceted median income 
ggplot(data = college_recent_grads)+
  geom_histogram(mapping = aes(x=median),binwidth = 5000)+
  facet_wrap(~major_category)+
  labs(
    x = "Median earnings, in $",
    y = "Frequency",
    title = "Distribution of median earnings for college majors",
    subtitle = "By major category"
  )

# avg median incomes for each major category 
college_recent_grads %>% 
  group_by(major_category ) %>% 
  summarise(avg_mean_income = mean(median)) %>% 
  arrange(desc(avg_mean_income))

# least popular majors
college_recent_grads %>%
  count(major_category) %>%
  arrange(n)

stem_categories <- c("Biology & Life Science",
                     "Computers & Mathematics",
                     "Engineering",
                     "Physical Sciences")
college_recent_grads <- college_recent_grads %>% 
  mutate(major_type = ifelse(major_category %in% stem_categories, "STEM", "Others"))

college_recent_grads %>% 
  relocate(major, major_type) %>% 
  filter(major_type=="STEM")

college_recent_grads %>%
  filter(
    major_type == "STEM",
    median < 36000
  ) %>%
  relocate(major, p25th, median, p75th) %>% 
  arrange(desc(median))

ggplot(data = college_recent_grads)+
  geom_smooth(mapping = aes(x = median, y = sharewomen, color=major_type))+
  labs(
    x = "Proportion of women",
    y = "Median income",
    color = "Major type"
  )+
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_number())
