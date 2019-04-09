# Kodiak rockfish length summaries for 
# Sonya Elmejjati
# 2019-04-09

# load ----
library(tidyverse)
library(lubridate)
library(FNGr) # devtools::install_github("ben-williams/FNGr")
theme_set(theme_sleek())

# data ----

read_csv('data/RF_lengths_Rclass.csv') %>% 
  rename(Fishery = fishery, landing_year = Landing_Year,
         date = Sample_Date, species_code = Species_Code,
         length = Length, total_fish = Total_Fish)  %>% 
  mutate(date = mdy(date),
         fish = map(total_fish, ~rep_len(1, .x))) %>% 
  unnest() %>% 
  dplyr::select(-total_fish, -fish, - species_code)

# examine data ----  
unique(df$Port)
unique(df$Section)
unique(df$Fishery)
unique(df$species_code)

# eda ----
# histogram
df %>% 
  ggplot(aes(length)) + 
    geom_histogram(aes(fill = Fishery), binwidth=1) +
    facet_wrap(~landing_year, dir = 'v') +
    xlab('\nLength (cm)') +
    ylab('Density\n') +
  scale_fill_grey()

# density
df %>% 
  ggplot(aes(length)) + 
  geom_density(aes(fill = Fishery), alpha= 0.3) +
  facet_wrap(~landing_year, dir = 'v') +
  xlab('\nLength (cm)') +
  ylab('Density\n') +
  scale_fill_grey()

# histogram on a density scale
df %>% 
  ggplot(aes(length)) + 
  geom_histogram(aes(fill = Fishery, y = ..density..), binwidth=1) +
  facet_wrap(~landing_year, dir = 'v') +
  xlab('\nLength (cm)') +
  ylab('Density\n') +
  scale_fill_grey()

# histogram and line (though I do not necessarily recommend this)

ggplot(data = filter(df, Fishery=='CF'), aes(length, color = Fishery, fill = Fishery)) + 
  geom_histogram(aes(y = ..density..), binwidth=1, alpha = 0.3) +
  geom_density(data = filter(df, Fishery=='SF'), aes(length), alpha = 0) +
  facet_wrap(~landing_year, dir = 'v') +
  xlab('\nLength (cm)') +
  ylab('Density\n') +
  scale_fill_grey() +
  scale_color_manual(values = c("#999999", 1)) 
