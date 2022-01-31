library(tidyverse)
library(tidyr)
library(dplyr)
library(ggthemes)

ndvi <- read_csv("data/ndiv_oct_2018_oct_2021.csv") %>% 
  rename(burned = 2, unburned = 3) %>% # rename columns
  filter(!is.na(burned), !is.na(unburned)) # comma in this context is essentially an 'and'

# this currently a wide dataset
# we are going to reorganize to a long format, the - symbol keeps all columns
# named after out of 'gather'

ndvi_long <- gather(ndvi, key = 'site', value = 'ndvi', -DateTime) 

ggplot(ndvi_long, aes(x=DateTime, y = ndvi, color = site)) +
    geom_line() +
    geom_point(shape=1) +
    theme_few() +
    scale_color_few()+
    theme(legend.position=c(.85, 0.85))

# Now if I had gotten all of the data that had been in the example (30yrs)
# it might be too dense to really see the trends, so we want
# to summarize by year

ndvi_annual <- ndvi_long %>%
  mutate(year=year(DateTime)) %>%
  group_by(site, year) %>%
  summarize(mean_NDVI=mean(NDVI))

# Now make the annual plot
ggplot(ndvi_annual, aes(x=DateTime, y = ndvi, color = site)) +
  geom_line() +
  geom_point(shape=1) +
  theme_few() +
  scale_color_few()+
  theme(legend.position=c(.85, 0.85))
