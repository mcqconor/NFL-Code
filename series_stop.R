library(nflfastR)
library(tidyverse)
library(ggthemes)

imp_pbp <- load_pbp(2016:2020)
colnames(imp_pbp)

messy <- c('End of half','QB kneel')
stop <- c('Punt','Turnover on downs','Safety','Turnover','Opp touchdown')

imp_pbp %>% 
  filter(!is.na(down)) %>%
  filter(down == 1) %>% 
  select(down, yards_gained, series, series_result) %>% 
  group_by(yards_gained, series_result) %>% 
  summarise(
    count = n()
  ) %>% 
  ungroup() %>% 
  filter(!series_result %in% messy) %>%
  mutate(
    type = ifelse(series_result %in% stop, 'Stop','Not')
  ) %>% 
  group_by(yards_gained, type) %>% 
  summarise(
    num = sum(count)
  ) %>% 
  ungroup() %>% 
  group_by(yards_gained) %>% 
  mutate(
    tot_num = sum(num)
  ) %>% 
  ungroup() %>% 
  mutate(
    rate = num/tot_num
  ) %>% 
  filter(type == 'Stop' & tot_num >99 & yards_gained < 10) %>% 
  ggplot()+
  geom_point(mapping = aes(x = yards_gained, y = rate*100))+
  geom_smooth(mapping = aes(x = yards_gained, y = rate*100), method = 'lm', se = FALSE)+
  # ylim(0,100)+
  scale_x_continuous(breaks = -13:9)+
  xlab('Yards Gained on First Down')+
  ylab('Series Stop Rate (%)')+
  theme_fivethirtyeight()+
  theme(axis.title = element_text())+
  labs(
    title = 'How Important is Limiting Yards on 1st Down?',
    subtitle = 'Stop = Punt, Turnover on Downs, Safety, Turnover | 2016-2020 | min 100 occurrences',
    caption = 'By: Conor R. McQuiston @ConorMcQ5 | Data: nflfastR'
  )

ggsave('fd_lim.png', height = 7, width = 13)
