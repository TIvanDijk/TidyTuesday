# libraries  -------------------------------------------------------------------
library(readr)
library(tidyverse)
library(ggplot2)
library(TTR)
library(ggtext)
library(ggimage)

# read the data ----------------------------------------------------------------
fishing <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-08/fishing.csv')

# prepare the data  ------------------------------------------------------------
# as there is no grand_total for Lake Michigan I use the 'value' column to calculate this
# To avoid double-counting I only count the regions that include 'total' and do not include 'state'
# The regions left-over are: Total Canada (ONT); U.S. Total; U.S. Total (MI); U.S. Total (NY)
# upon quick inspection, the latter three do not appear to overlap --> i.e. only 1 for each specie / year available

data <- dplyr::filter(fishing, str_detect(tolower(region), 'total'), 
                               !str_detect(tolower(region), 'state')) %>%
  group_by(lake, year) %>%
  summarise(total = sum(values, na.rm = T)) %>%
  group_by(lake) %>% 
  mutate(ma = TTR::runMean(total, 5)) %>% 
  dplyr::filter(year >= 1915, total > 1500) %>%
  group_by(year) %>% 
  mutate(fraction = ma / sum(ma), 
         cols = case_when(lake == 'Michigan' ~ 'A',
                          T ~ 'B'))

other.lakes = unique(data$lake)[unique(data$lake) != 'Michigan']

# plot the data  ---------------------------------------------------------------
ggplot(data, aes(x = year, y = fraction, group = lake)) +
  geom_line(aes(color = cols), size = 1.2) +
  geom_image( aes(image = 'fishingIcon.png', x = 2013.4, y = 0.82), size = 0.18, angle = 90) +
  labs(title = 'Fish Production in Major US Lakes', y = 'fish production (in %)',
       caption = '@ThomIvar | source: Great Lakes Fishery Commission | image: Freepik',
       subtitle = paste0("The rise and fall of <span style='color:#4682b4'>**Lake Michigan**</span> 
                         compared to Lake ", other.lakes[1], ', Lake ', other.lakes[2], ', Lake ', 
                         other.lakes[3], ' and Lake ', other.lakes[4], '. The figure shows the percentage of
       fish produced based on the 5-year moving average.')) + 
  scale_color_manual(values = c('#4682b4','grey80')) +
  scale_y_continuous(labels = scales::percent) +
  coord_cartesian(ylim = c(0, 0.75), clip = 'off') +
  theme_minimal() +
  theme(legend.position = 'none', text = element_text('mono'),
        axis.line.x = element_line(size = 1, color = 'grey50'),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 12, color = 'grey70'),
        axis.title = element_text(size = 14, color = 'grey50'),
        plot.title = element_text('mono', color = 'grey50', size = 24),
        plot.subtitle = element_textbox_simple(color = 'grey70', size = 14, 
                                               width = 0.84, hjust = 0),
        plot.caption = element_text(color = 'grey80', size = 12, vjust = 2),
        plot.margin = margin(0.5,1,0.5,1, 'cm'),
        plot.background = element_rect(fill = '#fffdf6', color =  '#fffdf6'),
        panel.background = element_rect(fill = '#fffdf6', color =  '#fffdf6'))

ggsave('fishing.png', width = 10.5, height = 6.5)





