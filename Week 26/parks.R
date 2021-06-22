# libraries
library(ggplot2)
library(tidyverse)
library(sf)
library(spData)
library(jsonlite)
library(patchwork)
library(ggtext)

# load data --------------------------------------------------------------------
dataset <- tidytuesdayR::tt_load(2021, 26)
parks <- dataset$parks

# prepare the data  ------------------------------------------------------------
data <- parks %>% 
  mutate(expenditure = as.numeric(str_remove(parks$spend_per_resident_data, '[\\$,]')), 
         size = as.numeric(str_remove(park_pct_city_data, '%')), 
         closeness = as.numeric(str_remove(pct_near_park_data, '%'))) %>% 
  select( c('year', 'city', 'expenditure', 'size', 'closeness')) %>% 
  dplyr::filter(year %in% c(2015, 2020) & city != 'Anchorage' & city != 'Honolulu') %>% 
  pivot_wider(names_from = year, values_from = c(expenditure, size, closeness)) %>% 
  drop_na()  %>%     # only focus on parks with obs. in both 2015 & 2020
  mutate(change_exp = (expenditure_2020 - expenditure_2015) / expenditure_2015, 
         change_size = (size_2020 - size_2015) / size_2015, 
         change_closeness = (closeness_2020 - closeness_2015) / closeness_2015) %>% 
  mutate_at(vars(matches('change')), function(x) round(100*x, 1)) %>% 
  mutate(gotCloser = ifelse(change_closeness > 0, 'y', 'n'), 
         gotLarger = ifelse(change_size > 0, 'y', 'n'), 
         city = case_when(city == 'Washington, D.C.' ~ 'Washington', 
                          city == 'Arlington, Texas' ~ 'Arlington', 
                          city == 'Nashville' ~ 'Nashville-Davidson', 
                          city == 'Lexington' ~ 'Lexington-Fayette', 
                          city == 'Louisville' ~ 'Louisville/Jefferson County',
                          T ~ city))
# get coordinates of used cities 
coords <- fromJSON('https://gist.githubusercontent.com/Miserlou/c5cd8364bf9b2420bb29/raw/2bf258763cdddd704f8ffd3ea9a3e81d25e2c6f6/cities.json', flatten = T)

citydata <- coords %>% 
  group_by(city) %>% 
  arrange(as.integer(rank)) %>% 
  slice_head() %>% 
  right_join(data[, c('city', 'gotLarger')])

table(data$gotLarger)

# plot the data  ---------------------------------------------------------------
size_legend = guide_legend(title = 'The dots indicate how \nmuch of the city area \nis dedicated to parkland:', direction = 'horizontal', 
                           title.position = 'top', override.aes=list(colour="steelblue"), 
                           title.theme = element_text('mono', color = 'grey70', size = 12),
                           label.theme = element_text('mono', color = 'grey50', size = 14, face = 'bold'))


main <- ggplot(data, aes(x = expenditure_2020, y = expenditure_2015, size = size_2020,
                 color = gotLarger)) + 
  geom_abline(slope = 1, size = 1.5, color = 'steelblue4', alpha = 0.8) +
  geom_point(alpha = 0.8) +
  geom_segment(aes(x = 239, y = 242, xend = 233, yend = 270), inherit.aes = F,
               color = '#97BC62FF', arrow = arrow(length = unit(0.2, "cm"))) +
  annotate('text', x = 240, y = 255, angle = 24, label = 'lower spendings', 
           hjust = 0, color = '#97BC62FF', family = 'mono') +
  geom_segment(aes(x = 240, y = 237, xend = 246, yend = 210), inherit.aes = F,
               color = '#E94B3CFF', arrow = arrow(length = unit(0.2, "cm"))) +
  annotate('text', x = 245, y = 228, angle = 24, label = 'higher spendings', 
           hjust = 0, color = '#E94B3CFF', family = 'mono') +
  xlim(10,400) + ylim(10,400) +
  scale_size_continuous(range = c(2,10), breaks = c(5, 15, 25), labels = c('5%', '15%', '25%')) +
  scale_color_manual(values = c('#5F4B8BFF', '#E69A8DFF')) +
  labs(title = 'US City Parks Expenditure', 
       subtitle = "Compared to 2015, most considered US cities have **increased** their city park expenditures. This did not necessarily result in *greener cities*: In <span style='color:#5F4B8BFF'>**27 cities**</span> the area dedicated to parkland decreased, whereas the area dedicated to parkland increased in <span style='color:#E69A8DFF'>**44 cities**</span>.", 
       x = 'Spending per US resident in USD, **2020**',
       y = 'Spending per US resident in USD, **2015**', 
       caption = '@ThomIvar | source: The Trust for Public Land') +
  guides(color = F, size = size_legend) +
  theme_classic() +
  theme(plot.background = element_rect(fill = '#fffdf6', color =  '#fffdf6'),
        panel.background = element_rect(fill = '#fffdf6', color =  '#fffdf6'),
        plot.title = element_text(color = 'grey50', size = 25, vjust = 6),
        plot.subtitle = element_textbox_simple(size = 15, color = 'grey70', vjust = -0.5), 
        plot.caption = element_text(color = 'grey80', size = 12), 
        axis.title.x = element_markdown('mono', size = 14, color = 'grey50'),
        axis.title.y = element_markdown('mono', size = 14, color = 'grey50'),
        axis.line = element_line(color = 'grey70'), 
        axis.ticks = element_line(color = 'grey70'),
        axis.text = element_text(color = 'grey70', size = 10),
        text = element_text('mono'), 
        legend.position = c(0.8, 0.2), 
        legend.background = element_rect(fill = '#fffdf6', color =  '#fffdf6'), 
        plot.margin = margin(1.5,1,0.5,1, 'cm'))

map <- st_as_sf(spData::us_states) %>% 
  ggplot() +
    geom_sf(fill = 'grey90', alpha = 0.4) +
    geom_point(data = citydata, aes(y = latitude, x = longitude, color = gotLarger), 
               size = 2.5, alpha = 0.8) +
    scale_color_manual(values = c('#5F4B8BFF', '#E69A8DFF')) +
    labs(title = 'Considered Cities') +
    theme_void() +
    theme(legend.position = 'none', 
          plot.title = element_text('mono', hjust = 0.2, face = 'bold', 
                                    color = 'grey50', size = 16))

main + inset_element(map, l = 0, r = 0.40, t = 1, b = 0.55) +
  plot_annotation(theme = theme(plot.background = element_rect(fill = '#fffdf6', color =  '#fffdf6')))

ggsave('parks.png', height = 7, width = 12)


