# libraries
library(tidyverse)
library(ggplot2)
library(ggtext)
library(showtext)
library(patchwork)

# Read the data ----------------------------------------------------------------
castaways <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-01/castaways.csv')

# Prepare the data -------------------------------------------------------------
data <- castaways %>% 
  dplyr::filter(season >= 36)

# y-label for {ggtext} per season
data.perseason <- data %>% 
  group_by(season, season_name) %>% 
  summarise(avg = mean(age)) %>% 
  mutate(season_name = str_remove(season_name, 'Survivor: '),  
         name = paste0("<span style='font-size:16pt'>**Season ", 
                       season, '**</span><br>*', season_name, '*'))

# labels for {ggtext} in the legend
legend.data <- data.frame(
  label = rep("<span style='font-size:18pt'> 25% </span><br>(of the observations)", 2), 
  x =  c(35.5, 40.5), y = c(0, 0)
)

# Find some statistics of the data ---------------------------------------------
median.age = quantile(castaways$age, 0.5)
youngest = castaways[which.min(castaways$age), c('full_name', 'age', 'season')]
oldest = castaways[which.max(castaways$age), c('full_name', 'age', 'season')]
oldest.data = data[which.max(data$age), c('full_name', 'age', 'season')]

# Make the plot ----------------------------------------------------------------
font_add_google("Sigmar One", "sigmar")
showtext_auto()

colors = c('#a8e6cf', '#dcedc1', '#ffd3b6', '#ffaaa5', '#ff8b94')
set.seed(42)

# create own legend
legend <- ggplot(filter(data, season == 40), aes( x = age)) +
  geom_boxplot(outlier.shape = NA, coef = 0, width = 1.5, fill = 'steelblue2',
               alpha = 0.6, color = 'grey30') +
  geom_richtext(data = legend.data, aes(x = x, y = y, label = label), 
            size = 2, color = 'grey95',fill = NA, label.color = NA) +
  coord_fixed(xlim = c(33,43)) +
  labs(title = 'Median') +
  theme_void() +
  theme(plot.title = element_text(color = 'grey50', size = 13, face = 'bold', hjust = 0.45))

# create main plot
main <- ggplot(data, aes(y = season, x = age, color = paste(season))) +
  geom_vline(xintercept = median.age, size = 2, color = 'grey90', alpha = 0.7) +
  geom_jitter(size = 3, height = 0.15, width = 0.05) +
  geom_boxplot(aes(y = season - 0.4, fill = paste(season)), outlier.shape = NA, width = 0.25, 
               coef = 0, color = 'grey30', alpha = 0.8) +
  geom_richtext(data = data.perseason, aes(x = 14, y = season - 0.2, label = name),
                label.color = NA, fill = NA, size = 4.5) +
  geom_curve(aes(x = 58, y = 39.5, xend = 59.5, yend = 39.15), color = 'grey50',
             angle = 90, arrow = arrow(30, unit(0.1, "inches"))) +
  geom_textbox(aes(x = 62, y = 40), color = 'grey70', fill = NA, size = 3,
              label = paste0('**', oldest.data$full_name, '** participated at age ', oldest.data$age,
                             '. The oldest candidate is **', oldest$full_name, '**, who was ', oldest$age, 
                             ' years old when he appeared in Season ', oldest$season, '.')) +
  geom_curve(aes(x = 27.5, y = 40.45, xend = 29.5, yend = 40.25), color = 'grey50',
             angle = 90, arrow = arrow(30, unit(0.1, "inches"))) +
  geom_text(aes(x = 27.5, y = 40.65), label = 'median age of \nall seasons', 
            color = 'grey70', size = 3) +
  coord_cartesian(xlim = c(16, 65), clip = 'off') +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(title = 'Age distribution of survivor contestants', 
       subtitle = '***Survivor*** is an American TV show where contestants are faced with
       a number of challenges. Contestants are progressively eliminated until one remains,
       who gets the title of ***Sole Survivor***. The show is based on the popular Swedish show 
       ***Expedition Robison***, which has spin-offs in many countries across the world.<br>',
       caption = '@ThomIvar | source: {survivoR} by Daniel Oehm',
       x = 'Age') +
  theme_classic() +
  theme(legend.position = 'none', 
        axis.title.y = element_blank(), axis.line.y = element_blank(),
        axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        axis.line.x = element_line(size = 1, color = 'grey50'),
        axis.ticks.x = element_line(size = 1, color = 'grey50'),
        axis.text.x = element_text(size = 12, color = 'grey70'),
        axis.title.x = element_text(size = 14, color = 'grey50'),
        plot.title = element_text('sigmar', color = 'grey50', size = 24),
        plot.subtitle = element_textbox_simple(color = 'grey70', size = 14),
        plot.caption = element_text(color = 'grey80', size = 12),
        plot.margin = margin(0.5,1,0.5,2, 'cm'),
        plot.background = element_rect(fill = '#fffdf6', color =  '#fffdf6'),
        panel.background = element_rect(fill = '#fffdf6', color =  '#fffdf6'))

# combine main plot and legend using {patchwork}
main + inset_element(legend, 0.7, 0.05, 0.95, 0.25) +
  plot_annotation(theme = theme(plot.background = element_rect(fill = '#fffdf6', color =  '#fffdf6')))

ggsave('survivor.png', width = 12, height = 7)

