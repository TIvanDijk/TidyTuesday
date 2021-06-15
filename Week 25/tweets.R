# libraries
library(ggplot2)
library(tidyverse)
library(ggforce)
library(ggfx)
library(emojifont)
library(ggtext)
library(showtext)

# load data --------------------------------------------------------------------
data <- tidytuesdayR::tt_load(2021, 25)
tweets <- data$tweets

# prepare the data  ------------------------------------------------------------
# function to get number in a nicer format
nice.num <- function(num){
  if (num < 1000) num
  else paste0( round(num / 1000, 1), 'K')
}

data <- tweets %>% 
  summarise(
    regular = sum(!verified, na.rm = T),
    verified = sum(verified, na.rm = T), 
    retweets = nice.num(sum(retweet_count, na.rm = T)),  
    likes = nice.num(sum(like_count, na.rm = T)), 
    quotes = nice.num(sum(quote_count, na.rm = T)), 
    mean.retweets = round(mean(retweet_count, na.rm = T), 1), 
    mean.likes = round(mean(like_count, na.rm = T), 1), 
    mean.chars = round(mean(nchar(content), na.rm = T), 0))

most.popular <- tweets %>% 
  mutate(engagements = retweet_count + like_count + quote_count) %>% 
  arrange(desc(engagements)) %>% 
  slice_head()

most.followers <- tweets %>% 
  arrange(desc(followers)) %>% 
  slice_head() %>% 
  mutate(followers = nice.num(followers))

tmp <- tweets[,1] %>% 
  mutate(day = format(datetime, '%B %d, %Y'), time = format(datetime, '%H:00 (%Z)')) 

pop.day <- count(tmp, day) %>% arrange(desc(n)) %>% slice_head()
pop.time <- count(tmp, time) %>%  arrange(desc(n)) %>% slice_head()

icon.data <- data.frame(
  fa = fontawesome(c('fa-user-circle', 'fa-heart', 'fa-retweet', 'fa-comment',
                     'fa-certificate', 'fa-check', 'fa-twitter')), 
  x = c(-0.85, -0.85, -0.40, 0.05, 0.35, 0.35, 0.9),
  y = c(0.75, -0.8, -0.8, -0.8, 0.8, 0.8, 0.85),
  size = c(14, 8, 8, 8, 6, 3, 8),
  color = c('grey60', 'red', '#1FA2F1', '#1FA2F1', '#1FA2F1', 'white', '#1FA2F1')
)

label.data <- data.frame(
  label = c(paste('Shared', data$verified ,'times by verified users'), 
            paste('@nd', data$regular, 'times by regular users'), 
            data$likes, data$retweets, data$quotes, 
            paste(pop.time$time, '·', pop.day$day)), 
  x = c(-0.75, -0.75, seq(-0.775, 0.125, 0.45), -0.925), 
  y = c(0.80, 0.67, rep(-0.825, 3), -0.6), 
  size = c(6, 5, rep(6, 3), 5), 
  color = c('grey10', rep('grey70', 5)), 
  bold = c(T, F, rep(T, 3), F)
)

main.tweet <- paste0("The average user of <span style='color:#1FA2F1'>#DuBoisChallenge</span> received **", 
                    data$mean.likes, ' likes** and **', data$mean.retweets, ' retweets**. ', 
                    'The average tweet consisted of **', data$mean.chars, ' characters**. <br><br>', 
                    "<span style='color:#1FA2F1'>@", most.popular$username, '</span> received **', 
                    most.popular$engagements, ' engagements** (likes + retweets), the highest number
                    of all participants. The most popular Twitter user that participated is ', 
                    "<span style='color:#1FA2F1'>@", most.followers$username, '</span>', 
                    ', who shared the hashtag with **', most.followers$followers, ' followers**.')

# plot the data  ---------------------------------------------------------------
font_add_google("Bebas Neue", 'bebas')
showtext_auto()


ggplot() +
  # tweet layout
  with_shadow(geom_shape(aes(x = c(-1, 1, 1, -1), y = c(-1,-1, 1,1)),
                         radius = unit(1.0, 'cm'), fill = 'white', color = 'grey50'),
            sigma = 8, colour = 'grey30') +
  geom_segment(aes(y = -0.7, yend = -0.7, x = -0.90, xend = 0.90), color = 'grey80') +
  # icon layout
  geom_text(data = icon.data, aes(x = x, y = y, label = fa), color = icon.data$color,
            family = 'fontawesome-webfont', size = icon.data$size) +
  # stats layout
  geom_text(data = label.data, aes(x = x, y = y, label = label, fontface = ifelse(bold, 'bold', 'plain')), 
            hjust = 0, size = label.data$size, color = label.data$color) +
  geom_textbox(aes(x = -0.95, y = 0.45), label = main.tweet, hjust = 0, vjust = 1,
               box.colour = 'white', text.colour = 'grey30', size = 5, 
               width = unit(16.5, 'cm')) +
  # other styling 
  geom_text( aes(x = 0.12, y = -1), label = '{', angle = 90, size = 22, color = 'grey20') +
  geom_text( aes(x = -0.15, y = -1.22), label = 'Total number of quote tweets. The other two show the total number of \n likes and retweets, respectively. The shown time (date) corresponds to \n the time (date) when most tweets were sent.', 
             size = 4, color = 'grey20', alpha = 0.6, hjust = 0, family = 'bebas') +
  labs(title = 'Reach of #DuBoisChallenge', caption = '@ThomIvar | source:  Anthony Starks, Allen Hillery & Sekou Tyler.') +
  xlim(-1.2, 1.2) + ylim(-1.25, 1.15) +
  theme_void() +
  theme(panel.background = element_rect(fill = '#1FA2F1', color = '#1FA2F1'),
        plot.background = element_rect(fill = '#1FA2F1', color = '#1FA2F1'),
        plot.margin = margin(0.5,1,0.5,1, 'cm'), 
        text = element_text('sans'), 
        plot.title = element_text('bebas', size = 40, color = 'white', face = 'bold'), 
        plot.caption = element_text('bebas', color = 'white'))

ggsave('tweet.png', width = 10, height = 6.5)





