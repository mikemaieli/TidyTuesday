#load packages
library(tidyverse)
library(hrbrthemes)
library(janitor)
library(tidytext)
library(scales)
library(ggbeeswarm)


#load data
beyonce_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/beyonce_lyrics.csv')
taylor_swift_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/taylor_swift_lyrics.csv')
sales <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/sales.csv')
charts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/charts.csv')

#categorize beyonce song_id by album
dangerous <- c("4210083", "141844", "81345", "141846", "141847", "78135", "141848", "32796", "141849", "32869", "141850", "166809", "435484", "435491", "68426")
bday <- c("32193", "141831", "141833", "32950", "78581", "72523", "131948", "54892", "2441980", "141834")
iamsasha <- c("83613", "52158", "78577", "52683", "55885", "75936", "50873", "152213", "68973", "56878", "1829007")
four <- c("50396", "58509", "58776", "51492", "50828", "62840", "71526", "55067", "51865", "71524", "73506", "52814")
beyonce <- c("299187", "299320", "299177", "299368", "299378", "299325", "299338", "299253", "299098", "299317", "299370", "299152", "299388", "299326")
lemonade <- c("2457299", "2461219", "2461230", "2461226", "2461229", "2461236", "2461238", "2461245", "2461261", "2461233", "2461241", "2419257")

#rename columns in taylor swift lyircs
taylor_lyrics <- taylor_swift_lyrics %>% 
  select(song = Title, album = Album, artist = Artist, lyric = Lyrics)

#analyze and plot
beyonce_lyrics %>%
  mutate(album = case_when(
    song_id %in% dangerous ~ "Dangrously in Love",
    song_id %in% bday ~ "B'Day",
    song_id %in% iamsasha ~ "I Am... Sasha Fierce",
    song_id %in% four ~ "4",
    song_id %in% beyonce ~ "Beyoncé",
    song_id %in% lemonade ~ "Lemonade",
  )) %>% 
  select(song = song_name, album, artist = artist_name, lyric = line) %>%
  drop_na() %>%
  full_join(taylor_lyrics) %>% 
  unnest_tokens(word, lyric) %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(artist, album, song) %>% 
  summarise(score = sum(value)) %>%
  ggplot(aes(x = reorder(album, score),
             y = score,
             fill = artist)) +
  geom_violin(color = "#949494") +
  scale_fill_manual(values = c("#ffb441", "#b1a7ef")) +
  geom_beeswarm(size = 2,
                alpha = .5) +
  labs(title = "Sentiment of Beyoncé and Taylor Swift Songs by Album",
       subtitle = "Albums are in order of average sentiment, most positive to most negative. Each dot represents on song on the album.",
       caption = "note: sentiment coded using the AFINN sentiment lexicon
       data source: Rosie Baillie and Dr. Sara Stoudt
       analysis and visualization for #tidytuesday by @mikemaieli") +
  annotate("text", y = -122, x = 5, label = "The Taylor Swift song with\nthe most negative sentiment\nis 'Shake it Off', driven by\nlyrics like 'hate, hate, hate'.", family = "Arial Narrow", hjust = 0, lineheight = 1) +
  annotate("text", y = -110, x = 12.7, label = "Three of the four albums\nwith the most positive sentiment\nare Beyoncé albums.", family = "Arial Narrow", hjust = 0, lineheight = 1) +
  annotate("text", y = 115, x = 12.7, label = "Love on Top", family = "Arial Narrow", hjust = 0) +
  annotate("text", y = 95, x = 13.7, label = "Dangerously In Love 2", family = "Arial Narrow", hjust = 0) +
  annotate("text", y = 105, x = 10.7, label = "Hold Up", family = "Arial Narrow", hjust = 0) +
  annotate("text", y = 135, x = 3.7, label = "This Love", family = "Arial Narrow", hjust = 0) +
  annotate("text", y = -110, x = 7.7, label = "I Knew You Were Trouble", family = "Arial Narrow", hjust = 0) +
  annotate("text", y = -93, x = 2, label = "mad woman", family = "Arial Narrow", hjust = 0) +
  annotate("text", y = -100, x = 1, label = "Ring the Alarm", family = "Arial Narrow", hjust = 0) +
  annotate("text", y = 60, x = 8.7, label = "Sweet Dreams", family = "Arial Narrow", hjust = 0) +
  theme_ipsum() +
  theme(legend.position = "top",
        legend.justification = "left",
        legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_y_continuous(limits = c(-150, 150),
                     breaks = c(-130, 0, 130), labels = c("negative sentiment", "neutral", "positive sentiment"),
                     minor_breaks = NULL,
                     position = "right") +
  coord_flip()
