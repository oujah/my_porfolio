## Following the 94th Academy Awards, what and how are the sentiments and underlying themes of the public as expressed on Twitter?


library(tidytext)
library(magrittr)
library(plotly)
library(tidyverse)
library(ggthemes)
library(rtweet) 
library(syuzhet)
library(see)
library(here)
library(tidytext)
library(tm)
library(wordcloud)
library(reshape2)
library(ggplot2)
library(see)
library(SnowballC)
library(wordcloud)



#### Import data

```{r}

osc <- read_csv(here::here("data", "oscars.csv"))



osc$text = gsub("@\\w+", "", osc$text)

osc$text = gsub('@\\S+', '', osc$text)

osc$text<- gsub("[[:digit:]]", "", osc$text) 

osc$text <- gsub("^[[:space:]]*","",osc$text) ## Remove leading whitespaces

osc$text <- gsub("[[:space:]]*$","",osc$text) ## Remove trailing whitespaces

osc$text <- gsub(' +',' ',osc$text) 

oscars <- 
  osc |> 
  select(text) |>
  mutate(text = text |>  str_to_lower())|>
  mutate(text = text |>  str_remove_all(pattern = '\\n')) |>
  mutate(text = text |>  str_remove_all(pattern = '&amp')) |>
  mutate(text = text |>  str_remove_all(pattern = 'https://t.co/[a-z,A-Z,0-9]*')) |>
  mutate(text = text |>  str_remove_all(pattern = 'https')) |>
  mutate(text = text |>  str_remove(pattern = '^(rt)')) |>
  mutate(text = text |>  str_remove_all(pattern = '\\_'))|>
  mutate(text = text |>  str_remove_all(pattern = '[:emoji:]'))


oscars$text <- gsub("[[:punct:]]", "", oscars$text)

oscars$text <- gsub("[^[:alnum:]]", " ", oscars$text)

oscars$text <- gsub(' +',' ',oscars$text) ## Remove extra whitespaces

oscars["DuplicateFlag"] = duplicated(oscars$text)
oscars = subset(oscars, oscars$DuplicateFlag=="FALSE")
oscars = subset(oscars, select = -c(DuplicateFlag))   


oscars <- 
  oscars|> 
  na_if("")|>
  na.omit()

oscars<- oscars[!duplicated(gsub("^(\\S+\\s+\\S+\\s+\\S+).*", "\\1", oscars$text)),]

##### Unnesting and Removing Stopwords

```{r}

sw <- c("rt","íí","get","didn","like","just","yes","know","will","good","day","people","doesn’t", "s","you’re","i’ll","he’s","doesn’t","dont","don't", "didnt", "wasnt" ,"yall","isnt", "doesnt", "hes", "wouldnt", "didn’t","it’s","wasn’t","wasn", "wouldn","zz","zzz","zzzz", "zzzzz", "zzzzzz", "zzzzzzz", "zzzzzzzz", "zzzzzzzzzz", "zyx", "wouldn’t", "abc","i’m", "youre", "im")

stopwords.df <- tibble(
  word = c(stopwords(kind = 'en'),  
           sw))

colnames(stopwords.df) <- "word"

words <- 
  oscars|>
  select(text)|>
  unnest_tokens(input = text, output = word) |>
  anti_join(y = stopwords.df, by = 'word')|>
  anti_join(stop_words) |>
  filter(!word %in% stop_words$word)

word_count <- words |>
  count(word, sort = TRUE) |>
  filter(n>1)

#### Plot of word count


plot <- word_count |>
  filter(word!="oscars")|>
  filter(word!="chris")|>
  filter(word!="rock")|>
  filter(word!="willsmith")|>
  filter(word!="oscar")|>
  filter(word!="jada")|>
  filter(word!="chrisrock")|>
  filter(word!="smith")|>
  filter(word!="oscars")|>
  filter(word!="academyawards")|>
  filter(word!="awards")|>
  filter(word!="academy")|>
  filter(word!="willandchris") |>
  top_n(30) |>
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = (n/(sum(n)))*100, fill = word)) +
  geom_bar(stat = 'identity') +
  ylab("Count (%)") +
  xlab(NULL)+
  labs(title = 'Fig. 1: Most common words in tweets') +
  coord_flip()+
  theme_pander() +
  theme(legend.position="none",
        axis.text.y = element_text(color = "grey20", 
                                   size = 20, 
                                   angle = 0, 
                                   hjust = 1, 
                                   vjust = 0, 
                                   face = "plain"),
        plot.title = element_text(size=22)) 


plot |> ggplotly()


set.seed(222)

wc <- word_count |>
  # Set count threshold. 
  filter(word!="oscars")|>
  filter(word!="chris")|>
  filter(word!="rock")|>
  filter(word!="willsmith")|>
  filter(word!="oscar")|>
  filter(word!="jada")|>
  filter(word!="chrisrock")|>
  filter(word!="smith")|>
  filter(word!="oscars")|>
  filter(word!="academyawards")|>
  filter(word!="academyaward")|>
  filter(word!="awards")|>
  filter(word!="academy")|>
  filter(word!="jadapinkettsmith")|>
  filter(word!="willandchris")|>
  filter(word!="ive")|>
  filter(word!="isn")|>
  filter(word!="aint")|>
  filter(word!="wont")|>
  filter(word!="ill")|>
  filter(word!="ain")|>
  filter(word!="ve")|>
  filter(word!="whats")|>
  filter(word!="id")

set.seed(123)
wordcloud(words = wc$word, 
          freq = wc$n,
          min.freq = 1500,
          random.order=FALSE,
          random.color = TRUE,
          rot.per=0.3, 
          scale= c(4,1), 
          colors = brewer.pal(7, 'Dark2'))




#### SENTIMENT ANALYSIS USING THE BING LEXICON


bing <- get_sentiments("bing")

bing.df <- oscars %>% 
  unnest_tokens(input = text, output = word) %>% 
  anti_join(y = stopwords.df, by = 'word')|>
  anti_join(stop_words) |>
  filter(!word %in% stop_words$word)|>
  filter(!word %in% stopwords.df$word)|>
  inner_join(bing)

checks <- bing.df |>
  mutate(word = SnowballC::wordStem(word))

class(checks$word)

colnames(checks) <- c( "word", "sentiment")

check <- checks |>
  inner_join(bing) |>
  select(sentiment) |>
  count(sentiment)|>
  mutate(sentiment = reorder(sentiment, n))|>
  ggplot(aes(x = sentiment, y = (n/sum(n))*100, fill = sentiment)) +
  coord_flip()+ 
  geom_bar(stat = 'identity')  +
  xlab("Words") +
  ylab("Count (%)") +
  theme(text = element_text(size = 20),
        axis.text.y = element_text(color = "grey20",
                                   size = 20, 
                                   angle = 0,
                                   hjust = 1,
                                   vjust = 0,
                                   face = "plain")) +
  ggtitle(label = 'Fig. 3: Overall sentiments of tweets using the Bing et al. Lexicon')

check


bingcounts <-oscars %>% 
  unnest_tokens(input = text, output = word) %>% 
  anti_join(y = stopwords.df, by = 'word')|>
  anti_join(stop_words) |>
  filter(!word %in% stop_words$word)|>
  filter(!word %in% stopwords.df$word)|>
  inner_join(bing)|>
  mutate(word = SnowballC::wordStem(word))|>
  count(word, sentiment, sort = TRUE) %>%
  ungroup()


bing_sent <- 
  bingcounts %>%
  group_by(sentiment) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, y = (n/sum(n))*100, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  coord_flip() +
  xlab("Words") +
  ylab("Count (%)") +
  ggtitle( "Fig. 4:Top 20 word contribution per sentiment")+
  theme(text = element_text(size = 20)) 


bing_sent


bing2 <- 
  bingcounts |>
  top_n(30) |>
  mutate(n =  (n/sum(n))*100)|>
  mutate(n = ifelse(sentiment=="negative", -n,n))|>
  ggplot(aes(fct_reorder(str_to_title(word),n),n, fill = str_to_title(sentiment))) +
  geom_col() +
  coord_flip() +
  scale_fill_brewer(type = "qual") +
  guides(fill = guide_legend(reverse = T)) +
  labs(title = "Fig. 5: Classification of top 30 words into positive and negative sentiments based on Bing et. al lexicon",
       y = "Compund sentiment score (%)",
       x = "",
       fill = "Sentiment") +
  theme(text = element_text(size = 20))

ggsave(here::here("bbing2.svg"), 
       bing2,
       height = 6,
       width = 6)

bing2
