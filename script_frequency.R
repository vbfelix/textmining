source("function_drob_scales.R")

# Packages ----------------------------------------------------------------

library(tidyverse)
library(tidytext)
library(tm)

# Import ------------------------------------------------------------------

df <- list.files(pattern = ".csv") %>% 
         read.csv(stringsAsFactors = F,encoding = "latin1")


df_sw <- data.frame(word = stopwords("portuguese"),stringsAsFactors = F) 


# Manipulate --------------------------------------------------------------

df %>%
  select(-c(retweetCount,created)) %>% 
  unnest_tokens(word,text) %>% 
  anti_join(df_sw) %>% 
  anti_join(data.frame(word = c("t.co","http","https")))-> df_word


# Top 10 ------------------------------------------------------------------

df_word %>% 
  count(word) %>% 
  filter(nchar(word)>2) %>% 
  top_n(10) %>% 
  mutate(word = fct_reorder(str_to_title(word),n)) %>% 
  ggplot(aes(y = n,word))+
  theme_bw(18)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  geom_col()+
  geom_text(aes(label = n, y = n*.88),
            col = "white",
            fontface = "bold",
            size = 5)+
  coord_flip()+
  scale_y_continuous(expand = c(0,0))+
  labs(y = "Frequência Absoluta",
       x = "Palavra")

# Top 5 by var -----------------------------------------------------------

df_word %>% 
  filter(nome %in% c("Aécio","Dilma")) %>% 
  count(word,nome) %>% 
  filter(nchar(word)>2) %>% 
  group_by(nome) %>% 
  top_n(7) %>% 
  mutate(word = fct_reorder(str_to_title(word),n)) %>% 
  ggplot(aes(y = n,reorder_within(word,n,nome)))+
  theme_bw(18)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  geom_col(aes(fill = nome),show.legend = F)+
  geom_text(aes(label = n, y = n*.82),
            col = "white",
            fontface = "bold",
            size = 5)+
  coord_flip()+
  scale_y_continuous(expand = c(0,0))+
  facet_wrap(.~nome,scales = "free")+
  scale_x_reordered()+
  scale_fill_brewer(palette = "Set1")+
  labs(y = "Frequência Absoluta",
       x = "Palavra")


# %var1 x %var2 ---------------------------------------------------------

df_word %>% 
  filter(nome %in% c("Aécio","Dilma")) %>% 
  count(word,nome) %>%
  group_by(nome) %>% 
  mutate(N = sum(n), p = 100*n/N) %>% 
  select(-c(n,N)) %>% 
  filter(p > .1) %>% 
  spread(nome,p) %>% 
  filter(is.na(Aécio)==F & is.na(Dilma)==F ) %>% 
  mutate(text = ifelse( (`Aécio`/Dilma > 3) | (Dilma/`Aécio` > 3) ,word,NA)) %>% 
  ggplot(aes(Aécio,Dilma))+
  theme_bw(18)+
  geom_point(alpha = .5)+
  geom_text(aes(label = text), vjust = -.5)+
  geom_abline(intercept = 0,slope = 1,col = "red",linetype = "dashed")+
  labs(x = "% Aécio",
       y = "% Dilma")+
  scale_x_continuous(breaks = seq(0,5,by = .5))+
  scale_y_continuous(breaks = seq(0,5,by = .25),
                     limits = c(0,2))

  

# n-gram -----------------------------------------------------------------

#2-gram
df %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2)  %>% 
  count(bigram) %>% 
  arrange(-n)

#2-gram
df %>% 
  unnest_tokens(trigram, text, token = "ngrams", n = 3)  %>% 
  count(trigram) %>% 
  arrange(-n)

