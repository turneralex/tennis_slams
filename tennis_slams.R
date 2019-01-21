library(tidyverse)
library(lubridate)
library(rvest)

slams_url <- "https://en.wikipedia.org/wiki/List_of_Grand_Slam_men%27s_singles_champions"

slam_winners <- slams_url %>% 
    read_html() %>% 
    html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[1]') %>% 
    html_table() 

slam_winners <- slam_winners[[1]] 

slam_winners <- slam_winners %>% 
    as_tibble()

slam_winners

colnames(slam_winners) <- colnames(slam_winners) %>% 
    tolower() %>% 
    str_replace_all(" ", "_")

slam_winners <- slam_winners %>%
    mutate_if(is.character, ~ str_remove_all(., "[^[:alpha:] ]") %>% str_trim()) 

slam_winners %>% 
    select(-1) %>% 
    map(~ which(. == "Roger Federer"))

slam_winners_sub <- slam_winners %>% 
    slice(129:nrow(.))

slam_winners_sub

slam_winners_sub %>% 
    select(-1) %>% 
    unlist() %>% 
    unique()

slam_winners_sub <- slam_winners_sub %>%
    mutate(australian_open = if_else(australian_open == "SRB Novak Djokovic", "Novak Djokovic", australian_open)) 

all_winners <- slam_winners_sub %>% 
    select(-1) %>% 
    unlist() %>% 
    unique()

all_winners

big_four <- all_winners[c(1, 3, 4, 7)]

slam_winners_sub %>%
    select(-1) %>% 
    map(table) %>% 
    map(~ as_tibble(., .name_repair = make.names) %>% 
            arrange(desc(n)))

test <- slam_winners_sub %>% 
    gather(slam, player, -year) %>% 
    mutate(indicator = 1) %>% 
    spread(player, indicator, fill = 0) %>% 
    mutate_if(is.double, cumsum) %>% 
    gather(player, total, -year, -slam) %>% 
    arrange(year, slam) %>% 
    mutate(year = year %>% factor(),
           slam = slam %>% factor(labels = c("Australian Open", "French Open", "US Open", "Wimbledon")) %>% 
               fct_relevel("Australian Open", "French Open", "Wimbledon"))






