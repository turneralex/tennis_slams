library(tidyverse)
library(gganimate)
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
    map(~ which(. == "Roger Federer")) %>% 
    unlist() %>% 
    min()

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

big_four <- all_winners[c(1, 3, 4, 7)] %>% 
    sort()

slam_winners_sub %>%
    select(-1) %>% 
    map(table) %>% 
    map(~ as_tibble(., .name_repair = make.names) %>% 
            arrange(desc(n))) 

slam_winners_sub_long <- slam_winners_sub %>% 
    gather(slam, player, -year) %>% 
    mutate(year = year %>% factor(),
           slam = slam %>% factor(labels = c("Australian Open", "French Open", "US Open", "Wimbledon")) %>% 
               fct_relevel("Australian Open", "French Open", "Wimbledon"),
           player = if_else(player %in% big_four, player, "Other Player") %>% 
               factor(labels = c(big_four[1:2], "Other Player", big_four[3:4])) %>% 
               fct_relevel(big_four, "Other Player"))
    
    
cumulative_slams <- slam_winners_sub %>% 
    gather(slam, player, -year) %>% 
    mutate(indicator = 1) %>% 
    spread(player, indicator, fill = 0) %>% 
    mutate_if(is.double, cumsum) %>% 
    mutate(other_player = select(., -year, -slam, -big_four) %>% 
               pmap_dbl(lift_vd(sum))) %>% 
    select(year, slam, big_four, other_player) %>% 
    gather(player, cumulative_total, -year, -slam) %>% 
    mutate(year = year %>% factor(),
           slam = slam %>% factor(labels = c("Australian Open", "French Open", "US Open", "Wimbledon")) %>% 
               fct_relevel("Australian Open", "French Open", "Wimbledon"),
           player = player %>% factor(labels = c(big_four[1:2], "Other Player", big_four[3:4])) %>% 
               fct_relevel(big_four, "Other Player"))

cumulative_slams %>% 
    ggplot(aes(year, cumulative_total, colour = player, group = player)) +
    geom_line(size = 2, alpha = 0.8) +
    transition_reveal(as.numeric(year))

slam_winners_sub_long %>% 
    mutate(player = if_else(player %in% big_four, "Big Four", "Other Player") %>% 
               factor()) %>% 
    ggplot(aes(year, slam, fill = player)) +
    geom_tile(colour = "grey")

slam_winners_sub_long %>% 
    mutate(indicator = 1) %>% 
    group_by(player, slam) %>% 
    summarise(total_slams = indicator %>% sum()) %>% 
    ggplot(aes(player, slam, fill = total_slams)) +
    geom_raster()
    