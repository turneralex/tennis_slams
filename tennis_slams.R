library(tidyverse)
library(rvest)

# wikipedia url
slams_url <- "https://en.wikipedia.org/wiki/List_of_Grand_Slam_men%27s_singles_champions"

# import data
slam_winners <- slams_url %>% 
    read_html() %>% 
    html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[1]') %>% 
    html_table(fill = T) %>% 
    magrittr::extract2(1) %>% # extract data frame from list     
    as_tibble() # convert to tibble for better print method

slam_winners

# change column names to snake case
colnames(slam_winners) <- colnames(slam_winners) %>% 
    tolower() %>% 
    str_replace_all(" ", "_")

# remove all characters that are non-alphabetic excluding whitespace
slam_winners <- slam_winners %>%
    mutate_if(is.character, ~ str_remove_all(., "[^[:alpha:] ]") %>% str_trim()) 

# check which row has roger federer's 1st grand slam win
slam_winners %>% 
    select(-year) %>% 
    map(~ str_which(., "Roger Federer")) %>% 
    flatten_int() %>% 
    min()

# create subset data frame for period of interest
slam_winners_sub <- slam_winners %>% 
    slice(128:(nrow(.) - 1)) # remove 2019 data also

slam_winners_sub

# check the list of winners from 2003 - 2018
slam_winners_sub %>% 
    select(-year) %>% 
    flatten_chr() %>% 
    unique()

# fix name errors
slam_winners_sub <- slam_winners_sub %>% 
    mutate_if(~ str_detect(., "SRB|SUI") %>% any(), ~ str_remove(., "SRB |SUI "))

# create vector all winners 2003 - 2018
all_winners <- slam_winners_sub %>% 
    select(-year) %>% 
    unlist() %>% 
    unique()

all_winners

# create vector for the 'big four'
big_four <- all_winners[c(2, 5, 4, 9)]

# create summary plots of totals for each player by slam
slam_winners_sub %>%
    select(-year) %>% 
    map(~ table(.) %>% 
            as_tibble(.name_repair = make.names) %>% 
            mutate(player = if_else(. %in% big_four, ., "Other Player")) %>% 
            group_by(player) %>% 
            summarise(total_slams = sum(n)) %>% 
            mutate(x = if_else(player %in% big_four, "Big Four", "Other Player")) %>% 
            ggplot(aes(reorder(player, -total_slams), total_slams, fill = x)) + 
            geom_col() +
            scale_fill_brewer(palette = "Set2") +
            labs(title = "Big 4 vs The Rest",
                 x = "Player",
                 y = "Total Wins") + 
            guides(fill = F)) 

# convert data to long / tidy format
slam_winners_sub_long <- slam_winners_sub %>% 
    gather(slam, player, -year) %>% 
    mutate(year = year %>% factor(),
           slam = slam %>% factor(labels = c("Australian Open", "French Open", "US Open", "Wimbledon")) %>% 
               fct_relevel("Australian Open", "French Open", "Wimbledon"),
           player = if_else(player %in% big_four, player, "Other Player") %>% 
               fct_relevel(big_four, "Other Player"))

# prepare cumulative slams by player data frame
cumulative_slams <- slam_winners_sub_long %>% 
    mutate(indicator = 1) %>% 
    spread(player, indicator, fill = 0) %>% 
    mutate_if(is.double, cumsum) %>% 
    gather(player, cumulative_total, -year, -slam) %>% 
    mutate(player = player %>% fct_relevel(big_four, "Other Player"))

# cumulative slams plot
cumulative_slams %>% 
    ggplot(aes(year, cumulative_total, colour = player, group = player)) +
    geom_line(size = 2, alpha = 0.8) +
    scale_colour_brewer(palette = "Set1") +
    labs(title = "Cumulative Total Grand Slams by Player",
         x = "Year",
         y = "Cumulative Total",
         colour = "Player")

# tile plot
slam_winners_sub_long %>% 
    mutate(player = if_else(player %in% big_four, "Big Four", "Other Player") %>% 
               factor()) %>% 
    ggplot(aes(year, fct_rev(slam), fill = player)) +
    geom_tile(colour = "black") +
    scale_fill_brewer(palette = "Set2") +
    labs(title = "Cumulative Total Grand Slams by Player",
         x = "Year",
         y = "Grand Slam",
         fill = "Winner")
