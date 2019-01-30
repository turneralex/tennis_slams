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
    flatten_dbl() %>% 
    min()

# create subset dataframe for period of interest
slam_winners_sub <- slam_winners %>% 
    slice(128:(nrow(.) - 1)) # remove 2019 data also

slam_winners_sub

# check the list of winners from 2003 - 2018
slam_winners_sub %>% 
    select(-1) %>% 
    unlist() %>% 
    unique()

# fix name errors
slam_winners_sub <- slam_winners_sub %>% 
    mutate_if(~ str_detect(., "SRB|SUI") %>% any(), ~ str_remove(., "SRB |SUI "))

# create vector all winners 2003 - 2018
all_winners <- slam_winners_sub %>% 
    select(-1) %>% 
    unlist() %>% 
    unique()

all_winners

# create vector for the 'big four'
big_four <- all_winners[c(2, 5, 4, 9)]

# create summaries of winner totals for each grand slam
slam_winners_sub %>%
    select(-1) %>% 
    map(table) %>% 
    map(~ as_tibble(., .name_repair = make.names) %>% 
            mutate(., X = if_else(X %in% big_four, X, "Other Player")) %>% 
            group_by(X) %>% 
            summarise(total_slams = sum(n)) %>% 
            arrange(desc(total_slams))) 


# convert data to long / tidy format
slam_winners_sub_long <- slam_winners_sub %>% 
    gather(slam, player, -year) %>% 
    mutate(year = year %>% factor(),
           slam = slam %>% factor(labels = c("Australian Open", "French Open", "US Open", "Wimbledon")) %>% 
               fct_relevel("Australian Open", "French Open", "Wimbledon"),
           player = if_else(player %in% big_four, player, "Other Player") %>% 
               fct_relevel(big_four, "Other Player"))

slam_winners_sub_long 

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
           player = player %>% factor(labels = c(big_four[4:3], "Other Player", big_four[2:1])) %>% 
               fct_relevel(big_four, "Other Player"))

cumulative_slams %>% 
    ggplot(aes(year, cumulative_total, colour = player, group = player)) +
    geom_line(size = 2, alpha = 0.8) 

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
    
# cumulative_slams %>% 
#     ggplot(aes(year, cumulative_total, colour = player, group = player)) +
#     geom_line(size = 2, alpha = 0.8) +
#     transition_reveal(as.numeric(year))
