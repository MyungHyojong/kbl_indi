library(dplyr)
library(readr)
files <- list.files(path = "data/", full.names = TRUE)

players <- read_csv(files[1], col_types = cols())
players$playoff <- 0
for(file in files[2:length(files)]){
  tmp_players <- read_csv(file, col_types = cols())
  if(grepl('playoff',file)){
    tmp_players$playoff <- 1
  } else{
    tmp_players$playoff <- 0
  }
  players <- rbind(players, tmp_players)
  
}

names(players) <- gsub('%','percentage', names(players))

players <- players %>%
  mutate(
    `2PTM` = sapply(strsplit(as.character(`2PT M/A`), '/'), `[`, 1),
    `2PTA` = sapply(strsplit(as.character(`2PT M/A`), '/'), `[`, 2),
    `3PTM` = sapply(strsplit(as.character(`3PT M/A`), '/'), `[`, 1),
    `3PTA` = sapply(strsplit(as.character(`3PT M/A`), '/'), `[`, 2),
    FGM = sapply(strsplit(as.character(`FG M/A`), '/'), `[`, 1),
    FGA = sapply(strsplit(as.character(`FG M/A`), '/'), `[`, 2),
    FTM = sapply(strsplit(as.character(`FT M/A`), '/'), `[`, 1),
    FTA = sapply(strsplit(as.character(`FT M/A`), '/'), `[`, 2),
    time_min = sapply(strsplit(as.character(MIN), ':'), `[`, 1)
  ) %>%
  mutate(across(c(6, 8, 10, 12, 14:15, 26:34), as.numeric)) %>% 
  mutate(Date = as.Date(gsub(" \\(.*\\)", "", Date), format = "%Y.%m.%d")) %>%
  mutate(foreign = ifelse(nchar(Name)<=3, 0, 1)) %>% 
  mutate(foreign = ifelse(Name == '라건아', 1, foreign))


names(players)
str(players)
write.csv(players, 'main_data_fixed.csv')
