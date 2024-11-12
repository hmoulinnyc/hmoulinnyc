#Quick thing: What are the 9-0 NFL teams with the worst point differentials in history? 


#install.packages("tidyverse")
#install.packages("nflverse")

library(tidyverse)
library(nflverse)

#standings <- read_csv("http://nflgamedata.com/standings.csv")
schedule <- nflreadr::load_schedules(2000:2024)

