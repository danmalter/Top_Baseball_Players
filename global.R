## global.R
library(Lahman)
library(plyr)
library(dplyr)
library(shiny)
library(shinydashboard)
library(rCharts)

Master$name <- paste(Master$nameFirst, Master$nameLast, sep=' ')

bstats <- battingStats(data = Lahman::Batting, 
                       idvars = c("playerID", "yearID", "stint", "teamID", "lgID"), 
                       cbind = TRUE)

batting <- merge(bstats,
                 Master[,c("playerID","name")],
                 by="playerID", all.x=TRUE)

pitching <- merge(Pitching, Master, all.x=TRUE)

top_batters <- subset(batting, playerID=="pujolal01" | playerID=="ruthba01" | playerID=="bondsba01"
                      | playerID=="gehrilo01" | playerID=="wagneho01" | playerID=="mantlmi01"
                      | playerID=="mayswi01" | playerID=="cobbty01" | playerID=="willite01" | playerID=="dimagjo01"
                      | playerID=="willite01")

top_pitchers <- subset(pitching, playerID=="johnswa01" | playerID=="grovele01" | playerID=="clemero02" 
                      | playerID=="gibsobo01" | playerID=="drysddo01" | playerID=="alexape01" | playerID=="mathech01"
                      | playerID=="planked01" | playerID=="seaveto01" | playerID=="youngcy01")

player_table <- aggregate(top_batters[c("SO", "HR", "H", "R", "X2B", "X3B", "BB", "SB")], by=top_batters[c("name","playerID")], FUN=sum)

#list of top ten  hitters

top_10 <- 
  subset(top_batters, playerID=="pujolal01" | playerID=="ruthba01" | playerID=="bondsba01"
         | playerID=="gehrilo01" | playerID=="wagneho01" | playerID=="mantlmi01"
         | playerID=="mayswi01" | playerID=="cobbty01" | playerID=="willite01" | playerID=="dimagjo01"
         | playerID=="willite01") 
#%>%
#  group_by(playerID, name) %>%
#  summarize(HR = sum(HR)) %>%
#  arrange(desc(HR))

players_in_menu <- top_10$playerID
#player_appearances = count(player_data, .(name))
#counts <- aggregate(player_data, by=list(player_data$name, player_data$HR), FUN=length)



