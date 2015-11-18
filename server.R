## global.R
library(Lahman)
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


## server.R
shinyServer(function(input, output) {
    

#### BATTING STATS ####  
  # Generate a plot
  output$plot <- renderChart({
    
    # Give column numbers to each batting varaible
    if (input$statistic == "R"){
      stat=8
    }else if (input$statistic == "H"){
      stat=9
    }else if (input$statistic == "X2B"){
      stat=10
    }else if (input$statistic == "X3B"){
      stat=11
    }else if (input$statistic == "HR"){
      stat=12
    }else if (input$statistic == "RBI"){
      stat=13
    }else if (input$statistic == "SB"){
      stat=14
    }else if (input$statistic == "CS"){
      stat=15
    }else if (input$statistic == "BB"){
      stat=16
    }else if (input$statistic == "SO"){
      stat=17
    }else if (input$statistic == "IBB"){
      stat=18
    }else if (input$statistic == "HBP"){
      stat=19
    }else if (input$statistic == "SH"){
      stat=20
    }else if (input$statistic == "SF"){
      stat=21
    }else if (input$statistic == "GIDP"){
      stat=22
    }else if (input$statistic == "BA"){
      stat=23
    }else if (input$statistic == "PA"){
      stat=24
    }else if (input$statistic == "TB"){
      stat=25
    }else if (input$statistic == "SlugPct"){
      stat=26
    }else if (input$statistic == "OBP"){
      stat=27
    }else if (input$statistic == "OPS"){
      stat=28
    }else if (input$statistic == "BABIP"){
      stat=29
    }
    
# Graphs
    
    if (input$statistic == "R") {   
       p1 <- hPlot(
         R ~ yearID, 
         data = top_batters, 
         group = "name",
         type = "line")
       p1$addParams(width = 700, height = 400, dom = 'plot')
       p1$xAxis(title = list(text = "Year"))
       p1$yAxis(list(list(title = list(text = 'Runs'))
                    , list(title = list(text = 'Home Runs'), opposite = TRUE)))  
       p1$title(text='Runs Comparison')
       visible = c("Albert Pujols", "Babe Ruth", "Ty Cobb", "Ted Williams", "Barry Bonds")
       p1$params$series = lapply(seq_along(p1$params$series), function(i){
         x = p1$params$series[[i]]
         x$visible = x$name %in% visible
         return(x)
         })
       p1
    }
    else if (input$statistic == "H") {
      p1 <- hPlot(
        H ~ yearID, 
        data = top_batters, 
        group = "name",
        type = "line")
      p1$addParams(width = 700, height = 400, dom = 'plot')
      p1$xAxis(title = list(text = "Year"))
      p1$yAxis(title = list(text = "Hits"))
      p1$title(text='Hits Comparison')
      visible = c("Albert Pujols", "Babe Ruth", "Ty Cobb", "Ted Williams", "Barry Bonds")
      p1$params$series = lapply(seq_along(p1$params$series), function(i){
        x = p1$params$series[[i]]
        x$visible = x$name %in% visible
        return(x)
      })
      p1
    }
    else if (input$statistic == "X2B") {
      p1 <- hPlot(
        X2B ~ yearID, 
        data = top_batters, 
        group = "name",
        type = "line")
      p1$addParams(width = 700, height = 400, dom = 'plot')
      p1$xAxis(title = list(text = "Year"))
      p1$yAxis(title = list(text = "Doubles"))
      p1$title(text='Doubles Comparison')
      visible = c("Albert Pujols", "Babe Ruth", "Ty Cobb", "Ted Williams", "Barry Bonds")
      p1$params$series = lapply(seq_along(p1$params$series), function(i){
        x = p1$params$series[[i]]
        x$visible = x$name %in% visible
        return(x)
      })
      p1
    }
    else if (input$statistic == "X3B") {
      p1 <- hPlot(
        X3B ~ yearID, 
        data = top_batters, 
        group = "name",
        type = "line")
      p1$addParams(width = 700, height = 400, dom = 'plot')
      p1$xAxis(title = list(text = "Year"))
      p1$yAxis(title = list(text = "Triples"))
      p1$title(text='Triples Comparison')
      visible = c("Albert Pujols", "Babe Ruth", "Ty Cobb", "Ted Williams", "Barry Bonds")
      p1$params$series = lapply(seq_along(p1$params$series), function(i){
        x = p1$params$series[[i]]
        x$visible = x$name %in% visible
        return(x)
      })
      p1
    }
    else if (input$statistic == "HR") {
      p1 <- hPlot(
        HR ~ yearID, 
        data = top_batters, 
        group = "name",
        type = "line")
      p1$addParams(width = 700, height = 400, dom = 'plot')
      p1$xAxis(title = list(text = "Year"))
      p1$yAxis(title = list(text = "Home Runs"))
      p1$title(text='Home Run Comparison')
      visible = c("Albert Pujols", "Babe Ruth", "Ty Cobb", "Ted Williams", "Barry Bonds")
      p1$params$series = lapply(seq_along(p1$params$series), function(i){
        x = p1$params$series[[i]]
        x$visible = x$name %in% visible
        return(x)
      })
      p1
    }
    else if (input$statistic == "RBI") {
      p1 <- hPlot(
        RBI ~ yearID, 
        data = top_batters, 
        group = "name",
        type = "line")
      p1$addParams(width = 700, height = 400, dom = 'plot')
      p1$xAxis(title = list(text = "Year"))
      p1$yAxis(title = list(text = "Runs Batted In"))
      p1$title(text='RBI Comparison')
      visible = c("Albert Pujols", "Babe Ruth", "Ty Cobb", "Ted Williams", "Barry Bonds")
      p1$params$series = lapply(seq_along(p1$params$series), function(i){
        x = p1$params$series[[i]]
        x$visible = x$name %in% visible
        return(x)
      })
      p1
    }
    else if (input$statistic == "SB") {
      p1 <- hPlot(
        SB ~ yearID, 
        data = top_batters, 
        group = "name",
        type = "line")
      p1$addParams(width = 700, height = 400, dom = 'plot')
      p1$xAxis(title = list(text = "Year"))
      p1$yAxis(title = list(text = "Stolen Bases"))
      p1$title(text='Stolen Bases Comparison')
      visible = c("Albert Pujols", "Babe Ruth", "Ty Cobb", "Ted Williams", "Barry Bonds")
      p1$params$series = lapply(seq_along(p1$params$series), function(i){
        x = p1$params$series[[i]]
        x$visible = x$name %in% visible
        return(x)
      })
      p1
    }
    else if (input$statistic == "CS") {
      p1 <- hPlot(
        CS ~ yearID, 
        data = top_batters, 
        group = "name",
        type = "line")
      p1$addParams(width = 700, height = 400, dom = 'plot')
      p1$xAxis(title = list(text = "Year"))
      p1$yAxis(title = list(text = "Caught Stealing"))
      p1$title(text='Caught Stealing Comparison')
      visible = c("Albert Pujols", "Babe Ruth", "Ty Cobb", "Ted Williams", "Barry Bonds")
      p1$params$series = lapply(seq_along(p1$params$series), function(i){
        x = p1$params$series[[i]]
        x$visible = x$name %in% visible
        return(x)
      })
      p1
    }
    else if (input$statistic == "BB") {
      p1 <- hPlot(
        BB ~ yearID, 
        data = top_batters, 
        group = "name",
        type = "line")
      p1$addParams(width = 700, height = 400, dom = 'plot')
      p1$xAxis(title = list(text = "Year"))
      p1$yAxis(title = list(text = "Walks"))
      p1$title(text='Walks Comparison')
      visible = c("Albert Pujols", "Babe Ruth", "Ty Cobb", "Ted Williams", "Barry Bonds")
      p1$params$series = lapply(seq_along(p1$params$series), function(i){
        x = p1$params$series[[i]]
        x$visible = x$name %in% visible
        return(x)
      })
      p1
    }
    else if (input$statistic == "SO") {
      p1 <- hPlot(
        SO ~ yearID, 
        data = top_batters, 
        group = "name",
        type = "line")
      p1$addParams(width = 700, height = 400, dom = 'plot')
      p1$xAxis(title = list(text = "Year"))
      p1$yAxis(title = list(text = "Strikeouts"))
      p1$title(text='Strikout Comparison')
      visible = c("Albert Pujols", "Babe Ruth", "Ty Cobb", "Ted Williams", "Barry Bonds")
      p1$params$series = lapply(seq_along(p1$params$series), function(i){
        x = p1$params$series[[i]]
        x$visible = x$name %in% visible
        return(x)
      })
      p1
    }
    else if (input$statistic == "IBB") {
      p1 <- hPlot(
        IBB ~ yearID, 
        data = top_batters, 
        group = "name",
        type = "line")
      p1$addParams(width = 700, height = 400, dom = 'plot')
      p1$xAxis(title = list(text = "Year"))
      p1$yAxis(title = list(text = "Intentional Walks"))
      p1$title(text='Intentional Walk Comparison')
      visible = c("Albert Pujols", "Babe Ruth", "Ty Cobb", "Ted Williams", "Barry Bonds")
      p1$params$series = lapply(seq_along(p1$params$series), function(i){
        x = p1$params$series[[i]]
        x$visible = x$name %in% visible
        return(x)
      })
      p1
    }
    else if (input$statistic == "HBP") {
      p1 <- hPlot(
        HBP ~ yearID, 
        data = top_batters, 
        group = "name",
        type = "line")
      p1$addParams(width = 700, height = 400, dom = 'plot')
      p1$xAxis(title = list(text = "Year"))
      p1$yAxis(title = list(text = "Hit By Pitch"))
      p1$title(text='Hit By Pitch Comparison')
      visible = c("Albert Pujols", "Babe Ruth", "Ty Cobb", "Ted Williams", "Barry Bonds")
      p1$params$series = lapply(seq_along(p1$params$series), function(i){
        x = p1$params$series[[i]]
        x$visible = x$name %in% visible
        return(x)
      })
      p1
    }
    else if (input$statistic == "SH") {
      p1 <- hPlot(
        SH ~ yearID, 
        data = top_batters, 
        group = "name",
        type = "line")
      p1$addParams(width = 700, height = 400, dom = 'plot')
      p1$xAxis(title = list(text = "Year"))
      p1$yAxis(title = list(text = "Sacrifice Hits"))
      p1$title(text='Sacrifice Hits Comparison')
      visible = c("Albert Pujols", "Babe Ruth", "Ty Cobb", "Ted Williams", "Barry Bonds")
      p1$params$series = lapply(seq_along(p1$params$series), function(i){
        x = p1$params$series[[i]]
        x$visible = x$name %in% visible
        return(x)
      })
      p1
    }
    else if (input$statistic == "SF") {
      p1 <- hPlot(
        SF ~ yearID, 
        data = top_batters, 
        group = "name",
        type = "line")
      p1$addParams(width = 700, height = 400, dom = 'plot')
      p1$xAxis(title = list(text = "Year"))
      p1$yAxis(title = list(text = "Sacrifice Flys"))
      p1$title(text='Sacrifice Flys Comparison')
      visible = c("Albert Pujols", "Babe Ruth", "Ty Cobb", "Ted Williams", "Barry Bonds")
      p1$params$series = lapply(seq_along(p1$params$series), function(i){
        x = p1$params$series[[i]]
        x$visible = x$name %in% visible
        return(x)
      })
      p1
    }
    else if (input$statistic == "GIDP") {
      p1 <- hPlot(
        GIDP ~ yearID, 
        data = top_batters, 
        group = "name",
        type = "line")
      p1$addParams(width = 700, height = 400, dom = 'plot')
      p1$xAxis(title = list(text = "Year"))
      p1$yAxis(title = list(text = "Grounders into Double Plays"))
      p1$title(text='Grounders Into Double Plays Comparison')
      visible = c("Albert Pujols", "Babe Ruth", "Ty Cobb", "Ted Williams", "Barry Bonds")
      p1$params$series = lapply(seq_along(p1$params$series), function(i){
        x = p1$params$series[[i]]
        x$visible = x$name %in% visible
        return(x)
      })
      p1
    }
    else if (input$statistic == "BA") {
      p1 <- hPlot(
        BA ~ yearID, 
        data = top_batters, 
        group = "name",
        type = "line")
      p1$addParams(width = 700, height = 400, dom = 'plot')
      p1$xAxis(title = list(text = "Year"))
      p1$yAxis(title = list(text = "Batting Average"))
      p1$title(text='Batting Average Comparison')
      visible = c("Albert Pujols", "Babe Ruth", "Ty Cobb", "Ted Williams", "Barry Bonds")
      p1$params$series = lapply(seq_along(p1$params$series), function(i){
        x = p1$params$series[[i]]
        x$visible = x$name %in% visible
        return(x)
      })
      p1
    }
    else if (input$statistic == "PA") {
      p1 <- hPlot(
        PA ~ yearID, 
        data = top_batters, 
        group = "name",
        type = "line")
      p1$addParams(width = 700, height = 400, dom = 'plot')
      p1$xAxis(title = list(text = "Year"))
      p1$yAxis(title = list(text = "Plate Appearances"))
      p1$title(text='Plate Appearances Comparison')
      visible = c("Albert Pujols", "Babe Ruth", "Ty Cobb", "Ted Williams", "Barry Bonds")
      p1$params$series = lapply(seq_along(p1$params$series), function(i){
        x = p1$params$series[[i]]
        x$visible = x$name %in% visible
        return(x)
      })
      p1
    }
    else if (input$statistic == "TB") {
      p1 <- hPlot(
        TB ~ yearID, 
        data = top_batters, 
        group = "name",
        type = "line")
      p1$addParams(width = 700, height = 400, dom = 'plot')
      p1$xAxis(title = list(text = "Year"))
      p1$yAxis(title = list(text = "Total Bases"))
      p1$title(text='Total Bases Comparison')
      visible = c("Albert Pujols", "Babe Ruth", "Ty Cobb", "Ted Williams", "Barry Bonds")
      p1$params$series = lapply(seq_along(p1$params$series), function(i){
        x = p1$params$series[[i]]
        x$visible = x$name %in% visible
        return(x)
      })
      p1
    }
    else if (input$statistic == "SlugPct") {
      p1 <- hPlot(
        SlugPct ~ yearID, 
        data = top_batters, 
        group = "name",
        type = "line")
      p1$addParams(width = 700, height = 400, dom = 'plot')
      p1$xAxis(title = list(text = "Year"))
      p1$yAxis(title = list(text = "Slugging Percentage"))
      p1$title(text='Slugging Percentage Comparison')
      visible = c("Albert Pujols", "Babe Ruth", "Ty Cobb", "Ted Williams", "Barry Bonds")
      p1$params$series = lapply(seq_along(p1$params$series), function(i){
        x = p1$params$series[[i]]
        x$visible = x$name %in% visible
        return(x)
      })
      p1
    }
    else if (input$statistic == "OBP") {
      p1 <- hPlot(
        OBP ~ yearID, 
        data = top_batters, 
        group = "name",
        type = "line")
      p1$addParams(width = 700, height = 400, dom = 'plot')
      p1$xAxis(title = list(text = "Year"))
      p1$yAxis(title = list(text = "On-Base Percentage"))
      p1$title(text='On-Base Percentage Comparison')
      visible = c("Albert Pujols", "Babe Ruth", "Ty Cobb", "Ted Williams", "Barry Bonds")
      p1$params$series = lapply(seq_along(p1$params$series), function(i){
        x = p1$params$series[[i]]
        x$visible = x$name %in% visible
        return(x)
      })
      p1
    }
    else if (input$statistic == "OPS") {
      p1 <- hPlot(
        OPS ~ yearID, 
        data = top_batters, 
        group = "name",
        type = "line")
      p1$addParams(width = 700, height = 400, dom = 'plot')
      p1$xAxis(title = list(text = "Year"))
      p1$yAxis(title = list(text = "On-Base + Slugging Percentage"))
      p1$title(text='On-Base + Slugging Percentage Comparison')
      visible = c("Albert Pujols", "Babe Ruth", "Ty Cobb", "Ted Williams", "Barry Bonds")
      p1$params$series = lapply(seq_along(p1$params$series), function(i){
        x = p1$params$series[[i]]
        x$visible = x$name %in% visible
        return(x)
      })
      p1
    }
    else if (input$statistic == "BABIP") {
      p1 <- hPlot(
        BABIP ~ yearID, 
        data = top_batters, 
        group = "name",
        type = "line")
      p1$addParams(width = 700, height = 400, dom = 'plot')
      p1$xAxis(title = list(text = "Year"))
      p1$yAxis(title = list(text = "Batting Avg. on Balls in Play"))
      p1$title(text='Batting Avg. on Balls in Play Comparison')
      visible = c("Albert Pujols", "Babe Ruth", "Ty Cobb", "Ted Williams", "Barry Bonds")
      p1$params$series = lapply(seq_along(p1$params$series), function(i){
        x = p1$params$series[[i]]
        x$visible = x$name %in% visible
        return(x)
      })
      p1
    }
  })
  
  
  #### PITCHING STATS ####  
  # Generate a plot
  output$plot2 <- renderChart({
    
    # Give column numbers to each batting varaible
    if (input$pitch_statistic == "W"){
      stat=6
    }else if (input$pitch_statistic == "L"){
      stat=7
    }else if (input$pitch_statistic == "G"){
      stat=8
    }else if (input$pitch_statistic == "GS"){
      stat=9
    }else if (input$pitch_statistic == "CG"){
      stat=10
    }else if (input$pitch_statistic == "SHO"){
      stat=11
    }else if (input$pitch_statistic == "IPouts"){
      stat=13
    }else if (input$pitch_statistic == "H"){
      stat=14
    }else if (input$pitch_statistic == "ER"){
      stat=15
    }else if (input$pitch_statistic == "HR"){
      stat=16
    }else if (input$pitch_statistic == "BB"){
      stat=17
    }else if (input$pitch_statistic == "SO"){
      stat=18
    }else if (input$pitch_statistic == "BAOpp"){
      stat=19
    }else if (input$pitch_statistic == "ERA"){
      stat=20
    }else if (input$pitch_statistic == "IBB"){
      stat=21
    }else if (input$pitch_statistic == "GF"){
      stat=26
    }else if (input$pitch_statistic == "R"){
      stat=27
    }
    
    # Graphs
    
    if (input$pitch_statistic == "W") {   
      p1 <- hPlot(
        W ~ yearID, 
        data = top_pitchers, 
        group = "name",
        type = "line")
      p1$addParams(width = 700, height = 400, dom = 'plot2')
      p1$xAxis(title = list(text = "Year"))
      p1$yAxis(title = list(text = "Wins"))
      p1$title(text='Wins Comparison')
      visible = c("Don Drysdale", "Bob Gibson")
      p1$params$series = lapply(seq_along(p1$params$series), function(i){
        x = p1$params$series[[i]]
        x$visible = x$name %in% visible
        return(x)
      })
      p1
    }
    else if (input$pitch_statistic == "L") {
      p1 <- hPlot(
        L ~ yearID, 
        data = top_pitchers, 
        group = "name",
        type = "line")
      p1$addParams(width = 700, height = 400, dom = 'plot2')
      p1$xAxis(title = list(text = "Year"))
      p1$yAxis(title = list(text = "Losses"))
      p1$title(text='Losses Comparison')
      visible = c("Don Drysdale", "Bob Gibson")
      p1$params$series = lapply(seq_along(p1$params$series), function(i){
        x = p1$params$series[[i]]
        x$visible = x$name %in% visible
        return(x)
      })
      p1
    }
    else if (input$pitch_statistic == "G") {
      p1 <- hPlot(
        G ~ yearID, 
        data = top_pitchers, 
        group = "name",
        type = "line")
      p1$addParams(width = 700, height = 400, dom = 'plot2')
      p1$xAxis(title = list(text = "Year"))
      p1$yAxis(title = list(text = "Games"))
      p1$title(text='Games Comparison')
      visible = c("Don Drysdale", "Bob Gibson")
      p1$params$series = lapply(seq_along(p1$params$series), function(i){
        x = p1$params$series[[i]]
        x$visible = x$name %in% visible
        return(x)
      })
      p1
    }
    else if (input$pitch_statistic == "GS") {
      p1 <- hPlot(
        GS ~ yearID, 
        data = top_pitchers, 
        group = "name",
        type = "line")
      p1$addParams(width = 700, height = 400, dom = 'plot2')
      p1$xAxis(title = list(text = "Year"))
      p1$yAxis(title = list(text = "Games Started"))
      p1$title(text='Games Started Comparison')
      visible = c("Don Drysdale", "Bob Gibson")
      p1$params$series = lapply(seq_along(p1$params$series), function(i){
        x = p1$params$series[[i]]
        x$visible = x$name %in% visible
        return(x)
      })
      p1
    }
    else if (input$pitch_statistic == "CG") {
      p1 <- hPlot(
        CG ~ yearID, 
        data = top_pitchers, 
        group = "name",
        type = "line")
      p1$addParams(width = 700, height = 400, dom = 'plot2')
      p1$xAxis(title = list(text = "Year"))
      p1$yAxis(title = list(text = "Complete Games"))
      p1$title(text='Complete Games Comparison')
      visible = c("Don Drysdale", "Bob Gibson")
      p1$params$series = lapply(seq_along(p1$params$series), function(i){
        x = p1$params$series[[i]]
        x$visible = x$name %in% visible
        return(x)
      })
      p1
    }
    else if (input$pitch_statistic == "SHO") {
      p1 <- hPlot(
        SHO ~ yearID, 
        data = top_pitchers, 
        group = "name",
        type = "line")
      p1$addParams(width = 700, height = 400, dom = 'plot2')
      p1$xAxis(title = list(text = "Year"))
      p1$yAxis(title = list(text = "Shutouts"))
      p1$title(text='Shutouts Comparison')
      visible = c("Don Drysdale", "Bob Gibson")
      p1$params$series = lapply(seq_along(p1$params$series), function(i){
        x = p1$params$series[[i]]
        x$visible = x$name %in% visible
        return(x)
      })
      p1
    }
    else if (input$pitch_statistic == "IPouts") {
      p1 <- hPlot(
        IPouts ~ yearID, 
        data = top_pitchers, 
        group = "name",
        type = "line")
      p1$addParams(width = 700, height = 400, dom = 'plot2')
      p1$xAxis(title = list(text = "Year"))
      p1$yAxis(title = list(text = "Outs Pitched"))
      p1$title(text='Outs Pitched Comparison')
      visible = c("Don Drysdale", "Bob Gibson")
      p1$params$series = lapply(seq_along(p1$params$series), function(i){
        x = p1$params$series[[i]]
        x$visible = x$name %in% visible
        return(x)
      })
      p1
    }
    else if (input$pitch_statistic == "H") {
      p1 <- hPlot(
        H ~ yearID, 
        data = top_pitchers, 
        group = "name",
        type = "line")
      p1$addParams(width = 700, height = 400, dom = 'plot2')
      p1$xAxis(title = list(text = "Year"))
      p1$yAxis(title = list(text = "Hits"))
      p1$title(text='Hits Comparison')
      visible = c("Don Drysdale", "Bob Gibson")
      p1$params$series = lapply(seq_along(p1$params$series), function(i){
        x = p1$params$series[[i]]
        x$visible = x$name %in% visible
        return(x)
      })
      p1
    }
    else if (input$pitch_statistic == "ER") {
      p1 <- hPlot(
        ER ~ yearID, 
        data = top_pitchers, 
        group = "name",
        type = "line")
      p1$addParams(width = 700, height = 400, dom = 'plot2')
      p1$xAxis(title = list(text = "Year"))
      p1$yAxis(title = list(text = "Earned Runs"))
      p1$title(text='Earned Runs Comparison')
      visible = c("Don Drysdale", "Bob Gibson")
      p1$params$series = lapply(seq_along(p1$params$series), function(i){
        x = p1$params$series[[i]]
        x$visible = x$name %in% visible
        return(x)
      })
      p1
    }
    else if (input$pitch_statistic == "HR") {
      p1 <- hPlot(
        HR ~ yearID, 
        data = top_pitchers, 
        group = "name",
        type = "line")
      p1$addParams(width = 700, height = 400, dom = 'plot2')
      p1$xAxis(title = list(text = "Year"))
      p1$yAxis(title = list(text = "Home Runs"))
      p1$title(text='Home Runs Comparison')
      visible = c("Don Drysdale", "Bob Gibson")
      p1$params$series = lapply(seq_along(p1$params$series), function(i){
        x = p1$params$series[[i]]
        x$visible = x$name %in% visible
        return(x)
      })
      p1
    }
    else if (input$pitch_statistic == "BB") {
      p1 <- hPlot(
        BB ~ yearID, 
        data = top_pitchers, 
        group = "name",
        type = "line")
      p1$addParams(width = 700, height = 400, dom = 'plot2')
      p1$xAxis(title = list(text = "Year"))
      p1$yAxis(title = list(text = "Walks"))
      p1$title(text='Walks Comparison')
      visible = c("Don Drysdale", "Bob Gibson")
      p1$params$series = lapply(seq_along(p1$params$series), function(i){
        x = p1$params$series[[i]]
        x$visible = x$name %in% visible
        return(x)
      })
      p1
    }
    else if (input$pitch_statistic == "SO") {
      p1 <- hPlot(
        SO ~ yearID, 
        data = top_pitchers, 
        group = "name",
        type = "line")
      p1$addParams(width = 700, height = 400, dom = 'plot2')
      p1$xAxis(title = list(text = "Year"))
      p1$yAxis(title = list(text = "Strikeouts"))
      p1$title(text='Strikeouts Comparison')
      visible = c("Don Drysdale", "Bob Gibson")
      p1$params$series = lapply(seq_along(p1$params$series), function(i){
        x = p1$params$series[[i]]
        x$visible = x$name %in% visible
        return(x)
      })
      p1
    }
    else if (input$pitch_statistic == "BAOpp") {
      p1 <- hPlot(
        BAOpp ~ yearID, 
        data = top_pitchers, 
        group = "name",
        type = "line")
      p1$addParams(width = 700, height = 400, dom = 'plot2')
      p1$xAxis(title = list(text = "Year"))
      p1$yAxis(title = list(text = "Opponents Batting Avg."))
      p1$title(text='Opponents Batting Avgerage Comparison')
      visible = c("Don Drysdale", "Bob Gibson")
      p1$params$series = lapply(seq_along(p1$params$series), function(i){
        x = p1$params$series[[i]]
        x$visible = x$name %in% visible
        return(x)
      })
      p1
    }
    else if (input$pitch_statistic == "ERA") {
      p1 <- hPlot(
        ERA ~ yearID, 
        data = top_pitchers, 
        group = "name",
        type = "line")
      p1$addParams(width = 700, height = 400, dom = 'plot2')
      p1$xAxis(title = list(text = "Year"))
      p1$yAxis(title = list(text = "Earned Run Average"))
      p1$title(text='Earned Run Average Comparison')
      visible = c("Don Drysdale", "Bob Gibson")
      p1$params$series = lapply(seq_along(p1$params$series), function(i){
        x = p1$params$series[[i]]
        x$visible = x$name %in% visible
        return(x)
      })
      p1
    }
    else if (input$pitch_statistic == "IBB") {
      p1 <- hPlot(
        IBB ~ yearID, 
        data = top_pitchers, 
        group = "name",
        type = "line")
      p1$addParams(width = 700, height = 400, dom = 'plot2')
      p1$xAxis(title = list(text = "Year"))
      p1$yAxis(title = list(text = "Intentional Walks"))
      p1$title(text='Intentional Walks Comparison')
      visible = c("Don Drysdale", "Bob Gibson")
      p1$params$series = lapply(seq_along(p1$params$series), function(i){
        x = p1$params$series[[i]]
        x$visible = x$name %in% visible
        return(x)
      })
      p1
    }
    else if (input$pitch_statistic == "GF") {
      p1 <- hPlot(
        GF ~ yearID, 
        data = top_pitchers, 
        group = "name",
        type = "line")
      p1$addParams(width = 700, height = 400, dom = 'plot2')
      p1$xAxis(title = list(text = "Year"))
      p1$yAxis(title = list(text = "Games Finished"))
      p1$title(text='Games Finished Comparison')
      visible = c("Don Drysdale", "Bob Gibson")
      p1$params$series = lapply(seq_along(p1$params$series), function(i){
        x = p1$params$series[[i]]
        x$visible = x$name %in% visible
        return(x)
      })
      p1
    }
    else if (input$pitch_statistic == "R") {
      p1 <- hPlot(
        R ~ yearID, 
        data = top_pitchers, 
        group = "name",
        type = "line")
      p1$addParams(width = 700, height = 400, dom = 'plot2')
      p1$xAxis(title = list(text = "Year"))
      p1$yAxis(title = list(text = "Runs"))
      p1$title(text='Runs Comparison')
      visible = c("Don Drysdale", "Bob Gibson")
      p1$params$series = lapply(seq_along(p1$params$series), function(i){
        x = p1$params$series[[i]]
        x$visible = x$name %in% visible
        return(x)
      })
      p1
    }

  })
 
  output$plot3 <- renderChart2({ 
  
    if (input$player_statistic == "pujolal01") {
      h <- Highcharts$new()
      players <- subset(top_batters, name=="Albert Pujols")
      players$yearID <- as.numeric(players$yearID)
      players <- players[order(players$yearID),] 
      h$xAxis(categories = players$yearID)
      h$yAxis(list(list(title = list(text = 'Runs'))
                   , list(title = list(text = 'Home Runs'), opposite = TRUE)
                   , list(title = list(text = 'Batting Avg.'), opposite = TRUE))
              
      )
      h$series(name = 'Runs', type = 'column', color = '#4572A7',
               data = players$R)
      h$series(name = 'Home Runs', type = 'spline', color = '#89A54E',
               data = players$HR,
               yAxis = 1)
      h$series(name = 'Batting Avg.', type = 'spline', color = '#AA4643', dashStyle = "ShortDash",
               data = players$BA,
               yAxis = 2)
      h$title(text='Albert Pujols')
      return(h)
    }
    else if (input$player_statistic == "ruthba01") {
      h <- Highcharts$new()
      players <- subset(top_batters, name=="Babe Ruth")
      players$yearID <- as.numeric(players$yearID)
      players <- players[order(players$yearID),] 
      h$xAxis(categories = players$yearID)
      h$yAxis(list(list(title = list(text = 'Runs'))
                   , list(title = list(text = 'Home Runs'), opposite = TRUE)
                   , list(title = list(text = 'Batting Avg.'), opposite = TRUE))
              
      )
      h$series(name = 'Runs', type = 'column', color = '#4572A7',
               data = players$R)
      h$series(name = 'Home Runs', type = 'spline', color = '#89A54E',
               data = players$HR,
               yAxis = 1)
      h$series(name = 'Batting Avg.', type = 'spline', color = '#AA4643', dashStyle = "ShortDash",
               data = players$BA,
               yAxis = 2)
      h$title(text='Babe Ruth')
      return(h)
    }
    else if (input$player_statistic == "bondsba01") {
      h <- Highcharts$new()
      players <- subset(top_batters, name=="Barry Bonds")
      players$yearID <- as.numeric(players$yearID)
      players <- players[order(players$yearID),] 
      h$xAxis(categories = players$yearID)
      h$yAxis(list(list(title = list(text = 'Runs'))
                   , list(title = list(text = 'Home Runs'), opposite = TRUE)
                   , list(title = list(text = 'Batting Avg.'), opposite = TRUE))
              
      )
      h$series(name = 'Runs', type = 'column', color = '#4572A7',
               data = players$R)
      h$series(name = 'Home Runs', type = 'spline', color = '#89A54E',
               data = players$HR,
               yAxis = 1)
      h$series(name = 'Batting Avg.', type = 'spline', color = '#AA4643', dashStyle = "ShortDash",
               data = players$BA,
               yAxis = 2)
      h$title(text='Barry Bonds')
      return(h)
    }
    else if (input$player_statistic == "gehrilo01") {
      h <- Highcharts$new()
      players <- subset(top_batters, name=="Lou Gehrig")
      players$yearID <- as.numeric(players$yearID)
      players <- players[order(players$yearID),] 
      h$xAxis(categories = players$yearID)
      h$yAxis(list(list(title = list(text = 'Runs'))
                   , list(title = list(text = 'Home Runs'), opposite = TRUE)
                   , list(title = list(text = 'Batting Avg.'), opposite = TRUE))
              
      )
      h$series(name = 'Runs', type = 'column', color = '#4572A7',
               data = players$R)
      h$series(name = 'Home Runs', type = 'spline', color = '#89A54E',
               data = players$HR,
               yAxis = 1)
      h$series(name = 'Batting Avg.', type = 'spline', color = '#AA4643', dashStyle = "ShortDash",
               data = players$BA,
               yAxis = 2)
      h$title(text='Lou Gehrig')
      return(h)
    }
    else if (input$player_statistic == "wagneho01") {
      h <- Highcharts$new()
      players <- subset(top_batters, name=="Honus Wagner")
      players$yearID <- as.numeric(players$yearID)
      players <- players[order(players$yearID),] 
      h$xAxis(categories = players$yearID)
      h$yAxis(list(list(title = list(text = 'Runs'))
                   , list(title = list(text = 'Home Runs'), opposite = TRUE)
                   , list(title = list(text = 'Batting Avg.'), opposite = TRUE))
              
      )
      h$series(name = 'Runs', type = 'column', color = '#4572A7',
               data = players$R)
      h$series(name = 'Home Runs', type = 'spline', color = '#89A54E',
               data = players$HR,
               yAxis = 1)
      h$series(name = 'Batting Avg.', type = 'spline', color = '#AA4643', dashStyle = "ShortDash",
               data = players$BA,
               yAxis = 2)
      h$title(text='Honus Wagner')
      return(h)
    }
    else if (input$player_statistic == "mantlmi01") {
      h <- Highcharts$new()
      players <- subset(top_batters, name=="Mickey Mantle")
      players$yearID <- as.numeric(players$yearID)
      players <- players[order(players$yearID),] 
      h$xAxis(categories = players$yearID)
      h$yAxis(list(list(title = list(text = 'Runs'))
                   , list(title = list(text = 'Home Runs'), opposite = TRUE)
                   , list(title = list(text = 'Batting Avg.'), opposite = TRUE))
              
      )
      h$series(name = 'Runs', type = 'column', color = '#4572A7',
               data = players$R)
      h$series(name = 'Home Runs', type = 'spline', color = '#89A54E',
               data = players$HR,
               yAxis = 1)
      h$series(name = 'Batting Avg.', type = 'spline', color = '#AA4643', dashStyle = "ShortDash",
               data = players$BA,
               yAxis = 2)
      h$title(text='Mickey Mantle')
      return(h)
    }
    else if (input$player_statistic == "cobbty01") {
      h <- Highcharts$new()
      players <- subset(top_batters, name=="Ty Cobb")
      players$yearID <- as.numeric(players$yearID)
      players <- players[order(players$yearID),] 
      h$xAxis(categories = players$yearID)
      h$yAxis(list(list(title = list(text = 'Runs'))
                   , list(title = list(text = 'Home Runs'), opposite = TRUE)
                   , list(title = list(text = 'Batting Avg.'), opposite = TRUE))
              
      )
      h$series(name = 'Runs', type = 'column', color = '#4572A7',
               data = players$R)
      h$series(name = 'Home Runs', type = 'spline', color = '#89A54E',
               data = players$HR,
               yAxis = 1)
      h$series(name = 'Batting Avg.', type = 'spline', color = '#AA4643', dashStyle = "ShortDash",
               data = players$BA,
               yAxis = 2)
      h$title(text='Ty Cobb')
      return(h)
    }
    else if (input$player_statistic == "willite01") {
      h <- Highcharts$new()
      players <- subset(top_batters, name=="Ted Williams")
      players$yearID <- as.numeric(players$yearID)
      players <- players[order(players$yearID),] 
      h$xAxis(categories = players$yearID)
      h$yAxis(list(list(title = list(text = 'Runs'))
                   , list(title = list(text = 'Home Runs'), opposite = TRUE)
                   , list(title = list(text = 'Batting Avg.'), opposite = TRUE))
              
      )
      h$series(name = 'Runs', type = 'column', color = '#4572A7',
               data = players$R)
      h$series(name = 'Home Runs', type = 'spline', color = '#89A54E',
               data = players$HR,
               yAxis = 1)
      h$series(name = 'Batting Avg.', type = 'spline', color = '#AA4643', dashStyle = "ShortDash",
               data = players$BA,
               yAxis = 2)
      h$title(text='Ted Williams')
      return(h)
    }
    else if (input$player_statistic == "dimagjo01") {
      h <- Highcharts$new()
      players <- subset(top_batters, name=="Joe DiMaggio")
      players$yearID <- as.numeric(players$yearID)
      players <- players[order(players$yearID),] 
      h$xAxis(categories = players$yearID)
      h$yAxis(list(list(title = list(text = 'Runs'))
                   , list(title = list(text = 'Home Runs'), opposite = TRUE)
                   , list(title = list(text = 'Batting Avg.'), opposite = TRUE))
              
      )
      h$series(name = 'Runs', type = 'column', color = '#4572A7',
               data = players$R)
      h$series(name = 'Home Runs', type = 'spline', color = '#89A54E',
               data = players$HR,
               yAxis = 1)
      h$series(name = 'Batting Avg.', type = 'spline', color = '#AA4643', dashStyle = "ShortDash",
               data = players$BA,
               yAxis = 2)
      h$title(text='Joe DiMaggio')
      return(h)
    }
    else if (input$player_statistic == "mayswi01") {
      h <- Highcharts$new()
      players <- subset(top_batters, name=="Willie Mays")
      players$yearID <- as.numeric(players$yearID)
      players <- players[order(players$yearID),] 
      h$xAxis(categories = players$yearID)
      h$yAxis(list(list(title = list(text = 'Runs'))
                   , list(title = list(text = 'Home Runs'), opposite = TRUE)
                   , list(title = list(text = 'Batting Avg.'), opposite = TRUE))
              
      )
      h$series(name = 'Runs', type = 'column', color = '#4572A7',
               data = players$R)
      h$series(name = 'Home Runs', type = 'spline', color = '#89A54E',
               data = players$HR,
               yAxis = 1)
      h$series(name = 'Batting Avg.', type = 'spline', color = '#AA4643', dashStyle = "ShortDash",
               data = players$BA,
               yAxis = 2)
      h$title(text='Willie Mays')
      return(h)
    }
})
  
  
  output$plot4 <- renderChart2({ 
    
    if (input$pitch_player_statistic == "johnswa01") {
      h <- Highcharts$new()
      players <- subset(top_pitchers, name=="Walter Johnson")
      players$yearID <- as.numeric(players$yearID)
      players <- players[order(players$yearID),] 
      h$xAxis(categories = players$yearID)
      h$yAxis(list(list(title = list(text = 'Wins'))
                   , list(title = list(text = 'Hits'), opposite = TRUE)
                   , list(title = list(text = 'Earned Run Average'), opposite = TRUE))
              
      )
      h$series(name = 'Wins', type = 'column', color = '#4572A7',
               data = players$W)
      h$series(name = 'Hits', type = 'spline', color = '#89A54E',
               data = players$H,
               yAxis = 1)
      h$series(name = 'Earned Run Average', type = 'spline', color = '#AA4643', dashStyle = "ShortDash",
               data = players$ERA,
               yAxis = 2)
      h$title(text='Walter Johnson')
      return(h)
    }
    else if (input$pitch_player_statistic == "grovele01") {
      h <- Highcharts$new()
      players <- subset(top_pitchers, name=="Lefty Grove")
      players$yearID <- as.numeric(players$yearID)
      players <- players[order(players$yearID),] 
      h$xAxis(categories = players$yearID)
      h$yAxis(list(list(title = list(text = 'Wins'))
                   , list(title = list(text = 'Hits'), opposite = TRUE)
                   , list(title = list(text = 'Earned Run Average'), opposite = TRUE))
              
      )
      h$series(name = 'Wins', type = 'column', color = '#4572A7',
               data = players$W)
      h$series(name = 'Hits', type = 'spline', color = '#89A54E',
               data = players$H,
               yAxis = 1)
      h$series(name = 'Earned Run Average', type = 'spline', color = '#AA4643', dashStyle = "ShortDash",
               data = players$ERA,
               yAxis = 2)
      h$title(text='Lefty Grove')
      return(h)
    }
    else if (input$pitch_player_statistic == "clemero02") {
      h <- Highcharts$new()
      players <- subset(top_pitchers, name=="Roger Clemens")
      players$yearID <- as.numeric(players$yearID)
      players <- players[order(players$yearID),] 
      h$xAxis(categories = players$yearID)
      h$yAxis(list(list(title = list(text = 'Wins'))
                   , list(title = list(text = 'Hits'), opposite = TRUE)
                   , list(title = list(text = 'Earned Run Average'), opposite = TRUE))
              
      )
      h$series(name = 'Wins', type = 'column', color = '#4572A7',
               data = players$W)
      h$series(name = 'Hits', type = 'spline', color = '#89A54E',
               data = players$H,
               yAxis = 1)
      h$series(name = 'Earned Run Average', type = 'spline', color = '#AA4643', dashStyle = "ShortDash",
               data = players$ERA,
               yAxis = 2)
      h$title(text='Roger Clemens')
      return(h)
    }
    else if (input$pitch_player_statistic == "gibsobo01") {
      h <- Highcharts$new()
      players <- subset(top_pitchers, name=="Bob Gibson")
      players$yearID <- as.numeric(players$yearID)
      players <- players[order(players$yearID),] 
      h$xAxis(categories = players$yearID)
      h$yAxis(list(list(title = list(text = 'Wins'))
                   , list(title = list(text = 'Hits'), opposite = TRUE)
                   , list(title = list(text = 'Earned Run Average'), opposite = TRUE))
              
      )
      h$series(name = 'Wins', type = 'column', color = '#4572A7',
               data = players$W)
      h$series(name = 'Hits', type = 'spline', color = '#89A54E',
               data = players$H,
               yAxis = 1)
      h$series(name = 'Earned Run Average', type = 'spline', color = '#AA4643', dashStyle = "ShortDash",
               data = players$ERA,
               yAxis = 2)
      h$title(text='Bob Gibson')
      return(h)
    }
    else if (input$pitch_player_statistic == "drysddo01") {
      h <- Highcharts$new()
      players <- subset(top_pitchers, name=="Don Drysdale")
      players$yearID <- as.numeric(players$yearID)
      players <- players[order(players$yearID),] 
      h$xAxis(categories = players$yearID)
      h$yAxis(list(list(title = list(text = 'Wins'))
                   , list(title = list(text = 'Hits'), opposite = TRUE)
                   , list(title = list(text = 'Earned Run Average'), opposite = TRUE))
              
      )
      h$series(name = 'Wins', type = 'column', color = '#4572A7',
               data = players$W)
      h$series(name = 'Hits', type = 'spline', color = '#89A54E',
               data = players$H,
               yAxis = 1)
      h$series(name = 'Earned Run Average', type = 'spline', color = '#AA4643', dashStyle = "ShortDash",
               data = players$ERA,
               yAxis = 2)
      h$title(text='Don Drysdale')
      return(h)
    }
    else if (input$pitch_player_statistic == "alexape01") {
      h <- Highcharts$new()
      players <- subset(top_pitchers, name=="Pete Alexander")
      players$yearID <- as.numeric(players$yearID)
      players <- players[order(players$yearID),] 
      h$xAxis(categories = players$yearID)
      h$yAxis(list(list(title = list(text = 'Wins'))
                   , list(title = list(text = 'Hits'), opposite = TRUE)
                   , list(title = list(text = 'Earned Run Average'), opposite = TRUE))
              
      )
      h$series(name = 'Wins', type = 'column', color = '#4572A7',
               data = players$W)
      h$series(name = 'Hits', type = 'spline', color = '#89A54E',
               data = players$H,
               yAxis = 1)
      h$series(name = 'Earned Run Average', type = 'spline', color = '#AA4643', dashStyle = "ShortDash",
               data = players$ERA,
               yAxis = 2)
      h$title(text='Pete (Grover Cleveland) Alexander')
      return(h)
    }
    else if (input$pitch_player_statistic == "mathech01") {
      h <- Highcharts$new()
      players <- subset(top_pitchers, name=="Christy Mathewson")
      players$yearID <- as.numeric(players$yearID)
      players <- players[order(players$yearID),] 
      h$xAxis(categories = players$yearID)
      h$yAxis(list(list(title = list(text = 'Wins'))
                   , list(title = list(text = 'Hits'), opposite = TRUE)
                   , list(title = list(text = 'Earned Run Average'), opposite = TRUE))
              
      )
      h$series(name = 'Wins', type = 'column', color = '#4572A7',
               data = players$W)
      h$series(name = 'Hits', type = 'spline', color = '#89A54E',
               data = players$H,
               yAxis = 1)
      h$series(name = 'Earned Run Average', type = 'spline', color = '#AA4643', dashStyle = "ShortDash",
               data = players$ERA,
               yAxis = 2)
      h$title(text='Christy Mathewson')
      return(h)
    }
    else if (input$pitch_player_statistic == "planked01") {
      h <- Highcharts$new()
      players <- subset(top_pitchers, name=="Eddie Plank")
      players$yearID <- as.numeric(players$yearID)
      players <- players[order(players$yearID),] 
      h$xAxis(categories = players$yearID)
      h$yAxis(list(list(title = list(text = 'Wins'))
                   , list(title = list(text = 'Hits'), opposite = TRUE)
                   , list(title = list(text = 'Earned Run Average'), opposite = TRUE))
              
      )
      h$series(name = 'Wins', type = 'column', color = '#4572A7',
               data = players$W)
      h$series(name = 'Hits', type = 'spline', color = '#89A54E',
               data = players$H,
               yAxis = 1)
      h$series(name = 'Earned Run Average', type = 'spline', color = '#AA4643', dashStyle = "ShortDash",
               data = players$ERA,
               yAxis = 2)
      h$title(text='Eddie Plank')
      return(h)
    }
    else if (input$pitch_player_statistic == "seaveto01") {
      h <- Highcharts$new()
      players <- subset(top_pitchers, name=="Tom Seaver")
      players$yearID <- as.numeric(players$yearID)
      players <- players[order(players$yearID),] 
      h$xAxis(categories = players$yearID)
      h$yAxis(list(list(title = list(text = 'Wins'))
                   , list(title = list(text = 'Hits'), opposite = TRUE)
                   , list(title = list(text = 'Earned Run Average'), opposite = TRUE))
              
      )
      h$series(name = 'Wins', type = 'column', color = '#4572A7',
               data = players$W)
      h$series(name = 'Hits', type = 'spline', color = '#89A54E',
               data = players$H,
               yAxis = 1)
      h$series(name = 'Earned Run Average', type = 'spline', color = '#AA4643', dashStyle = "ShortDash",
               data = players$ERA,
               yAxis = 2)
      h$title(text='Tom Seaver')
      return(h)
    }
    else if (input$pitch_player_statistic == "youngcy01") {
      h <- Highcharts$new()
      players <- subset(top_pitchers, name=="Cy Young")
      players$yearID <- as.numeric(players$yearID)
      players <- players[order(players$yearID),] 
      h$xAxis(categories = players$yearID)
      h$yAxis(list(list(title = list(text = 'Wins'))
                   , list(title = list(text = 'Hits'), opposite = TRUE)
                   , list(title = list(text = 'Earned Run Average'), opposite = TRUE))
              
      )
      h$series(name = 'Wins', type = 'column', color = '#4572A7',
               data = players$W)
      h$series(name = 'Hits', type = 'spline', color = '#89A54E',
               data = players$H,
               yAxis = 1)
      h$series(name = 'Earned Run Average', type = 'spline', color = '#AA4643', dashStyle = "ShortDash",
               data = players$ERA,
               yAxis = 2)
      h$title(text='Cy Young')
      return(h)
    }
  })
  
  # Generate a table summarizing each players stats
  output$batterTable <- renderDataTable({
    aggregate(top_batters[c("R", "H", "X2B", "HR", "RBI", "SB", "CS", "BB", "SO", "SF")], by=top_batters[c("name")], FUN=sum, na.rm=TRUE)
  })
  
  output$pitcherTable <- renderDataTable({
    aggregate(top_pitchers[c("W", "L", "G", "CG", "ER", "ERA", "SO", "BB", "BAOpp", "R")], by=top_pitchers[c("name")], FUN=sum, na.rm=TRUE)
  })
  
})