#load packages: XML to scrape, dplyr to 
require(XML)
require(dplyr)

#create table of team abbreviations

teams<-readHTMLTable("http://www.baseball-reference.com/leagues/MLB/2015.shtml", stringsAsFactors=FALSE)
teams<-teams[[1]]
teams<-select(teams, Tm)
teams<-filter(teams, Tm!="LgAvg")

#create function for scraping all records for all teams in 2015
scrape_results<-function(Tm) {
  url <- paste0("http://www.baseball-reference.com/teams/", Tm, "/2015-schedule-scores.shtml")
  data <- readHTMLTable(url, stringsAsFactors = FALSE)
  data <- data[[6]]
  data
}

#apply the scrape_results function to every team in 2015
results_2015<-teams %>% group_by(Tm) %>% do(scrape_results(.))

#tidy up the data.frame using dplyr
cols<-c(3:5, 9:10)
results_2015<-results_2015[,cols]
names(results_2015)<-c("Date", "box", "Team", "R", "RA")
results_2015<-filter(results_2015, box=="boxscore")

#conert R and RA columns to numeric

results_2015$R<-as.numeric(results_2015$R)
results_2015$RA<-as.numeric(results_2015$RA)

#calculate volatility scores for each team using Gini coefficients and the reldist package

require(reldist)

RGini<-aggregate(R ~ Team, data = results_2015, FUN = "gini")

RAGini<-aggregate(RA ~ Team, data = results_2015, FUN = "gini")

#scrape daily standings data from FanGraphs

fg<-readHTMLTable("http://www.fangraphs.com/depthcharts.aspx?position=BaseRuns", stringsAsFactors=FALSE)

#clean up the standings

standings<-fg[[15]]
standings<-standings[-(1:2), c(1:8, 11, 12, 15, 16)]

#rename columns
names(standings)<-c("Team", "G", "W", "L","W%", "RDif", "RS/G", "RA/G", "PythagenPat W%", "Wins Above/Below Pythag", "BaseRuns W%", "Wins Above/Below BaseRuns")

#create match table for BRef and FanGraphs team names
fg_t<-c("Diamondbacks", "Braves", "Orioles", "Red Sox", "Cubs", "White Sox", "Reds", "Indians", "Rockies", "Tigers", "Astros", "Royals", "Angels", "Dodgers", "Marlins", "Brewers", "Twins", "Mets", "Yankees", "Atheltics", "Phillies", "Pirates", "Padres", "Mariners", "Giants", "Cardinals", "Rays", "Rangers", "Blue Jays", "Nationals")

teams$FG_teams<-fg_t
colnames(teams)[1]<-"bref_t"

#join Volatility columns into a single data frame

VOL<-left_join(RGini, RAGini, by = "Team")
VOL$R<-round(VOL$R, 2)
VOL$RA<-round(VOL$RA, 2)
colnames(VOL)[1]<-"bref_t"

#join VOL scores to standings, via the teams table
standings<-left_join(standings, teams, by = c("Team" = "FG_teams"))
standings<-left_join(standings, VOL, by = "bref_t")