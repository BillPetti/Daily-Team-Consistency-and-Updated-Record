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
#clean up some attributes left over from scraping the data from B-REF
attr(results_2015, "vars")<-NULL
results_2015<-filter(results_2015, box=="boxscore")

#convert R and RA columns to numeric

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
standings<-standings %>% arrange(desc(`W%`))

#rename columns

names(standings)<-c("Team", "G", "W", "L","W%", "RDif", "RS/G", "RA/G", "PythagenPat W%", "Wins Above/Below Pythag", "BaseRuns W%", "Wins Above/Below BaseRuns")

#transform columns to numeric

attach(standings)
standings$L %>% as.numeric()
standings$`W%` <- standings$`W%` %>% as.numeric()
standings$RDif <- RDif %>% as.numeric()
standings$`RS/G` <- `RS/G` %>% as.numeric()
standings$`RA/G` <- `RA/G` %>% as.numeric()
standings$`PythagenPat W%`<- `PythagenPat W%` %>% as.numeric()
standings$`Wins Above/Below Pythag` <- `Wins Above/Below Pythag` %>% as.numeric()
standings$`BaseRuns W%` <- `BaseRuns W%` %>% as.numeric()
standings$`Wins Above/Below BaseRuns` <- `Wins Above/Below BaseRuns` %>% as.numeric()
detach(standings)

#create match table for BRef and FanGraphs team names

fg_t<-c("Diamondbacks", "Braves", "Orioles", "Red Sox", "Cubs", "White Sox", "Reds", "Indians", "Rockies", "Tigers", "Astros", "Royals", "Angels", "Dodgers", "Marlins", "Brewers", "Twins", "Mets", "Yankees", "Athletics", "Phillies", "Pirates", "Padres", "Mariners", "Giants", "Cardinals", "Rays", "Rangers", "Blue Jays", "Nationals")

teams$FG_teams<-fg_t
colnames(teams)[1]<-"bref_t"

#join Volatility columns into a single data frame

VOL<-left_join(RGini, RAGini, by = "Team")
VOL$R<-round(VOL$R, 2)
VOL$RA<-round(VOL$RA, 2)
colnames(VOL)[1]<-"bref_t"

#calculate percentiles for each team for both R and RA.

VOL<-VOL %>% mutate(percrank = rank(R)/length(R))
colnames(VOL)[4]<-"R_Ptile"
VOL<-VOL %>% mutate(percrank = rank(RA)/length(RA))
colnames(VOL)[5]<-"RA_Ptile"

#round and multiple each percentile by 100 

VOL$R_Ptile<-round(VOL$R_Ptile, 2)*100
VOL$RA_Ptile<-round(VOL$RA_Ptile, 2)*100

#join VOL scores to standings, via the teams table

standings<-left_join(standings, teams, by = c("Team" = "FG_teams"))
standings<-left_join(standings, VOL, by = "bref_t")

#normalize VOL scores

standings$`R_VOL+`<-round((standings$R/mean(standings$R)),2)*100
standings$`RA_VOL+`<-round((standings$RA/mean(standings$RA)),2)*100

#create dummy variables to categorize teams in terms of whether they are optimized for each of the volatility metrics

standings$RA_75<-ifelse(standings$RA_Ptile >= 75, 1, 0)
standings$R_25<-ifelse(standings$R_Ptile <= 25, 1, 0)
standings$Optimal<-ifelse(standings$R_Ptile <= 25 & standings$RA_Ptile >= 75, 1, 0)

#view averege wins above/below pythag expectation based on optimization

aggregate(`Wins Above/Below Pythag`~RA_75 + R_25, standings, mean)

#export standings file as csv for ploting, etc. 

standings %>% write.csv(file="standings.csv")

#plot R vs. RA volatility (percentiles)

require(ggplot2)

g<-ggplot(standings, aes(x=R_Ptile, y=RA_Ptile, label=bref_t))
g<-g + geom_text(aes(colour=factor(`Wins Above/Below Pythag`))) 
g<- g + geom_hline(yintercept=75) + geom_vline(xintercept=25)
g<- g + xlab("Runs Scored Volatiltiy (Percentile--lower is better)") + ylab("Runs Allowed Volatility (Percentile--higher is better)")
g
