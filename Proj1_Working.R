
library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(tidyr)


get_franchise_data<-function(ID=NULL){
  myurl<-"https://records.nhl.com/site/api/franchise"
  a<-GET (myurl)
  a<-content(a,"text")
  a<-fromJSON(a, flatten=TRUE)
  a<-as.data.frame(a)
  a<-tibble(a)
  if(is.null(ID)==FALSE){a<-filter(a,data.id==ID)}
  a
}

get_franchise_data()


get_franchise_totals<-function(ID=NULL){
  myurl<-"https://records.nhl.com/site/api/franchise-team-totals"
  a<-GET (myurl)
  a<-content(a,"text")
  a<-fromJSON(a, flatten=TRUE)
  a<-as.data.frame(a)
  a<-tibble(a)
  if(is.null(ID)==FALSE){a<-filter(a,data.franchiseId==ID)}
  a
}

get_franchise_records<-function(ID=NULL){
  if(is.null(ID)){stop("Need a valid franchise ID number")}
  a<-GET (paste0("https://records.nhl.com/site/api/franchise-season-records?cayenneExp=franchiseId=",as.character(ID)))
  a<-content(a,"text")
  a<-fromJSON(a, flatten=TRUE)
  a<-as.data.frame(a)
  a<-tibble(a)
  a
}

get_goalie_records<-function(ID=NULL){
  if(is.null(ID)){stop("Need a valid franchise ID number")}
  a<-GET (paste0("https://records.nhl.com/site/api/franchise-goalie-records?cayenneExp=franchiseId=",as.character(ID)))
  a<-content(a,"text")
  a<-fromJSON(a, flatten=TRUE)
  a<-as.data.frame(a)
  a<-tibble(a)
  a
}

get_skater_records<-function(ID=NULL){
  if(is.null(ID)){stop("Need a valid franchise ID number")}
  a<-GET (paste0("https://records.nhl.com/site/api/franchise-skater-records?cayenneExp=franchiseId=",as.character(ID)))
  a<-content(a,"text")
  a<-fromJSON(a, flatten=TRUE)
  a<-as.data.frame(a)
  a<-tibble(a)
  a
}

get_stats<-function(ID=NULL, mod=NULL){
  if (is.null(ID)){partial_url<-"https://statsapi.web.nhl.com/api/v1/teams"}
    else {partial_url<-paste0("https://statsapi.web.nhl.com/api/v1/teams/",ID)}
  a<-if(is.null(mod)){partial_url}
        else {paste0(partial_url,mod)}
  a<-GET (a)
  a<-content(a,"text")
  a<-fromJSON(a, flatten=TRUE)
  a<-as.data.frame(a)
  a<-tibble(a)
  a
}

get_stats(ID=54)

get_hockey_data<-function(endpoint=NULL,ID=NULL,mod=NULL){
  if(is.null(endpoint)){stop("Need valid endpoint")}
    else if (endpoint=="franchise_data"){get_franchise_data(ID)}
    else if (endpoint=="franchise_totals"){get_franchise_totals(ID)}
    else if (endpoint=="franchise_records"){get_franchise_records(ID)}
    else if (endpoint=="goalie_records"){get_goalie_records(ID)}
    else if (endpoint=="skater_records"){get_skater_records(ID)} 
    else if (endpoint=="team_stats"){get_stats(ID)} 
}


#Create a a new column that indicates whether team existed before World War II
a<-get_hockey_data(endpoint="team_stats")
a<-mutate(a,preWWII=(as.numeric(teams.firstYearOfPlay)<1939))

#Create a contingency table to show which conference (Eastern or Western) has
#more pre-WWII teams
table(a$preWWII,a$teams.conference.name)

#Set options so that R will print all rows of tibble
options(tibble.print_max = Inf)

#Get goalie data for Carolina Hurricanes franchise
b<-get_hockey_data(endpoint="goalie_records",ID=26)

#Create a column that indicates whether a goalie is a longTimer, meaning they 
#have played more games than the average goalie (within the Canes franchise)
b<-mutate(b,longTimer=(data.gamesPlayed>mean(b$data.gamesPlayed)))

#Create a table telling us if any of the active goalies are longTimers
table(b$data.activePlayer,b$longTimer)

#Get total games by franchise
c<-get_hockey_data(endpoint="franchise_totals")
c<-c%>%group_by(data.franchiseId)%>%summarise(totalGames=sum(data.gamesPlayed))
c
#Get franchise data
d<-get_hockey_data(endpoint="franchise_data")
d<-rename(d,data.franchiseId=data.id)

#Join by franchiseId so that totalGames column is included
e<-inner_join(c,d)

#Calculate franchise age and create scatter plot of franchise age 
#vs.total games played
es<-separate(e,data.firstSeasonId,into=c("startYear","firstSpring"),sep=4)
es<-mutate(es,FranchiseAge=2020-as.numeric(startYear))

g<-ggplot(es,aes(x=FranchiseAge,y=totalGames))

g+geom_point()+ylab("Total Games Played")+xlab("Franchise Age")+ 
  geom_smooth(method=lm,col="Green") + 
  labs(title="Total Games Played by Franchise Age")


#Create new variable indicating if the franchise is active
e<-mutate(e,active=is.na(data.lastSeasonId))
table(e$active)

#Create quantitative summary by active status
e %>% group_by(active) %>% summarise(avgGames=mean(totalGames),medGames=median(
  totalGames),grandTot=sum(totalGames))
e

#Calculate average goals for and against for the Hurricanes franchise, by team.
f<-get_hockey_data(endpoint="franchise_totals",ID=26)
f<- f %>% mutate(avgGoals=data.goalsFor/data.gamesPlayed,
                  avgGoalsAgainst=data.goalsAgainst/data.gamesPlayed)
h<-mutate(f, goalRatio=avgGoals/avgGoalsAgainst)
h %>% select(data.teamName,goalRatio,data.gameTypeId)
g<-ggplot(h,aes(data.teamName,goalRatio,fill=as.factor(data.gameTypeId)))
g+geom_bar(stat="identity",position="dodge")+scale_fill_discrete(name="Game Type",labels=c("Regular Season","Playoff"))+
  xlab("Team Name")+ylab("Ratio of Goals Scored to Goals Against")

#Create box plots comparing the Games played for Active and Inactive Teams
g<-ggplot(e,aes(x=active))
g+geom_boxplot(aes(y=totalGames))#need to add labels/title

