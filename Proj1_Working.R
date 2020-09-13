
library(httr)
library(jsonlite)
library(dplyr)


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
    else if (endpoint=="franchise_records"){get_franchise_records(ID)}
    else if (endpoint=="goalie_records"){get_goalie_records(ID)}
    else if (endpoint=="skater_records"){get_skater_records(ID)} 
    else if (endpoint=="team_stats"){get_stats(ID)} 
}

get_hockey_data(endpoint="team_stats",ID=3)

