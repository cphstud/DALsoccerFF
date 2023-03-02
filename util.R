library(mongolite)
library(jsonlite)
library(tidyverse)
library(ggplot2)
library(ggsoccer)

# hente database fra mongo ned i R
con <- mongo(
  url = "mongodb://127.0.0.1:27017/?directConnection=true&serverSelectionTimeoutMS=2000&appName=mongosh+1.6.2",
  db="statsbomb",
  collection = "events"
)

get_shots <- function(m_id) {
  df <- con$find(query = paste0('{"type.name" : "Shot" ,"matchid": ', m_id, '}'))
  return(df)
}

clean_shots<-function(df){
  shots <- unnest_wider(df, type, names_sep = "_")
  shots <- unnest_wider(shots, possession_team, names_sep = "_")
  shots <- unnest_wider(shots,team, names_sep = "_")
  shots <- unnest_wider(shots, shot, names_sep = "_")
  shots <- unnest_wider(shots, player, names_sep = "_")
  shots <- unnest_wider(shots, location, names_sep = "")
  shots <- unnest_wider(shots, shot_end_location, names_sep = "")
  shots <- unnest_wider(shots, shot_end_location1, names_sep = "")
  
  ts<-shots[,c(15,17,19,20,24,25)]
  names<-c("team","player","x","y","x2","y2")
  colnames(ts)=names
  #ts2<-ts[,c(1,3:6)]
  #colnames(ts2) <- c('team','x','y','x2','y2')}
  return(ts)
}
