# 1. Klient zażyczył sobie także podróże spoza Warszawy (gminy ościenne). Stąd też 
#   wygenerowany w poprzednim kroku plik (ways). Nie jest wystarczający. 
# 2. Dla rejonów komunikacyjnych spoza warszawy wyszukiwania najbliższych punktów 
#   i kombinacja punkt_a -> punkt_b dla wszystkich kombinacji
library(magrittr); library(dplyr); library(ggplot2); library(rgeos); library(oddsandsods); library(rgdal)
setwd("~/Documents/eclipse_projects/wizualizacje/warszawa_podroze")
sqlConnect("mazowieckie_pgr")

# gminy
  rejony <- readOGR("mapy/rejonywarszawa.shp", "rejonywarszawa") 
  rejony_centr <- gCentroid(rejony,byid=TRUE) %>% attributes %>% .$coords %>% as.data.frame %>% select(x,y)
  rejony@data <- cbind(rejony@data, rejony_centr)
  rejony_centr <- rejony@data
#
  source("00_functions.R");

  
  # dopasowanie do PUNKTÓW NA GRAFIE
  for(i in 1:nrow(rejony_centr)){
    point <- psqlSelectWithinRej("source",rejony_centr$REJ_1000[i])
    if(nrow(point)>0){ 
      point %<>% sample_n(1) %>% .[[1]] 
    } else {
      point <- findNearestSource(rejony_centr$x[i],rejony_centr$y[i]) %>% .[[1]]
    }
    rejony_centr$source_node_id[i] <- point
    if(i %in% seq(1,900, by=50)) print(i)
  }
  
  for(i in 1:nrow(rejony_centr)){
    point <- psqlSelectWithinRej("target",rejony_centr$REJ_1000[i])
    if(nrow(point)>0){ 
      point %<>% sample_n(1) %>% .[[1]] 
    } else {
      point <- findNearestTarget(rejony_centr$x[i],rejony_centr$y[i]) %>% .[[1]]
    }
    rejony_centr$target_node_id[i] <- point
    if(i %in% seq(1,900, by=50)) print(i)
  }
  
  rejony_centr$source_node_id %<>% unlist
  rejony_centr$target_node_id %<>% unlist

  # test czy node są rzeczywiście w obszarach rejonów
  idx <- sample(1:nrow(rejony_centr),1)
  nodes <- 
    pgisRead(customQuery("SELECT * FROM ways_vertices_pgr where id in (%s,%s);", return_query=T,
                          rejony_centr$source_node_id[idx], rejony_centr$target_node_id[idx]),
                    stringsAsFactors=FALSE) %>% coordinates %>% as.data.frame
  
  ggplot(rejony[idx,], aes(x=long, y=lat))+
    geom_polygon(aes(group=group))+
    geom_point(data=nodes, aes(x=coords.x1, y=coords.x2), colour="red")
  
  # POSSIBLE ROUTES
  ways <-
    expand.grid(source_node_id =  rejony_centr$source_node_id, 
                target_node_id =  rejony_centr$target_node_id)
  ways %<>% left_join(rejony_centr %>% select(source_node_id, source_REJ_1000 = REJ_1000))
  ways %<>% left_join(rejony_centr %>% select(target_node_id, target_REJ_1000 = REJ_1000))
  ways %<>% filter(source_REJ_1000!=target_REJ_1000)
  ways %<>% mutate(idtrasy = 1:n())

  # dodanie możliwej podróży wewnątrz rejonu
  ways_within <- 
    rejony_centr %>% 
    select(source_node_id, target_node_id, source_REJ_1000=REJ_1000) %>% 
    filter(source_node_id!=target_node_id)
  ways_within$target_REJ_1000 <- ways_within$source_REJ_1000
  ways_within$idtrasy <- 1:nrow(ways_within)+max(ways$idtrasy)
  ways %<>% bind_rows(ways_within)
  #

    
  # BAZA rejonów z informacją o najbliższym vertice'm 
  sqlConnect(dbname="mazowieckie_pgr")
  dbWriteTable(con,"rejony_nodes",rejony_centr %>% select(rej=REJ_1000,x,y,source_node_id, target_node_id),row.names=F)
  dbWriteTable(con,"idtras",as.data.frame(ways), row.names=F)
  
  rm(list=setdiff(ls(),c("rejony","rejony_centr","ways","ways_within")))
  save.image("01_possible_routes.RData")
  