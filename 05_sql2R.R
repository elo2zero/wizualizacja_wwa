library(magrittr); library(dplyr); library(ggplot2); library(rgeos); library(oddsandsods); library(rgdal);library(ggmap)
setwd("~/Documents/eclipse_projects/wizualizacje/warszawa_podroze")
minuty <- data.frame(id = 1:1440, 
                     godzina_lab = seq(as.POSIXct('2016-04-21 00:00:00'),as.POSIXct('2016-04-21 23:59:00'), length.out=1440) %>% as.character,stringsAsFactors = F)

#
# częśc właściwa 
query <- readLines("05_getRouteAtMinute.sql")

for(i in seq(964,1212,by=3)){
  print(i)
  sqlConnect("mazowieckie_pgr") 
  trasy <- 
    query %>% 
    sprintf(minuty$godzina_lab[i]) %>% 
    paste0(collapse=" ") %>% pgisRead(query,tmp="vw_tmp_read_ogr")
  dbDisconnect(con);rm(con)
  
  if(is.null(trasy)) next
  points <- trasy@data %>% mutate(id = rownames(trasy@data)) %>% .[,c("id","x","y", "motyw")]
  routes <- fortify(trasy)
  
  routes %<>% left_join(points[,c("id", "motyw")])
  saveRDS(points,sprintf("dane/df3/%s_points.rds",i))
  saveRDS(routes,sprintf("dane/df3/%s_routes.rds",i))
 
  }
