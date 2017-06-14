library(magrittr); library(dplyr); library(ggplot2); library(rgeos); library(oddsandsods); library(rgdal)
setwd("~/Documents/eclipse_projects/wizualizacje/warszawa_podroze")
load("01_possible_routes.RData") 
minuty <- data.frame(id = 1:1440,godzina_lab = seq(as.POSIXct('2016-04-21 00:00:00'),as.POSIXct('2016-04-21 23:59:00'), length.out=1440))

# IMPORT TRAS --------
sqlConnect("mazowieckie_pgr")
trasy <- pgisRead(customQuery("SELECT idtrasy,  FROM podroze_model_3 limit 5;", return_query=T),
                  stringsAsFactors=FALSE)

trasy <- pgisRead(customQuery("SELECT idtrasy, motyw,tduration/tlength progress, CASE 
                                WHEN tduration/tlength>0 THEN st_linesubstring(st_simplify(route_geom,0.001), 0,tduration/tlength) 
                                ELSE NULL
                                END geom       
                              FROM podroze_model_3 WHERE godzina_minuta = '%s' limit 5;",
                              gsub("CEST","",minuty$godzina_lab[1000]), return_query=T),
    stringsAsFactors=FALSE)

dbDisconnect(con)

set.seed(2202)
idx <- sample(1:nrow(trasy),5)

# SPRAWDZENIE ---------
library(magrittr); library(dplyr); library(ggplot2); library(rgeos); library(oddsandsods); library(rgdal)
setwd("~/Documents/eclipse_projects/wizualizacje/warszawa_podroze")
trasy_temp <- trasy[,]
data <- 
  trasy_temp@data %>% 
  left_join(ways) %>%
  left_join(rejony_centr %>% select(x1=x, y1=y, REJ_1000), by=c("source_REJ_1000"="REJ_1000")) %>%
  left_join(rejony_centr %>% select(x2=x, y2=y, REJ_1000), by=c("target_REJ_1000"="REJ_1000"))
trasy_temp@data <- data


bbox <- bbox(rejony)

library(ggmap)
qmap(bbox, color="bw") +
  geom_polygon(data=rejony, aes(x=long, y=lat, group=group),color = "black",fill=NA, size=.05) +
  #geom_rect(xmin=bbox[[1]], xmax=bbox[[3]],ymin=bbox[[2]], ymax=bbox[[4]],color="black",alpha=0.3) +
  geom_path(data=trasy_temp, aes(x=long, y=lat, group=group, colour=group)) +
  #    geom_path(data=coords,aes(x=long, y=lat, group=group), colour="yellow",size=2) +
  geom_point(data=trasy_temp@data,aes(x=x1,y=y1, group=0), colour="red")+
  geom_point(data=trasy_temp@data,aes(x=x2,y=y2, group=0), colour="blue")+
  theme_bw()
