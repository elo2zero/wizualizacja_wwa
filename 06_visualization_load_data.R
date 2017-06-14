# GET DATA ------
library(magrittr); library(dplyr); library(ggplot2); library(rgeos); library(oddsandsods); library(rgdal);library(ggmap); library(grid) 
setwd("~/Documents/eclipse_projects/wizualizacje/warszawa_podroze")
load("01_possible_routes.RData") 
bbox <- bbox(rejony)
minuty <- data.frame(id = 1:1440,
                     godzina_lab = seq(as.POSIXct('2016-04-21 00:00:00'),as.POSIXct('2016-04-21 23:59:00'), length.out=1440) %>% as.character,stringsAsFactors = F)
# mapa warszawy i okolic ----------
  sqlConnect("mazowieckie_osm")
    wwa <- pgisRead({"SELECT st_transform(way,4326) geom FROM planet_osm_polygon WHERE admin_level = '6' AND name = 'Warszawa';"})
    rivers <- pgisRead({"
                       SELECT ST_Intersection(ST_MakeEnvelope(20.66197, 51.98570, 21.48310, 52.47815, 4326), st_transform(way,4326)) geom 
                       FROM planet_osm_line
                       WHERE waterway = 'river' AND  name IN ('Wisła','Bug', 'Kanał Żerański', 'Jezioro Zegrzyńskie','Narew') and
                       ST_Intersects(ST_MakeEnvelope(20.66197, 51.98570, 21.48310, 52.47815, 4326), st_transform(way,4326))"})
    lakes <- pgisRead({"
                      SELECT  ST_Intersection(ST_MakeEnvelope(20.66197, 51.98570, 21.48310, 52.47815, 4326), st_transform(way,4326)) geom 
                      FROM planet_osm_polygon
                      WHERE water in ('lake') and
                      ST_Intersects(ST_MakeEnvelope(20.66197, 51.98570, 21.48310, 52.47815, 4326), st_transform(way,4326))"})
    motorways <-pgisRead({"
                         SELECT ST_Intersection(ST_MakeEnvelope(20.66197, 51.98570, 21.48310, 52.47815, 4326), st_transform(way,4326)) geom
                         FROM planet_osm_line
                         WHERE highway IN ('motorway') AND
                         ST_Intersects(ST_MakeEnvelope(20.66197, 51.98570, 21.48310, 52.47815, 4326), st_transform(way,4326));"})
    trunk <-pgisRead({"
                     SELECT ST_Intersection(ST_MakeEnvelope(20.66197, 51.98570, 21.48310, 52.47815, 4326), st_transform(way,4326)) geom
                     FROM planet_osm_line
                     WHERE highway IN ('trunk') AND
                     ST_Intersects(ST_MakeEnvelope(20.66197, 51.98570, 21.48310, 52.47815, 4326), st_transform(way,4326));"})
    primary <-pgisRead({"
                       SELECT ST_Intersection(ST_MakeEnvelope(20.66197, 51.98570, 21.48310, 52.47815, 4326), st_transform(way,4326)) geom
                       FROM planet_osm_line
                       WHERE highway IN ('primary') AND
                       ST_Intersects(ST_MakeEnvelope(20.66197, 51.98570, 21.48310, 52.47815, 4326), st_transform(way,4326));"})
    secondary <-pgisRead({"
                         SELECT ST_Intersection(ST_MakeEnvelope(20.66197, 51.98570, 21.48310, 52.47815, 4326), st_transform(way,4326)) geom
                         FROM planet_osm_line
                         WHERE highway IN ('secondary') AND
                         ST_Intersects(ST_MakeEnvelope(20.66197, 51.98570, 21.48310, 52.47815, 4326), st_transform(way,4326));"})
    streets <- pgisRead({"SELECT ST_Intersection(ST_MakeEnvelope(20.66197, 51.98570, 21.48310, 52.47815, 4326), st_transform(way,4326)) geom
                         FROM planet_osm_line
                         WHERE highway is not null and
                         ST_Intersects(ST_MakeEnvelope(20.66197, 51.98570, 21.48310, 52.47815, 4326), st_transform(way,4326));"})
    bds <-pgisRead({'
                   SELECT ST_Intersection(ST_MakeEnvelope(20.66197, 51.98570, 21.48310, 52.47815, 4326), st_transform(way,4326)) geom
                   FROM planet_osm_polygon
                   WHERE "addr:housenumber" is not null and
                   ST_Intersects(ST_MakeEnvelope(20.66197, 51.98570, 21.48310, 52.47815, 4326), st_transform(way,4326));'})
  dbDisconnect(con)
# podróże ----------
  sqlConnect("mazowieckie_pgr")
    podroze <- customQuery('SELECT idtrasy,"source_REJ_1000","target_REJ_1000",start,finish,motyw,st_x(st_endpoint(route_geom)) x,st_y(st_endpoint(route_geom)) y FROM podroze3_model_2 WHERE st_x(st_endpoint(route_geom)) IS NOT null;')
  dbDisconnect(con)
# rejony----------
  rejony_f <- 
    fortify(rejony) %>%
    left_join(rejony@data %>% mutate(id=rownames(rejony@data)))
  
# trasy ----------
  sqlConnect("mazowieckie_pgr")
    trasy <- pgisRead('SELECT * from trasy_merged 
                      where idtrasy in (select distinct idtrasy from podroze3_model);')
  dbDisconnect(con)

# 
  podroze_w_mintach <- list(NULL)
  for(i in 1:1440) {
    podroze_w_mintach[[i]] <- podroze %>% filter(start<=minuty$godzina[i] & finish>=minuty$godzina[i])
  }
 
# RASTERS ------
  kolory <- c("szkoła" = "#e22113","praca" = "#0089ca","dom" = "#b800ff","uczelnia"="#f8b020","inne"="#00a053")
  
  #
  ends <- 
    podroze %>%
    group_by(REJ_1000 = target_REJ_1000,finish,motyw,x,y) %>%
    summarize(n=n()) %>%
    ungroup %>%
    mutate(int = n/max(n)) %>%
    filter(!is.na(motyw))
  
  # proporcje 
  bbox <- bbox(wwa)
  rbbox <- apply(bbox,1,diff)
  bbox2 <- bbox+c(-rbbox[1]*.6,-rbbox[2]*.01,rbbox[1]*.6,rbbox[2]*.01)
  rbbox2 <- apply(bbox2,1,diff)
  
  
  real_ratio <- 1.04336
  map_ratio <- rbbox[2]/rbbox[1]
  box_ratio <- rbbox2[2]/rbbox2[1]
  
  #
  height <- 720
  width <- 720*(rbbox/rbbox2)[2]/(rbbox/rbbox2)[1]/real_ratio
  theme <-    
    theme(axis.line=element_blank(),
          axis.ticks=element_blank(),
          axis.ticks.length = unit(0,"null"),
          axis.ticks.margin = unit(0,"null"),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background= element_rect(fill = "grey30", colour=NA),
          plot.margin=unit(c(0,0,0,0), "null"),
          panel.margin=unit(c(0,0,0,0), "null"),
          legend.margin=unit(0, "null"),
          plot.background =element_rect(fill = "transparent",colour = NA)) 
  
  mapa <-
    ggplot(rivers,aes(x=long,y=lat, group=0))+
    geom_path(data=streets, aes(x=long, y=lat, group=group),color = "black",fill="grey05",alpha=1, size=.07) +
    geom_path(data=rejony, aes(x=long, y=lat, group=group),color = "black",fill=NA,alpha=1, size=.05) +
    geom_path(data=rivers, aes(x=long, y=lat, group=group),color = "#0073e6",fill="#0073e6",alpha=1, size=2) +
    geom_path(data=motorways, aes(x=long, y=lat, group=group),color = "black",fill="black",alpha=1, size=.2) +
    geom_path(data=trunk, aes(x=long, y=lat, group=group),color = "black",fill="black",alpha=1, size=.2) +
    geom_path(data=primary, aes(x=long, y=lat, group=group),color = "black",fill="black",alpha=1, size=.2) +
    geom_path(data=secondary, aes(x=long, y=lat, group=group),color = "black",fill="black",alpha=1, size=.1) +
    scale_y_continuous(expand = c(0,0)) + 
    scale_x_continuous(expand = c(0,0))+
    coord_cartesian(xlim = c(bbox2["x",]),ylim=c(bbox2["y",])) +
    # miejsce na trasy
    theme +
    labs(x=NULL, y=NULL)
  
  ggsave("output/remapa/background.png", mapa, dpi=100,height=height/100, width=width/100)
  
  
   
rm(rejony_centr,ways, ways_within)
save.image("06_vis_inputs.RData")
