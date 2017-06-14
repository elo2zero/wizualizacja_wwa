kolory <- c("szkoła" = "#e22113","praca" = "#0089ca","dom" = "#b800ff","uczelnia"="#f8b020","inne"="#00a053")
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
# proporcje 
bbox <- bbox(wwa)
rbbox <- apply(bbox,1,diff)
bbox2 <- bbox+c(-rbbox[1]*.6,-rbbox[2]*.01,rbbox[1]*.6,rbbox[2]*.01)
rbbox2 <- apply(bbox2,1,diff)


real_ratio <- 1.04336
map_ratio <- rbbox[2]/rbbox[1]
box_ratio <- rbbox2[2]/rbbox2[1]

#
height <- 480
width <- 480*(rbbox/rbbox2)[2]/(rbbox/rbbox2)[1]/real_ratio

img <- readPNG("output/remapa/background edtv.png", TRUE)
for(i in seq(7, 1440, by=3)) {
  # i<-480    
  points_file <- sprintf("dane/df3/%s_points.rds",i)
  routes_file <- sprintf("dane/df3/%s_routes.rds",i)
  points <- readRDS(points_file)
  routes <- readRDS(routes_file)
  
  points %<>% filter(!is.na(motyw))
  routes %<>% filter(!is.na(motyw))
  
  ends_0 <- podroze %>% filter(finish == minuty$godzina_lab[i]) %>% group_by(x,y,motyw) %>% summarize(n=n()*.1/72*20)
  ends_1 <- podroze %>% filter(finish == minuty$godzina_lab[i-1]) %>% group_by(x,y,motyw) %>% summarize(n=n()*.2/72*20)
  ends_2 <- podroze %>% filter(finish == minuty$godzina_lab[i-2]) %>% group_by(x,y,motyw) %>% summarize(n=n()*.4/72*20)
  ends_3 <- podroze %>% filter(finish == minuty$godzina_lab[i-3]) %>% group_by(x,y,motyw) %>% summarize(n=n()*.6/72*20)
  ends_4 <- podroze %>% filter(finish == minuty$godzina_lab[i-4]) %>% group_by(x,y,motyw) %>% summarize(n=n()*.8/72*20)
  ends_5 <- podroze %>% filter(finish == minuty$godzina_lab[i-5]) %>% group_by(x,y,motyw) %>% summarize(n=n()/72*20)
  
  gg <-
    ggplot(rivers,aes(x=long,y=lat, group=0))+
    annotation_custom(rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 
                      xmin=bbox2["x","min"],xmax=bbox2["x","max"],ymin=bbox2["y","min"],ymax =bbox2["y","max"]) +
    geom_point(data=ends_0,aes(x=x,y=y, color=motyw, size=n),alpha=0.2) +
    geom_point(data=ends_0,aes(x=x,y=y, size=n*.2),alpha=0.2, color="white") +
    geom_point(data=ends_1,aes(x=x,y=y, color=motyw, size=n),alpha=0.2) +
    geom_point(data=ends_1,aes(x=x,y=y, size=n*.2),alpha=0.2, color="white") +
    geom_point(data=ends_2,aes(x=x,y=y, color=motyw, size=n),alpha=0.2) +
    geom_point(data=ends_2,aes(x=x,y=y, size=n*.2),alpha=0.2, color="white") +
    geom_point(data=ends_3,aes(x=x,y=y, color=motyw, size=n),alpha=0.2) +
    geom_point(data=ends_3,aes(x=x,y=y, size=n*.2),alpha=0.2, color="white") +
    geom_point(data=ends_4,aes(x=x,y=y, color=motyw, size=n),alpha=0.2) +
    geom_point(data=ends_4,aes(x=x,y=y, size=n*.2),alpha=0.2, color="white") +
    geom_point(data=ends_5,aes(x=x,y=y, color=motyw, size=n),alpha=0.2) +
    geom_point(data=ends_5,aes(x=x,y=y, size=n*.2),alpha=0.2, color="white") +    
    #geom_polygon(data=ends_i, aes(x=x, y=y, group=REJ_1000,fill=motyw,alpha=int),color = NA, size=0.2) +
    geom_path(data=rejony, aes(x=long, y=lat, group=group),color = "black",fill=NA,alpha=1, size=.05) +
    geom_polygon(data=wwa, aes(x=long, y=lat, group=group),color = "red",fill=NA,alpha=.5, size=0.2) +
    #geom_polygon(data=lakes, aes(x=long, y=lat, group=group),color = "#0073e6",fill="#0073e6",alpha=1, size=0) +
    geom_path(data=rivers, aes(x=long, y=lat, group=group),color = "#0073e6",fill="#0073e6",alpha=.3, size=2) +
    geom_path(data=motorways, aes(x=long, y=lat, group=group),color = "black",fill="black",alpha=1, size=.2) +
    geom_path(data=trunk, aes(x=long, y=lat, group=group),color = "black",fill="black",alpha=1, size=.2) +
    geom_path(data=primary, aes(x=long, y=lat, group=group),color = "black",fill="black",alpha=1, size=.2) +
    geom_path(data=secondary, aes(x=long, y=lat, group=group),color = "black",fill="black",alpha=1, size=.1) +
    geom_path(data=routes, aes(x=long, y=lat, group=group, colour=motyw), alpha=.2,size=.35) +
    geom_point(data=points, aes(x=x, y=y), colour="white",alpha=1,size=.2) +
    scale_color_manual(values = kolory) +
    scale_size(range = range(c(ends_0$n,ends_1$n,ends_2$n,ends_3$n,ends_4$n,ends_5$n))) +
    scale_fill_manual(values = kolory) +
    scale_y_continuous(expand = c(0,0)) + 
    scale_x_continuous(expand = c(0,0))+
    coord_cartesian(xlim = c(bbox2["x",]),ylim=c(bbox2["y",])) +
    theme +
    labs(x=NULL, y=NULL)
  ggsave(sprintf("output/bigmaps edtv/%s_plot.png"  ,i), gg, dpi=100,height=height/100, width=width/100)
  
}
