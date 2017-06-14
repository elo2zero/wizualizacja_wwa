  library(magrittr); library(dplyr);require(extrafont); library(ggplot2); library(rgeos); library(oddsandsods); library(rgdal);library(ggmap);library(grid)
  setwd("~/Documents/eclipse_projects/wizualizacje/warszawa_podroze")
  load("06_vis_inputs.RData")
  kolory <- c("szkoła" = "#e22113","praca" = "#0089ca","dom" = "#b800ff","uczelnia"="#f8b020","inne"="#00a053")
  
  zliczwminutach <- list(NULL)
  for(i in 1:1440) {
    temp <- podroze %>% filter(start<=minuty$godzina[i] & finish>=minuty$godzina[i])
    zliczwminutach[[i]] <- table(temp$motyw) 
  }
  minuty$godzina <- strptime(minuty$godzina_lab,"%Y-%m-%d %H:%M:%S")

#### PANEL BOCZNY + PROGRESSBAR #####
# proporcje
  height = 1080
  width = 588
  panel_left <- data.frame(xmin=0, ymin=0, ymax=height, xmax=width)

# czcionka
  font_import(pattern = "Lato", prompt = FALSE) 
  fonts()
#
  ends_agg <- 
    podroze %>% 
      group_by(godzina = finish, motyw) %>%
      summarize(n=n()) %>%
      group_by(godzina) %>%
      mutate(frac=n/sum(n)) %>%
      ungroup
  
# i<-450
for(i in 1:nrow(minuty)){
  
  #points <- readRDS(sprintf("dane/df/%s_points.rds",i))
  #routes <- readRDS(sprintf("dane/df/%s_routes.rds",i))
  #ends <- podroze %>% filter(finish == minuty$godzina_lab[i])
  #ends1 <- podroze %>% filter(finish == minuty$godzina_lab[i])
  #ends2 <- podroze %>% filter(finish == minuty$godzina_lab[i])
  #ends3 <- podroze %>% filter(finish == minuty$godzina_lab[i])

  ends_agg_i <- ends_agg
  ends_agg_i$n[ends_agg_i$godzina>=minuty$godzina_lab[i]] <- 0
  area <-
    ends_agg_i %>%
    ggplot(aes(x=godzina, y=n))+
    geom_area(aes(fill=motyw), position="stack")+
    scale_fill_manual(values = c("szkoła" = "red","praca" = "cyan","dom" = "magenta","uczelnia"="yellow","inne"="green"))+
    scale_y_continuous(expand = c(0,0)) + 
    coord_equal() +
    theme(axis.line=element_blank(),axis.ticks=element_blank(),
          axis.ticks.length = unit(0,"null"),axis.ticks.margin = unit(0,"null"),
          axis.text.x=element_blank(),axis.text.y=element_blank(),
          axis.title.x=element_blank(),axis.title.y=element_blank(),
          legend.position="none",
          panel.background=element_blank(),panel.border=element_blank(),
          panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
          plot.background= element_rect(fill = "transparent", colour=NA),
          plot.margin=unit(c(0,0,0,0), "null"),panel.margin=unit(c(0,0,0,0), "null"),
          legend.margin=unit(0, "null"),plot.background =element_rect(fill = "transparent",colour = NA)) +
    labs(x=NULL, y=NULL)
    
  
  p <- 
    ggplot(data=panel_left, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)) +
    geom_rect(fill="grey15",color=NA) +
    annotate(geom = "text", x = 50, y = height-50,
             label = sprintf("%s  Warszawa",format(minuty$godzina_lab[i],"%H:%M")),
             hjust = 0,label="Warszawa",family="Lato Medium", color="white", size=14)+
    geom_rect(xmin=40, xmax=width-40, ymin=height-10, ymax=height, fill="white") + 
    geom_rect(xmin=40, xmax=40+round(i/1440*(width-80)), ymin=height-10, ymax=height, fill="#0073e6") + 
#    geom_rect(xmin=40, xmax=55, ymin=height-400, ymax=height-300, fill="cyan") +
#    geom_rect(xmin=40, xmax=55, ymin=height-500, ymax=height-400, fill="magenta") +
#    geom_rect(xmin=40, xmax=55, ymin=height-600, ymax=height-500, fill="yellow") +
#    geom_rect(xmin=40, xmax=55, ymin=height-700, ymax=height-600, fill="red") +
#    geom_rect(xmin=40, xmax=55, ymin=height-800, ymax=height-700, fill="green") +
#    annotate(geom = "text",label="Praca", x = 40, y = height-350, label = "20:45 Warszawa",hjust = 0,family="Lato Medium", color="white", size=5)+
#    annotate(geom = "text",label="Dom", x = 40, y = height-450, label = "20:45 Warszawa",hjust = 0,family="Lato Medium", color="white", size=5)+
#    annotate(geom = "text",label="Uczelnia", x = 40, y = height-550, label = "20:45 Warszawa",hjust = 0,family="Lato Medium", color="white", size=5)+
#    annotate(geom = "text",label="Szkoła", x = 40, y = height-650, label = "20:45 Warszawa",hjust = 0,family="Lato Medium", color="white", size=5)+
#    annotate(geom = "text",label="Inne", x = 40, y = height-750, label = "20:45 Warszawa",hjust = 0,family="Lato Medium", color="white", size=5)+
    scale_y_continuous(expand = c(0,0)) + 
    scale_x_continuous(expand = c(0,0))+
    coord_equal() +
    theme(axis.line=element_blank(),axis.ticks=element_blank(),
          axis.ticks.length = unit(0,"null"),axis.ticks.margin = unit(0,"null"),
          axis.text.x=element_blank(),axis.text.y=element_blank(),
          axis.title.x=element_blank(),axis.title.y=element_blank(),
          legend.position="none",
          panel.background=element_blank(),panel.border=element_blank(),
          panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
          plot.background= element_rect(fill = "transparent", colour=NA),
          plot.margin=unit(c(0,0,0,0), "null"),panel.margin=unit(c(0,0,0,0), "null"),
          legend.margin=unit(0, "null"),plot.background =element_rect(fill = "transparent",colour = NA)) +
    labs(x=NULL, y=NULL)
  
  ggsave(sprintf("output/progress/progress%s.png",i),p,height=height/100, width=width/100, dpi = 100, bg = "transparent")
  ggsave(sprintf("output/areas/area%s.png",i),area,height=height/1000, width=width/100, dpi = 100, bg = "transparent")

}
#### ZEGAR ANALOGOWY -----
  analogDegrees <- function(center = c(0,0),diameter = 1, npoints = 60){
    r = diameter/2
    t <- seq(2.5*pi,.5*pi,length.out = npoints)
    x <- center[1] + r * cos(t)
    y <- center[2] + r * sin(t)
    return(data.frame(x = x, y = y))
  }
  
  width=90
  height=90
  
  round <- analogDegrees(c(0,0),2.5,npoints = 61)
  minutes_ext <- analogDegrees(c(0,0),2,npoints = 61)
  minutes_int <- analogDegrees(c(0,0),.1,npoints = 61)
  
  minutes_ext$minute <- 0:60
  minutes_int$minute <- 0:60
  minutes <- bind_rows(minutes_ext, minutes_int)
  
  hours_ext <- analogDegrees(c(0,0),1.5,npoints = 13)
  hours_int <- analogDegrees(c(0,0),.1,npoints = 13)
  hours_ext$hour <- 0:12
  hours_int$hour <- 0:12
  hours <- bind_rows(hours_ext, hours_int)
  
  for(i in 1:1440){
    minute <- minuty[i,]$godzina$min
    hour <- minuty[i,]$godzina$hour
    clock <- 
      ggplot(round,aes(x,y)) + 
        geom_path(color="white",size=4) +
        #geom_path(data=dat[1:20,],aes(x,y), color="black", size=10) +
        geom_path(data=minutes[minutes$minute==minute,],aes(x,y), size=4,color="white",lineend = "round") +
        geom_path(data=hours[hours$hour==hour,],aes(x,y), size=4,color="white",lineend = "round") +
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
              plot.background= element_rect(fill = "transparent", colour=NA),
              plot.margin=unit(c(0,0,0,0), "null"),
              panel.margin=unit(c(0,0,0,0), "null"),
              legend.margin=unit(0, "null"),
              plot.background =element_rect(fill = "transparent",colour = NA)) +
        labs(x=NULL, y=NULL)
    png(file = sprintf("output/clock/%s_clock.png"  ,i), bg = "transparent", type = c("cairo"), width=150, height=150, res=100)
      print(clock)
    dev.off()
  }
  #
  
  
#### WYKRES KOLOWY ####
  podroze$motyw %<>% as.factor
  width=100
  height=100
  gg_theme <- 
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
          plot.background= element_rect(fill = "grey20", colour=NA),
          plot.margin=unit(c(0,0,0,0), "null"),
          panel.margin=unit(c(0,0,0,0), "null"),
          legend.margin=unit(0, "null"),
          plot.background =element_rect(fill = "transparent",colour = NA))
  
  
  for(i in 240:1440) {
    
    temp_podroze <- podroze %>% filter(start<=minuty$godzina[i] & finish>=minuty$godzina[i])
    temp_podroze <- table(temp_podroze$motyw) %>% as.data.frame
    total <- sum(temp_podroze$Freq)
    if(total==0) total<-1
    
    gg <-
      ggplot(temp_podroze)+
      geom_rect(aes(xmin=0, ymin=0,xmax=1.01,ymax=total),fill="black") +
      geom_rect(aes(xmin=0, ymin=0,xmax=.95,ymax=total),fill="grey20") +
      
      geom_bar(aes(y=Freq,x=.475, fill=Var1),width = .95, stat = "identity", alpha=1) +
      scale_fill_manual(values = c("szkoła" = "red","praca" = "cyan","dom" = "magenta","uczelnia"="yellow","inne"="green")) +
      coord_polar("y", start=0) +
      gg_theme +
      labs(x=NULL, y=NULL)
    # miejsce na trasy
    ggsave(sprintf("output/piechart/%s_plot.png"  ,i), gg, dpi=100,height=height/100, width=width/100)
    
  }
  
#### BAR CHART ####
  podroze$motyw %<>% as.factor
  width=100
  height=100
  gg_theme <- 
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
          plot.background= element_rect(fill = "transparent", colour=NA),
          plot.margin=unit(c(0,0,0,0), "null"),
          panel.margin=unit(c(0,0,0,0), "null"),
          legend.margin=unit(0, "null"),
          plot.background =element_rect(fill = "transparent",colour = NA))
  
  for(i in 1:1440) {
    
    temp_podroze <- podroze %>% filter(start<=minuty$godzina[i] & finish>=minuty$godzina[i])
    temp_podroze <- table(temp_podroze$motyw) %>% as.data.frame
    total <- sum(temp_podroze$Freq)
    
    gg <-
      ggplot(temp_podroze)+
      geom_bar(aes(y=Freq,x=Var1, fill=Var1), stat = "identity", alpha=1,width=.5) +
      geom_bar(aes(y=50,x=Var1, fill=Var1), stat = "identity", alpha=1,width=.3) +
      scale_fill_manual(values = kolory) +
      gg_theme +
      ylim(0,193882)+
      labs(x=NULL, y=NULL)
    # miejsce na trasy
    png(file = sprintf("output/1barchart/%s_plot.png",i), bg = "transparent", type = c("cairo"), width=110, height=110, res=100)
      print(gg)
    dev.off()
    
  }
#### BAR CHART (UDZIAŁY) ####
  podroze$motyw %<>% factor(levels=c("praca","dom","szkoła","uczelnia","inne"))
  width=53
  height=53
  gg_theme <- 
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
          plot.background= element_rect(fill = "transparent", colour=NA),
          plot.margin=unit(c(0,0,0,0), "null"),
          panel.margin=unit(c(0,0,0,0), "null"),
          legend.margin=unit(0, "null"),
          plot.background =element_rect(fill = "transparent",colour = NA))
  
  for(i in 1:1440) {
    
    temp_podroze <- podroze %>% filter(start<=minuty$godzina[i] & finish>=minuty$godzina[i])
    temp_podroze <- table(temp_podroze$motyw) %>% as.data.frame
    temp_podroze %<>% mutate(frac = Freq/sum(Freq))
    
    gg <-
      ggplot(temp_podroze)+
      geom_bar(aes(y=frac,x=Var1, fill=Var1), stat = "identity", alpha=1,width=.5) +
      scale_fill_manual(values = kolory) +
      gg_theme +
      ylim(0,1) +
      labs(x=NULL, y=NULL)
    # miejsce na trasy
    png(file = sprintf("output/3barchart_fraction/%s_plot.png",i), bg = "transparent", type = c("cairo"), width=width, height=height, res=100)
     print(gg)
    dev.off()
    
  }
  
  
#### ZEGAR CYFROWY #####
  font_import(pattern = "Lato", prompt = FALSE) 
  fonts()
  clock_size <- data.frame(xmin=-81, xmax=81, ymin=-34, ymax=34)
  
  gg_theme <- 
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
          plot.background= element_rect(fill = "transparent", colour=NA),
          plot.margin=unit(c(0,0,0,0), "null"),
          panel.margin=unit(c(0,0,0,0), "null"),
          legend.margin=unit(0, "null"),
          plot.background =element_rect(fill = "transparent",colour = NA))
  
  
  for(i in 1:nrow(minuty)){
    gg<- 
      ggplot(data=clock_size, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)) +
      geom_rect(fill=NA,color="white") +
      annotate(geom = "text", x = 0, y = 0,
               label = format(minuty$godzina[i],"%H:%M"),
               hjust = .5,family="Lato Medium", color="white", size=5) +
      gg_theme
      
    png(file = sprintf("output/3clock_digit/%s_clockdigit.png",i), bg = "transparent", type = c("cairo"), width=71, height=30, res=100)
    
      print(gg)
    dev.off()
    
  }