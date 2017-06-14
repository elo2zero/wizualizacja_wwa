library(reshape2);library(magrittr); library(dplyr); library(ggplot2); library(rgeos); library(oddsandsods); library(rgdal); library(data.table)
setwd("~/Documents/eclipse_projects/wizualizacje/warszawa_podroze")
# import danych z trasami
# dane modelu
library(memisc)

  model_do_pracy <- foreign:::read.spss('dane/do pracy.sav', to.data.frame=T) %>% as.data.table
  model_na_uczelnie <- foreign:::read.spss("dane/na uczelnie.sav",to.data.frame = T) %>% as.data.table
  model_do_domu <- foreign:::read.spss("dane/do_domu.sav",to.data.frame = T) %>% as.data.table
  model_do_szkoly <- foreign:::read.spss("dane/do_szkoły.sav",to.data.frame = T) %>% as.data.table
  model_pozost <- foreign:::read.spss("dane/pozostale.sav",to.data.frame = T) %>% as.data.table
  
# godziny:minuty dnia
  godziny <- attr(model_do_pracy,"variable.labels") %>% names
  godziny_lab <- attr(model_do_pracy,"variable.labels") %>% c
  godziny_lab <- paste( "2016-04-21" ,godziny_lab) %>% strptime(.,format="%Y-%m-%d %H:%M")
  godziny_labels <- data.table(godzina=godziny, godzina_lab = godziny_lab)
  
# dodanie motywacji
  model_do_pracy$motyw <- "praca"
  model_na_uczelnie$motyw <- "uczelnia"
  model_do_domu$motyw <- "dom"
  model_do_szkoly$motyw <- "szkoła"
  model_pozost$motyw <- "inne"
#
  model_do_pracy %<>% 
    melt(id.vars=c("startREJON","stopREJON","l_podr", "motyw"), variable.name="godzina",variable.factor = F)
  model_na_uczelnie %<>% 
    melt(id.vars=c("startREJON","stopREJON","l_podr", "motyw"), variable.name="godzina",variable.factor = F)
  model_do_domu %<>% 
    melt(id.vars=c("startREJON","stopREJON","l_podr", "motyw"), variable.name="godzina",variable.factor = F)
  model_do_szkoly %<>% 
    melt(id.vars=c("startREJON","stopREJON","l_podr", "motyw"), variable.name="godzina",variable.factor = F)
  model_pozost %<>% 
    melt(id.vars=c("startREJON","stopREJON","l_podr", "motyw"), variable.name="godzina",variable.factor = F)
    
# Usuwanie przedziałów czasowych w których nie ma podróży
  model_do_pracy %<>% .[value>0]
  model_na_uczelnie %<>% .[value>0]
  model_do_domu %<>% .[value>0]
  model_do_szkoly %<>% .[value>0]
  model_pozost %<>% .[value>0]

#  Losowanie podróży gdzie  0 < value < 1
  model_do_pracy1 <- model_do_pracy[value<1,]
  model_do_pracy <- model_do_pracy[value>=1,]
  model_do_pracy %<>% .[,value := round(value)]
  model_do_pracy1 %<>% .[,value := rbinom(length(value),1,value)]
  model_do_pracy1 %<>% .[value>0]
  model_do_pracy <- rbind(model_do_pracy, model_do_pracy1)
  
  
  model_do_domu1 <- model_do_domu[value<1,]
  model_do_domu <- model_do_domu[value>=1,]
  model_do_domu %<>% .[,value := round(value)]
  model_do_domu1 %<>% .[,value := rbinom(length(value),1,value)]
  model_do_domu1 %<>% .[value>0]
  model_do_domu <- rbind(model_do_domu, model_do_domu1)
  
  model_do_szkoly1 <- model_do_szkoly[value<1,]
  model_do_szkoly <- model_do_szkoly[value>=1,]
  model_do_szkoly %<>% .[,value := round(value)]
  model_do_szkoly1 %<>% .[,value := rbinom(length(value),1,value)]
  model_do_szkoly1 %<>% .[value>0]
  model_do_szkoly <- rbind(model_do_szkoly, model_do_szkoly1)
  
  model_na_uczelnie1 <- model_na_uczelnie[value<1,]
  model_na_uczelnie <- model_na_uczelnie[value>=1,]
  model_na_uczelnie %<>% .[,value := round(value)]
  model_na_uczelnie1 %<>% .[,value := rbinom(length(value),1,value)]
  model_na_uczelnie1 %<>% .[value>0]
  model_na_uczelnie <- rbind(model_na_uczelnie, model_na_uczelnie1)
  
  model_pozost1 <- model_pozost[value<1,]
  model_pozost <- model_pozost[value>=1,]
  model_pozost %<>% .[,value := round(value)]
  model_pozost1 %<>% .[,value := rbinom(length(value),1,value)]
  model_pozost1 %<>% .[value>0]
  model_pozost <- rbind(model_pozost, model_pozost1)
  
# Łączenie motywacji
  model <- rbind(model_do_pracy, model_na_uczelnie, model_do_szkoly, model_do_domu, model_pozost)
  model <- merge(model, godziny_labels, by="godzina")

# liczba podróży do pojedyńczych podróży (rzędów)
  model_long <- model[rep(seq(nrow(model)), value), c(2,3,5,6,7), with=F]  
  rm(model)
  
  set.seed(2202)
  model_long <- 
    model_long[,start:=(godzina_lab+60*sample(0:14,length(godzina_lab), replace=T)) %>% as.POSIXct(origin="1970-01-01")]
  
# łączenie z punktami
  load("01_possible_routes.RData") 
  setattr(ways, "class", c("data.table","data.frame"))  
  setnames(model_long,c("startREJON","stopREJON"),c("source_REJ_1000","target_REJ_1000"))
  model_long %<>% merge(ways, by=c("source_REJ_1000","target_REJ_1000"))
  #save.image("05_temp.RData");load("05_temp.RData")

# Zestaw godzin w dobie
  minuty <- 
    data.frame(
      id = 1:1440,
      godzina_lab = seq(as.POSIXct('2016-04-21 00:00:00'),as.POSIXct('2016-04-21 23:59:00'), length.out=1440)
    )
  sqlConnect(dbname="mazowieckie_pgr")
  dbWriteTable(con,"podroze3_model",model_long,row.names=F)
  dbWriteTable(con,"klatki",minuty,row.names=F)
  dbDisconnect(con)
#
  
  head(model)
  sums <- apply(model, 2, sum)
