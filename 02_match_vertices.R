# Dla pliku ways wyszukiwanie trasy z bazy postgis, używając pgrouting.
# (pgsql) Insert do bazy każdej trasy optymalnej
# (pgsql) trasa optymalna składa się z wielu wierszy, które następnie są łączone w jedną geometrię
library(dplyr); library(oddsandsods)
setwd("~/Documents/eclipse_projects/wizualizacje/warszawa_podroze")
load("01_possible_routes.RData") 
source("00_functions.R");
sqlConnect(dbname="mazowieckie_pgr")

# psqlCreateRoutesTable()
idx <- customQuery("select max(idtrasy) + 1 from trasy")[[1]]
dbDisconnect(con)

for(i in idx:nrow(ways)){
  print(i)
  sqlConnect(dbname="mazowieckie_pgr")
  trasa <- psqlGetShortestPath(i,ways$source_node_id[i], ways$target_node_id[i])
  dbDisconnect(con)
  
  if(i %in% c(nrow(ways),seq(0, nrow(ways), by=1000))){ 
    q(save="no")
  }
}



#  test <- lapply(routes, nrow) %>% unlist
# ways[which(test == 0),] %>% View
  