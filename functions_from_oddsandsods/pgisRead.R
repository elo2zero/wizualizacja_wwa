#' Read postgis data from given connection
#'
#' Read postgis data from given connection
#' @param sql sql query
#' @examples
#' pgisRead("select * from planet_osm_polygon where admin_level='9'")

# sql <- "select osm_id, way from planet_osm_line limit 10"

pgisRead <- function (sql, data.frame=F, esri = 4326) {
  tmp <- "tmp_read_ogr"
  if(!any(rgdal:::ogrDrivers()$name=='PostgreSQL'))
    stop("PostreSQL driver is missing in your rgdal library")

  connInfo <- dbGetInfo(con) %>% unlist %>% .[c("dbname","user","host","port")]
  connInfo <- paste0(names(connInfo),"='",connInfo,"'") %>% paste(collapse=" ")
  dsn <- paste0("PG:",connInfo)

  # Retrive data from VIEW in existing psql
  strCreateView = paste("CREATE table", tmp, "AS", sql)
  dbSendQuery(con, strCreateView)

  rgdal:::ogrInfo(dsn = dsn, layer = tmp) %>% print
  sp <- rgdal:::readOGR(dsn = dsn, layer = tmp)
  if(data.frame==T){
    require(ggplot2); require(dplyr)
    data <- sp@data
    data$id = rownames(sp@data)
    sp %<>%
      fortify() %>%
      left_join(data)
  }
  dbSendQuery(con, paste("DROP table if exists ",tmp))
  return(sp)
}
