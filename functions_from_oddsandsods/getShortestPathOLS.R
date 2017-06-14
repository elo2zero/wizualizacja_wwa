#' Get shortest path from openLS server
#'
#'
#' Get shortest path from openLS server
#' @param start starting point coordinates
#' @param end ending point coordinates
#' @return list points


getShortestPathOLS <- function(start, end){
  url <- "http://openls.geog.uni-heidelberg.de/route?start=FROM&end=TO&via=&lang=de&distunit=KM&routepref=Bicycle&weighting=Shortest&avoidAreas=&useTMC=false&noMotorways=false&noTollways=false&noUnpavedroads=false&noSteps=false&noFerries=false&instructions=false" %>%
    gsub("FROM",start,x=.) %>% gsub("TO",end,x=.)
  resp <- POST(url)
  route <- content(resp) %>% xmlToList()
  return(route)
}
