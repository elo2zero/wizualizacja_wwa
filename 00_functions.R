library(RPostgreSQL);library(httr);library(XML);require(rgdal)


psqlCreateRoutesTable <- function(){
  customQuery("drop table if exists trasy;")
  customQuery("CREATE TABLE 
                trasy(
                  idtrasy INTEGER,
                  seq INTEGER, 
                  node_id INTEGER, 
                  edge_id INTEGER, 
                  cost DOUBLE PRECISION)")
  customQuery("SELECT AddGeometryColumn( 'trasy', 'route_geom', 4326, 'GEOMETRY', 2 );")
  return(NULL)
}

psqlGetShortestPath <- function(idtrasy,from, to) {
  customQuery("INSERT INTO trasy 
                (idtrasy,seq, node_id, edge_id, cost, route_geom) 
              (SELECT 
                 %s idtrasy,
                 seq, node_id, edge_id, cost,
                 the_geom::geometry route_geom
              FROM 
                 (SELECT seq, id1 AS node_id, id2 AS edge_id, cost 
                  FROM pgr_dijkstra('
                            SELECT gid AS id, source::integer, target::integer, length::DOUBLE PRECISION AS cost, reverse_cost
                            FROM ways',
                            %s, %s, true, true)
                 ) AS r, 
                 ways w 
              WHERE w.gid = r.edge_id);", 
              idtrasy,from, to)
}


psqlSelectWithinRej <- function(where,rej){
  customQuery('
        SELECT 
          id AS node_id
        FROM 
          (SELECT * 
          FROM ways_vertices_pgr 
          WHERE id IN (SELECT DISTINCT %s FROM ways WHERE class_id <= 112) ) w,
          
          ( SELECT st_setsrid(geom, 4326) geom, REJ_1000
          FROM rejonywarszawa 
          WHERE REJ_1000 = %s
          ) rej
        WHERE 
          st_within(w.the_geom, rej.geom);
        ',where,rej)
  }

findNearestTarget <- function(x, y){
  customQuery("
    SELECT
      id AS node_id,
      CAST(
        st_distance_sphere(
            the_geom,
            st_setsrid(st_makepoint(%1$s,%2$s),4326)
        ) AS INT
      ) AS d
    FROM
      ways_vertices_pgr
    WHERE
      id in (select distinct source from ways where class_id <= 112) 
    ORDER BY the_geom <-> st_setsrid(st_makepoint(%1$s, %2$s), 4326)
    LIMIT 1;", x, y)
}

findNearestSource <- function(x, y){
  customQuery("
    SELECT
      id AS node_id,
      CAST(
        st_distance_sphere(
          the_geom,
          st_setsrid(st_makepoint(%1$s,%2$s),4326)
        ) AS INT
      ) AS d
    FROM
      ways_vertices_pgr
    WHERE
      id in (select distinct source from ways where class_id <= 112)
    ORDER BY the_geom <-> st_setsrid(st_makepoint(%1$s,%2$s), 4326)
    LIMIT 1;", x, y)
}

getShortestPathOLS <- function(start, end){
  url <- "http://openls.geog.uni-heidelberg.de/route?start=FROM&end=TO&via=&lang=de&distunit=KM&routepref=Bicycle&weighting=Shortest&avoidAreas=&useTMC=false&noMotorways=false&noTollways=false&noUnpavedroads=false&noSteps=false&noFerries=false&instructions=false" %>%
    gsub("FROM",start,x=.) %>% gsub("TO",end,x=.)
  resp <- POST(url)
  route <- content(resp) %>% xmlToList()
  return(route)
}