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
      ST_DWithin(the_geom, st_setsrid(st_makepoint(%1$s,%2$s), 4326), 10000)
    ORDER BY the_geom <-> st_setsrid(st_makepoint(%1$s, %2$s), 4326)
    LIMIT 1;", x, y)
}

