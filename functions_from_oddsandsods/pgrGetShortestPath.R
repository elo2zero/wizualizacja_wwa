pgrGetShortestPath <- function(idtrasy,x1,x2,y1,y2, nodeFrom, nodeTo) {
  if(missing(nodeFrom)) nodeFrom <- findNearestSource(x1,y1)
  if(missing(nodeTo)) nodeTo <- findNearestTarget(x2,y2)

  if(nrow(nodeFrom)==0) stop("Cannot locate origin node near provided coordinates")
  if(nrow(nodeTo)==0) stop("Cannot locate target node near provided coordinates")

  query <-
    sprintf("CREATE table pgr_shortest_route AS
            SELECT seq, id1 AS node_id, id2 AS edge_id, cost length
            FROM pgr_dijkstra('
                      SELECT gid AS id, source::integer, target::integer, length::DOUBLE PRECISION AS cost, reverse_cost
                      FROM ways',
                      %1$s, %2$s, true, true) AS r;",
            nodeFrom$node_id, nodeTo$node_id)

  customQuery(query)
  geom <- pgisRead("
    SELECT
      r.length,
      w.x1,w.y1, w.x2, w.y2, w.name, w.the_geom
    FROM
      pgr_shortest_route r
    LEFT JOIN
      ways w ON w.gid = r.edge_id")


  customQuery("DROP TABLE IF EXISTS pgr_shortest_route")
  return(geom)
}
