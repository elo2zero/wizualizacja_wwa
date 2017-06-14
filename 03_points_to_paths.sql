DROP VIEW if EXISTS vw_tmp_read_ogr;

DROP TABLE IF EXISTS trasy_merged;
CREATE TABLE trasy_merged AS
SELECT 
    idtrasy, 
    st_makeline(the_geom) route_geom
FROM 
    (SELECT idtrasy, v.the_geom 
     FROM trasy t
     LEFT JOIN ways_vertices_pgr v ON v.id = t.node_id
     ORDER BY idtrasy, seq) x
GROUP BY 1;

CREATE TABLE podroze_model_2 AS
SELECT *
FROM podroze_model p
LEFT JOIN trasy_merged t using(idtrasy)
ORDER BY start;


ALTER TABLE podroze_model_2 add COLUMN finish TIMESTAMPtz;
ALTER TABLE podroze_model_2 add COLUMN tlength float;
UPDATE podroze_model_2
SET tlength = floor(st_length(route_geom, true)/1000*60/20)+1,
    finish = START + (floor(st_length(route_geom, true)/1000*60/20)+1) * INTERVAL '1 minute';
UPDATE podroze_model_2
SET finish = finish - INTERVAL '1 day',
    START = START - INTERVAL '1 day'
WHERE date(finish) > '2016-04-21';


CREATE temporary TABLE podroze_model_2_temp AS
SELECT * FROM podroze_model_2
ORDER BY START;

CREATE TABLE podroze_model_3 AS
SELECT 
    k.id,k.godzina_lab godzina_minuta, p.*
FROM klatki k
LEFT JOIN podroze_model_2_temp p
    ON k.godzina_lab >= p.start AND
       k.godzina_lab <= p.finish
ORDER BY 1;
CREATE index ON podroze_model_3(godzina_minuta); 

ALTER TABLE podroze_model_3 add COLUMN tduration FLOAT;
UPDATE podroze_model_3 p
SET tduration = DATE_PART('hour', godzina_minuta - START )* 60 + DATE_PART('minute', godzina_minuta - "start");


SELECT AddGeometryColumn( 'podroze_model_3', 'route_actual', 4326, 'GEOMETRY', 2 );
SELECT AddGeometryColumn( 'podroze_model_3', 'plemnik05', 4326, 'GEOMETRY', 2 );
SELECT AddGeometryColumn( 'podroze_model_3', 'plemnik1', 4326, 'GEOMETRY', 2 );
SELECT AddGeometryColumn( 'podroze_model_3', 'plemnik15', 4326, 'GEOMETRY', 2 );
SELECT AddGeometryColumn( 'podroze_model_3', 'plemnik20', 4326, 'GEOMETRY', 2 );

UPDATE podroze_model_3
SET route_actual = st_linesubstring(route_geom, 0,tduration/tlength);

UPDATE podroze_model_3
SET plemnik05 = 
    CASE 
        WHEN tduration/tlength <= .05 THEN  st_linesubstring(route_geom, 0,tduration/tlength)
        ELSE st_linesubstring(route_geom, tduration/tlength-0.05,tduration/tlength)
    END;





SELECT finish, COUNT(route_geom) FROM podroze_model_2 GROUP BY 1
ORDER BY 1;


SELECT godzina_minuta,count(*) FROM podroze_model_3 GROUP BY 1 ORDER BY 1;
SELECT godzina_minuta,count(*) FROM podroze_model_3 GROUP BY 1 ORDER BY 1;
