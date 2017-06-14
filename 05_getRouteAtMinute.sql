SELECT 
    idtrasy, motyw, 
    st_x(st_line_interpolate_point(route_geom,
        (DATE_PART('hour', '%1$s' - start )* 60 + DATE_PART('minute', '%1$s' - start))/
        (DATE_PART('hour', finish - start )* 60 + DATE_PART('minute', finish - start))
    )) x,
    st_y(st_line_interpolate_point(route_geom,
        (DATE_PART('hour', '%1$s' - start )* 60 + DATE_PART('minute', '%1$s' - start))/
        (DATE_PART('hour', finish - start )* 60 + DATE_PART('minute', finish - start))
    )) y,


    CASE WHEN 
        (DATE_PART('hour', '%1$s' - start )* 60 + DATE_PART('minute', '%1$s' - start))/
        (DATE_PART('hour', finish - start )* 60 + DATE_PART('minute', finish - start)) BETWEEN .01 AND .05
        THEN
        st_linesubstring(
            route_geom,0,
            (DATE_PART('hour', '%1$s' - start )* 60 + DATE_PART('minute', '%1$s' - start))/
            (DATE_PART('hour', finish - start )* 60 + DATE_PART('minute', finish - start))
        )
        WHEN 
        (DATE_PART('hour', '%1$s' - start )* 60 + DATE_PART('minute', '%1$s' - start))/
        (DATE_PART('hour', finish - start )* 60 + DATE_PART('minute', finish - start)) > .05
        THEN 
        st_linesubstring(
            route_geom,
            (DATE_PART('hour', '%1$s' - start )* 60 + DATE_PART('minute', '%1$s' - start))/
            (DATE_PART('hour', finish - start )* 60 + DATE_PART('minute', finish - start)) - 0.05,
            (DATE_PART('hour', '%1$s' - start )* 60 + DATE_PART('minute', '%1$s' - start))/
            (DATE_PART('hour', finish - start )* 60 + DATE_PART('minute', finish - start))
        )
    END AS geom
FROM podroze3_model_2
WHERE
    start <= '%1$s' AND 
    finish >= '%1$s' and
    route_geom is not null;