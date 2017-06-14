# Wizualizacja transportu w Warszawie i gminach ościennych
Projekt składał się z kilku etapów, którego finalnym produktem są wizualizacje zawarte w folderze final_viz. Kopia wizualizacji dostępna również na [youtube](https://www.youtube.com/watch?v=mpYSy47jqlM) oraz na stronie [UM Warszawa](http://transport.um.warszawa.pl/wbr2015).
Celem projektu miałobyć zwizualizowanie przemieszczania się pasażerów w metropolii warszawskiej.
Za pozyskanie danych z próby odpowiedzialny była agencja badawcza PBS. Wagi wynikające z modelu przestrzennego zostały estymowane przez Politechnikę Krakowską na podstawie modelu przestrzennego. Wizualizację stworzył Dawid Kałędkowski.
Wykorzystane technologie:
a. R - scripting
b. PostgreSQL - database 
c. PGIS - spatian extension to PostreSQL  
d. osm2pgis - tool to import openstreetmap data to postgres
e. Pgrouting - routing algorithms to replicate optimal path between start/end points
f. blender - combined frames and layers together for final animation
