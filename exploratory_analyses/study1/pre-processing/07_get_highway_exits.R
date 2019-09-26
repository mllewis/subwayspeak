library(osmdata)

EXIT_PATH <-  "../data/highway_exits.csv"
bb = getbb('Iowa, USA')
q <- opq(bbox = bb)

junctions <-  q %>%
  add_osm_feature(key = 'highway', value = "motorway_junction") %>%
  osmdata_sp()


junction_data <- junctions$osm_points %>%
  as.data.frame() %>%
  select(highway, lat, lon)

write_csv(junction_data, EXIT_PATH)
