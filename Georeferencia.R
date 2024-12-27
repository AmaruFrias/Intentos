#Georreferencia

#Cargamos paquetes 

library(pacman)
p_load("tidyverse", 
       "sf",   #Paquete clave para manipular datos espaciales
       "tmap",
       "mapproj", 
       "chilemapas", #mapas de Chile
       "scales") #utilidad para visualización de datos

mapa_comunas <- chilemapas::mapa_comunas
mapa_comunas

grafico_comunas <- mapa_comunas %>% 
  st_set_geometry(mapa_comunas$geometry) %>% # asignar geometría
  ggplot() + # gráfico
  geom_sf() # capa geométrica

grafico_comunas +
  theme_classic()

#cortar el mapa, para que muestre Chile continental

grafico_comunas +
  coord_sf(xlim = c(-77,-65)) +
  theme_classic()

#agrupar datos, para que el mapa sea solamente de regines

mapa_regiones <- mapa_comunas %>%
  group_by(codigo_region) %>%
  summarise(geometry = st_union(geometry)) #resumir los datos agrupado, resumiendolos

mapa_regiones

#grafico por regiones 

grafico_regiones <- mapa_regiones %>%
  st_set_geometry(mapa_regiones$geometry) %>%
  ggplot() +
  geom_sf() +
  coord_sf(xlim = c(-77,-65))

grafico_regiones +
  theme_classic()