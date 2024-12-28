#Georreferencia

#Cargamos paquetes 

library(pacman)
p_load("tidyverse", 
       "sf",   #Paquete clave para manipular datos espaciales
       "tmap",
       "mapproj", 
       "chilemapas", #mapas de Chile
       "scales",
       "rvest",
       "janitor") #utilidad para visualización de datos

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

library(rvest)


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

#Visualizar datos en el mapa


library(rvest)

# dirección de wikipedia con tabla de comunas de Chile
url <- "https://es.wikipedia.org/wiki/Anexo:Comunas_de_Chile"

# obtener tabla con datos de comunas con web scraping
tabla <- session(url) |> 
  read_html() |> 
  html_table(convert = FALSE)

tabla[[1]]

# limpiar datos
datos_comunas <- tabla[[1]] |> 
  clean_names() |> 
  # seleccionar y renombrar columnas
  select(codigo_comuna = cut_codigo_unico_territorial,
         nombre, region, superficie_km2,
         poblacion = poblacion2020) |> 
  # eliminar espacios de la columna de población
  mutate(poblacion = str_remove_all(poblacion, " "),
         poblacion = as.numeric(poblacion)) |> 
  # eliminar los separadores de miles
  mutate(superficie_km2 = str_remove_all(superficie_km2, "\\."),
         # convertir comas a puntos
         superficie_km2 = str_replace(superficie_km2, ",", "."),
         superficie_km2 = as.numeric(superficie_km2))

datos_comunas

# ahora viene juntar los datos 

mapa_comunas_2 <- mapa_comunas |> 
  # adjuntar datos al mapa, coincidiendo por columna de código de comunas
  left_join(datos_comunas,
            by = join_by(codigo_comuna)) |> 
  relocate(geometry, .after = 0) # tirar geometría al final

mapa_comunas_2


mapa_comunas_2 |> 
  st_set_geometry(mapa_comunas_2$geometry) |> # asignar geometría
  ggplot() + # gráfico
  aes(fill = poblacion) +
  geom_sf(linewidth = 0) + # capa geométrica
  theme_classic() +
  scale_fill_distiller(type = "seq", palette = 12, 
                       labels = label_comma(big.mark = ".")) + # colores
  scale_x_continuous(breaks = seq(-76, -65, length.out = 3) |> floor()) + # escala x
  coord_sf(xlim = c(-77, -65)) + # recortar coordenadas
  theme(legend.key.width = unit(3, "mm"))



mapa_comunas_2 |> 
  st_set_geometry(mapa_comunas_2$geometry) |>
  ggplot() +
  aes(fill = superficie_km2) + # variable de relleno
  geom_sf(linewidth = 0) +
  theme_classic() +
  scale_fill_distiller(type = "seq", palette = 11,
                       labels = label_comma(big.mark = ".")) + 
  scale_x_continuous(breaks = seq(-76, -65, length.out = 3) |> floor()) +
  coord_sf(xlim = c(-77, -65)) + 
  theme(legend.key.width = unit(3, "mm"))


mapa_comunas_filtro <- mapa_comunas_2 |> 
  filter(codigo_region == "06")

# filtrar datos
mapa_comunas_filtro <- mapa_comunas_2 |> 
  filter(codigo_region == "06")

# mapa
mapa_comunas_filtro |> 
  st_set_geometry(mapa_comunas_filtro$geometry) |>
  ggplot() +
  aes(fill = poblacion) +
  geom_sf(linewidth = 0.12, color = "white") +
  geom_sf_text(aes(label = comma(poblacion, big.mark = ".")), 
               size = 2, color = "white", check_overlap = T) +
  theme_classic() +
  scale_fill_distiller(type = "seq", palette = 12,
                       labels = label_comma(big.mark = ".")) + 
  theme(legend.key.width = unit(3, "mm")) +
  theme(axis.title = element_blank())


# dirección del sitio del banco central
url <- "https://si3.bcentral.cl/Siete/ES/Siete/Cuadro/CAP_CCNN/MN_CCNN76/CCNN2018_PIB_REGIONAL_N/637899740344107786"

# obtener tabla con datos de comunas con web scraping
tabla_pib <- session(url) |> 
  read_html() |> 
  html_table(convert = FALSE)

datos_regiones <- tabla_pib [[1]] |> 
  janitor::clean_names() |> 
  select(region = serie, pib = x2023) |> 
  mutate(pib = str_remove_all(pib, "\\."),
         pib = as.numeric(pib)) |> 
  filter(str_detect(region, "Región"))

datos_regiones




# Filtramos solo Maipú
maipu <- mapa_comunas %>%
  filter(comuna == "Maipú")
