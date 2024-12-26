#Georreferencia

#Cargamos paquetes 

library(pacman)
p_load("tidyverse", 
       "sf",   #Paquete clave para manipular datos espaciales
       "tmap"
       ,"mapproj") #uno de los paquetes para hacer mapas


# Creamos un Data Frame con los datos necesarios
datos <- data.frame(lat = c(-34.714656, 51.532068), long = c(-58.785999, 
                                                             -0.177305), ubicacion = c("UTDT", "Abbey Road"))
# Lo convertimos a un objeto sf
puntosEspaciales <- st_as_sf(datos, coords = c("long", "lat"), 
                             crs = 4326)
st_distance(puntosEspaciales)  # En metros

st_distance(puntosEspaciales)/1000  # En kilómetros

barrios <- st_read("barrios_badata")
st_crs(barrios)

# 4.4.1. CABA o