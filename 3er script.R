#Empezamos con left_join 

mortalidadInfantil <- read.table(file="https://raw.githubusercontent.com/martintinch0/CienciaDeDatosParaCuriosos/master/data/MortalidadInfantilLong.csv",
                                 sep = ",",
                                 header = TRUE,
                                 stringsAsFactors = FALSE)


gapminder_argentina <- gapminder_df %>% 
  filter(country == "Argentina")
gapminder_argentina1952 <- gapminder_argentina %>%
  filter(year==1952)
mortalidadInfantilArgentina1952 <- mortalidadInfantil %>%
  filter(Year==1952 & country=="Argentina")
gapminder_argentina1952 <- left_join(gapminder_argentina1952,
                                     mortalidadInfantilArgentina1952,
                                     by="country")
gapminder_argentina1952


barriosOriginal <- read.table(file="https://github.com/datalab-UTDT/datasets/raw/master/barriosSample.csv",
                              sep = ";",
                              header = TRUE,
                              stringsAsFactors = FALSE)
dim(barriosOriginal) # Número de filas y de columnas

colnames(barriosOriginal)

barriosOriginal <- barriosOriginal %>% 
  select(BARRIOS, price_usd_per_m2, year)
str(barriosOriginal)


#Ahora, necesitaremos calcular el precio PROMEDIO por barrio y año

barriosOriginal <- barriosOriginal %>% group_by(BARRIOS,year) %>%
  summarise(precio_promedio = mean(price_usd_per_m2))


barriosOriginal <- barriosOriginal %>% pivot_wider(names_from = year,
                                                   values_from = precio_promedio)

colnames(barriosOriginal)[2:6] <- paste('USDm2_',colnames(barriosOriginal)[2:6], sep="")
head(barriosOriginal, n =3)


#Ejercicio

#Vuelve a cargar el data frame de gapminder y responde las siguientes preguntas:
#1.¿ Cuál es la observación con mayor expectativa al nacer de todo el dataset?
#¿A que país corresponde y a que año?

gapminder_df <- gapminder_df %>% arrange(desc(lifeExp)) # es Japon con 82.603, en el año 2007

#2.¿Cuál es la expectativa de vida a nacer promedio por contienente en 1952?¿y en 1957?

gapminder_media <- gapminder_df %>% filter(year %in% c(1952,1957)) # se crea un objeto, filtrando solamente para los años 1952 y 1957
gapminder_media <- gapminder_media %>% arrange(year) # ordenar segun año 1952 y luego 1957

#Ahora hay que sacar la media

gapminder_media <- gapminder_media %>% group_by(continent,year) %>%
  summarise(lifeExp_media = mean(lifeExp))
#1 Africa     1952          39.1
#2 Africa     1957          41.3
#3 Americas   1952          53.3
#4 Americas   1957          56.0
#5 Asia       1952          46.3
#6 Asia       1957          49.3
#7 Europe     1952          64.4
#8 Europe     1957          66.7
#9 Oceania    1952          69.3
#10 Oceania    1957          70.3

#3.¡Cuánto aumento la expectativa de vida al nacer por continente entre 2007 y 1952?
#4. ¿Cuál fue el país, por conntinente que más aumentó su expectativa de vida al nacer en términos absolutos?
#5. Entre 1952 y 2007¿Cuál fue el país que más aumento PIB per cápita?
#¿Y por contienente?
#6. ¿ Cuanto aumento el PIB per cápita de Argentina entre 1952 y 2007?
#¿Y entre 1977 y 2002?


  