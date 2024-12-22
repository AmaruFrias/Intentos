gapminder_df <- read.table(file = "https://raw.githubusercontent.com/martintinch0/CienciaDeDatosParaCuriosos/master/data/gapminder.csv",
                           sep=';',
                           header = TRUE,
                           stringsAsFactors = FALSE)

#codigo para saber los nombres de las variable colnames("base de datos)

colnames(gapminder_df)
library("tidyverse")
library("esquisse")

grandminder_df <- gapminder_df %>% select(country)

gapminderCol <- gapminder_df %>% select(country) 
class(gapminderCol)


gapminder_df %>% filter(year == 2002) #año 2002

gapminder_df %>% filter(country == "Argentina", year == 2007)

gapminder_argentina <- gapminder_df %>% filter(country == "Argentina")


esquisse::esquisser(gapminder_argentina)
ggplot(gapminder_argentina) +
  aes(x = year, y = lifeExp) +
  geom_line(colour = "#4682B4") +
  ggthemes::theme_stata()

gapminder_Chile <- gapminder_df %>% filter(country == "Chile")

esquisse::esquisser(gapminder_Chile)



gapminder_df %>% filter(continent == "Americas", year == 2007) %>% arrange(desc(lifeExp))



# 2.3.4 Creando y modificando variables: mutate() Hasta aqui quedaste en libro men 


new_gapminder <- gapminder_df %>% mutate(pop = pop/1000000)
head(new_gapminder,n = 3)

gapminder_df %>% mutate(gdp = gdpPercap * pop)

#¿Cuál es la expectativa de vida al nacer promedio por continente para cada uno de los años?

gapminder_df %>% group_by(year,continent) %>% summarise(mean_lifeexp = mean(lifeExp))

# ¿Cuál es el país más pobre y más rico, medido por PIB per cápita, de cada continente para 
#cada uno de los años?

# Segunda pregunta
gapminder_df %>% group_by(year, continent) %>%
  summarise(poor_country = min(gdpPercap),
            rich_country = max(gdpPercap),
            poor_country_nom = country[gdpPercap == poor_country],
            rich_country_nom = country[gdpPercap == rich_country])

#¿Cuál es la diferencia en la expectativa de vida al nacer para cada país con respecto a la media 
#del continente para cada año?


gapminder_df <- gapminder_df %>% group_by(continent,year) %>% mutate(mean(lifeExp)) 
                               
esquisse::esquisser(gapminder_df)
ggplot(gapminder_df) +
  aes(
    x = year,
    y = `mean(lifeExp)`,
    colour = continent,
    group = continent
  ) +
  geom_line() +
  scale_color_hue(direction = 1) +
  labs(
    y = "Expectativa de vida al nacer",
    title = "Expectativa de vida al nacer (Promedio)",
    subtitle = "Promedio por continente",
    caption = "Fuente: elaboración propia con base en datos de Gapminder"
  ) +
  theme_minimal()


gapminder_sub <- gapminder_df %>% 
  select(country,year,pop) %>%
  mutate(pop = round(pop / 1000000,1)) 
head(gapminder_sub,n = 5) # Muestra las primeras cinco filas

masAncho <- gapminder_sub %>%
  pivot_wider(names_from = year,values_from = pop)
head(masAncho,6)


write.table(x = masAncho,
            file = 'PaisPob.csv',
            sep = ';',
            row.names=FALSE)


# El parámetro header sirve para avisar que los datos tienen una primera
# fila que es el nombre de las columnas y check.names es otro parámetro
# que es neceario cuando los nombres de las columnas son números
masAncho <- read.table(file = 'PaisPob.csv',
                       sep = ';',
                       header=TRUE,
                       check.names = FALSE,
                       stringsAsFactors = FALSE)
head(masAncho)


masLarga <- masAncho %>% pivot_longer(cols = starts_with(c("1","2")),
                                      names_to = "year",
                                      values_to = "pop" )


identical(gapminder_sub,reconstruccion)

class(gapminder_df$year)
class(masLarga$year)

masLarga <- masLarga %>% mutate(year= as.integer(year))

#https://martinmontane.github.io/CienciaDeDatosBook/transformando-nuestros-datos-data-wrangling.html