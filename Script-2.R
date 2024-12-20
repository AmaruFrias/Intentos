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



                               
View(gapminder_df)
