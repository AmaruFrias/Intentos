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


#en veldad en veldad, VAMOS QUE SE PUEEEEEE LOCOOOTRON