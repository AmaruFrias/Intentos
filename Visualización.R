#Visualización 

gapminder_df <- read.table(file = "https://raw.githubusercontent.com/martintinch0/CienciaDeDatosParaCuriosos/master/data/gapminder.csv",
                           sep=';',
                           header = TRUE,
                           stringsAsFactors = FALSE)
gapminderLastCut <- gapminder_df %>% filter(year==2007)


library(pacman)
p_load("tidyverse",
       "esquisse")

esquisse::esquisser(gapminderLastCut)

ggplot(gapminderLastCut) +
  aes(x = gdpPercap, y = lifeExp) +
  geom_point(shape = "circle plus", colour = "#114640") +
  theme_minimal()

ggplot(gapminderLastCut) +
  aes(x = gdpPercap, y = lifeExp) +
  geom_point(colour = "#112446") +
  labs(
    x = "PIB per cápita",
    y = "Expectativa de vida al nacer (en años)",
    title = "A más ingresos mayor tiempo en la vida?",
    subtitle = "Expectativa de vida al nacer según nivel de ingreso",
    caption = "Fuente: Gapminder"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold",
                              hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )


