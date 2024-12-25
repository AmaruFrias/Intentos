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
  aes(x = gdpPercap, y = lifeExp, colour = continent) +
  geom_point() +
  scale_color_hue(direction = 1) +
  labs(
    x = "PIB per cápita",
    y = "Expectativa de vida al nacer (en años)",
    title = "A más ingresos mayor tiempo de vida?",
    subtitle = "Expectativa de al nacer según nivel de ingreso",
    caption = "Fuente: Gapminder",
    color = "Continentes "
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold",
                              hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )


meanlivexpcontinent <- gapminder_df %>% group_by(continent,year) %>%
  summarise(MediaExp = mean(lifeExp))

esquisse::esquisser(gapminderLastCut)

ggplot(gapminderLastCut) +
  aes(
    x = gdpPercap,
    y = lifeExp,
    colour = continent,
    size = pop
  ) +
  geom_point(shape = "circle small") +
  scale_color_manual(
    values = c(Africa = "#F8766D",
               Americas = "#0F36A4",
               Asia = "#1B9E4F",
               Europe = "#9D3BC8",
               Oceania = "#DCDE5A")
  ) +
  scale_x_continuous(trans = "log10") +
  theme_minimal() +
  theme(legend.position = "bottom")

install.packages("gganimate")
library(gganimate)


esquisse::esquisser(gapminder_df)

library(gganimate)
gapminderAnim <- ggplot(data = gapminder_df,
                        mapping = aes(x = gdpPercap,
                                      y = lifeExp,
                                      color=continent,
                                      size=pop)) +
  geom_point() +
  guides(size=FALSE) +
  theme_minimal() +
  scale_x_continuous(trans = 'log10',breaks = c(1000,10000,70000)) +
  scale_color_manual(breaks=c("Europe","Asia","Oceania","Africa","Americas"),
                     values = c("#E41A1C","#377EB8","#4DAF4A" ,"#984EA3","#FF7F00")) + 
  labs(x = "PIB per cápita",
       y = "Expectativa de vida al nacer (en años)",
       title="Año: {frame_time}",
       subtitle="Expectativa de vida al nacer según nivel de ingreso",
       color="Continente",
       caption="Fuente: Gapminder") +
  transition_time(year) + 
  ease_aes('linear')

view(gapminderAnim)

