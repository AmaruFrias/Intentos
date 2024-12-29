#¿Podemos predecir quién se murió en el titanic?

titanic <- read.csv(file = "https://raw.githubusercontent.com/martintinch0/CienciaDeDatosParaCuriosos/master/data/titanic.csv",
                    stringsAsFactors = FALSE,
                    sep = ';')

#¿Podemos crear un árbol de decisión que nos permita predecir quienes sobrevivieron y quienes no en base a variables 
#como su edad, género y clase en la que viajaron? 

p_load("C50")

glimpse(titanic)

class(titanic$survived)

titanic <- titanic %>%
  mutate(survived = factor(survived),
         sex = factor(sex))
class(titanic$survived)

primerArbol <- C5.0(formula= survived ~.,
                    data = titanic)

plot(primerArbol)

#Primer arbol