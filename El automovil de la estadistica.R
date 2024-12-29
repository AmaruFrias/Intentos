#El automovil de la estadistica 

load(url('https://github.com/datalab-UTDT/GIS2/raw/master/Data/HowellData.RData'))
str(Howell1)


esquisse::esquisser(Howell1)
ggplot(Howell1) +
  aes(x = weight, y = height) +
  geom_point(colour = "#112446") +
  labs(x = "Peso (Kg)", y = "Altura (kg)") +
  ggthemes::theme_fivethirtyeight()

Howell1Adults <- Howell1 %>% filter(age>=18) #sacamos a los menores de 18 años 
regresion1 <- lm(data = Howell1Adults, formula = height ~ 1) #regresiom lineal

summary(regresion1)

mean(Howell1Adults$height)

regresion2 <- lm(data = Howell1Adults, formula = height ~ weight)
summary(regresion2)

ggplot(Howell1Adults) +
  geom_abline(slope = coef(regresion2)[2],intercept = coef(regresion2)[1]) +
  geom_point(aes(x=weight, y=height)) +
  theme_fivethirtyeight() +
  labs(x='Peso (kg)', y = 'Altura (kg)') +
  theme(axis.title = element_text(size=14))


#intervalos de confianza

confint(regresion2,level = 0.95)



set.seed(4)
samples <- c(1:1000)
weightCoefs <- c()
for(sample in samples) {
  indices <- sample.int(n = nrow(Howell1Adults),size = nrow(Howell1Adults),
                        replace = TRUE)
  weightCoefs<- c(weightCoefs,
                  coef(lm(data = Howell1Adults[indices,], formula = height ~ weight))[2])
}


sum(weightCoefs>=0.8223315 & weightCoefs<=0.9877267)/1000

quantile(weightCoefs,probs = c(0.025,0.975))

round(sum((predict(regresion2)-mean(regresion2$model$height))^2)/
        sum((regresion2$model$height-mean(regresion2$model$height))^2),2)

Howell1Adults$male <- factor(Howell1Adults$male)
summary(lm(data = Howell1Adults,
           formula = height ~ weight + male))
