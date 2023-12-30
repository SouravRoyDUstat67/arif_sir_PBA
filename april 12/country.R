library(tidyverse)

data = read.csv("C:/Users/Sourov/OneDrive/Desktop/arif sir/april 12/country.csv")

names(data)

data %>% select(urban, gdp, radio, phone, hospbed, docs, lifeexpf) %>% na.omit() -> lifexpf

lm(lifeexpf~., data = lifexpf) -> model
model %>% summary

car::vif(model)

lifexpf %>% pivot_longer(!lifeexpf) %>% 
  ggplot(aes(x = lifeexpf, y = value))+
  geom_point()+
  geom_smooth(se = F)+
  facet_wrap(~name, scales = "free")+
  theme_minimal()+
  theme(axis.title =  element_text(face = "bold", size = 20),
        axis.text =  element_text(face = "bold", size = 15),
        strip.text = element_text(face = "bold", size = 15))

hist(lifexpf$phone, probability = T)
lines(density(lifexpf$lifeexpf))

par(mfrow = c(1, 2))
hist(model$residuals, prob = T)
lines(density(model$residuals))

qqnorm(model$residuals)
qqline(model$residuals)
########  Correction of Multicolinearity -------

lifexpf[, -7] %>% cor()

lm(lifeexpf~urban+log(radio)+log(hospbed)+docs, data = lifexpf) -> model
model %>% summary
car::vif(model)

lm(lifeexpf~log(urban)+log(radio)+log(hospbed)+docs, data = lifexpf) -> model
model %>% summary
car::vif(model)

hist(model$residuals)
qqnorm(model$residuals)
qqline(model$residuals)
