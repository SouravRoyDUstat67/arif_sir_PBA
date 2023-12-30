library(tidyverse)

data = read.csv("C:\\Users\\Sourov\\OneDrive\\Desktop\\arif sir\\april 16\\country.csv") %>%
  na.omit()

names(data)

data = data %>% select(lifeexpf, urban, gdp, hospbed)

lm(lifeexpf~., data = data) -> model
model %>% summary

par(mfrow = c(2, 2))
plot(model)

car::vif(model)

data %>% pivot_longer(!lifeexpf) %>%
  mutate(value = log(value)) %>%
  ggplot(aes(x = lifeexpf, y = value))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~name, scales = "free")

plot(data$lifeexpf, data$urban)

data %>% pivot_longer(!lifeexpf)%>%
  ggplot(aes(y = value))+
  stat_boxplot()+
  facet_wrap(~name, scales = "free")+
  theme_minimal()+
  theme(axis.title =  element_text(face = "bold", size = 20),
        axis.text =  element_text(face = "bold", size = 15),
        strip.text = element_text(face = "bold", size = 15))

plot(model$fitted.values, model$residuals)
abline(h = 0)
lmtest::bptest(model)

wt <- 1 / lm(abs(model$residuals) ~ model$fitted.values)$fitted.values^2
wt

wls_model <- lm(lifeexpf ~ urban+gdp+hospbed, data = data, weights = wt)
wls_model %>% summary

plot(wls_model$fitted.values, wls_model$residuals)

lmtest::bptest(wls_model)
abline(h = 0)
