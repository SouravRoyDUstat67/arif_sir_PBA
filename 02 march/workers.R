library(tidyverse)
library(readxl)

home = read_excel("...\\GSS2018DataCorrected (2).xlsx")

home %>% select(SEX, HRS1) %>%
  filter(HRS1 >= 0 & HRS1 <= 89) -> t


men = t$HRS1[t$SEX == 1]
men %>% mean()
women = t$HRS1[t$SEX == 2]
women %>% mean()

var.test(men, women, alternative = "two.sided")

t.test(men, women, alternative = "greater", var.equal = F)


