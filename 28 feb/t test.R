library(tidyverse)
library(readxl)

home = read_excel("...\\Homeland Security Federal Viewpoint Survey 2019 Subset.xlsx")

t = home %>% select(DLEAVING, Q49, Q70) %>%
  mutate(DL_cat = ifelse(DLEAVING == "A", "No", "Yes")) %>%
  na.omit()

t

# here I have created a new variable named DL_cat. DLEAVING variable has 4 categories,
# for the test purpose, I have convert it into 2 categories. A represents those
# who are not leaving their organization & B, C & D represents those who are
# switching their organization.


# For the first hypothesis

t$Q49[t$DL_cat == "No"] -> x
t$Q49[t$DL_cat == "Yes"] ->y

var(x); var(y)

var.test(x, y, alternative = "two.sided")

t.test(x, y, alternative = "greater", var.equal = F)

# For the second hypothesis

t$Q70[t$DL_cat == "No"] -> x
t$Q70[t$DL_cat == "Yes"] ->y

var.test(x, y, alternative = "two.sided")

t.test(x, y, alternative = "greater", var.equal = F)

