library(tidyverse)

data = foreign::read.spss("C:\\Users\\Sourov\\OneDrive\\Desktop\\arif sir\\april 28\\cleaned.sav", to.data.frame = T)

data %>% dim()

data %>% select(age) %>%
  ggplot(aes(x = age))+
  geom_histogram(bins = 25, fill = "cornflowerblue", color = "black", aes(y = ..density..))+
  geom_density(size = 1)+
  scale_y_continuous(labels = scales::percent_format())+
  labs(x = "Age", y = "Percentage")+
  theme_minimal()+
  theme(axis.text = element_text(angle = 0, size = 15),
        axis.title = element_text(face = "bold", size = 15))


data %>% select(sex) %>% na.omit() %>% 
  mutate(sex = ifelse(sex == 1, "Male", "Female")) %>%
  group_by(sex) %>%
  summarise(count = n()) %>%
  mutate(frac = count/sum(count), ymax = cumsum(frac), ymin = c(0, head(ymax, -1)),
         labelPosition = (ymax + ymin)/2, label = scales::percent(frac)) %>%
  ggplot(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin =3, fill = sex))+
  geom_rect()+
  coord_polar(theta = "y")+
#  xlim(c(2, 4))+
  geom_label(x = 3.5, aes(y = labelPosition, label = label), size = 5)+
#  scale_fill_brewer(palette = 1)+
  theme_void()

data %>% select(race) %>%
  group_by(race) %>%
  summarise(count = n()) %>% na.omit() %>%
  filter(race != ".x") %>%
  mutate(p = count/sum(count), label = scales::percent(p)) %>%
  ggplot(aes(x = race, y = count))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 1500, size = 5)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, size = 15),
        axis.text.y = element_text(angle = 0, size = 15),
        axis.title = element_text(face = "bold", size = 15))

data %>% select(degree) %>%
  group_by(degree) %>%
  summarise(count = n()) %>% na.omit() %>%
  mutate(p = count/sum(count), label = scales::percent(p)) %>%
  ggplot(aes(x = degree, y = count))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 1500, size = 5)+
  scale_x_discrete(label = c("Less than High School", "High School", "Junior College",
                             "Bachelor's", "Graduate"))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, size = 15),
        axis.text.y = element_text(angle = 0, size = 15),
        axis.title = element_text(face = "bold", size = 15))+
  coord_flip()

data %>% select(satjob) %>%
  group_by(satjob) %>%
  summarise(count = n()) %>% na.omit() %>%
  filter(satjob != ".x" & satjob != "  ") %>%
  mutate(p = count/sum(count), label = scales::percent(p)) %>%
  ggplot(aes(x = satjob, y = count))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 1000, size = 5)+
  labs(x = "Job satisfaction")+
  scale_x_discrete(label = c("Very Satisfied", "Moderately Satisfied", "Slightly Satisfied", "Very Dissatisfied"))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, size = 15),
        axis.text.y = element_text(angle = 0, size = 15),
        axis.title = element_text(face = "bold", size = 15))+
  coord_flip()





