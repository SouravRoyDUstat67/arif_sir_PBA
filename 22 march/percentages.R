library(tidyverse)
library(readxl)

data = read_excel("C:\\Users\\Sourov\\OneDrive\\Desktop\\arif sir\\22 march\\SPPS OtherStatePP.xlsx")

data %>% select(`Thought-InPA-SAA`) %>%
  mutate(`Thought-InPA-SAA` = as.character(`Thought-InPA-SAA`)) %>%
  mutate(x = 1) %>%
  group_by(`Thought-InPA-SAA`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  ggplot(aes(x = `Thought-InPA-SAA`, y = prop, label = label))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.01, size = 5)+
  scale_y_continuous(labels = scales::percent_format())+
  labs(y = "Percentages(%)")+
  theme_minimal()+
  theme(axis.text = element_text(angle = 0, size = 15),
        axis.title = element_text(face = "bold", size = 15))


data %>% select(`Willing-InPA-SAA`) %>%
  mutate(`Willing-InPA-SAA` = as.character(`Willing-InPA-SAA`)) %>%
  mutate(x = 1) %>%
  group_by(`Willing-InPA-SAA`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  ggplot(aes(x = `Willing-InPA-SAA`, y = prop, label = label))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.03, size = 5)+
  scale_y_continuous(labels = scales::percent_format())+
  labs(y = "Percentages(%)")+
  theme_minimal()+
  theme(axis.text = element_text(angle = 0, size = 15),
        axis.title = element_text(face = "bold", size = 15))


data %>% select(`Intent-InPA-SAA-5`) %>%
  mutate(`Intent-InPA-SAA-5` = as.character(`Intent-InPA-SAA-5`)) %>%
  mutate(x = 1) %>%
  group_by(`Intent-InPA-SAA-5`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  ggplot(aes(x = `Intent-InPA-SAA-5`, y = prop, label = label))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.03, size = 5)+
  labs(y = "Percentages(%)")+
  theme_minimal()+
  theme(axis.text = element_text(angle = 0, size = 15),
        axis.title = element_text(face = "bold", size = 15))


data %>% select(`Intent-InPA-SAA-Life`) %>%
  mutate(`Intent-InPA-SAA-Life` = as.character(`Intent-InPA-SAA-Life`)) %>%
  mutate(x = 1) %>%
  group_by(`Intent-InPA-SAA-Life`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  ggplot(aes(x = `Intent-InPA-SAA-Life`, y = prop, label = label))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.03, size = 5)+
  scale_y_continuous(label = scales::percent_format())+
  labs(y = "Percentages(%)")+
  theme_minimal()+
  theme(axis.text = element_text(angle = 0, size = 15),
        axis.title = element_text(face = "bold", size = 15))

data %>% select(`PP-Employ1`) %>%
  mutate(`PP-Employ1` = as.character(`PP-Employ1`)) %>%
  mutate(x = 1) %>%
  group_by(`PP-Employ1`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  ggplot(aes(x = `PP-Employ1`, y = prop, label = label))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.02, size = 5)+
  scale_y_continuous(labels = scales::percent_format())+
  labs(y = "Percentages(%)")+
  theme_minimal()+
  theme(axis.text = element_text(angle = 0, size = 15),
        axis.title = element_text(face = "bold", size = 15))

data %>% select(`PP-Employ2`) %>%
  mutate(`PP-Employ2` = as.character(`PP-Employ2`)) %>%
  mutate(x = 1) %>%
  group_by(`PP-Employ2`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  ggplot(aes(x = `PP-Employ2`, y = prop, label = label))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.02, size = 5)+
  scale_y_continuous(labels = scales::percent_format())+
  labs(y = "Percentages(%)")+
  theme_minimal()+
  theme(axis.text = element_text(angle = 0, size = 15),
        axis.title = element_text(face = "bold", size = 15))


data %>% select(`PP-Employ3`) %>%
  mutate(`PP-Employ3` = as.character(`PP-Employ3`)) %>%
  mutate(x = 1) %>%
  group_by(`PP-Employ3`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  ggplot(aes(x = `PP-Employ3`, y = prop, label = label))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.01, size = 5)+
  scale_y_continuous(labels = scales::percent_format())+
  labs(y = "Percentages(%)")+
  theme_minimal()+
  theme(axis.text = element_text(angle = 0, size = 15),
        axis.title = element_text(face = "bold", size = 15))


data %>% select(`PP-Housing`) %>%
  mutate(`PP-Housing` = as.character(`PP-Housing`)) %>%
  mutate(x = 1) %>%
  group_by(`PP-Housing`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  ggplot(aes(x = `PP-Housing`, y = prop, label = label))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.01, size = 5)+
  scale_y_continuous(labels = scales::percent_format())+
  labs(y = "Percentages(%)")+
  theme_minimal()+
  theme(axis.text = element_text(angle = 0, size = 15),
        axis.title = element_text(face = "bold", size = 15))

data %>% select(`PP-CostsLiving1`) %>%
  mutate(`PP-CostsLiving1` = as.character(`PP-CostsLiving1`)) %>%
  mutate(x = 1) %>%
  group_by(`PP-CostsLiving1`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  ggplot(aes(x = `PP-CostsLiving1`, y = prop, label = label))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.01, size = 5)+
  scale_y_continuous(labels = scales::percent_format())+
  labs(y = "Percentages(%)")+
  theme_minimal()+
  theme(axis.text = element_text(angle = 0, size = 15),
        axis.title = element_text(face = "bold", size = 15))


data %>% select(`PP-Education1`) %>%
  mutate(`PP-Education1` = as.character(`PP-Education1`)) %>%
  mutate(x = 1) %>%
  group_by(`PP-Education1`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  ggplot(aes(x = `PP-Education1`, y = prop, label = label))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.01, size = 5)+
  scale_y_continuous(labels = scales::percent_format())+
  labs(y = "Percentages(%)")+
  theme_minimal()+
  theme(axis.text = element_text(angle = 0, size = 15),
        axis.title = element_text(face = "bold", size = 15))


data %>% select(`PP-Education2`) %>%
  mutate(`PP-Education2` = as.character(`PP-Education2`)) %>%
  mutate(x = 1) %>%
  group_by(`PP-Education2`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  ggplot(aes(x = `PP-Education2`, y = prop, label = label))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.01, size = 5)+
  scale_y_continuous(labels = scales::percent_format())+
  labs(y = "Percentages(%)")+
  theme_minimal()+
  theme(axis.text = element_text(angle = 0, size = 15),
        axis.title = element_text(face = "bold", size = 15))


data %>% select(`PP-Healthcare1`) %>%
  mutate(`PP-Healthcare1` = as.character(`PP-Healthcare1`)) %>%
  mutate(x = 1) %>%
  group_by(`PP-Healthcare1`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  ggplot(aes(x = `PP-Healthcare1`, y = prop, label = label))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.01, size = 5)+
  scale_y_continuous(labels = scales::percent_format())+
  labs(y = "Percentages(%)")+
  theme_minimal()+
  theme(axis.text = element_text(angle = 0, size = 15),
        axis.title = element_text(face = "bold", size = 15))

data %>% select(`PP-Healthcare2`) %>%
  mutate(`PP-Healthcare2` = as.character(`PP-Healthcare2`)) %>%
  mutate(x = 1) %>%
  group_by(`PP-Healthcare2`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  ggplot(aes(x = `PP-Healthcare2`, y = prop, label = label))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.01, size = 5)+
  scale_y_continuous(labels = scales::percent_format())+
  labs(y = "Percentages(%)")+
  theme_minimal()+
  theme(axis.text = element_text(angle = 0, size = 15),
        axis.title = element_text(face = "bold", size = 15))


data %>% select(`PP-Arts`) %>%
  mutate(`PP-Arts` = as.character(`PP-Arts`)) %>%
  mutate(x = 1) %>%
  group_by(`PP-Arts`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  ggplot(aes(x = `PP-Arts`, y = prop, label = label))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.01, size = 5)+
  scale_y_continuous(labels = scales::percent_format())+
  labs(y = "Percentages(%)")+
  theme_minimal()+
  theme(axis.text = element_text(angle = 0, size = 15),
        axis.title = element_text(face = "bold", size = 15))

data %>% select(`PP-Sports-Leisure`) %>%
  mutate(`PP-Sports-Leisure` = as.character(`PP-Sports-Leisure`)) %>%
  mutate(x = 1) %>%
  group_by(`PP-Sports-Leisure`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  ggplot(aes(x = `PP-Sports-Leisure`, y = prop, label = label))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.01, size = 5)+
  scale_y_continuous(labels = scales::percent_format())+
  labs(y = "Percentages(%)")+
  theme_minimal()+
  theme(axis.text = element_text(angle = 0, size = 15),
        axis.title = element_text(face = "bold", size = 15))

data %>% select(`PP-Food`) %>%
  mutate(`PP-Food` = as.character(`PP-Food`)) %>%
  mutate(x = 1) %>%
  group_by(`PP-Food`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  ggplot(aes(x = `PP-Food`, y = prop, label = label))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.01, size = 5)+
  scale_y_continuous(labels = scales::percent_format())+
  labs(y = "Percentages(%)")+
  theme_minimal()+
  theme(axis.text = element_text(angle = 0, size = 15),
        axis.title = element_text(face = "bold", size = 15))

data %>% select(`PP-Outdoor`) %>%
  mutate(`PP-Outdoor` = as.character(`PP-Outdoor`)) %>%
  mutate(x = 1) %>%
  group_by(`PP-Outdoor`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  ggplot(aes(x = `PP-Outdoor`, y = prop, label = label))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.01, size = 5)+
  scale_y_continuous(labels = scales::percent_format())+
  labs(y = "Percentages(%)")+
  theme_minimal()+
  theme(axis.text = element_text(angle = 0, size = 15),
        axis.title = element_text(face = "bold", size = 15))

data %>% select(`PP-Diversity`) %>%
  mutate(`PP-Diversity` = as.character(`PP-Diversity`)) %>%
  mutate(x = 1) %>%
  group_by(`PP-Diversity`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  ggplot(aes(x = `PP-Diversity`, y = prop, label = label))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.01, size = 5)+
  scale_y_continuous(labels = scales::percent_format())+
  labs(y = "Percentages(%)")+
  theme_minimal()+
  theme(axis.text = element_text(angle = 0, size = 15),
        axis.title = element_text(face = "bold", size = 15))


data %>% select(`PP-Family`) %>%
  mutate(`PP-Family` = as.character(`PP-Family`)) %>%
  mutate(x = 1) %>%
  group_by(`PP-Family`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  ggplot(aes(x = `PP-Family`, y = prop, label = label))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.01, size = 5)+
  scale_y_continuous(labels = scales::percent_format())+
  labs(y = "Percentages(%)")+
  theme_minimal()+
  theme(axis.text = element_text(angle = 0, size = 15),
        axis.title = element_text(face = "bold", size = 15))


data %>% select(`PP-RelaxLifePace`) %>%
  mutate(`PP-RelaxLifePace` = as.character(`PP-RelaxLifePace`)) %>%
  mutate(x = 1) %>%
  group_by(`PP-RelaxLifePace`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  ggplot(aes(x = `PP-RelaxLifePace`, y = prop, label = label))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.01, size = 5)+
  scale_y_continuous(labels = scales::percent_format())+
  labs(y = "Percentages(%)")+
  theme_minimal()+
  theme(axis.text = element_text(angle = 0, size = 15),
        axis.title = element_text(face = "bold", size = 15))

data %>% select(`PP-Community`) %>%
  mutate(`PP-Community` = as.character(`PP-Community`)) %>%
  mutate(x = 1) %>%
  group_by(`PP-Community`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  ggplot(aes(x = `PP-Community`, y = prop, label = label))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.01, size = 5)+
  scale_y_continuous(labels = scales::percent_format())+
  labs(y = "Percentages(%)")+
  theme_minimal()+
  theme(axis.text = element_text(angle = 0, size = 15),
        axis.title = element_text(face = "bold", size = 15))


data %>% select(`PP-PublicServices`) %>%
  mutate(`PP-PublicServices` = as.character(`PP-PublicServices`)) %>%
  mutate(x = 1) %>%
  group_by(`PP-PublicServices`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  ggplot(aes(x = `PP-PublicServices`, y = prop, label = label))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.01, size = 5)+
  scale_y_continuous(labels = scales::percent_format())+
  labs(y = "Percentages(%)")+
  theme_minimal()+
  theme(axis.text = element_text(angle = 0, size = 15),
        axis.title = element_text(face = "bold", size = 15))


data %>% select(`PP-Internet`) %>%
  mutate(`PP-Internet` = as.character(`PP-Internet`)) %>%
  mutate(x = 1) %>%
  group_by(`PP-Internet`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  ggplot(aes(x = `PP-Internet`, y = prop, label = label))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.01, size = 5)+
  scale_y_continuous(labels = scales::percent_format())+
  labs(y = "Percentages(%)")+
  theme_minimal()+
  theme(axis.text = element_text(angle = 0, size = 15),
        axis.title = element_text(face = "bold", size = 15))

data %>% select(`PP-Civic`) %>%
  mutate(`PP-Civic` = as.character(`PP-Civic`)) %>%
  mutate(x = 1) %>%
  group_by(`PP-Civic`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  ggplot(aes(x = `PP-Civic`, y = prop, label = label))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.01, size = 5)+
  scale_y_continuous(labels = scales::percent_format())+
  labs(y = "Percentages(%)")+
  theme_minimal()+
  theme(axis.text = element_text(angle = 0, size = 15),
        axis.title = element_text(face = "bold", size = 15))

data %>% select(`PP-Commute`) %>%
  mutate(`PP-Commute` = as.character(`PP-Commute`)) %>%
  mutate(x = 1) %>%
  group_by(`PP-Commute`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  ggplot(aes(x = `PP-Commute`, y = prop, label = label))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.01, size = 5)+
  scale_y_continuous(labels = scales::percent_format())+
  labs(y = "Percentages(%)")+
  theme_minimal()+
  theme(axis.text = element_text(angle = 0, size = 15),
        axis.title = element_text(face = "bold", size = 15))


