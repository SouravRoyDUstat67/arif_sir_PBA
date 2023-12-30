library(tidyverse)

pen = readxl::read_excel("C:\\Users\\Sourov\\OneDrive\\Desktop\\arif sir\\08 march\\pennsylvania\\Pennsylvania-Demographics.xlsx")

pen = pen[-1, ]

pen %>% select(`CLS State-County_2`) %>%
  mutate(x = 1) %>%
  group_by(`CLS State-County_2`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = round(prop.table(x), 3), label = scales::percent(prop)) %>%
  top_n(20) %>%
  ggplot(aes(x = reorder(`CLS State-County_2`, -prop), y = prop))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), size = 3, nudge_y = 0.005)+
  scale_y_continuous(labels = scales::percent_format())+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90),
        axis.title = element_text(face = "bold", size = 15))+
  labs(x = "County", y = "Percentage(%)")


pen %>% select(`CLS-DefineLive`) %>%
  mutate(x = 1) %>%
  group_by(`CLS-DefineLive`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  ggplot(aes(x = reorder(`CLS-DefineLive`, -prop), y = prop))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.01)+
  scale_y_continuous(labels = scales::percent_format())+
  labs(x = "Residence Type", y = "Percentage(%)")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90),
        axis.title = element_text(face = "bold", size = 15))


pen %>% select(`Demo-Gender`) %>%
  mutate(x = 1) %>%
  group_by(`Demo-Gender`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  ggplot(aes(x = `Demo-Gender`, y = prop))+
  geom_bar(stat = "identity", fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.02)+
  scale_y_continuous(labels = scales::percent_format())+
  labs(x = "Gender", y = "Percentage(%)")+
  theme_minimal()+
  theme(axis.title = element_text(face = "bold", size = 15))


pen %>% select(`Demo-Race`) %>%
  mutate(x = 1) %>%
  group_by(`Demo-Race`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  mutate(Race = word(`Demo-Race`, 1)) %>%
  ggplot(aes(x = reorder(Race, -prop), y = prop))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.02)+
  scale_y_continuous(labels = scales::percent_format())+
  labs(x = "Race", y = "Percentage(%)")+
  theme_minimal()+
  theme(axis.title = element_text(face = "bold", size = 15))


pen %>% select(`Demo-Education`) %>%
  mutate(x = 1) %>%
  group_by(`Demo-Education`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  mutate(Education = word(`Demo-Education`, 1, 2)) %>%
  ggplot(aes(x = reorder(Education, -prop), y = prop))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.02)+
  scale_y_continuous(labels = scales::percent_format())+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90),
        axis.title = element_text(face = "bold", size = 15))+
  labs(x = "Education", y = "Percentage(%)")


pen %>% select(`Demo-Marital`) %>%
  mutate(x = 1) %>%
  group_by(`Demo-Marital`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  mutate(Married = word(`Demo-Marital`, 1)) %>%
  ggplot(aes(x = reorder(Married, -prop), y= prop))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.02)+
  scale_y_continuous(labels = scales::percent_format())+
  labs(x = "Marrital Status", y = "Percentage(%)")+
  theme_minimal()+
  theme(axis.title = element_text(face = "bold", size = 15))


pen %>% select(`Demo-HouseholdSize`) %>%
  mutate(x = 1) %>%
  group_by(`Demo-HouseholdSize`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  ggplot(aes(x = `Demo-HouseholdSize`, y = prop))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.01)+
  scale_y_continuous(labels = scales::percent_format())+
  labs(x = "Household Size", y = "Percentage(%)")+
  theme_minimal()+
  theme(axis.title = element_text(face = "bold", size = 15))


pen %>% select(`Demo-HouseholdChild`) %>%
  mutate(x = 1) %>%
  group_by(`Demo-HouseholdChild`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  ggplot(aes(x = reorder(`Demo-HouseholdChild`, -prop), y = prop))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.02)+
  scale_y_continuous(label = scales::percent_format())+
  labs(x = "Household Child", y = "Percentage(%)")+
  theme_minimal()+
  theme(axis.title = element_text(face = "bold", size = 15))


pen %>% select(`Demo-RentOwn`) %>%
  mutate(x = 1) %>%
  group_by(`Demo-RentOwn`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  ggplot(aes(x = reorder(`Demo-RentOwn`, -prop), y = prop))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.01)+
  scale_y_continuous(labels = scales::percent_format())+
  labs(x = "Rent Type", y = "Percentage(%)")+
  theme_minimal()+
  theme(axis.title = element_text(face = "bold", size = 15))


pen %>% select(`Demo-Income`) %>%
  mutate(x = 1) %>%
  group_by(`Demo-Income`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  ggplot(aes(x = reorder(`Demo-Income`, -prop), y = prop))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.01)+
  scale_y_continuous(labels = scales::percent_format())+
  labs(x = "Income", y = "Percentage(%)")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90),
        axis.title = element_text(face = "bold", size = 15))


pen %>% select(`Demo-StudentDebt`) %>%
  mutate(x = 1) %>%
  group_by(`Demo-StudentDebt`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  ggplot(aes(x = reorder(`Demo-StudentDebt`, -prop), y = prop))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.02)+
  scale_y_continuous(labels = scales::percent_format())+
  labs(x = "Student Debt", y = "Percentage(%)")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90),
        axis.title = element_text(face = "bold", size = 15))


pen %>% select(`Demo-PoliticalParty`) %>%
  mutate(x = 1) %>%
  group_by(`Demo-PoliticalParty`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  ggplot(aes(x = reorder(`Demo-PoliticalParty`, -prop), y = prop))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.015)+
  labs(x = "Political Party", y = "Percentage(%)")+
  scale_y_continuous(labels = scales::percent_format())+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 0),
        axis.title = element_text(face = "bold", size = 15))


pen %>% select(`Demo-PoliticalViews`) %>%
  mutate(x = 1) %>%
  group_by(`Demo-PoliticalViews`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  ggplot(aes(x = reorder(`Demo-PoliticalViews`, -prop), y = prop))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.01)+
  scale_y_continuous(labels = scales::percent_format())+
  labs(x = "Political Views", y = "Percentage(%)")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 0),
        axis.title = element_text(face = "bold", size = 15))


pen %>% select(`Demo-EmployStatus`) %>%
  mutate(x = 1) %>%
  group_by(`Demo-EmployStatus`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  ggplot(aes(x = reorder(`Demo-EmployStatus`, -prop), y = prop))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.03)+
  scale_y_continuous(labels = scales::percent_format())+
  labs(x = "Employment Status", y = "Percentage(%)")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = .75, vjust = .9),
        axis.title = element_text(face = "bold", size = 15))


pen %>% select(`Demo-EmployType`) %>%
  mutate(x = 1) %>%
  group_by(`Demo-EmployType`) %>%
  summarise(x = sum(x)) %>%
  na.omit() %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  ggplot(aes(x = reorder(`Demo-EmployType`, -prop), y = prop))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.02)+
  scale_y_continuous(labels = scales::percent_format())+
  labs(x = "Employment type", y = "Percentage(%)")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90),
        axis.title = element_text(face = "bold", size = 15))


pen %>% select(`Demo-EmployLocation`) %>%
  mutate(x = 1) %>%
  group_by(`Demo-EmployLocation`) %>%
  summarise(x = sum(x)) %>%
  na.omit() %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  ggplot(aes(x = reorder(`Demo-EmployLocation`, -prop), y = prop))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.02)+
  scale_y_continuous(labels = scales::percent_format())+
  labs(x = "Employment Location", y = "Percentage(%)")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90),
        axis.title = element_text(face = "bold", size = 15))


pen %>% select(`Demo-EmployRemote`) %>%
  mutate(x = 1) %>%
  group_by(`Demo-EmployRemote`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  ggplot(aes(x = reorder(`Demo-EmployRemote`, -prop), y = prop))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.01)+
  scale_y_continuous(labels = scales::percent_format())+
  labs(x = "Employment Remote Job", y = "Percentage(%)")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90),
        axis.title = element_text(face = "bold", size = 15))



