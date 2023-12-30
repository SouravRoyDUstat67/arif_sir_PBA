library(tidyverse)

data = readxl::read_excel("C:\\Users\\Sourov\\OneDrive\\Desktop\\arif sir\\08 march\\other\\OtherState-Demographics.xlsx")

data = data[-1, ]

data %>% select(`CLS State-County_1`) %>%
  mutate(x = 1) %>%
  group_by(`CLS State-County_1`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  ggplot(aes(x = reorder(`CLS State-County_1`, -prop), y = prop))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.01)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45),
        axis.title = element_text(face = "bold", size = 12))+
  labs(x = "States", y = "Percentage(%)")


data %>% select(`CLS State-County_2`) %>%
  mutate(x = 1) %>%
  group_by(`CLS State-County_2`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = round(prop.table(x), 3), label = scales::percent(prop)) %>%
  top_n(20) %>%
  mutate(County = word(`CLS State-County_2`, 1)) %>%
  ggplot(aes(x = reorder(County, -prop), y = prop))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), size = 3, nudge_y = .002)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 3))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90),
        axis.title = element_text(face = "bold", size = 15))+
  labs(x = "County", y = "Percentage(%)")

data %>% select(`Demo-Gender`) %>%
  mutate(x = 1) %>%
  group_by(`Demo-Gender`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  ggplot(aes(x = `Demo-Gender`, y = prop))+
  geom_bar(stat = "identity", fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.02)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = "Gender", y = "Percentage(%)")+
  theme_minimal()+
  theme(axis.title = element_text(face = "bold", size = 15),
        axis.text.x = element_text(size = 10))


data %>% select(`Demo-Race`) %>%
  mutate(x = 1) %>%
  group_by(`Demo-Race`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  mutate(Race = word(`Demo-Race`, 1)) %>%
  ggplot(aes(x = reorder(`Demo-Race`, -prop), y = prop))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.02)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = "Race", y = "Percentage(%)")+
  theme_minimal()+
  theme(axis.title = element_text(face = "bold", size = 15),
        axis.text.x = element_text(angle = 45, vjust = .7))


data %>% select(`Demo-Education`) %>%
  mutate(x = 1) %>%
  group_by(`Demo-Education`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  mutate(Education = word(`Demo-Education`, 1, 2)) %>%
  ggplot(aes(x = reorder(Education, -prop), y = prop))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.02)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90),
        axis.title = element_text(face = "bold", size = 15))+
  labs(x = "Education", y = "Percentage(%)")


data %>% select(`Demo-Marital`) %>%
  mutate(x = 1) %>%
  group_by(`Demo-Marital`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  mutate(Married = word(`Demo-Marital`, 1)) %>%
  ggplot(aes(x = reorder(Married, -prop), y= prop))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.02)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = "Marrital Status", y = "Percentage(%)")+
  theme_minimal()+
  theme(axis.title = element_text(face = "bold", size = 15))


data %>% select(`Demo-HouseholdSize`) %>%
  mutate(x = 1) %>%
  group_by(`Demo-HouseholdSize`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  ggplot(aes(x = `Demo-HouseholdSize`, y = prop))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.01)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = "Household Size", y = "Percentage(%)")+
  theme_minimal()+
  theme(axis.title = element_text(face = "bold", size = 15))


data %>% select(`Demo-HouseholdChild`) %>%
  mutate(x = 1) %>%
  group_by(`Demo-HouseholdChild`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = prop.table(x), label = scales::percent(prop)) %>%
  ggplot(aes(x = reorder(`Demo-HouseholdChild`, -prop), y = prop))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.01)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = "Household Child", y = "Percentage(%)")+
  theme_minimal()+
  theme(axis.title = element_text(face = "bold", size = 15))


data %>% select(`Demo-RentOwn`) %>%
  mutate(x = 1) %>%
  group_by(`Demo-RentOwn`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = round(prop.table(x), 3), label = scales::percent(prop)) %>%
  ggplot(aes(x = reorder(`Demo-RentOwn`, -prop), y = prop))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.01)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = "Rent Type", y = "Percentage(%)")+
  theme_minimal()+
  theme(axis.title = element_text(face = "bold", size = 15))


data %>% select(`Demo-Income`) %>%
  mutate(x = 1) %>%
  group_by(`Demo-Income`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = round(prop.table(x), 3), label = scales::percent(prop)) %>%
  ggplot(aes(x = reorder(`Demo-Income`, -prop), y = prop))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.01)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = "Income", y = "Count")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90),
        axis.title = element_text(face = "bold", size = 15))


data %>% select(`Demo-StudentDebt`) %>%
  mutate(x = 1) %>%
  group_by(`Demo-StudentDebt`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = round(prop.table(x), 3), label = scales::percent(prop)) %>%
  ggplot(aes(x = reorder(`Demo-StudentDebt`, -prop), y = prop))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.01)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = "Student Debt", y = "Percentage(%)")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90),
        axis.title = element_text(face = "bold", size = 15))


data %>% select(`Demo-PoliticalParty`) %>%
  mutate(x = 1) %>%
  group_by(`Demo-PoliticalParty`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = round(prop.table(x), 3), label = scales::percent(prop)) %>%
  ggplot(aes(x = reorder(`Demo-PoliticalParty`, -prop), y = prop))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.01)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = "Political Party", y = "Percentage(%)")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 0),
        axis.title = element_text(face = "bold", size = 15))


data %>% select(`Demo-PoliticalViews`) %>%
  mutate(x = 1) %>%
  group_by(`Demo-PoliticalViews`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = round(prop.table(x), 3), label = scales::percent(prop)) %>%
  ggplot(aes(x = reorder(`Demo-PoliticalViews`, -prop), y = prop))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.01)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = "Political Views", y = "Percentage(%)")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 0),
        axis.title = element_text(face = "bold", size = 15))


data %>% select(`Demo-EmployStatus`) %>%
  mutate(x = 1) %>%
  group_by(`Demo-EmployStatus`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = round(prop.table(x), 3), label = scales::percent(prop)) %>%
  ggplot(aes(x = reorder(`Demo-EmployStatus`, -prop), y = prop))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.05)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = "Employment Status", y = "Percentage(%)")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = .75),
        axis.title = element_text(face = "bold", size = 15))


data %>% select(`Demo-EmployType`) %>%
  mutate(x = 1) %>%
  group_by(`Demo-EmployType`) %>%
  summarise(x = sum(x)) %>%
  na.omit() %>%
  mutate(prop = round(prop.table(x), 3), label = scales::percent(prop)) %>%
  ggplot(aes(x = reorder(`Demo-EmployType`, -prop), y = prop))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.03)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = "Employment type", y = "Percentage(%)")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90),
        axis.title = element_text(face = "bold", size = 15))


data %>% select(`Demo-EmployLocation`) %>%
  mutate(x = 1) %>%
  group_by(`Demo-EmployLocation`) %>%
  summarise(x = sum(x)) %>%
  na.omit() %>%
  mutate(prop = round(prop.table(x), 3), label = scales::percent(prop)) %>%
  ggplot(aes(x = reorder(`Demo-EmployLocation`, -prop), y = prop))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.03)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = "Employment Location", y = "Percentage(%)")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90),
        axis.title = element_text(face = "bold", size = 15))


data %>% select(`Demo-EmployRemote`) %>%
  mutate(x = 1) %>%
  group_by(`Demo-EmployRemote`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = round(prop.table(x), 3), label = scales::percent(prop)) %>%
  ggplot(aes(x = reorder(`Demo-EmployRemote`, -prop), y = prop))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.01)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = "Employment Remote Job", y = "Percentage(%)")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 0),
        axis.title = element_text(face = "bold", size = 15))


data %>% select(`CLS-DefineLive`) %>%
  mutate(x = 1) %>%
  group_by(`CLS-DefineLive`) %>%
  summarise(x = sum(x)) %>%
  mutate(prop = round(prop.table(x), 3), label = scales::percent(prop)) %>%
  ggplot(aes(x = reorder(`CLS-DefineLive`, -prop), y = prop))+
  geom_col(fill = "cornflowerblue")+
  geom_text(aes(label = label), nudge_y = 0.01)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = "Residence Type", y = "Percentage(%)")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90),
        axis.title = element_text(face = "bold", size = 15))

