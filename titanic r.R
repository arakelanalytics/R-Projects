#Install and load packages------------------------------------------------------

install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("Hmisc")
library(tidyverse)
library(ggplot2)
library(dplyr)
library(Hmisc)


#upload titanic data------------------------------------------------------------

titanic <- read.csv("~/Desktop/Data Analyst Portfolio/Datasets/titanic.csv")

head(titanic)

#inspect data-----------------------------------------------------------------

#check structure of columns

str(titanic)

#change pclass column structures to character, survived to boolean


titanic$Pclass = as.character(titanic$Pclass)


#check description

describe(titanic)

# Purpose of analysis: Try to identify characteristics of the passengers who 
# had the highest survival percentage.




#ANALYSIS-----------------------------------------------------------------------

# looking at survival rate vs passenger class

#total passengers per class

total_pass_per_class <- titanic %>%
  count(Pclass)

head(total_pass_per_class)

# looking at survival rate vs sex 
survival_rate_by_sex <- titanic %>% 
  group_by(Sex, Survived) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))

head(survival_rate_by_sex)

# looking at survival rate vs class
survival_rate_by_class <- titanic %>% 
  group_by(Pclass, Survived) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count)) %>% 
  filter(Survived == 1)
head(survival_rate_by_class)

# looking at survival rate vs Age
survival_rate_by_age <- titanic %>% 
  mutate(agegroup = case_when(Age >= 0  & Age <= 9 ~ 'Under 10 yo',
                              Age >= 10  & Age <= 19 ~ '10-20 yo',
                              Age >= 20  & Age <= 40 ~ '20 -40')) %>% 
  group_by(agegroup, Survived) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count)) %>% 
  filter(Survived == 1, agegroup != "NA")
head(survival_rate_by_age) 


#VISUALIZATIONS-----------------------------------------------------------------

#number of passengers per class
ggplot(data = titanic) +
  geom_bar(mapping = aes(x = Pclass, fill = factor(Pclass)))

#number of male and female passengers per class. 
#insight = the largest group of passengers where the males in class 3
ggplot(data = titanic) +
  geom_bar(mapping = aes(x = Pclass,, fill = factor(Sex))) +
  facet_wrap(~Sex)

#percentage of male and female passengers per class. 
#insight = the largest group of passengers where the males in class 3
ggplot(data = titanic) +
  geom_bar(mapping = aes(x = Pclass)) + facet_wrap(~Sex)

#number of passengers who survived vs died
ggplot(data = titanic) +
  geom_bar(mapping = aes(x = factor(Survived)))

#survival rate by class
ggplot(survival_rate_by_class, aes(x = factor(Pclass), y = perc*100), fill = factor(Pclass)) +
  geom_col(position = "dodge", width = 0.5) +
  labs(x = "Pclass", y = "Percent Survived", fill = 'perc') +
  theme_minimal(base_size = 15)

#survival rate by age. Passengers under 10 years old survived 20% more than those above 10 years old
ggplot(survival_rate_by_age, aes(x = reorder(agegroup, -perc), y = perc*100), fill = factor(agegroup)) +
  geom_col(position = "dodge", width = 0.5) +
  labs(x = "Age Group", y = "Percent Survived", fill = 'perc') +
  theme_minimal(base_size = 15)


#END VIZUALIZATIONS-------------------------------------------------------------

