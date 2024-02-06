## Author Details
### Name: Ray will 
### Date: Oct 9 2023
### Purpose: Paper 2
#Load the libraries
library(dplyr)
library(descr)
library(forcats)
library(ggplot2)
#view the data
?gss_cat
gss_cat

#check categories

#check categories for race
summary(gss_cat$race)

#Check categories for partyid
summary(gss_cat$partyid)

#Preparing the data for analysis
#exploratory analysis of each variable
str(gss_cat)
# Check for missing values in race column
sum(is.na(gss_cat$race))

# Check for missing values in the entire dataset
sum(is.na(gss_cat$partyid))
# Remove rows with missing values
gss_cat <- na.omit(gss_cat)

#exploring data from 2010
df<-gss_cat%>%
  na.omit()%>%
  filter(year==2010)%>%
  select(year,race,partyid)
df
head(df)

#getting frequency table
df%>%count(race)
df%>%count(partyid)

#Recoding categories into dichotomous
#race

df %>% 
  filter(year == 2010) %>% 
  filter(race != "Other") %>% 
  count(race) %>% 
  mutate(prop = prop.table(n))

#partyid
df %>%
  mutate(partyid = fct_recode(partyid,
                              "Republican"= "Strong republican",
                              "Republican"= "Not str republican",
                              "Republican"= "Ind,near rep",
                              "Democrat" ="Ind,near dem",
                                "Democrat" ="Not str democrat",
                                "Democrat" ="Strong democrat"))

# Create plots for race and partyid
# Bar plot for race distribution
ggplot(df, aes(x = race)) +
  geom_bar() +
  labs(title = "Distribution of Race in 2010",
       x = "Race",
       y = "Count") +
  theme_minimal()

# Bar plot for partyid distribution
ggplot(df, aes(x = partyid)) +
  geom_bar() +
  labs(title = "Distribution of Party Affiliation in 2010",
       x = "Party Affiliation",
       y = "Count") +
  theme_minimal()

# Create a grouped bar plot for Party Affiliation vs. Race in 2010
ggplot(df, aes(x = race, fill = partyid)) +
  geom_bar(position = "dodge") +
  labs(title = "Party Affiliation vs. Race in 2010",
       x = "Race",
       y = "Count") +
  scale_fill_brewer(palette = "Set1") +  # Adjust the color palette
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
colnames(gss_cat)

