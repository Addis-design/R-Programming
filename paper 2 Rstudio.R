## Preamble
### Name: Clayton Perrin
### Date: Oct 9 2023
### Purpose: Paper 2 (race and reported income)
library(dplyr)
library(descr)
library(forcats)
library(ggplot2)

## create a table of gss_cat
table(gss_cat$race)
table(gss_cat$rincome)

## create data frame & remove missing values
df1 <- gss_cat %>% 
  na.omit() %>% 
  filter(year == 2010) %>% 
  select(year, race, rincome)

## making variable dichotomous, eliminating categories from data
df1 %>% 
  filter(year == 2010) %>% 
  filter(race != "Other")

df1 %>% 
  filter(year == 2010) %>% 
  filter(rincome  != " Don't know" 
         & rincome != "Refused" 
         & rincome  != "Not applicable" )

## get a quick count of the sample size
count(df1)

## get the relative frequency for the race variable
df1 %>% 
  filter(year == 2010) %>% 
  filter(race != "Other") %>% 
  count(race) %>% 
  mutate(prop = prop.table(n))

## get relative frquency for the response variable (income)
df1 %>% 
  filter(year == 2010) %>% 
  filter(rincome != "No answer"
         & rincome != "Refused"
         & rincome != "Not applicable"
         & rincome != "Don't Know" ) %>% 
  mutate(rincome = fct_recode(rincome, 
                              "More than 20000" = "$25000 or more",
                              "More than 20000" = "$20000 - 24999",
                              "Less than 20000" = "$15000 - 19999",
                              "Less than 20000" = "$10000 - 14999",
                              "Less than 20000" = "$8000 to 9999",
                              "Less than 20000" = "$7000 to 7999",
                              "Less than 20000" = "$6000 to 6999",
                              "Less than 20000" = "$5000 to 5999",
                              "Less than 20000" = "$4000 to 4999",
                              "Less than 20000" = "$3000 to 3999",
                              "Less than 20000" = "$1000 to 2999",
                              "Less than 20000" = "Lt $1000"))   %>% 
  count(rincome) %>% 
  mutate(prop = prop.table(n))

# Filter and prepare the data for visualization
data_to_plot <- df1 %>%
  filter(year == 2010) %>%
  filter(race != "Other" &
           rincome != "No answer" &
           rincome != "Refused" &
           rincome != "Not applicable" &
           rincome != "Don't Know")


# Create a bar plot for race
ggplot(df1 %>% 
         filter(year == 2010) %>% 
         filter(race != "Other") %>% 
         count(race) %>% 
         mutate(prop = prop.table(n)), 
       aes(x = race, y = prop, fill = race)) +
  geom_bar(stat = "identity") +
  labs(title = "Relative Frequency of Race in 2010",
       x = "Race",
       y = "Proportion") +
  theme_minimal()

# Create a grouped bar plot
ggplot(data_to_plot, aes(x = race, fill = rincome)) +
  geom_bar(position = "dodge") +
  labs(title = "Income vs Race in 2010",
       x = "Race",
       y = "Count") +
  scale_fill_brewer(palette = "Set1") +  # Adjust the color palette
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

