library(tidyverse)
library(shiny)
library(here)

#setwd("/srv/connect/apps/EVS_Data_Exploration")
#Dat <- read_csv("www/CleanData.csv")
 
Dat <- read_csv("C:\\Users\\Owner\\SURV675_Final\\App_Files\\www\\CleanData.csv")

Dat <- Dat %>%
  mutate(across(c(Sex, Edu, Country), ~ fct_explicit_na(as.factor(.), na_level = "Missing")))

Dat$Age_Group <- cut(Dat$Age, 
                     breaks = c(17, 34, 49, 64, 82), 
                     labels = c("Young Adult", "Middle Adult", "Elder Adult", "Senior Adult"), 
                     right = TRUE)
Dat$Age_Group <- fct_explicit_na(Dat$Age_Group, na_level = "Missing")
table(Dat$Age_Group)

Dat <- Dat %>% 
  group_by(Age_Group) %>%
  mutate(
    Ave_GenRol_Age = round(mean(GenRol, na.rm = T), 2),
    Ave_Immig_Age = round(mean(Immig, na.rm = T), 2)
  ) %>% 
  ungroup() %>% 
  group_by(Sex) %>%
  mutate(
    Ave_GenRol_Sex = round(mean(GenRol, na.rm = T), 2),
    Ave_Immig_Sex = round(mean(Immig, na.rm = T), 2)
  ) %>% 
  ungroup() %>% 
  group_by(Edu) %>%
  mutate(
    Ave_GenRol_Edu = round(mean(GenRol, na.rm = T), 2), 
    Ave_Immig_Edu = round(mean(Immig, na.rm = T), 2)
  ) %>% 
  ungroup()  %>% 
  group_by(Country) %>%
  mutate(
    Ave_GenRol_Country = round(mean(GenRol, na.rm = T), 2), 
    Ave_Immig_Country = round(mean(Immig, na.rm = T), 2)
  ) %>% 
  ungroup() 
 
Dat1 <- Dat

AllGen_long <- Dat1 %>% 
  mutate(
    Age_Group = factor(Age_Group, levels = c("Young Adult", "Middle Adult", "Senior Adult", "Elder Adult", "Missing")),
    Sex = factor(Sex, levels = c("Male", "Female", "Missing")),
    Edu = factor(Edu, levels = c("Incomplete Elementary", "Completed Elementary", 
                                 "Incomplete Secondary", "Completed Secondary", 
                                 "Incomplete Technical", "Completed Technical",
                                 "Incomplete University", "Completed University", "Missing")),
    Country = factor(Country, levels = unique(Country))  
  ) %>%
  select(Age_Group, Sex, Edu, Country,
         Ave_GenRol_Age, Ave_GenRol_Sex, 
         Ave_GenRol_Edu, Ave_GenRol_Country) %>%
  distinct() %>%
  pivot_longer(cols = starts_with("Ave_GenRol"), 
               names_to = "Variable", 
               values_to = "Average") %>%
  mutate(
    Group = case_when(
      Variable == "Ave_GenRol_Age" ~ Age_Group,
      Variable == "Ave_GenRol_Sex" ~ Sex,
      Variable == "Ave_GenRol_Edu" ~ Edu, 
      Variable == "Ave_GenRol_Country" ~ Country
    ),
    Variable = recode(Variable,
                      "Ave_GenRol_Age" = "Age_Group",
                      "Ave_GenRol_Sex" = "Sex",
                      "Ave_GenRol_Edu" = "Edu", 
                      "Ave_GenRol_Country" = "Country")
  ) 

AllExpGenDat <- AllGen_long %>% 
  filter(Variable != "Country") %>%
  select(Variable, Average, Group) %>% unique() 

SelectedExpGenDat <- AllGen_long %>% 
  select(Country, Variable, Average, Group) %>% unique()
 
AllImmig_long <- Dat1 %>% 
  mutate(
    Age_Group = factor(Age_Group, levels = c("Young Adult", "Middle Adult", "Senior Adult", "Elder Adult", "Missing")),
    Sex = factor(Sex, levels = c("Male", "Female", "Missing")),
    Edu = factor(Edu, levels = c("Incomplete Elementary", "Completed Elementary", 
                                 "Incomplete Secondary", "Completed Secondary", 
                                 "Incomplete Technical", "Completed Technical",
                                 "Incomplete University", "Completed University", "Missing")),
    Country = factor(Country, levels = unique(Country))  
  ) %>%
  select(Age_Group, Sex, Edu, 
         Ave_Immig_Age, Ave_Immig_Sex, 
         Ave_Immig_Edu, Country, Ave_Immig_Country) %>%
  distinct() %>%
  pivot_longer(cols = starts_with("Ave_Immig"), 
               names_to = "Variable", 
               values_to = "Average") %>%
  mutate(
    Group = case_when(
      Variable == "Ave_Immig_Age" ~ Age_Group,
      Variable == "Ave_Immig_Sex" ~ Sex,
      Variable == "Ave_Immig_Edu" ~ Edu,
      Variable == "Ave_Immig_Country" ~ Country
    ),
    Variable = recode(Variable,
                      "Ave_Immig_Age" = "Age_Group",
                      "Ave_Immig_Sex" = "Sex",
                      "Ave_Immig_Edu" = "Edu", 
                      "Ave_Immig_Country" = "Country")
  ) 

AllExpImmigDat <- AllImmig_long %>% 
  filter(Variable != "Country") %>%
  select(Variable, Average, Group) %>% unique()

SelectedExpImmigDat <- AllImmig_long %>% 
  select(Country, Variable, Average, Group) %>% unique() 


