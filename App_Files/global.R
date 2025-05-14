library(tidyverse)
library(shiny)
library(here)

setwd("/srv/connect/apps/EVS_Data_Exploration")
Dat <- read_csv("www/CleanData.csv")
 

Dat <- Dat %>%
  mutate(across(c(Sex, Edu, Country), as.factor))

Dat$Age_Group <- cut(Dat$Age, 
                     breaks = c(17, 34, 49, 64, 82), 
                     labels = c("Young Adult", "Middle Adult", "Elder Adult", "Senior Adult"), 
                     right = TRUE)
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
  select(Age_Group, Sex, Edu, 
         Ave_GenRol_Age, Ave_GenRol_Sex, 
         Ave_GenRol_Edu) %>%
  distinct() %>%
  pivot_longer(cols = starts_with("Ave_GenRol"), 
               names_to = "Variable", 
               values_to = "Average") %>%
  mutate(
    Group = case_when(
      Variable == "Ave_GenRol_Age" ~ as.character(Age_Group),
      Variable == "Ave_GenRol_Sex" ~ as.character(Sex),
      Variable == "Ave_GenRol_Edu" ~ as.character(Edu)
    ),
    Variable = recode(Variable,
                      "Ave_GenRol_Age" = "Age_Group",
                      "Ave_GenRol_Sex" = "Sex",
                      "Ave_GenRol_Edu" = "Edu")
  ) 
AllExpGenDat <- AllGen_long %>% 
  select(Variable, Average, Group) %>% unique()
 

  
AllImmig_long <- Dat1 %>%
  select(Age_Group, Sex, Edu, 
         Ave_Immig_Age, Ave_Immig_Sex, 
         Ave_Immig_Edu) %>%
  distinct() %>%
  pivot_longer(cols = starts_with("Ave_Immig"), 
               names_to = "Variable", 
               values_to = "Average") %>%
  mutate(
    Group = case_when(
      Variable == "Ave_Immig_Age" ~ as.character(Age_Group),
      Variable == "Ave_Immig_Sex" ~ as.character(Sex),
      Variable == "Ave_Immig_Edu" ~ as.character(Edu)
    ),
    Variable = recode(Variable,
                      "Ave_Immig_Age" = "Age_Group",
                      "Ave_Immig_Sex" = "Sex",
                      "Ave_Immig_Edu" = "Edu")
  ) 
AllExpImmigDat <- AllImmig_long %>% 
  select(Variable, Average, Group) %>% unique()
 
