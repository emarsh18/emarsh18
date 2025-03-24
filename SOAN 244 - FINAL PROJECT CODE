library(devtools)
devtools::install_github('Tom-Wolff/ideanet', ref="main", force = TRUE)
library(ideanet)
library(igraph)
install.packages("table1")
library(table1)
library(dplyr)
library(reshape2)
install.packages("tidyverse")
library(tidyverse)
install.packages("modelsummary")
install.packages ("sjPlot")
library(modelsummary)
library(sjPlot)
load("~/wlu_nets.Rda")


############################################### DATA PREP #######################################################
list2env(wlu_nets, .GlobalEnv)
glimpse(summaries)
egos <- egos %>% left_join(summaries)
glimpse(egos)


#####table with race
ego_compositional <- alters %>% group_by(ego_id) %>%
  summarize(prop_white = mean(race_ethn_White, na.rm = T),
           prop_Black = mean(race_ethn_Black, na.rm = T),
           prop_other = mean(race_ethn_Other, na.rm = T),
          prop_Asian = mean(race_ethn_Asian, na.rm = T),
         prop_Latinx = mean(race_ethn_Latinx, na.rm = T),
        prop_MENA = mean(race_ethn_MENA, na.rm = T),
       prop_Multi = mean(race_ethn_Multi, na.rm = T),
      prop_Pacific = mean(race_ethn_Pacific, na.rm = T))

##organizing race
egos <- egos %>% mutate(race = case_when(race_ethn_Indigenous == T ~ "Indigenous",
                                         race_ethn_Asian == T ~ "Asian",
                                         race_ethn_Black == T ~ "Black",
                                         race_ethn_Latinx == T ~ "Latinx",
                                         race_ethn_MENA == T ~ "MENA", 
                                         race_ethn_Multi == T ~ "Multi", 
                                         race_ethn_Pacific == T ~ "Pacific", 
                                         race_ethn_White == T ~ "White",
                                         race_ethn_Other == T ~ "Other"), race = as_factor(race))
##organizing greek status
egos <- egos %>% mutate(greek = case_when(Greek_1 == T ~ "Yes",
                                          Greek_0 == T ~ "No"), greek = as_factor(greek))

##organizing minioirty status
egos <- egos %>% mutate(minority = case_when(race == "White" ~ "No",
                                             race != "White" ~ "Yes"), minority = as_factor(minority))

##organizing grade level status
egos <- egos %>% mutate(grade = case_when(class_year_2025 == T ~ "2025",
                                          class_year_2026 == T ~ "2026",
                                          class_year_2027 == T ~ "2027", 
                                          class_year_2028 == T ~ "2028"), grade = as_factor(grade))

##adding ego race to compositional
ego_race <- egos %>% select(ego_id, race)
ego_compositional <- ego_compositional %>% left_join(ego_race)

##adding ego effective size to compositional 
ego_effectSize <- egos %>% select (ego_id, effective_size)
ego_compositional <- ego_compositional %>% left_join (ego_effectSize)

##adding minority to compositional 
ego_minority <- egos %>% select (ego_id, minority)
ego_compositional <- ego_compositional %>% left_join (ego_minority)


##adding grade level to compositional
ego_grade <- egos %>% select (ego_id, grade)
ego_compositional <- ego_compositional %>% left_join (ego_grade)

##adding ego greek to compositional
ego_greek <- egos %>% select(ego_id, greek)
ego_compositional <- ego_compositional %>% left_join(ego_greek)

##measuere of central tendancy and dispersion
label(ego_compositional$minority)       <- "Minority Status"
label(ego_compositional$greek)       <- "Greek Affiliation Status"
label(ego_compositional$grade)     <- "Class Year"
label(ego_compositional$effective_size) <- "Effective Size"

caption  <- "Table of Measure of Central Tendancy and Dispersion"

table1(~ minority + greek + grade + effective_size, 
       data=ego_compositional, 
       overall=c(left="Total"), caption=caption)

##test the hypothesis that the minority status of ego influences the effective size
effectSizelm <- lm (effective_size ~ minority, data = ego_compositional)
#tab_model(effectSizelm)
effectSizelm2 <- lm (effective_size ~ minority + greek, data = ego_compositional)
#tab_model(effectSizelm2)
effectSizelm3 <- lm (effective_size ~ minority + minority*greek, data = ego_compositional)
tab_model(effectSizelm, effectSizelm2, effectSizelm3)


############################################ VISUALIZATIONS ##################################################
#simple bar chart showing distribution of greek
ggplot (egos, aes (greek)) + geom_bar(fill = "lightpink") + 
  labs(title = "Distribution of Greek Status", x="Greek Status", y="Count") + theme_minimal()

#boxplots of if greek and effective size
ggplot(egos, aes(greek, effective_size)) + geom_boxplot(fill = "lightblue") + theme_minimal() + 
  labs(title = "Effective Size Based on Affiliation", x="Greek Status", y="Effective Size") 

#boxplots of if minoirty and effective size
ggplot(egos, aes(minority, effective_size)) + geom_boxplot(fill = "yellow") + theme_minimal() + 
  labs(title = "Effective Size Based on Minority", x="Minority Status", y="Effective Size") 

#distribution of proportion white alters across the egonets
ggplot(ego_compositional, aes(prop_white)) + geom_histogram(fill = "darkorange") + 
  labs(title = "Distribution of Proportion of White Alters", x="Proportion White", y = "Count") + theme_minimal()

#distribution of ego effective size 
ggplot(ego_compositional, aes(effective_size)) + geom_histogram(fill = "chartreuse3") + 
  labs(title = "Distribution of Egos Effective Size", x='Effective Size', y="Count") + theme_minimal()

#distribution of the number of races in egos
ggplot(ego_race, aes(race)) + geom_bar(fill = "indianred1") + 
  labs(title = "Distribution of Race in Egos", x="Race", y="Count") + theme_minimal()
