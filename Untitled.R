---
title: "Project Group 10 - Report"
author: "Eugénie Mathieu and Maeva Marin"
date: " 15 march 2020"
output: html_document
---
library(readr)
library(tidyverse)
library(kableExtra)
library(ggplot2)
library(knitr)
library (here)
driverspassengers <- `drivers-and-passengers-searched-by-sex-race-and-ethnicity`
summary(driverspassengers)
driverspassengers %>%
  arrange(desc(Total.By.Ethnicity)) %>%
  select(Total.By.Ethnicity, everything()) %>%
  kable(
    caption = ""
  ) %>%
  kable_styling(
    bootstrap_options = "striped")

diff2014 <- driverspassengers %>%
  mutate(DIFF = driverspassengers$White-driverspassengers$Black) %>%
  filter(year == 2014)
diff2015 <- driverspassengers %>%
  mutate(DIFF = driverspassengers$White-driverspassengers$Black) %>%
  filter(year == 2015)
