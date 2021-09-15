---
title: "Final Project - Baseball Analysis"
author: "MAL"
date: "3/16/2021"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(dplyr)
```

##Before beginning, this analysis utilizes the following packages: tidy verse

###Import

```{r}
## crtl + atl + i

##mlb <- read.csv("https://raw.githubusercontent.com/MALangheim/2019teamData/main/2019%20##season%20raw%20data.txt", na.strings = c("", "NA"))

##npf <- read.csv("https://docs.google.com/spreadsheets/d/1lwyA2vlCKp-15yWwS8_QfNUgxpLBwp##ON/export?format=csv")

mlb <- read.csv(file.choose(), na.strings = c("", "NA"))
npf <- read.csv(file.choose())
```

###Tidy

```{r}
mlb2 <- subset(mlb, yearID >= 1998) %>%
  select(yearID, lgID, name, G, W, L, AB, H) %>%
  relocate("Team" = name, .before = lgID) %>%
  rename("Year" = yearID, "League" = lgID, "Games" = G, "Wins" = W, "Losses" = L) 

npf2 <- npf %>%
  rename("Year" = ï..Year, "Wins" = W, "Losses" = L) %>%
  select(Year, Team, Wins, Losses, G, AB, H, AVG) %>%
  relocate("Games" = G, .after = Team)
  ##slice(-10)
  ##remove teams with only one season

```

###Transform

```{r}
mlb2 <- mlb2 %>%
  mutate(AVG = H / AB) %>%
  mutate(Team = str_replace(Team, "Anaheim Angels", "Los Angeles Angels")) %>%
  mutate(Team = str_replace(Team, "Los Angeles Angels of Anaheim", "Los Angeles Angels")) %>%
  mutate(Team = str_replace(Team, "Florida Marlins", "Miami Marlins")) %>%
  mutate(Team = str_replace(Team, "Tampa Bay Devil Rays", "Tampa Bay Rays"))

mlb3 <- mlb2 %>%
  group_by(Team) %>%
  summarise(across(c(Games, Wins, AVG), mean))
  ##mutate(Wins = Wins/3, Games = Games/3)
  ##3 mlb observations (games) per every one npf observation

npf3 <- npf2 %>%
  group_by(Team) %>%
  summarise(across(c(Games, Wins, AVG), mean))

```

###Visualize

```{r}
ggplot(mlb2, 
       aes(x = AVG, y = Wins)) +
  geom_point(color = 'red') +
  geom_point(data = npf2, color = 'blue') +
  xlim(0, .5)

ggplot(mlb3, 
       aes(x = AVG, y = Wins)) +
  geom_point(color = 'red') +
  geom_point(data = npf3, color = 'blue') +
  xlim(0, .5)

ggplot(mlb2,
       aes(x = Team, y = AVG, color = Team)) +
  geom_boxplot() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

ggplot(mlb2,
       aes(x = Team, y = Wins, color = Team)) +
  geom_boxplot() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

ggplot(npf2,
       aes(x = Team, y = AVG, color = Team)) +
  geom_boxplot() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

ggplot(npf2,
       aes(x = Team, y = Wins, color = Team)) +
  geom_boxplot() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

```

###Model

```{r}
mlb_model1 <- aov(Wins ~ AVG, data = mlb2)
summary(mlb_model1)

npf_model1 <- aov(Wins ~ AVG, data = npf2)
summary(npf_model1)

mlb_model2 <- aov(Wins ~ AVG, data = mlb3)
summary(mlb_model2)

npf_model2 <- aov(Wins ~ AVG, data = npf3)
summary(npf_model2)
```
