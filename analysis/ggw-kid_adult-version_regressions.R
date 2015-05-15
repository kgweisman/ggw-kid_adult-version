########################################################### preliminaries #####

# --- PACKAGES & FUNCTIONS ----------------------------------------------------

# libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(scatterplot3d)
library(lme4)
library(psych)
library(stats)
library(scales)
library(smacof)

# clear environment
rm(list=ls())

# clear graphics
dev.off()

# --- IMPORTING DATA ----------------------------------------------------------

# read in data: character means
d = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid_adult-version/data/run-01_2015-05-09_charmeans.csv")[-1] # get rid of column of obs numbers

glimpse(d)

# read in data: individual scores
dd = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/ggw-kid_adult-version/data/run-01_2015-05-09_data_anonymized.csv")[-1] # get rid of column of obs numbers

glimpse(dd)

# --- FILTERING BY COUNTRY ----------------------------------------------------

# d_us = d %>% filter(country == "us")
# dd_us = dd %>% filter(country == "us")
# 
# d_india = d %>% filter(country == "india")
# dd_india = dd %>% filter(country == "india")

# set group of interest
# # ... to US:
# d = d_us
# dd = dd_us

# # ... to India:
# d = d_india
# dd = dd_india

# --- FILTERING BY ETHNICITY --------------------------------------------------

d_white = d %>%
  filter(ethnicity == "white")
dd_white = dd %>%
  filter(ethnicity == "white")

d_nonwhite = d %>%
  filter(ethnicity != "white" & 
           ethnicity != "NA" & 
           ethnicity != "other_prefNo")

dd_nonwhite = dd %>%
  filter(ethnicity != "white" & 
           ethnicity != "NA" & 
           ethnicity != "other_prefNo")

# set group of interest
# ... to white:
# d = d_white
# dd = dd_white

# # ... to nonwhite:
# d = d_nonwhite
# dd = dd_nonwhite

# --- FORMATTING DATA ---------------------------------------------------------

# make table of character means by mental capacity
charmeans = dd %>%
  filter(phase == "test") %>%
  select(subid, predicate, leftCharacter, rightCharacter, response, responseNum) %>%
  mutate(grownup = ifelse(leftCharacter == "grownup", responseNum,
                          ifelse(rightCharacter == "grownup", -1 * responseNum,
                                 NA)),
         kid = ifelse(leftCharacter == "kid", responseNum,
                      ifelse(rightCharacter == "kid", -1 * responseNum,
                             NA)),
         baby = ifelse(leftCharacter == "baby", responseNum,
                       ifelse(rightCharacter == "baby", -1 * responseNum,
                              NA)),
         dog = ifelse(leftCharacter == "dog", responseNum,
                      ifelse(rightCharacter == "dog", -1 * responseNum,
                             NA)),
         bear = ifelse(leftCharacter == "bear", responseNum,
                       ifelse(rightCharacter == "bear", -1 * responseNum,
                              NA)),
         bug = ifelse(leftCharacter == "bug", responseNum,
                      ifelse(rightCharacter == "bug", -1 * responseNum,
                             NA)),
         robot = ifelse(leftCharacter == "robot", responseNum,
                        ifelse(rightCharacter == "robot", -1 * responseNum,
                               NA)),
         computer = ifelse(leftCharacter == "computer", responseNum,
                           ifelse(rightCharacter == "computer", -1 * responseNum,
                                  NA)),
         car = ifelse(leftCharacter == "car", responseNum,
                      ifelse(rightCharacter == "car", -1 * responseNum,
                             NA)),
         stapler = ifelse(leftCharacter == "stapler", responseNum,
                          ifelse(rightCharacter == "stapler", -1 * responseNum,
                                 NA))
  ) %>%
  select(predicate, subid, grownup, kid, baby, dog, bear, bug, 
         robot, computer, car, stapler) %>%  
  gather(character, response,
         -predicate, -subid) %>%
  group_by(predicate, character) %>%
  summarise(mean = mean(response, na.rm = T))

glimpse(charmeans)

# format into wideform with characters as rows
charmeans_table = charmeans %>%
  spread(predicate, mean)

charnames = as.character(charmeans_table$character)

d1 = charmeans_table[-1]
rownames(d1) = charnames
print(d1)

# NOTE: not yet implemented for this study as of 2015-05-09
# # make table of mental capacity means by character
# # formatted in wideform with characters as rows
# condmeans = d %>%
#   select(condition, subid, gerald_schiff_pvs, toby_chimp, fetus, god,
#          delores_gleitman_deceased, sharon_harvey_woman, green_frog,
#          todd_billingsley_man, charlie_dog, nicholas_gannon_baby,
#          samantha_hill_girl, kismet_robot, you) %>%  
#   gather(character, response,
#          -condition, -subid) %>%
#   group_by(condition, character) %>%
#   summarise(mean = mean(response, na.rm = T))
# 
# # format into wideform with characters as rows
# condmeans_table = condmeans %>%
#   spread(character, mean)
# 
# subidnames = condmeans_table$subid
# 
# d3 = condmeans_table[-1]
# d3 = d3[-1]
# names(d3) = charnames
# rownames(d3) = subidnames
# print(d3)

##################################################### regression analyses #####

# read in contrasts
c0 = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid/Stim development/analysis/contrasts 2015-05-15.csv")

c1 = c0 %>% arrange(pair)
rownames(c1) = c1[,1]
c1 = c1[-1]

# add pairs variable to dataframe ('d1') ------
d1 = dd %>% 
  filter(phase == "test") %>%
  mutate(
    pair = 
      ifelse(
        leftCharacter == "grownup" | 
          rightCharacter == "grownup",
        ifelse(
          rightCharacter == "kid" | 
            leftCharacter == "kid", 
          "grownup.kid",
          ifelse(
            rightCharacter == "baby" | 
              leftCharacter == "baby", 
            "grownup.baby",
            ifelse(
              rightCharacter == "dog" | 
                leftCharacter == "dog", 
              "grownup.dog",
              ifelse(
                rightCharacter == "bear" | 
                  leftCharacter == "bear", 
                "grownup.bear",
                ifelse(
                  rightCharacter == "bug" | 
                    leftCharacter == "bug", 
                  "grownup.bug",
                  ifelse(
                    rightCharacter == "robot" | 
                      leftCharacter == "robot", 
                    "grownup.robot",
                    ifelse(
                      rightCharacter == "computer" | 
                        leftCharacter == "computer", 
                      "grownup.computer",
                      ifelse(
                        rightCharacter == "car" | 
                          leftCharacter == "car", 
                        "grownup.car",
                        ifelse(
                          rightCharacter == "stapler" | 
                            leftCharacter == "stapler", 
                          "grownup.stapler",
                          NA))))))))),
        ifelse(
          leftCharacter == "kid" | 
            rightCharacter == "kid",
          ifelse(
            rightCharacter == "baby" | 
              leftCharacter == "baby", 
            "kid.baby",
            ifelse(
              rightCharacter == "dog" | 
                leftCharacter == "dog", 
              "kid.dog",
              ifelse(
                rightCharacter == "bear" | 
                  leftCharacter == "bear", 
                "kid.bear",
                ifelse(
                  rightCharacter == "bug" | 
                    leftCharacter == "bug", 
                  "kid.bug",
                  ifelse(
                    rightCharacter == "robot" | 
                      leftCharacter == "robot", 
                    "kid.robot",
                    ifelse(
                      rightCharacter == "computer" | 
                        leftCharacter == "computer", 
                      "kid.computer",
                      ifelse(
                        rightCharacter == "car" | 
                          leftCharacter == "car", 
                        "kid.car",
                        ifelse(
                          rightCharacter == "stapler" | 
                            leftCharacter == "stapler", 
                          "kid.stapler",
                          NA)))))))),
          ifelse(
            leftCharacter == "baby" | 
              rightCharacter == "baby",
            ifelse(
              rightCharacter == "dog" | 
                leftCharacter == "dog", 
              "baby.dog",
              ifelse(
                rightCharacter == "bear" | 
                  leftCharacter == "bear", 
                "baby.bear",
                ifelse(
                  rightCharacter == "bug" | 
                    leftCharacter == "bug", 
                  "baby.bug",
                  ifelse(
                    rightCharacter == "robot" | 
                      leftCharacter == "robot", 
                    "baby.robot",
                    ifelse(
                      rightCharacter == "computer" | 
                        leftCharacter == "computer", 
                      "baby.computer",
                      ifelse(
                        rightCharacter == "car" | 
                          leftCharacter == "car", 
                        "baby.car",
                        ifelse(
                          rightCharacter == "stapler" | 
                            leftCharacter == "stapler", 
                          "baby.stapler",
                          NA))))))),
            ifelse(
              leftCharacter == "dog" | 
                rightCharacter == "dog",
              ifelse(
                rightCharacter == "bear" | 
                  leftCharacter == "bear", 
                "dog.bear",
                ifelse(
                  rightCharacter == "bug" | 
                    leftCharacter == "bug", 
                  "dog.bug",
                  ifelse(
                    rightCharacter == "robot" | 
                      leftCharacter == "robot", 
                    "dog.robot",
                    ifelse(
                      rightCharacter == "computer" | 
                        leftCharacter == "computer", 
                      "dog.computer",
                      ifelse(
                        rightCharacter == "car" | 
                          leftCharacter == "car", 
                        "dog.car",
                        ifelse(
                          rightCharacter == "stapler" | 
                            leftCharacter == "stapler", 
                          "dog.stapler",
                          NA)))))),
              ifelse(
                leftCharacter == "bear" | 
                  rightCharacter == "bear",
                ifelse(
                  rightCharacter == "bug" | 
                    leftCharacter == "bug", 
                  "bear.bug",
                  ifelse(
                    rightCharacter == "robot" | 
                      leftCharacter == "robot", 
                    "bear.robot",
                    ifelse(
                      rightCharacter == "computer" | 
                        leftCharacter == "computer", 
                      "bear.computer",
                      ifelse(
                        rightCharacter == "car" | 
                          leftCharacter == "car", 
                        "bear.car",
                        ifelse(
                          rightCharacter == "stapler" | 
                            leftCharacter == "stapler", 
                          "bear.stapler",
                          NA))))),
                ifelse(
                  leftCharacter == "bug" | 
                    rightCharacter == "bug",
                  ifelse(
                    rightCharacter == "robot" | 
                      leftCharacter == "robot", 
                    "bug.robot",
                    ifelse(
                      rightCharacter == "computer" | 
                        leftCharacter == "computer", 
                      "bug.computer",
                      ifelse(
                        rightCharacter == "car" | 
                          leftCharacter == "car", 
                        "bug.car",
                        ifelse(
                          rightCharacter == "stapler" | 
                            leftCharacter == "stapler", 
                          "bug.stapler",
                          NA)))),
                  ifelse(
                    leftCharacter == "robot" | rightCharacter == "robot",
                    ifelse(
                      rightCharacter == "computer" | 
                        leftCharacter == "computer", 
                      "robot.computer",
                      ifelse(
                        rightCharacter == "car" | 
                          leftCharacter == "car", 
                        "robot.car",
                        ifelse(
                          rightCharacter == "stapler" | 
                            leftCharacter == "stapler", 
                          "robot.stapler",
                          NA))),
                    ifelse(
                      leftCharacter == "computer" | 
                        rightCharacter == "computer",
                      ifelse(
                        rightCharacter == "car" | 
                          leftCharacter == "car", 
                        "computer.car",
                        ifelse(
                          rightCharacter == "stapler" | 
                            leftCharacter == "stapler", 
                          "computer.stapler",
                          NA)),
                      ifelse(
                        leftCharacter == "car" | 
                          rightCharacter == "car",
                        ifelse(
                          rightCharacter == "stapler" | 
                            leftCharacter == "stapler", 
                          "car.stapler",
                          NA),
                        NA)))))))))) %>%
  mutate(pair = factor(pair),
         responseNumFlip = 
           ifelse(
             substr(leftCharacter,1,3) == substr(pair,1,3),
             responseNum,
             -1 * responseNum))

# set contrasts ----
contrasts(d1$pair) = as.matrix(c1)

r0 = lm(responseNumFlip ~ pair, d1); summary(r0)
r1 = lmer(responseNumFlip ~ pair + (1 | subid), d1); summary(r1)

