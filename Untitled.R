#load package

setwd("/users/lizhouxin/downloads")
library(ipumsr)
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)
library(lubridate)
library(ggplot2)
library(forcats)
library(forecast)
library(ggpubr)
library(seasonal)

#create industry crosswalk

#ddi <- read_ipums_ddi("usa_00002.xml")
#data <- read_ipums_micro(ddi)

#classification of industry (https://www.bls.gov/jlt/jltnaics.htm)

#my_data <- distinct(data, INDNAICS, .keep_all = TRUE)
#crosswalk <- my_data[,-c(1:9)] %>%
#  filter(str_sub(INDNAICS,1,4) != 9281, IND1990 != 0)
#save(crosswalk, file = "crosswalk.RData")
load("crosswalk.RData")

Leisure <- filter(crosswalk, str_sub(INDNAICS,1,1) == 7)
Manufacturing <- filter(crosswalk, str_sub(INDNAICS,1,1) == 3)
Otherindustries <- filter(crosswalk, str_sub(INDNAICS,1,1) != 3, 
                          str_sub(INDNAICS,1,1) != 7)

ddi <- read_ipums_ddi("cps_00004.xml")
data <- read_ipums_micro(ddi)

#drop useless variables & first variable changes to date format
df <- data %>%
  select(c("YEAR", "MONTH", "WTFINL", "CPSIDP", "EMPSTAT", 
           "LABFORCE", "IND1990", "REGION")) %>%
  unite(Year_Month, YEAR, MONTH, sep = "-") %>%
  mutate(Year_Month = ym(Year_Month))

#add variables to represent date and region 
df <- distinct(df, Year_Month) %>%
  arrange() %>%
  mutate(i=c(1:266)) %>%
  right_join(df) %>%
  rename(Date = Year_Month) %>%
  mutate(Region = if_else(REGION == 11|REGION == 12, "Northeast", 
                          if_else(REGION == 21|REGION == 22, "Midwest", 
                                  if_else(str_sub(REGION,1,1) == 3, "South",
                                          if_else(str_sub(REGION,1,1) == 4, "West", "Unknow")))))

# 5th party data-Leisure
  #select current month
  Lei1 <-filter(df, i == 1) %>%
    filter(IND1990 %in% Leisure$IND1990, EMPSTAT == 10 | EMPSTAT == 12) 
  #select people still in survey, and add variables to represent status we need
  Lei2 <- filter(df, i == 4) %>%
    filter(CPSIDP %in% Lei1$CPSIDP) %>%
    mutate(une = if_else(EMPSTAT == 21 | EMPSTAT ==22, 1, 0),
           not = if_else(LABFORCE == 1, 1, 0)) %>%
    select(c("CPSIDP", "une", "not"))
  #add columns from lei2 to lei1
  Lei3 <- inner_join(Lei1, Lei2, by = "CPSIDP")
  #loop 
  z = 2
  while (z <= 263) {
    Lei1 <-filter(df, i == z) %>%
      filter(IND1990 %in% Leisure$IND1990, EMPSTAT == 10 | EMPSTAT == 12) 
    Lei2 <- filter(df, i == z+3) %>%
      filter(CPSIDP %in% Lei1$CPSIDP) %>%
      mutate(une = if_else(EMPSTAT == 21 | EMPSTAT ==22, 1, 0),
             not = if_else(LABFORCE == 1, 1, 0)) %>%
      select(c("CPSIDP", "une", "not"))
    Lei4 <- inner_join(Lei1, Lei2, by = "CPSIDP")
    Lei3 <- bind_rows(Lei3, Lei4)
    z = z + 1
  }
  
  Lei5 <- Lei3 %>% group_by(Date, Region) %>%
    summarise(count=n(), count_une=sum(une), count_not=sum(not),
              trans_une = 100 * round(sum(une*WTFINL)/sum(WTFINL), 3), 
              trans_not = 100 * round(sum(not*WTFINL)/sum(WTFINL), 3))

  Lei6 <- Lei3 %>% group_by(Date) %>%
    summarise(count=n(), count_une=sum(une), count_not=sum(not),
              trans_une = 100 * round(sum(une*WTFINL)/sum(WTFINL), 3), 
              trans_not = 100 * round(sum(not*WTFINL)/sum(WTFINL), 3))
  
# 5th party data-Manufacturing
  #select current month
  Man1 <-filter(df, i == 1) %>%
    filter(IND1990 %in% Manufacturing$IND1990, EMPSTAT == 10 | EMPSTAT == 12) 
  #select people still in survey, and add variables to represent status we need
  Man2 <- filter(df, i == 4) %>%
    filter(CPSIDP %in% Man1$CPSIDP) %>%
    mutate(une = if_else(EMPSTAT == 21 | EMPSTAT ==22, 1, 0),
           not = if_else(LABFORCE == 1, 1, 0)) %>%
    select(c("CPSIDP", "une", "not"))
  #add columns from Man2 to Man1
  Man3 <- inner_join(Man1, Man2, by = "CPSIDP")
  #loop 
  z = 2
  while (z <= 263) {
    Man1 <-filter(df, i == z) %>%
      filter(IND1990 %in% Manufacturing$IND1990, EMPSTAT == 10 | EMPSTAT == 12) 
    Man2 <- filter(df, i == z+3) %>%
      filter(CPSIDP %in% Man1$CPSIDP) %>%
      mutate(une = if_else(EMPSTAT == 21 | EMPSTAT ==22, 1, 0),
             not = if_else(LABFORCE == 1, 1, 0)) %>%
      select(c("CPSIDP", "une", "not"))
    Man4 <- inner_join(Man1, Man2, by = "CPSIDP")
    Man3 <- bind_rows(Man3, Man4)
    z = z + 1
  }
  
  Man5 <- Man3 %>% group_by(Date, Region) %>%
    summarise(count=n(), count_une=sum(une), count_not=sum(not),
              trans_une = 100 * round(sum(une*WTFINL)/sum(WTFINL), 3), 
              trans_not = 100 * round(sum(not*WTFINL)/sum(WTFINL), 3))
  
  Man6 <- Man3 %>% group_by(Date) %>%
    summarise(count=n(), count_une=sum(une), count_not=sum(not),
              trans_une = 100 * round(sum(une*WTFINL)/sum(WTFINL), 3), 
              trans_not = 100 * round(sum(not*WTFINL)/sum(WTFINL), 3))

# 5th party data-Other_industries
  #select current month
  Oth1 <-filter(df, i == 1) %>%
    filter(IND1990 %in% Otherindustries$IND1990, EMPSTAT == 10 | EMPSTAT == 12) 
  #select people still in survey, and add variables to represent status we need
  Oth2 <- filter(df, i == 4) %>%
    filter(CPSIDP %in% Oth1$CPSIDP) %>%
    mutate(une = if_else(EMPSTAT == 21 | EMPSTAT ==22, 1, 0),
           not = if_else(LABFORCE == 1, 1, 0)) %>%
    select(c("CPSIDP", "une", "not"))
  #add columns from Oth2 to Oth1
  Oth3 <- inner_join(Oth1, Oth2, by = "CPSIDP")
  #loop 
  z = 2
  while (z <= 263) {
    Oth1 <-filter(df, i == z) %>%
      filter(IND1990 %in% Otherindustries$IND1990, EMPSTAT == 10 | EMPSTAT == 12) 
    Oth2 <- filter(df, i == z+3) %>%
      filter(CPSIDP %in% Oth1$CPSIDP) %>%
      mutate(une = if_else(EMPSTAT == 21 | EMPSTAT ==22, 1, 0),
             not = if_else(LABFORCE == 1, 1, 0)) %>%
      select(c("CPSIDP", "une", "not"))
    Oth4 <- inner_join(Oth1, Oth2, by = "CPSIDP")
    Oth3 <- bind_rows(Oth3, Oth4)
    z = z + 1
  }
  
  Oth5 <- Oth3 %>% group_by(Date, Region) %>%
    summarise(count=n(), count_une=sum(une), count_not=sum(not),
              trans_une = 100 * round(sum(une*WTFINL)/sum(WTFINL), 3), 
              trans_not = 100 * round(sum(not*WTFINL)/sum(WTFINL), 3))
  
  Oth6 <- Oth3 %>% group_by(Date) %>%
    summarise(count=n(), count_une=sum(une), count_not=sum(not),
              trans_une = 100 * round(sum(une*WTFINL)/sum(WTFINL), 3), 
              trans_not = 100 * round(sum(not*WTFINL)/sum(WTFINL), 3))
# save data
  save(Lei5, file = "Lei5.RData")
  save(Lei6, file = "Lei6.RData")
  save(Man5, file = "Man5.RData")
  save(Man6, file = "Man6.RData")
  save(Oth5, file = "Oth5.RData")
  save(Oth6, file = "Oth6.RData")
  
# 6th&7th party data-Leisure
  #select current month
  Lei1 <-filter(df, i == 1) %>%
    filter(IND1990 %in% Leisure$IND1990, EMPSTAT == 21 | EMPSTAT == 22) 
  #select people still in survey, and add variables to represent status we need
  Lei2 <- filter(df, i == 4) %>%
    filter(CPSIDP %in% Lei1$CPSIDP) %>%
    mutate(Lei = if_else(IND1990 %in% Leisure$IND1990, if_else(
             EMPSTAT == 10 | EMPSTAT == 12, 1, 0), 0),
           Man = if_else(IND1990 %in% Manufacturing$IND1990, if_else(
             EMPSTAT == 10 | EMPSTAT == 12, 1, 0), 0),
           Oth = if_else(IND1990 %in% Otherindustries$IND1990,if_else(
             EMPSTAT == 10 | EMPSTAT == 12, 1, 0), 0)) %>%
    select(c("CPSIDP", "Lei", "Man", "Oth"))
  #add columns from lei2 to lei1
  Lei3 <- inner_join(Lei1, Lei2, by = "CPSIDP")
  #loop 
  z = 2
  while (z <= 263) {
    Lei1 <-filter(df, i == z) %>%
      filter(IND1990 %in% Leisure$IND1990, EMPSTAT == 21 | EMPSTAT == 22)
    Lei2 <- filter(df, i == z+3) %>%
      filter(CPSIDP %in% Lei1$CPSIDP) %>%
      mutate(Lei = if_else(IND1990 %in% Leisure$IND1990, if_else(
               EMPSTAT == 10 | EMPSTAT == 12, 1, 0), 0),
             Man = if_else(IND1990 %in% Manufacturing$IND1990, if_else(
               EMPSTAT == 10 | EMPSTAT == 12, 1, 0), 0),
             Oth = if_else(IND1990 %in% Otherindustries$IND1990,if_else(
               EMPSTAT == 10 | EMPSTAT == 12, 1, 0), 0)) %>%
      select(c("CPSIDP", "Lei", "Man", "Oth"))
    Lei4 <- inner_join(Lei1, Lei2, by = "CPSIDP")
    Lei3 <- bind_rows(Lei3, Lei4)
    z = z + 1
  }
  
  unLei5 <- Lei3 %>% group_by(Date, Region) %>%
    summarise(count=n(), count_Lei=sum(Lei), count_Man=sum(Man), count_Oth=sum(Oth),
              trans_Lei = 100 * round(sum(Lei*WTFINL)/sum(WTFINL), 5), 
              trans_Man = 100 * round(sum(Man*WTFINL)/sum(WTFINL), 5),
              trans_Oth = 100 * round(sum(Oth*WTFINL)/sum(WTFINL), 5))
  
  unLei6 <- Lei3 %>% group_by(Date) %>%
    summarise(count=n(), count_Lei=sum(Lei), count_Man=sum(Man), count_Oth=sum(Oth),
              trans_Lei = 100 * round(sum(Lei*WTFINL)/sum(WTFINL), 5), 
              trans_Man = 100 * round(sum(Man*WTFINL)/sum(WTFINL), 5),
              trans_Oth = 100 * round(sum(Oth*WTFINL)/sum(WTFINL), 5))
  
# 6th&7th party data-Manufacturing
  #select current month
  Man1 <-filter(df, i == 1) %>%
    filter(IND1990 %in% Manufacturing$IND1990, EMPSTAT == 21 | EMPSTAT == 22) 
  #select people still in survey, and add variables to represent status we need
  Man2 <- filter(df, i == 4) %>%
    filter(CPSIDP %in% Man1$CPSIDP) %>%
    mutate(Lei = if_else(IND1990 %in% Leisure$IND1990, if_else(
      EMPSTAT == 10 | EMPSTAT == 12, 1, 0), 0),
      Man = if_else(IND1990 %in% Manufacturing$IND1990, if_else(
        EMPSTAT == 10 | EMPSTAT == 12, 1, 0), 0),
      Oth = if_else(IND1990 %in% Otherindustries$IND1990,if_else(
        EMPSTAT == 10 | EMPSTAT == 12, 1, 0), 0)) %>%
    select(c("CPSIDP", "Lei", "Man", "Oth"))
  #add columns from Man2 to Man1
  Man3 <- inner_join(Man1, Man2, by = "CPSIDP")
  #loop 
  z = 2
  while (z <= 263) {
    Man1 <-filter(df, i == z) %>%
      filter(IND1990 %in% Manufacturing$IND1990, EMPSTAT == 21 | EMPSTAT == 22)
    Man2 <- filter(df, i == z+3) %>%
      filter(CPSIDP %in% Man1$CPSIDP) %>%
      mutate(Lei = if_else(IND1990 %in% Leisure$IND1990, if_else(
        EMPSTAT == 10 | EMPSTAT == 12, 1, 0), 0),
        Man = if_else(IND1990 %in% Manufacturing$IND1990, if_else(
          EMPSTAT == 10 | EMPSTAT == 12, 1, 0), 0),
        Oth = if_else(IND1990 %in% Otherindustries$IND1990,if_else(
          EMPSTAT == 10 | EMPSTAT == 12, 1, 0), 0)) %>%
      select(c("CPSIDP", "Lei", "Man", "Oth"))
    Man4 <- inner_join(Man1, Man2, by = "CPSIDP")
    Man3 <- bind_rows(Man3, Man4)
    z = z + 1
  }
  
  unMan5 <- Man3 %>% group_by(Date, Region) %>%
    summarise(count=n(), count_Lei=sum(Lei), count_Man=sum(Man), count_Oth=sum(Oth),
              trans_Lei = 100 * round(sum(Lei*WTFINL)/sum(WTFINL), 5), 
              trans_Man = 100 * round(sum(Man*WTFINL)/sum(WTFINL), 5),
              trans_Oth = 100 * round(sum(Oth*WTFINL)/sum(WTFINL), 5))
  
  unMan6 <- Man3 %>% group_by(Date) %>%
    summarise(count=n(), count_Lei=sum(Lei), count_Man=sum(Man), count_Oth=sum(Oth),
              trans_Lei = 100 * round(sum(Lei*WTFINL)/sum(WTFINL), 5), 
              trans_Man = 100 * round(sum(Man*WTFINL)/sum(WTFINL), 5),
              trans_Oth = 100 * round(sum(Oth*WTFINL)/sum(WTFINL), 5))
  
# 6th&7th party data-Other_industries
  #select current month
  Oth1 <-filter(df, i == 1) %>%
    filter(IND1990 %in% Otherindustries$IND1990, EMPSTAT == 21 | EMPSTAT == 22) 
  #select people still in survey, and add variables to represent status we need
  Oth2 <- filter(df, i == 4) %>%
    filter(CPSIDP %in% Oth1$CPSIDP) %>%
    mutate(Lei = if_else(IND1990 %in% Leisure$IND1990, if_else(
      EMPSTAT == 10 | EMPSTAT == 12, 1, 0), 0),
      Man = if_else(IND1990 %in% Manufacturing$IND1990, if_else(
        EMPSTAT == 10 | EMPSTAT == 12, 1, 0), 0),
      Oth = if_else(IND1990 %in% Otherindustries$IND1990,if_else(
        EMPSTAT == 10 | EMPSTAT == 12, 1, 0), 0)) %>%
    select(c("CPSIDP", "Lei", "Man", "Oth"))
  #add columns from Oth2 to Oth1
  Oth3 <- inner_join(Oth1, Oth2, by = "CPSIDP")
  #loop 
  z = 2
  while (z <= 263) {
    Oth1 <-filter(df, i == z) %>%
      filter(IND1990 %in% Otherindustries$IND1990, EMPSTAT == 21 | EMPSTAT == 22)
    Oth2 <- filter(df, i == z+3) %>%
      filter(CPSIDP %in% Oth1$CPSIDP) %>%
      mutate(Lei = if_else(IND1990 %in% Leisure$IND1990, if_else(
        EMPSTAT == 10 | EMPSTAT == 12, 1, 0), 0),
        Man = if_else(IND1990 %in% Manufacturing$IND1990, if_else(
          EMPSTAT == 10 | EMPSTAT == 12, 1, 0), 0),
        Oth = if_else(IND1990 %in% Otherindustries$IND1990,if_else(
          EMPSTAT == 10 | EMPSTAT == 12, 1, 0), 0)) %>%
      select(c("CPSIDP", "Lei", "Man", "Oth"))
    Oth4 <- inner_join(Oth1, Oth2, by = "CPSIDP")
    Oth3 <- bind_rows(Oth3, Oth4)
    z = z + 1
  }
  
  unOth5 <- Oth3 %>% group_by(Date, Region) %>%
    summarise(count=n(), count_Lei=sum(Lei), count_Man=sum(Man), count_Oth=sum(Oth),
              trans_Lei = 100 * round(sum(Lei*WTFINL)/sum(WTFINL), 5), 
              trans_Man = 100 * round(sum(Man*WTFINL)/sum(WTFINL), 5),
              trans_Oth = 100 * round(sum(Oth*WTFINL)/sum(WTFINL), 5))
  
  unOth6 <- Oth3 %>% group_by(Date) %>%
    summarise(count=n(), count_Lei=sum(Lei), count_Man=sum(Man), count_Oth=sum(Oth),
              trans_Lei = 100 * round(sum(Lei*WTFINL)/sum(WTFINL), 5), 
              trans_Man = 100 * round(sum(Man*WTFINL)/sum(WTFINL), 5),
              trans_Oth = 100 * round(sum(Oth*WTFINL)/sum(WTFINL), 5))  
  # save data
  save(unLei5, file = "unLei5.RData")
  save(unLei6, file = "unLei6.RData")
  save(unMan5, file = "unMan5.RData")
  save(unMan6, file = "unMan6.RData")
  save(unOth5, file = "unOth5.RData")
  save(unOth6, file = "unOth6.RData")
  

  