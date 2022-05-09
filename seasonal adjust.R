library(seasonal)
load("Man3.Rdata")
Man3$Year_Month <- ym(Man3$Year_Month)
a <- select(Man3, 3) %>%
  ts(start = 2000, frequency = 12) %>%
  seas() %>%
  as.data.frame() %>%
  select(c(1,2)) %>%
  rename(unemployment = final)

b <- select(Man3, 4) %>%
  ts(start = 2000, frequency = 12) %>%
  seas() %>%
  as.data.frame() %>%
  select(c(1,2)) %>%
  rename(not_in_laborforce = final)

adjMan3 <- full_join(a, b, by="date")

#save(adjMan3, file = "adjMan3.RData")

load("undLei5.Rdata")
undLei5$Year_Month <- ym(undLei5$Year_Month)
a <- select(undLei5, 3) %>%
  ts(start = 2000, frequency = 12) %>%
  seas() %>%
  as.data.frame() %>%
  select(c(1,2)) %>%
  rename(unemployment = final)

b <- select(undLei5, 4) %>%
  ts(start = 2000, frequency = 12) %>%
  seas() %>%
  as.data.frame() %>%
  select(c(1,2)) %>%
  rename(not_in_laborforce = final)

adjLei3 <- full_join(a, b, by="date")
#save(adjLei3, file = "adjLei3.RData")

load("Oth3.Rdata")
Oth3$Year_Month <- ym(Oth3$Year_Month)
a <- select(Oth3, 3) %>%
  ts(start = 2000, frequency = 12) %>%
  seas() %>%
  as.data.frame() %>%
  select(c(1,2)) %>%
  rename(unemployment = final)

b <- select(Oth3, 4) %>%
  ts(start = 2000, frequency = 12) %>%
  seas() %>%
  as.data.frame() %>%
  select(c(1,2)) %>%
  rename(not_in_laborforce = final)

adjOth3 <- full_join(a, b, by="date")
#save(adjOth3, file = "adjOth3.RData")

#Man
load("undMan5.Rdata")
undMan5$Year_Month <- ym(undMan5$Year_Month)
a <- select(undMan5, 3) %>%
  ts(start = 2000, frequency = 12) %>%
  seas() %>%
  as.data.frame() %>%
  select(c(1,2)) %>%
  rename(inMan = final)

b <- select(undMan5, 4) %>%
  ts(start = 2000, frequency = 12) %>%
  seas() %>%
  as.data.frame() %>%
  select(c(1,2)) %>%
  rename(inOth = final)

c <- select(undMan5, 5) %>%
  ts(start = 2000, frequency = 12) %>%
  seas() %>%
  as.data.frame() %>%
  select(c(1,2)) %>%
  rename(inLei = final)

adjundMan5 <- full_join(a, b, by="date") %>%
  full_join(c, by="date")
save(adjundMan5, file = "adjundMan5.RData")


#Leisure
load("undLei5.Rdata")
undLei5$Year_Month <- ym(undLei5$Year_Month)
a <- select(undLei5, 3) %>%
  ts(start = 2000, frequency = 12) %>%
  seas() %>%
  as.data.frame() %>%
  select(c(1,2)) %>%
  rename(inMan = final)

b <- select(undLei5, 4) %>%
  ts(start = 2000, frequency = 12) %>%
  seas() %>%
  as.data.frame() %>%
  select(c(1,2)) %>%
  rename(inOth = final)

c <- select(undLei5, 5) %>%
  ts(start = 2000, frequency = 12) %>%
  seas() %>%
  as.data.frame() %>%
  select(c(1,2)) %>%
  rename(inLei = final)

adjundLei5 <- full_join(a, b, by="date") %>%
  full_join(c, by="date")
save(adjundLei5, file = "adjundLei5.RData")

#Oth
load("undOth5.Rdata")
undOth5$Year_Month <- ym(undOth5$Year_Month)
a <- select(undOth5, 3) %>%
  ts(start = 2000, frequency = 12) %>%
  seas() %>%
  as.data.frame() %>%
  select(c(1,2)) %>%
  rename(inMan = final)

b <- select(undOth5, 4) %>%
  ts(start = 2000, frequency = 12) %>%
  seas() %>%
  as.data.frame() %>%
  select(c(1,2)) %>%
  rename(inOth = final)

c <- select(undOth5, 5) %>%
  ts(start = 2000, frequency = 12) %>%
  seas() %>%
  as.data.frame() %>%
  select(c(1,2)) %>%
  rename(inLei = final)

adjundOth5 <- full_join(a, b, by="date") %>%
  full_join(c, by="date")
save(adjundOth5, file = "adjundOth5.RData")

