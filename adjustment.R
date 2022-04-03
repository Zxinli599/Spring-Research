Reg <- c("Midwest", "Northeast", "South", "West")
data1 <- c("Lei5", "Man5", "Oth5")
data2 <- c("Lei6", "Man6", "Oth6")
data3 <- c("unLei5", "unMan5", "unOth5")
data4 <- c("unLei6", "unMan6", "unOth6")
#region dataframe adjustment
z <- 1
while(z <= 3) {
a <- get(data1[z]) %>% ungroup() %>% 
  filter(Region == Reg[1]) %>%
  select("trans_une") %>%
  ts(start = 2000, frequency = 12) %>%
  seas() %>%
  as.data.frame() %>%
  select(c(1,2)) %>%
  rename(une_adj = final)

b <- get(data1[z]) %>% ungroup() %>% 
  filter(Region == Reg[1]) %>%
  select("trans_not") %>%
  ts(start = 2000, frequency = 12) %>%
  seas() %>%
  as.data.frame() %>%
  select(c(1,2)) %>%
  rename(not_adj = final)

c <- full_join(a, b, by="date") %>%
  mutate(Region = Reg[1])

i <- 2
while(i <= 4){
  a <- get(data1[z]) %>% ungroup() %>% 
    filter(Region == Reg[i]) %>%
    select("trans_une") %>%
    ts(start = 2000, frequency = 12) %>%
    seas() %>%
    as.data.frame() %>%
    select(c(1,2)) %>%
    rename(une_adj = final)
  
  b <- get(data1[z]) %>% ungroup() %>% 
    filter(Region == Reg[i]) %>%
    select("trans_not") %>%
    ts(start = 2000, frequency = 12) %>%
    seas() %>%
    as.data.frame() %>%
    select(c(1,2)) %>%
    rename(not_adj = final)
  
  d <- full_join(a, b, by="date") %>%
    mutate(Region = Reg[i])
  
  c <- bind_rows(c,d)
  i = i+1
}

assign(paste(data1[z], "adj", sep = "_"), c[,c(1,4,2,3)])
z = z+1
}

#nationwide data adjustment
z <- 1
while(z <= 3) {
  a <- get(data2[z]) %>% ungroup() %>% 
    select("trans_une") %>%
    ts(start = 2000, frequency = 12) %>%
    seas() %>%
    as.data.frame() %>%
    select(c(1,2)) %>%
    rename(une_adj = final)
  
  b <- get(data2[z]) %>% ungroup() %>% 
    select("trans_not") %>%
    ts(start = 2000, frequency = 12) %>%
    seas() %>%
    as.data.frame() %>%
    select(c(1,2)) %>%
    rename(not_adj = final)
  
  c <- full_join(a, b, by="date")
  
  assign(paste(data2[z], "adj", sep = "_"), c)
  z = z+1
}

#region join industry data adjustment
z <- 1
while(z <= 3) {
  a <- get(data3[z]) %>% ungroup() %>% 
    filter(Region == Reg[1]) %>%
    select("trans_Lei") %>%
    ts(start = 2000, frequency = 12) %>%
    seas(transform.function = "none", outlier = NULL) %>%
    as.data.frame() %>%
    select(c(1,2)) %>%
    rename(trans_Lei_adj = final)
  
  b <- get(data3[z]) %>% ungroup() %>% 
    filter(Region == Reg[1]) %>%
    select("trans_Man") %>%
    ts(start = 2000, frequency = 12) %>%
    seas(transform.function = "none", outlier = NULL) %>%
    as.data.frame() %>%
    select(c(1,2)) %>%
    rename(trans_Man_adj = final)
  
  c <- get(data3[z]) %>% ungroup() %>% 
    filter(Region == Reg[1]) %>%
    select("trans_Oth") %>%
    ts(start = 2000, frequency = 12) %>%
    seas(transform.function = "none", outlier = NULL) %>%
    as.data.frame() %>%
    select(c(1,2)) %>%
    rename(trans_Oth_adj = final)
  
  d <- full_join(a, b, by="date") %>%
    full_join(c, by="date") %>%
    mutate(Region = Reg[1])
  
  i <- 2
  while(i <= 4){
    a <- get(data3[z]) %>% ungroup() %>% 
      filter(Region == Reg[i]) %>%
      select("trans_Lei") %>%
      ts(start = 2000, frequency = 12) %>%
      seas(transform.function = "none", outlier = NULL) %>%
      as.data.frame() %>%
      select(c(1,2)) %>%
      rename(trans_Lei_adj = final)
    
    b <- get(data3[z]) %>% ungroup() %>% 
      filter(Region == Reg[i]) %>%
      select("trans_Man") %>%
      ts(start = 2000, frequency = 12) %>%
      seas(transform.function = "none", outlier = NULL) %>%
      as.data.frame() %>%
      select(c(1,2)) %>%
      rename(trans_Man_adj = final)
    
    c <- get(data3[z]) %>% ungroup() %>% 
      filter(Region == Reg[i]) %>%
      select("trans_Oth") %>%
      ts(start = 2000, frequency = 12) %>%
      seas(transform.function = "none", outlier = NULL) %>%
      as.data.frame() %>%
      select(c(1,2)) %>%
      rename(trans_Oth_adj = final)
    
    e <- full_join(a, b, by="date") %>%
      full_join(c, by="date") %>%
      mutate(Region = Reg[i])
    
    d <- bind_rows(d,e)
    i = i+1
  }
  
  assign(paste(data3[z], "adj", sep = "_"), d[,c(1,5,2,3,4)])
  z = z+1
}

#nationwide adjustment
#region join industry data adjustment
z <- 1
while(z <= 3) {
  a <- get(data4[z]) %>% ungroup() %>% 
    select("trans_Lei") %>%
    ts(start = 2000, frequency = 12) %>%
    seas(transform.function = "none", outlier = NULL) %>%
    as.data.frame() %>%
    select(c(1,2)) %>%
    rename(trans_Lei_adj = final)
  
  b <- get(data4[z]) %>% ungroup() %>% 
    select("trans_Man") %>%
    ts(start = 2000, frequency = 12) %>%
    seas(transform.function = "none", outlier = NULL) %>%
    as.data.frame() %>%
    select(c(1,2)) %>%
    rename(trans_Man_adj = final)
  
  c <- get(data4[z]) %>% ungroup() %>% 
    select("trans_Oth") %>%
    ts(start = 2000, frequency = 12) %>%
    seas(transform.function = "none", outlier = NULL) %>%
    as.data.frame() %>%
    select(c(1,2)) %>%
    rename(trans_Oth_adj = final)
  
  d <- full_join(a, b, by="date") %>%
    full_join(c, by="date") 
  
  assign(paste(data4[z], "adj", sep = "_"), d)
  z = z+1
}

#find unchanged and save
