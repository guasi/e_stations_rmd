library(tidyverse)

# STATIONS per zipcode -----------------------------------------------
stations <- readRDS("data/stations.RDS")
stations_p_e <- stations %>% 
  filter(STATUS_CODE == "E",             #Available
         FUEL_TYPE_CODE == "ELEC",        #Electric
         ACCESS_CODE == "public") %>%    #public
  group_by(ZIP) %>% 
  summarize(e_stations = n(),
            sex = "BTSX")
  

# INCOME per zipcode ------------------------------------
meta <- readRDS("data/S2001/S2001_meta_processed.RDS")

income <- read_csv("data/S2001/S2001.csv")

income_disag <- income %>% 
  select(NAME,
         meta$NAME[4:12],
         meta$NAME[18:26],
         meta$NAME[32:40])

income_disag <- income_disag %>% 
  mutate(NAME = str_extract(NAME,".....$")) %>% 
  pivot_longer(cols = 2:28,names_to = "key", values_to = "w_full_time")

income_disag <- income_disag %>% 
  mutate(
    income_brk = case_when(
      substr(key,10,14) == "_004E" ~ "1_9999",
      substr(key,10,14) == "_005E" ~ "10000_14999",
      substr(key,10,14) == "_006E" ~ "15000_24999",
      substr(key,10,14) == "_007E" ~ "25000_34999",
      substr(key,10,14) == "_008E" ~ "35000_49999",
      substr(key,10,14) == "_009E" ~ "50000_64999",
      substr(key,10,14) == "_010E" ~ "65000_74999",
      substr(key,10,14) == "_011E" ~ "75000_99999",
      substr(key,10,14) == "_012E" ~ "100000_more"),
    sex = case_when(
      substr(key,6,9) == "_C01" ~ "BTSX",
      substr(key,6,9) == "_C03" ~ 		"M",
      substr(key,6,9) == "_C05" ~ 		"F")
  ) %>% 
  rename(ZIP = NAME)

income_medians_1 <- income %>% 
  select(NAME,
         S2001_C01_001E,
         S2001_C03_001E,
         S2001_C05_001E) %>% 
  rename(ZIP = NAME,
       BTSX = S2001_C01_001E,
       M = S2001_C03_001E,
       `F` = S2001_C05_001E) %>% 
  pivot_longer(cols = 2:4, names_to = "sex", values_to = "w")

income_medians_2 <- income %>% 
  select(NAME,
         S2001_C01_002E,
         S2001_C03_002E,
         S2001_C05_002E) %>% 
  rename(ZIP = NAME,
       BTSX = S2001_C01_002E,
       M = S2001_C03_002E,
       `F` = S2001_C05_002E) %>% 
  pivot_longer(cols = 2:4, names_to = "sex", values_to = "income_ave")
  
income_medians_3 <- income %>% 
  select(NAME,
         S2001_C01_003E,
         S2001_C03_003E,
         S2001_C05_003E) %>% 
  rename(ZIP = NAME,
       BTSX = S2001_C01_003E,
       M =    S2001_C03_003E,
       `F` =    S2001_C05_003E) %>% 
  pivot_longer(cols = 2:4, names_to = "sex", values_to = "w_full_time")

income_sums <- 
  left_join(income_medians_1,income_medians_2) %>% 
  left_join(income_medians_3) %>% 
  mutate(ZIP = str_extract(ZIP,".....$"))
  

rm(income_medians_1,income_medians_2,income_medians_3)

# JOINING E stations and Income --------------------------------------
stations_incomes <- 
  income_sums %>% 
  left_join(stations_p_e, by = c("ZIP","sex")) %>% 
  mutate(has_e = ifelse(is.na(e_stations),"no","yes"))


# PLOTS ------------------------------------------------------------
stations_incomes %>% 
  filter(!is.na(income_ave),!is.na(e_stations)) %>% 
  ggplot(aes(income_ave, e_stations)) +
  geom_bin_2d(bins = 100) +
  scale_fill_continuous(type = "viridis") +
  labs(x = "zip code median",
       y = "electric fuel stations per zip code",
       fill = "zipcode count")

stations_incomes %>% 
  filter(!is.na(income_ave)) %>% 
  ggplot(aes(income_ave)) + 
  geom_density(aes(fill = has_e, color = has_e), alpha = 1/3)

stations_incomes %>% 
  filter(!is.na(income_ave)) %>% 
  ggplot(aes(income_ave, fill = has_e, color = has_e)) + 
  geom_histogram(position = 'identity', alpha = .2, binwidth = 2000)
            
stations_incomes %>% 
  filter(!is.na(income_ave)) %>% 
  ggplot(aes(income_ave, has_e, fill = has_e)) + 
  geom_boxplot(varwidth = TRUE, alpha=1/3)
