# junk file to test snippets

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

iee_data %>% 
  group_by(has_e) %>% 
  ggplot(aes(cut_width(fam_med_income,40000), fill = has_e)) +
  geom_bar()

iee_data_t <- iee_data %>% 
  mutate(fam_quant = cut(fam_med_income,quantile(fam_med_income, na.rm =T),include.lowest = T))

fam_prop <- iee_data %>% 
  filter(!is.na(fam_med_income)) %>% 
  group_by(has_e,fam_quant) %>%  
  summarise(freq = n()) %>% 
  group_by(fam_quant) %>% 
  mutate(prop = round(freq/sum(freq),2)) %>% 
  ungroup()

iee_data %>% 
  filter(!is.na(fam_med_income)) %>% 
  group_by(has_e) %>% 
  ggplot(aes(fam_quant, fill = has_e)) +
  geom_bar() +
  geom_text(data = fam_prop, aes(label = prop, x = fam_quant, y = c(rep(5000,4),rep(1000,4)))) +
  scale_x_discrete(labels = c("Q1","Q2","Q3","Q4"))

iee_data %>% count(fam_quant)

cut_clean <- function(df) {
  df %>% 
  mutate(quant = str_remove_all(quant, "\\(|\\]")) %>% 
  separate(quant, into = c('q1', 'q2'), sep=",", convert = TRUE) %>% 
  mutate(across(q1:q2, ~ scales::dollar(.x)), 
         quant = glue::glue("({q1},{q2}]"), q1 = NULL, q2 = NULL)
}

df <- data.frame(income = rnorm(1000,77345,30569))
df$quant <- cut(df$income, quantile(df$income),include.lowest = T)

flevel <- levels(df$quant[1])

x <- flevel[1]

clean_interval <- function(x) {
  oc <- c(str_sub(x,1,1),str_sub(x,-1,-1))
  x <- str_sub(x,2,str_length(x)-1)
  x <- as.double(str_split(x,",",simplify=T))
  x <- scales::dollar(x)
  paste0(oc[1],x[1],",",x[2],oc[2])
}
lapply(flevel,clean_interval) 
