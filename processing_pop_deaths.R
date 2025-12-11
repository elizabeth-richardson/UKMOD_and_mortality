# Extracting population and deaths data for income modelling
# Dec 2025: latest available are 2023 for deaths (maybe 2024), but still only 2022 for SAPE population

library(tidyverse)
library(odbc) # for reading oracle databases
library(zoo)
library(here)
library(openxlsx)
library(arrow)


## age group function (now standard age groups, including 10-14 not 10-15)
add_agegp20_LE <- function(df) {
  df <- df %>%
    mutate(agegp20_LE = case_when(age == 0 ~ 1,
                                  age %in% c(1:4) ~ 2,
                                  age %in% c(5:9) ~ 3,
                                  age %in% c(10:14) ~ 4,
                                  age %in% c(15:19) ~ 5,
                                  age %in% c(20:24) ~ 6,
                                  age %in% c(25:29) ~ 7,
                                  age %in% c(30:34) ~ 8,
                                  age %in% c(35:39) ~ 9,
                                  age %in% c(40:44) ~ 10,
                                  age %in% c(45:49) ~ 11,
                                  age %in% c(50:54) ~ 12,
                                  age %in% c(55:59) ~ 13,
                                  age %in% c(60:64) ~ 14,
                                  age %in% c(65:69) ~ 15,
                                  age %in% c(70:74) ~ 16,
                                  age %in% c(75:79) ~ 17,
                                  age %in% c(80:84) ~ 18,
                                  age %in% c(85:89) ~ 19,
                                  age >= 90 ~ 20))
  
}


## Get SIMD lookup ----
###############################################.

# SIMD 2020 data for 2011 datazones (NHS open data)
open_simd_2020 <- "https://www.opendata.nhs.scot/dataset/78d41fa9-1a62-4f7b-9edb-3e8522a93378/resource/acade396-8430-4b34-895a-b3e757fa346e/download/simd2020v2_22062020.csv"
dz11_simd <- read.csv(open_simd_2020) %>%
  select(DataZone, SIMD2020V2CountryQuintile, SIMD2020V2CountryDecile) %>%
  setNames(tolower(names(.)))  #variables to lower case


## Extract deaths data from SMRA ----
###############################################.
# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), 
                                      pwd=.rs.askForPassword("SMRA Password:")))
# #get all col names
# allcols <- tibble::as_tibble(dbGetQuery(channel, statement=
#                                           "SELECT *
#                             FROM ANALYSIS.GRO_DEATHS_C
#                             WHERE date_of_registration between '1 January 2023' AND '31 January 2023'
#                             ")) %>%  setNames(tolower(names(.)))  #variables to lower case
# 
# names(allcols) #157 vars
# # country_of_residence
# table(allcols$country_of_residence)
# #want all == XS (Scotland)

# Extract deaths for 2022 to 2023  
deaths_allage <- tibble::as_tibble(dbGetQuery(channel, statement=
                                                "SELECT year_of_registration year, age, SEX sex, datazone_2011 datazone  
                            FROM ANALYSIS.GRO_DEATHS_C 
                            WHERE date_of_registration between '1 January 2022' AND '31 December 2023'
                            AND country_of_residence = 'XS'
                            ")) %>%  setNames(tolower(names(.)))  #variables to lower case

##link to SIMD data

deaths <- deaths_allage %>%
  mutate(sex = case_when(sex=="1" ~ "M",
                         sex=="2" ~ "F")) %>%
  add_agegp20_LE() %>%
  merge(y=dz11_simd, by="datazone", all.x=T) %>%
  mutate(simd5 = as.integer(simd2020v2countryquintile)) %>%
  mutate(simd10 = as.integer(simd2020v2countrydecile))

# do aggregations 
# by decile
deaths_SIMD10_sex_age <- deaths %>%
  group_by(year, simd10, sex, agegp20_LE) %>%
  summarise(deaths = n()) %>%
  ungroup()
# by quintile
deaths_SIMD5_sex_age <- deaths %>%
  group_by(year, simd5, sex, agegp20_LE) %>%
  summarise(deaths = n()) %>%
  ungroup()

## Get population data ----
###############################################.

# Population for datazones
# available to 2022 currently (Dec 2025) (NHS open data)
open_pop_file <- "https://www.opendata.nhs.scot/dataset/7f010430-6ce1-4813-b25c-f7f335bdc4dc/resource/c505f490-c201-44bd-abd1-1bd7a64285ee/download/dz2011-pop-est_21112024.csv"

# 2011 datazones
pop_2022 <- read.csv(open_pop_file) 

pop_2022 <- pop_2022 %>%
  setNames(tolower(names(.))) %>% #variables to lower case
  select(year, datazone, sex,
         starts_with("age")) %>%
  pivot_longer(cols = starts_with("age"), names_to="age", names_prefix="age", values_to = "pop" ) %>%
  mutate(age = case_when(age=="90plus" ~ "90",
                         TRUE ~ age),
         age = as.integer(age)) %>%
  mutate(sex = case_when(sex=="Female" ~ "F",
                         sex=="Male" ~ "M",
                         sex=="All" ~ "All")) %>%
  filter(sex!="All") %>%
  filter(datazone!="S92000003") %>%
  filter(year==2022) %>%
  add_agegp20_LE() %>%
  group_by(year, datazone, sex, agegp20_LE) %>%
  summarise(pop = sum(pop, na.rm=T)) %>% 
  ungroup() %>%
  merge(y=dz11_simd, by="datazone", all.x=T) %>%
  mutate(simd5 = as.integer(simd2020v2countryquintile)) %>%
  mutate(simd10 = as.integer(simd2020v2countrydecile))

# use 2022 pop data for 2023, before 2023 data are released
# do aggregations 
# by decile
pop_2023_SIMD10_sex_age <- pop_2022 %>%
  group_by(simd10, sex, agegp20_LE) %>%
  summarise(pop_agegp20_LE = sum(pop, na.rm=T)) %>%
  ungroup() %>%
  mutate(year = 2023)
# by quintile
pop_2023_SIMD5_sex_age <- pop_2022 %>%
  group_by(simd5, sex, agegp20_LE) %>%
  summarise(pop_agegp20_LE = sum(pop, na.rm=T)) %>%
  ungroup() %>%
  mutate(year = 2023)


# Combine data and calc rates
###############################################.

# CRUDE RATES
# by decile
pop_deaths_SIMD10_agegp20_LE <- pop_2023_SIMD10_sex_age %>%
  merge(y = deaths_SIMD10_sex_age, by=c("year", "simd10", "sex", "agegp20_LE"), all.x=TRUE) %>%
  replace_na(list(deaths = 0)) %>%
  rename(pop = pop_agegp20_LE) %>%
  mutate(rate1000 = 1000 * deaths / pop)


# by quintile
pop_deaths_SIMD5_agegp20_LE <- pop_2023_SIMD5_sex_age %>%
  merge(y = deaths_SIMD5_sex_age, by=c("year", "simd5", "sex", "agegp20_LE"), all.x=TRUE) %>%
  replace_na(list(deaths = 0)) %>%
  rename(pop = pop_agegp20_LE) %>%
  mutate(rate1000 = 1000 * deaths / pop)




# Save pop and deaths data (2023)
###############################################.

arrow::write_parquet(pop_deaths_SIMD10_agegp20_LE,
                     sink = here("analysis", "pop_deaths_SIMD10_agegp20_LE.parquet"),
                     compression = "zstd")
arrow::write_parquet(pop_deaths_SIMD5_agegp20_LE,
                     sink = here("analysis", "pop_deaths_SIMD5_agegp20_LE.parquet"),
                     compression = "zstd")

pop_deaths_SIMD10_agegp20_LE <- arrow::read_parquet(here("analysis", "pop_deaths_SIMD10_agegp20_LE.parquet"))
pop_deaths_SIMD5_agegp20_LE <- arrow::read_parquet(here("analysis", "pop_deaths_SIMD5_agegp20_LE.parquet"))



# EASRs:
###############################################.

#Get the European Standard Population (2013) and age group entry ages
# agegp1 = 0 year olds
# agegp20 = 90+ year olds
#esp with oldest age group 90+
agegp20_LE <- c(1:20)
esp2013pop <- c(1000, 4000, 5500, 5500, 5500, 6000, 6000, 6500, 7000, 7000, 7000, 7000, 6500, 6000,
                5500, 5000, 4000, 2500, 1500, 1000) 

entryage <- c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90)
esp2013 <- data.frame(agegp20_LE, esp2013pop, entryage, stringsAsFactors=FALSE)

#calc whole pop EASRs: by decile
easrs_simd10_2023 <- pop_deaths_SIMD10_agegp20_LE %>%
  merge(y=esp2013, by="agegp20_LE", all.x=TRUE) %>%
  mutate(asr100k = deaths*100000/pop,
         asr_esp = asr100k * esp2013pop) %>%
  group_by(simd10, year) %>%
  summarise(asr_esp = sum(asr_esp, na.rm=TRUE),
            esp2013pop = sum(esp2013pop, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(easr = asr_esp / esp2013pop) %>%
  select(simd10, year, easr)

#save
arrow::write_parquet(easrs_simd10_2023,
                     sink = here("analysis", "easrs_simd10_2023.parquet"),
                     compression = "zstd")

#calc whole pop EASRs: by quintile
easrs_simd5_2023 <- pop_deaths_SIMD5_agegp20_LE %>%
  merge(y=esp2013, by="agegp20_LE", all.x=TRUE) %>%
  mutate(asr100k = deaths*100000/pop,
         asr_esp = asr100k * esp2013pop) %>%
  group_by(simd5, year) %>%
  summarise(asr_esp = sum(asr_esp, na.rm=TRUE),
            esp2013pop = sum(esp2013pop, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(easr = asr_esp / esp2013pop) %>%
  select(simd5, year, easr)

#save
arrow::write_parquet(easrs_simd5_2023,
                     sink = here("analysis", "easrs_simd5_2023.parquet"),
                     compression = "zstd")


