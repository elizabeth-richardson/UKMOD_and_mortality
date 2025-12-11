# Script to facilitate working with UKMOD outputs.
# It covers:
# Running the UKMOD systems (requires EUROMOD and UKMOD)
# Importing and processing UKMOD results files (fiscal, poverty and income inequality stats)
# Getting SIMD deciles for the FRS households
# Equivalising the incomes for UKMOD results files
# 

###############################
# PACKAGES
###############################

library(tidyverse)
library(readxl) # reading in excel files
library(matrixStats) # for weighted median
library(broom) # for CIs in models
library(here) # for filepaths
library(arrow) # for working with parquet files
library(haven) # for reading in .dta files (survey data from UK Data Service)


###############################
# FILEPATHS
###############################

# make required subdirectories for processed data files (if they already exist RStudio will give a warning)
dir.create(here("ukmod output")) # will warn if it already exists
dir.create(here("ukmod output", "rds")) # will warn if it already exists
dir.create(here("ukmod output", "equiv")) # will warn if it already exists
dir.create(here("results")) # will warn if it already exists
dir.create(here("analysis")) # will warn if it already exists


#####################################
## RUN YOUR UKMOD SYSTEMS
#####################################

# Now run UKMOD
# Using uk_2023_b1 as input file (3 year pooled) (this gives the best sample size and the most up to date FRS data, with possibility for linking in SIMD deciles too)
# (input file uk_2023_b1 combines FRS 21/22, 22/23, 23/24)

# systems:
# sc_2023
# sc_2024
# sc_2025
# sc_2025_scpup (2025 with an increase of SCP to £50/week)

# Copy output files to this project's "ukmod output" folder
# Get summary fiscal and poverty results:
# Go to Applications > EUROMOD Statistics > Statistics Presenter > Default > select all the systems of interest
# Click the multiple floppy disc icon to save results files from UKMOD's stats presenter to this project's "results" folder (keep default name)
# (should save a single xlsx containing the results for all the systems, split into different tabs)


####################################
## IMPORT UKMOD RESULTS FILE:
#####################################

# Function to import and process the single spreadsheet saved from UKMOD's stats presenter: 
#tab_name <- "SC_2024"
#get_ukmod_data("SC_2024")

get_ukmod_data <- function(tab_name) {
  
  # Fiscal data, annual, £mill
  df <- read_excel(path = here("results", "UKMOD Statistics - Default.xlsx"), sheet = tab_name, range = "A8:B73") %>%
    filter(grepl("Government", ...1)) %>%
    mutate(system = tab_name) %>%
    select(fiscal=...1, mill = Amounts, system)
  df_name <- paste0("fiscal_", tab_name)
  assign(df_name, df, envir=.GlobalEnv)
  
  # Poverty rates
  df_bhc <- read_excel(path = here("results", "UKMOD Statistics - Default.xlsx"), sheet = tab_name, range = "A83:B90") %>%
    mutate(basis = "BHC") 
  df_ahc <- read_excel(path = here("results", "UKMOD Statistics - Default.xlsx"), sheet = tab_name, range = "A94:B101") %>%
    mutate(basis = "AHC") 
  df <- rbind(df_ahc, df_bhc) %>%
    mutate(system = tab_name) 
  names(df) <- c("group", "rate", "basis", "system")
  df_name <- paste0("poverty_", tab_name)
  assign(df_name, df, envir=.GlobalEnv)
  
  # Income inequals
  df_bhc <- read_excel(path = here("results", "UKMOD Statistics - Default.xlsx"), sheet = tab_name, range = "A110:C114") %>%
    mutate(basis = "BHC") 
  df_ahc <- read_excel(path = here("results", "UKMOD Statistics - Default.xlsx"), sheet = tab_name, range = "A117:C121") %>%
    mutate(basis = "AHC") 
  df <- rbind(df_ahc, df_bhc) %>%
    pivot_longer(cols = c("Gini", "S80/S20"), names_to = "index", values_to = "value") %>%
    mutate(system = tab_name) %>%
    select(inc_type = ...1, basis, index, value, system)
  df_name <- paste0("inc_ineq_", tab_name)
  assign(df_name, df, envir=.GlobalEnv)
  
  # Mean equiv income (weekly)
  df_bhc <- read_excel(path = here("results", "UKMOD Statistics - Default.xlsx"), sheet = tab_name, range = "A134:C146") %>%
    mutate(basis = "BHC") 
  df_ahc <- read_excel(path = here("results", "UKMOD Statistics - Default.xlsx"), sheet = tab_name, range = "A149:C161") %>%
    mutate(basis = "AHC") 
  df <- rbind(df_ahc, df_bhc) %>%
    pivot_longer(cols = -c(...1, basis), names_to = "index", values_to = "mean_inc") %>%
    mutate(system = tab_name) %>%
    select(inc_decile = ...1, basis, index, mean_inc, system)
  df_name <- paste0("equiv_inc_", tab_name)
  assign(df_name, df, envir=.GlobalEnv)
  
  # Cut-offs (weekly)
  df_bhc <- read_excel(path = here("results", "UKMOD Statistics - Default.xlsx"), sheet = tab_name, range = "A312:B322") %>%
    mutate(basis = "BHC") 
  names(df_bhc) <- c("inc_decile", "mean_inc", "basis")
  df_ahc <- read_excel(path = here("results", "UKMOD Statistics - Default.xlsx"), sheet = tab_name, range = "A325:B335") %>%
    mutate(basis = "AHC") 
  names(df_ahc) <- c("inc_decile", "mean_inc", "basis")
  df <- rbind(df_ahc, df_bhc) %>%
    mutate(system = tab_name) %>%
    select(inc_decile, basis, mean_inc, system)
  df_name <- paste0("cut_offs_", tab_name)
  assign(df_name, df, envir=.GlobalEnv)
  
}

# Get the data from the SIMD file
sheets <- grep("SC_", readxl::excel_sheets(path = here("results", "UKMOD Statistics - Default.xlsx")), value=TRUE) # all the tabs starting SC_ (i.e., all the systems we ran)

for (tab in sheets) {
  get_ukmod_data(tab)
}

# combine the dfs for the different systems

# fiscal
fiscal_data <- mget(ls(pattern = "fiscal_"), .GlobalEnv) %>% # gets the dataframes starting with fiscal_
  do.call(rbind.data.frame, .)  # rbinds them all together
write_rds(fiscal_data, here("results", "fiscal_data.rds"))

# poverty
poverty_data <- mget(ls(pattern = "poverty_"), .GlobalEnv) %>% # gets the dataframes starting with poverty_
  do.call(rbind.data.frame, .)  # rbinds them all together
write_rds(poverty_data, here("results", "poverty_data.rds"))

# inc_ineq
inc_ineq_data <- mget(ls(pattern = "inc_ineq_"), .GlobalEnv) %>% # gets the dataframes starting with inc_ineq_
  do.call(rbind.data.frame, .)  # rbinds them all together
write_rds(inc_ineq_data, here("results", "inc_ineq_data.rds"))

# fiscal
equiv_inc_data <- mget(ls(pattern = "equiv_inc_"), .GlobalEnv) %>% # gets the dataframes starting with equiv_inc__
  do.call(rbind.data.frame, .)  # rbinds them all together
write_rds(equiv_inc_data, here("results", "equiv_inc_data.rds"))

# cut-offs
cut_offs_data <- mget(ls(pattern = "cut_offs_"), .GlobalEnv) %>% # gets the dataframes starting with cut_offs__
  do.call(rbind.data.frame, .)  # rbinds them all together
write_rds(cut_offs_data, here("results", "cut_offs_data.rds"))


########################################################
# Get SIMD deciles for each household 
########################################################

# SIMD deciles for FRS households were recently made available in recent UK Data Service end-user licence FRS files.
# Use 3yr pooled FRS files: More Scottish data.
# input file uk_2023_b1 combines 21/22, 22/23, 23/24

# what idhh codes are used in the pooled data?
# input_file <- "UK_2023_b1.txt"
uk_2023_b1 <- read.delim("../UKMOD-PUBLIC-B2025.06/UKMOD-PUBLIC-B2025.06/Input/UK_2023_b1.txt", 
                         stringsAsFactors = FALSE, quote = "", sep = "\t") 
# they concatenate year and sernum/hh number to give an idhh var

# let's make the same idhh var in the FRS EUL files for the included years:
simdfile_2021 <- haven::read_dta(here("..", "FRS data from UKDS/FRS 2021-22/UKDA-9073-stata/stata/stata13_se/househol.dta")) %>% 
  janitor::clean_names() %>%
  mutate(imds = ifelse(imds==-1, as.numeric(NA), imds),
         idhh = paste0("2021", sernum)) %>%
  select(idhh, simd20_decile = imds) 
simdfile_2022 <- haven::read_dta(here("..", "FRS data from UKDS/FRS 2022-23/UKDA-9252-stata/stata/stata13_se/househol.dta")) %>% 
  janitor::clean_names() %>%
  mutate(idhh = paste0("2022", sernum)) %>%
  select(idhh, simd20_decile = imds) 
simdfile_2023 <- haven::read_dta(here("..", "FRS data from UKDS/FRS 2023-24/UKDA-9367-stata/stata/stata13/househol.dta")) %>% 
  janitor::clean_names() %>%
  mutate(idhh = paste0("2023", sernum)) %>%
  select(idhh, simd20_decile = imds) 
simdfile_2023_b1 = rbind(simdfile_2021,
                         simdfile_2022,
                         simdfile_2023)

count(!is.na(simdfile_2023_b1$simd20_decile)) 
# shows that ~10% of the records have SIMD deciles: makes sense as this is a UK-wide FRS file

write_rds(simdfile_2023_b1, here("analysis/simdfile_2023_b1.rds"))
simdfile_2023_b1 <- readRDS(here("analysis/simdfile_2023_b1.rds"))




#####################################
## EQUIVALISE HOUSEHOLD INCOMES FOR THE SCENARIOS:
#####################################
#***************************************************************************************************************************
#  **NB UKMOD INCOME VALUES ARE MONTHLY AND SHOULD BE MULTIPLIED BY 12 TO GIVE ANNUAL FIGURES**
#***************************************************************************************************************************

# Function to read in a EUROMOD output file (.txt), calculate equivalised hhd income, and save result as rds file

calc_equiv_income <- function(filename) {
  
  # Read in the UKMOD output file (.txt) (one file for each scenario modelled, with a row for each FRS respondent)
  # Sum taxes and benefits under that scenario 
  file <- read.delim(filename, stringsAsFactors = FALSE, quote = "", sep = "\t") %>% 
    #sum total tax and national insurance contributions (employee, self-employed, employer, and credit contributions)
    mutate(tax_sic = 
             ils_tax + #income tax amount
             ils_sicee + #employees' social insurance contribution
             ils_sicse + #self-employeds' social insurance contribution
             ils_sicer + #employers' social insurance contribution
             ils_sicct, #credit contributions
           inc_emp = yem + yse) #sum total income from employment (employed and self-employed)
  
  #Tabulate MONTHLY benefit receipts (ils_ben), tax and NIC contributions (tax_sic), and income (inc_emp) for the scenario,
  #weighted by the FRS differential-response weight (dwt). 
  #This takes the individual-level file, summarises to give national figures, and saves as an object for merging together later.
  #This data will be used to estimate government revenue and cost for each scenario.
  sumfile <- file %>%
    summarise(benrecpt=sum(ils_ben*dwt), 
              taxsic=sum(tax_sic*dwt), 
              incemp=sum(inc_emp*dwt)) %>%
    mutate(scenario = filename) 
  
  # Save the scenario input file in RDS format
  # trim the txt part from the filename then save the data as .rds format, in the rds folder we created above
  stub<-gsub("_std.txt","",filename)
  saveRDS(sumfile, here("ukmod output", "rds", paste0(stub,".rds")))
  
  # Calculate equivalised income for the scenario
  #*Apply OECD modified equivalence scale (IFS/HBAI method: where reference = couple with no children)
  #*This stage takes the individual-level file and summarises to the household level.
  file_hh <- file %>%
    group_by(idhh) %>%
    arrange(desc(dag)) %>% #sort descending age (dag) order: oldest is first 
    mutate(order = order(dag)) %>% #*generate 'order' to number the household members, in descending age order
    mutate(eq_scale = case_when(order==1 ~ 0.67,             #oldest person in the hh is assigned 0.67
                                order>1 & dag>=14 ~ 0.33,    #other adults (>=14y) = 0.33
                                order>1 & dag<14 ~ 0.2)) %>% #and children = 0.2
    
    group_by(idhh) %>%
    summarise(eq_scale_total = sum(eq_scale), #sum the equiv scale across all household members (i.e., household sum)
              hh_dpi = sum(ils_dispy), #sum disposable income (ils_dispy) for each household
              eq_dpi = hh_dpi / eq_scale_total, #calculate equiv. disposable income  
              dwt = first(dwt)) %>% 
    ungroup() %>%
    select(idhh, dwt, eq_scale_total, eq_dpi, hh_dpi) #hh_dpi (unequivalised) needed for the MIS calc
  
  #rename the eq_dpi variable with the scenario name
  names(file_hh) <- c("idhh", "dwt", "eq_scale_total", stub, paste0(stub, "_hh_dpi"))
  
  #save as .rds format, in the rds folder
  saveRDS(file_hh, here("ukmod output", "equiv", paste0(stub,"_eqincome.rds")))
  
}

# Get the names of the output files to be processed:
# Set up the 'all.files' object to contain all the .txt filenames in the data directory
all.files <- list.files(pattern = "*.txt$", rec=F)
# Then loop through each of the files in 'all.files' to save as .rds files and calc equiv income
sapply(all.files, calc_equiv_income)

# Each of these files contains disposable income for each household, their FRS weight and the equivalence scale for the household.
# the scenario name with no suffix contains equivalised income, and the scenario name with _hh_dpi suffix contains the unequivalised income
# equivalised = unequivalised / equivalence scale


#####################################
## MERGE THE SCENARIO DATA: 
#####################################

#*set up the 'all.files' variable to contain all the _eqincome.rds filenames in the data directory
all.files2 <- list.files(pattern = "*eqincome.rds", rec=T)

# Read in the files and join them
files_list <- lapply(all.files2, readRDS) #read all the files in and store in a list
equivincdf <- files_list %>% 
  reduce(left_join, by = c("idhh", "dwt", "eq_scale_total")) 

write_rds(equivincdf, here("analysis", "equivincdf.rds"))
equivincdf <- readRDS(here("analysis", "equivincdf.rds"))
# This file contains all the scenario data, still at household level


#####################################
## ADD SIMD QUINTILES FOR THE HOUSEHOLDS:
#####################################

# Merge in SIMD info for the FRS respondents 2021/22, 22/23 and 23/24. (the years included in uk_2023_b1)

simdfile_2023_b1 <- readRDS(here("analysis/simdfile_2023_b1.rds")) #5622 Scottish FRS respondents

#merge SIMD with eqvincome data, and recode deciles to quintiles
equivincdf_simd5 <- equivincdf %>% 
  merge(y=simdfile_2023_b1, by="idhh") %>%
  mutate(simd5=recode(simd20_decile, 
                      `1`=1,`2`=1, 
                      `3`=2,`4`=2, 
                      `5`=3,`6`=3, 
                      `7`=4,`8`=4, 
                      `9`=5,`10`=5)) %>%
  select(idhh, dwt, eq_scale_total, simd5, simd10=simd20_decile, starts_with("sc_")) #keep deciles and quintiles
write_rds(equivincdf_simd5, here("analysis", "equivincdf_simd5.rds"))

# All FRS households in Scotland have an SIMD match now:

table(equivincdf_simd5$simd5, useNA = c("always"))
#    1    2    3    4    5 <NA> 
#  964 1120 1126 1322 1090    0 

table(equivincdf_simd5$simd10, useNA = c("always"))
#     1    2    3    4    5    6    7    8    9   10 <NA> 
#   435  529  579  541  553  573  665  657  590  500    0 


#####################################
## ASSIGN THE HOUSEHOLDS TO INCOME QUINTILES:
#####################################

# Base = sc_2025
# Sort households on baseline equiv hhd income, calculate cumulative
# sum of dwt weights, and use this to create variable indicating the quintile
# of responses. 1 = lowest, 5 = highest
# (there will definitely be a better way to do this, but this works!)
equivincdf_inc5_inc10_2025base <- equivincdf_simd5 %>% 
  dplyr::arrange(sc_2025) %>% # there will be some negative incomes
  dplyr::mutate(cumulative_weight = cumsum(dwt),
                cumulative_weight_prop = cumulative_weight / sum(dwt),
                inc5 = dplyr::case_when(cumulative_weight_prop < 0.20 ~ 1,
                                        cumulative_weight_prop >= 0.20 & cumulative_weight_prop < 0.40 ~ 2,
                                        cumulative_weight_prop >= 0.40 & cumulative_weight_prop < 0.60 ~ 3,
                                        cumulative_weight_prop >= 0.60 & cumulative_weight_prop < 0.80 ~ 4,
                                        cumulative_weight_prop >= 0.80 ~ 5),
                inc10 = dplyr::case_when(cumulative_weight_prop < 0.10 ~ 1,
                                         cumulative_weight_prop >= 0.10 & cumulative_weight_prop < 0.20 ~ 2,
                                         cumulative_weight_prop >= 0.20 & cumulative_weight_prop < 0.30 ~ 3,
                                         cumulative_weight_prop >= 0.30 & cumulative_weight_prop < 0.40 ~ 4,
                                         cumulative_weight_prop >= 0.40 & cumulative_weight_prop < 0.50 ~ 5,
                                         cumulative_weight_prop >= 0.50 & cumulative_weight_prop < 0.60 ~ 6,
                                         cumulative_weight_prop >= 0.60 & cumulative_weight_prop < 0.70 ~ 7,
                                         cumulative_weight_prop >= 0.70 & cumulative_weight_prop < 0.80 ~ 8,
                                         cumulative_weight_prop >= 0.80 & cumulative_weight_prop < 0.90 ~ 9,
                                         cumulative_weight_prop >= 0.90 ~ 10)) %>%
  select(-starts_with("cumul"), -eq_scale_total) 
write_rds(equivincdf_inc5_inc10_2025base, here("analysis", "equivincdf_inc5_inc10_2025base.rds"))

# #make long-form data file 
equivincdf_inc5_inc10_2025base_long <- equivincdf_inc5_inc10_2025base %>%
  pivot_longer(cols = c(-idhh, -dwt, -simd5, -simd10, -inc5, -inc10), names_to = "scenario", values_to = "equivinc")


#####################################
## SUMMARY DATA (AGGREGATE)
#####################################

# Done here for deciles, can be done for quintiles too
# Income distribution (mean and median) across income deciles
# #*weighted sum of equiv income by income decile:
equivincdf_inc10_2025base_long_agg <- equivincdf_inc5_inc10_2025base_long %>% 
  arrange(equivinc, .by_group=T) %>%
  group_by(inc10, scenario) %>%
  summarise(inc10_wtmean=weighted.mean(equivinc, dwt, na.rm=T),
            inc10_wtmedian=matrixStats::weightedMedian(equivinc, dwt, na.rm=T)) %>%
  ungroup() %>%
  arrange(scenario, inc10)
write.csv(equivincdf_inc10_2025base_long_agg, here("analysis", "equivincdf_inc10_2025base_long_agg.csv"), row.names=F)

# Income distribution (mean and median) across SIMD deciles
# #*weighted sum of equiv income by SIMD decile:
equivincdf_simd10_2025base_long_agg <- equivincdf_inc5_inc10_2025base_long %>% 
  group_by(simd10, scenario) %>%
  arrange(equivinc, .by_group=T) %>%
  summarise(simd10_wtmean=weighted.mean(equivinc, dwt, na.rm=T),
            simd10_wtmedian=matrixStats::weightedMedian(equivinc, dwt, na.rm=T)) %>%
  ungroup() %>%
  arrange(scenario, simd10)
write.csv(equivincdf_simd10_2025base_long_agg, here("analysis", "equivincdf_simd10_2025base_long_agg.csv"), row.names=F)

# Calculate the average equivalised hh income at baseline
baseline_income_summary_2025base <- equivincdf_inc5_inc10_2025base_long %>% 
  filter(scenario=="sc_2025") %>%
  summarise(wtmedian=matrixStats::weightedMedian(equivinc, dwt, na.rm=T), # gives £2953
            wtmean=weighted.mean(equivinc, dwt, na.rm=T)) # gives £3457



# Weighted population distribution table for inc10 by simd10
wted_tab_deciles <- equivincdf_inc5_inc10_2025base_long %>% 
  group_by(inc10, simd10) %>%
  summarise(weight = sum(dwt, na.rm=T)) %>%
  ungroup() %>%
  pivot_wider(names_from = inc10, values_from = weight, names_prefix = "inc10_") %>%
  filter(!is.na(simd10)) %>%
  mutate(totpop = inc10_1 + inc10_2 + inc10_3 + inc10_4 + inc10_5 + inc10_6 + inc10_7 + inc10_8 + inc10_9 + inc10_10) %>%
  mutate(inc10_1_pc = 100 * inc10_1 / totpop,
         inc10_2_pc = 100 * inc10_2 / totpop,
         inc10_3_pc = 100 * inc10_3 / totpop,
         inc10_4_pc = 100 * inc10_4 / totpop,
         inc10_5_pc = 100 * inc10_5 / totpop,
         inc10_6_pc = 100 * inc10_6 / totpop,
         inc10_7_pc = 100 * inc10_7 / totpop,
         inc10_8_pc = 100 * inc10_8 / totpop,
         inc10_9_pc = 100 * inc10_9 / totpop,
         inc10_10_pc = 100 * inc10_10 / totpop)
write.csv(wted_tab_deciles, here("analysis", "wted_tab_deciles.csv"), row.names=F)

# Weighted population distribution table for inc5 by simd5
wted_tab_quintiles <- equivincdf_inc5_inc10_2025base_long %>% 
  group_by(inc5, simd5) %>%
  summarise(weight = sum(dwt, na.rm=T)) %>%
  ungroup() %>%
  pivot_wider(names_from = inc5, values_from = weight, names_prefix = "inc5_") %>%
  filter(!is.na(simd5)) %>%
  mutate(totpop = inc5_1 + inc5_2 + inc5_3 + inc5_4 + inc5_5) %>%
  mutate(inc5_1_pc = 100 * inc5_1 / totpop,
         inc5_2_pc = 100 * inc5_2 / totpop,
         inc5_3_pc = 100 * inc5_3 / totpop,
         inc5_4_pc = 100 * inc5_4 / totpop,
         inc5_5_pc = 100 * inc5_5 / totpop)
write.csv(wted_tab_quintiles, here("analysis", "wted_tab_quintiles.csv"), row.names=F)




#####################################
## CALCULATE THE MORTALITY RATE RATIO
#####################################

# Only needs running once to produce the model coeffs.
# option below to hard code the 2023 coeffs in, so this modelling step wouldn't be required

# We used European age-sex standardised death rates for 2023, by SIMD decile
# EASRs for 2023 (latest available)(males and females combined)
# NB. uses 2022 populations, as small-area estimates for 2023 not available at time of analysis
easrs_simd10_2023 <- arrow::read_parquet(here("analysis", "easrs_simd10_2023.parquet"))

# Use the UKMOD system that gives the required household income distribution for Scotland
# merge SIMD with eqvincome data
# Here we use the 2023 UKMOD system (based on 3yr pooled household data for 21/22, 22/23, 23/24), and deaths for 2023 
incomes2023_by_simd10 <- equivincdf_simd10_2025base_long_agg %>%
  filter(scenario == "sc_2023") 

incomes2023_and_easrs2023 <- incomes2023_by_simd10 %>%
  merge(y=easrs_simd10_2023, by="simd10")
write.csv(incomes2023_and_easrs2023, here("analysis", "incomes2023_and_easrs2023.csv"), row.names=F)

# Plot the relationship
ggplot(incomes2023_and_easrs2023, aes(x=simd10_wtmean, y=easr)) +
  geom_point(size=3) +
  #  expand_limits(y=0) +
  scale_y_continuous(name = "death rate per 100,000 population (EASR, 2023)") +
  scale_x_continuous(name ="mean equivalised disposable household income, before housing costs (£/month, 2023)") +
  theme_light(16) +
  geom_smooth(method = "lm", se = FALSE)
ggsave(here("analysis", "income2023_mort2023.png"))
# looks curvy


# Run a non-linear least squares logistic model to predict EASR from income.
# SSlogis does this by creating its own initial estimates of the parameters Asym, xmid and scal, and fits the function
# y ~ Asym/(1+exp((xmid-input)/scal))
# We model 1/easr here to transform the data into a growth curve with values between 0 and 1.

## using a selfStart model
incmortmodel <- nls(1/easr ~ SSlogis(simd10_wtmean, Asym, xmid, scal), incomes2023_and_easrs2023)
## Model summary
summary(incmortmodel)
## the coefficients only:
coef(summary(incmortmodel))
## the individual coeffs
Asym <- coef(summary(incmortmodel))[1] #0.001505894
xmid <- coef(summary(incmortmodel))[2] #2923.042
scal <- coef(summary(incmortmodel))[3] #922.3876

# Plot the model fit and original data
new_inc2023<- data.frame(simd10_wtmean=seq(1500, 5000, by=100))
pred_demo <- data.frame(new_inc2023,
                        pred = predict(incmortmodel, newdata=new_inc2023)) %>%
  mutate(pred_easr = 1/pred)
ggplot() +
  geom_line(data=pred_demo, aes(x=simd10_wtmean, y=pred_easr), color = "black") +
  geom_point(data=incomes2023_and_easrs2023, aes(x=simd10_wtmean, y=easr), color = "red", size=4) +
  expand_limits(x=0,y=0) +
  scale_y_continuous(name = "death rate per 100,000 population (EASR, 2023)") +
  scale_x_continuous(name ="mean household income (£/month, 2023)") +
  theme_classic(16)
ggsave(here("analysis", "income_mort_sigmoid_2023.png"))
# that's a better fit to the data




#####################################
## PREDICTING EASRS FOR SCENARIOS
#####################################

# If the model is not in memory:

# Manually input the coeffs:
Asym <- 0.001505894
xmid <- 2923.042
scal <- 922.3876

# For each scenario we have the average income for each SIMD decile, so can use the model coeffs to estimate the mortality rate for each of these new income levels

# scenario data
equivincdf_simd10_2025base_long_agg <- read.csv(here("analysis", "equivincdf_simd10_2025base_long_agg.csv")) %>%
  filter(!str_detect(scenario, 'hh_dpi')) # remove the unequivalised incomes

# For each scenario, estimate the mortality rate by quintile/decile
scenarios_pred <- equivincdf_simd10_2025base_long_agg %>%
  mutate(pred_easr2023 = (1+exp((xmid-simd10_wtmean)/scal))/Asym) %>% #formula inverted as 1/easr was the dependent var for original model
  select(scenario, simd10, simd10_wtmean, pred_easr2023) %>%
  filter(!is.na(simd10)) %>%
  group_by(simd10) %>%
  mutate(baseline_easr = pred_easr2023[scenario=="sc_2023"]) %>%
  ungroup() %>%
  mutate(rr = pred_easr2023 / baseline_easr) # rate ratio
# This rate ratio is then used to estimate deaths by age group and sex for each scenario

# Sensitivity: weaken the income effect from the model by 50%
scenarios_pred_50pc <- scenarios_pred %>%
  mutate(rr = (pred_easr2023 + 0.5*(baseline_easr-pred_easr2023)) / baseline_easr) %>%
  mutate(scenario = paste0(scenario, "50pc")) %>%
  filter(scenario != "sc_2023")



#####################################
## PREDICTING EASRS FOR AN INCOME MATRIX
#####################################

# An alternative to using UKMOD scenarios is to estimate change in deaths across a matrix of changes in income:

# Predicting EASRs for an income matrix (example)
base_df <-  equivincdf_simd10_2025base_long_agg %>%
  filter(scenario == "sc_2023" & !is.na(simd10)) %>%
  select(simd10, scenario, inc=simd10_wtmean)

# set up the matrix
inc_matrix <- rbind(base_df,
                    base_df,
                    base_df,
                    base_df,
                    base_df,
                    base_df,
                    base_df) %>%
  mutate(percent = rep(c(-6, -4, -2, 0, 2, 4, 6), each=10),
         simd10_wtmean = inc + (inc*(percent/100))) %>% # calc the incomes from -6% to +6% change
  mutate(scenario = paste0(as.character(percent),"%"))

# If the model is not in memory:
inc_matrix_pred <- inc_matrix %>%
  mutate(pred_easr2023 = (1+exp((xmid-simd10_wtmean)/scal))/Asym) %>% #formula inverted as 1/easr was the dependent var for original model
  select(scenario, percent, simd10, simd10_wtmean, pred_easr2023) %>%
  group_by(simd10) %>%
  mutate(baseline_easr = pred_easr2023[percent==0]) %>%
  ungroup() %>%
  mutate(rr = pred_easr2023 / baseline_easr) %>%
  select(-percent)
write.csv(inc_matrix_pred, here("analysis", "inc_matrix_pred.csv"), row.names=F)

inc_matrix_pred_wide = inc_matrix_pred %>%
  select(simd10, scenario, rr) %>%
  pivot_wider(names_from = scenario, values_from = rr)
write.csv(inc_matrix_pred_wide, here("analysis", "inc_matrix_pred_wide.csv"), row.names=F)


# combine all the scenarios for the life table
all_scenarios <- rbind(scenarios_pred,
                       scenarios_pred_50pc,
                       inc_matrix_pred)

#################################################################
## INPUT DATA FOR LIFE EXPECTANCY MODELLING: POPULATION, DEATHS
#################################################################

# Population and deaths data from NRS data
# These data use 20 age groups: 0, 1-4, 5-9, 10-14, 15-19, ..., 90+
# These data were extracted and aggregated in script processing_pop_deaths. 
pop_deaths_simd10 <- arrow::read_parquet(here("analysis", "pop_deaths_SIMD10_agegp20_LE.parquet"))

#Get the European Standard Population (2013) and age group entry ages
#esp with oldest age group 90+
agegp20_LE <- c(1:20)
esp2013pop <- c(1000, 4000, 5500, 5500, 5500, 6000, 6000, 6500, 7000, 7000, 7000, 7000, 6500, 6000,
                5500, 5000, 4000, 2500, 1500, 1000) 
entryage <- c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90)
esp2013 <- data.frame(agegp20_LE, esp2013pop, entryage, stringsAsFactors=FALSE)

#####################################
## DEATHS CALCULATION
#####################################

model_data_by_simd_and_sex <- pop_deaths_simd10  %>%
  merge(y=esp2013, by="agegp20_LE", all=T) %>%
  filter(sex %in% c("M", "F") & simd10 %in% c(1:10)) %>% # remove the aggregated rows (they'll be created again later)
  rename(pop_nrs = pop,
         deaths_nrs = deaths) %>%
  merge(y=all_scenarios, by.x="simd10", by.y="simd10", all=T) %>% # N.B. this makes a complete copy of all rows for each scenario with RR data
  mutate(deaths_scenario = (deaths_nrs * rr)) #%>%
 # mutate(deaths_scenario = case_when(agegp20_LE %in% c(1:4) ~ deaths_nrs,
  #                                   TRUE ~ deaths_scenario)) 


# Calculate aggregated figures and merge back in
model_data_agg_simd10 <- model_data_by_simd_and_sex %>%
  group_by(agegp20_LE, sex, year, scenario) %>%
  summarise(pop_nrs = sum(pop_nrs), 
            deaths_scenario = sum(deaths_scenario)) %>%
  ungroup() %>%
  mutate(scenario = paste0(scenario, "_allsimd10"),
         simd10 = 125) # 125 = all SIMD deciles
model_data_agg_sex <- model_data_by_simd_and_sex %>%
  group_by(agegp20_LE, simd10, year, scenario) %>%
  summarise(pop_nrs = sum(pop_nrs),
            deaths_scenario = sum(deaths_scenario)) %>%
  ungroup() %>%
  mutate(scenario = paste0(scenario, "_bothsex"),
         sex = "both")
model_data_agg_simd10sex <- model_data_by_simd_and_sex %>%
  group_by(agegp20_LE, year, scenario) %>%
  summarise(pop_nrs = sum(pop_nrs),
            deaths_scenario = sum(deaths_scenario)) %>%
  ungroup() %>%
  mutate(scenario = paste0(scenario, "_allsimd10_bothsex"),
         sex = "both",
         simd10 = 125)
model_data_all_simd10 <- model_data_by_simd_and_sex %>%
#  select(-deaths_scenario) %>%
  bind_rows(model_data_agg_sex,
            model_data_agg_simd10,
            model_data_agg_simd10sex) %>%
  select(simd10, agegp20_LE, sex, year, pop_nrs, scenario, deaths_scenario)

#####################################
## CALCULATE LIFE EXPECTANCIES FOR EACH SCENARIO
#####################################

# Generate life table - Life expectancy
scenario_lifetable_data_simd10 <- model_data_all_simd10 %>%
  group_by(sex, year, simd10, scenario) %>%
  arrange(agegp20_LE, .by_group=TRUE) %>%
  mutate(mx = deaths_scenario / pop_nrs,              # death rate
         n = c(1,4,5,6,4,rep(5,14),2/mx[20]),        # age intervals (<1=1, 1-4=4, 5-9=5, 10-14=6, 15-19=4, 20-85=5 years, 90+=2/Mx)
         ax = c(0.1, rep(0.5,19)),            # ax = Fraction of the age interval lived by those who die in the interval - chiang methodolgy sets ax to 0.1 for first age band and 0.5 for all others 
         qx = case_when(agegp20_LE == 20 ~ 1,     # qx = Conditional probability individual entering age band will die
                        TRUE ~ n*mx/(1+n*(1-ax)*mx)),
         px= 1-qx,                             # px = Conditional probability individual entering age band will survive
         radix = 100000 ,                      # set radix (total imaginary cohort population)
         pre_Ix = cumprod(1-qx)*radix ,        # first step in calculating life table pop - second step (to calculate Ix) uses a lag function which only seems to work when lag value already defined.
         Ix= case_when(agegp20_LE == 1 ~ radix,   # Ix =  Life table population (usually 100,000 at birth)
                       TRUE ~ lag(pre_Ix)),
         dx = Ix*qx,                           # dx =  Number of life table deaths
         Lx = case_when(agegp20_LE == 20 ~ n*ax*dx,           # Lx = Total number of years lived during the time period. Last age group treated differently as this is wider/open ended age band
                        TRUE ~  n*(lead(Ix,1)+(ax*dx))),
         Tx = rev(cumsum(rev(Lx))),           # Total # years lived beyond entry age
         LEx =Tx/Ix,                          # Life expectancy
         var_qx=(n^2*mx*(1-ax*n*mx))/(pop_nrs*(1+(1-ax)*n*mx)^3),  #variance of qx (prob of death) 1984 method
         se1=case_when(agegp20_LE==20~0, TRUE ~ (Ix^2)*(((1-ax)*n+lead(LEx,1))^2)*var_qx), # deriving standard error - requires 2 steps (se1 & se2)
         se2=rev(cumsum(rev(se1))),
         var_ex=se2/(Ix^2),     # variance of life expectancy
         se=sqrt(var_ex),       # standard error of life expectancy
         lci=LEx-(1.96*se),     # 95% lower confidence interval
         uci=LEx+(1.96*se)) %>%     # 95% upper confidence interval
  ungroup ()


#####################################
## CALCULATE LEXP FOR THE NRS DATA (USED IN PREM MORT CALC)
#####################################

# make aggregations (by simd10, sex, and both) and then merge all
input_data_agg_simd10 <- pop_deaths_simd10  %>%
  group_by(agegp20_LE, sex, year) %>%
  summarise(pop = sum(pop),
            deaths = sum(deaths)) %>%
  ungroup() %>%
  mutate(simd10 = 125)
input_data_agg_sex <- pop_deaths_simd10  %>%
  group_by(agegp20_LE, simd10, year) %>%
  summarise(pop = sum(pop),
            deaths = sum(deaths)) %>%
  ungroup() %>%
  mutate(sex = "both")
input_data_agg_simd10sex <- pop_deaths_simd10  %>%
  group_by(agegp20_LE, year) %>%
  summarise(pop = sum(pop),
            deaths = sum(deaths)) %>%
  ungroup() %>%
  mutate(sex = "both",
         simd10 = 125)
input_data_all_simd10 <- pop_deaths_simd10  %>%
  bind_rows(input_data_agg_sex,
            input_data_agg_simd10,
            input_data_agg_simd10sex) %>%
  merge(y=esp2013, by="agegp20_LE", all=T) %>%
  rename(pop_nrs = pop,
         deaths_nrs = deaths)

# Generate life table - Life expectancy
lifetable_data_simd10 <- input_data_all_simd10 %>%
  group_by(sex, year, simd10) %>%
  arrange(agegp20_LE, .by_group=TRUE) %>%
  mutate(mx = deaths_nrs / pop_nrs,              # death rate
         n = c(1,4,5,6,4,rep(5,14),2/mx[20]),        # age intervals (<1=1, 1-4=4, 5-9=5, 10-14=6, 15-19=4, 20-85=5 years, 90+=2/Mx)
         ax = c(0.1, rep(0.5,19)),            # ax = Fraction of the age interval lived by those who die in the interval - chiang methodolgy sets ax to 0.1 for first age band and 0.5 for all others 
         qx = case_when(agegp20_LE == 20 ~ 1,     # qx = Conditional probability individual entering age band will die
                        TRUE ~ n*mx/(1+n*(1-ax)*mx)),
         px= 1-qx,                             # px = Conditional probability individual entering age band will survive
         radix = 100000 ,                      # set radix (total imaginary cohort population)
         pre_Ix = cumprod(1-qx)*radix ,        # first step in calculating life table pop - second step (to calculate Ix) uses a lag function which only seems to work when lag value already defined.
         Ix= case_when(agegp20_LE == 1 ~ radix,   # Ix =  Life table population (usually 100,000 at birth)
                       TRUE ~ lag(pre_Ix)),
         dx = Ix*qx,                           # dx =  Number of life table deaths
         Lx = case_when(agegp20_LE == 20 ~ n*ax*dx,           # Lx = Total number of years lived during the time period. Last age group treated differently as this is wider/open ended age band
                        TRUE ~  n*(lead(Ix,1)+(ax*dx))),
         Tx = rev(cumsum(rev(Lx))),           # Total # years lived beyond entry age
         LEx =Tx/Ix,                          # Life expectancy
         var_qx=(n^2*mx*(1-ax*n*mx))/(pop_nrs*(1+(1-ax)*n*mx)^3),  #variance of qx (prob of death) 1984 method
         se1=case_when(agegp20_LE==20~0, TRUE ~ (Ix^2)*(((1-ax)*n+lead(LEx,1))^2)*var_qx), # deriving standard error - requires 2 steps (se1 & se2)
         se2=rev(cumsum(rev(se1))),
         var_ex=se2/(Ix^2),     # variance of life expectancy
         se=sqrt(var_ex),       # standard error of life expectancy
         lci=LEx-(1.96*se),     # 95% lower confidence interval
         uci=LEx+(1.96*se)) %>%     # 95% upper confidence interval
  ungroup ()


#####################################
## COMBINE LEXP RESULTS FOR SCENARIOS AND NRS DATA
#####################################

LE_data_simd10 <- lifetable_data_simd10 %>%
  select(year, sex, agegp20_LE, simd10, deaths_nrs, nrs_data_LEx = LEx) %>%
  merge(y = scenario_lifetable_data_simd10, by=c("simd10", "agegp20_LE", "sex", "year"), all=T) %>%
  select(-c(mx:Tx), -c(var_qx:uci)) %>%
  rename(scenario_LEx = LEx)
write.csv(LE_data_simd10, here("analysis", "scenario_LE_data_simd10.csv"), row.names=F)

scenario_LE_results_simd10 <- LE_data_simd10 %>%
  filter(agegp20_LE == 1) %>%
  mutate(diff_yrs = scenario_LEx - nrs_data_LEx)
write.csv(scenario_LE_results_simd10, here("analysis", "scenario_LE_results_simd10.csv"), row.names=F)


#prem mort = EASR of deaths under 75 (i.e., age groups 0 to 16 in agegp20_LE)
#yll = sum across subgroups((life exp - age at death) * number dying)
LE_data_YLL_PM_simd10 <- LE_data_simd10 %>%
  mutate(yll_scenario = deaths_scenario * (nrs_data_LEx), # life exp already accounts for mean age of the age group
         yll_nrs_data = deaths_nrs * (nrs_data_LEx),
         premmort = case_when(agegp20_LE %in% c(1:16) ~ as.numeric(deaths_scenario),
                              TRUE ~ as.numeric(0)),
         premmort_nrs_data = case_when(agegp20_LE %in% c(1:16) ~ as.numeric(deaths_nrs),
                                       TRUE ~ as.numeric(0))) 
LE_data_agg_simd10 <- LE_data_YLL_PM_simd10 %>%
  group_by(scenario, simd10, sex, year) %>%
  summarise(yll_nrs_data = sum(yll_nrs_data),
            yll_scenario = sum(yll_scenario),
            premmort_scen = sum(premmort),
            premmort_nrs = sum(premmort_nrs_data),
            deaths_nrs = sum(deaths_nrs),
            deaths_scenario = sum(deaths_scenario), 
            pop_nrs = sum(pop_nrs),
            nrs_data_LEx = nrs_data_LEx[agegp20_LE==1],
            scenario_LEx = scenario_LEx[agegp20_LE==1]) %>%
  ungroup()

#calc prem mort EASRs 
easrs_premmort_simd10 <- LE_data_YLL_PM_simd10 %>%
  merge(y=esp2013, by="agegp20_LE", all.x=TRUE) %>%
  filter(agegp20_LE %in% c(1:16)) %>%
  mutate(asr100k = premmort*100000/pop_nrs,
         asr_esp = asr100k * esp2013pop,
         asr100k_nrs = premmort_nrs_data*100000/pop_nrs,
         asr_esp_nrs = asr100k_nrs * esp2013pop) %>%
  group_by(scenario, simd10, sex, year) %>%
  summarise(asr_esp = sum(asr_esp, na.rm=TRUE),
            asr_esp_nrs = sum(asr_esp_nrs, na.rm=TRUE),
            esp2013pop = sum(esp2013pop, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(easr_scen_prem = asr_esp / esp2013pop,
         easr_nrs_prem = asr_esp_nrs / esp2013pop) %>%
  select(scenario, simd10, sex, year, easr_scen_prem, easr_nrs_prem)

#calc whole pop EASRs 
easrs_wholepop_simd10 <- LE_data_YLL_PM_simd10 %>%
  merge(y=esp2013, by="agegp20_LE", all.x=TRUE) %>%
  mutate(asr100k = deaths_scenario*100000/pop_nrs,
         asr_esp = asr100k * esp2013pop,
         asr100k_nrs = deaths_nrs*100000/pop_nrs,
         asr_esp_nrs = asr100k_nrs * esp2013pop) %>%
  group_by(scenario, simd10, sex, year) %>%
  summarise(asr_esp = sum(asr_esp, na.rm=TRUE),
            asr_esp_nrs = sum(asr_esp_nrs, na.rm=TRUE),
            esp2013pop = sum(esp2013pop, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(easr_scen = asr_esp / esp2013pop,
         easr_nrs = asr_esp_nrs / esp2013pop) %>%
  select(scenario, simd10, sex, year, easr_scen, easr_nrs)

#Combine results
scenario_results_simd10 <- easrs_wholepop_simd10 %>%
  merge(y=easrs_premmort_simd10, by=c("scenario", "simd10", "sex", "year")) %>%
  merge(y=LE_data_agg_simd10, by=c("scenario", "simd10", "sex", "year")) %>%
  mutate(scenario = gsub("_bothsex", "", scenario),
         scenario = gsub("_allsimd10", "", scenario))

write.csv(scenario_results_simd10, here("analysis", "scenario_results_simd10.csv"), row.names=F)

# format results for presenting / working with in excel
scenario_results_simd10_xls <- scenario_results_simd10 %>%
  select(scenario, simd10, sex, easr_scen, easr_scen_prem, scenario_LEx) %>%
  pivot_longer(cols = c(easr_scen, easr_scen_prem, scenario_LEx),
               names_to = "metric", values_to = "value") %>%
  pivot_wider(names_from = scenario, values_from = value ) %>%
  mutate(metric = case_when(metric == "easr_scen" ~ "EASR",
                            metric == "easr_scen_prem" ~ "Premature mortality EASR",
                            metric == "scenario_LEx" ~ "Life expectancy at birth")) %>%
  arrange(metric, sex, simd10) %>%
  select(simd10, sex, metric, 
         "-6%", "-4%", "-2%", "0%", "2%", "4%", "6%", 
         sc_2023, sc_2024, sc_2025, sc_2025_scpup,
         everything())
write.csv(scenario_results_simd10_xls, here("analysis", "scenario_results_simd10_xls.csv"), row.names=F)

# format results for presenting / working with in excel: KEEP LONG
scenario_results_simd10_long <- scenario_results_simd10 %>%
  select(scenario, simd10, sex, easr_scen, easr_scen_prem, scenario_LEx) %>%
  pivot_longer(cols = c(easr_scen, easr_scen_prem, scenario_LEx),
               names_to = "metric", values_to = "value") %>%
  filter(scenario!="0 GBP change/month") %>%
  mutate(scenario = case_when(scenario == "sc_2023" ~ "no change",
                              substr(scenario, 1, 1) == "-" ~ paste0("'", scenario),
                              TRUE ~ paste0("'+", scenario))) %>%
  mutate(scenario = case_when(str_detect(scenario, "GBP") ~ gsub(" ", "", paste0(substr(scenario, 1, 2), 
                                                                                 "£",
                                                                                 substr(scenario, 3,5))),
                              TRUE ~ scenario)) %>%
  mutate(metric = case_when(metric == "easr_scen" ~ "EASR",
                            metric == "easr_scen_prem" ~ "Premature mortality EASR",
                            metric == "scenario_LEx" ~ "Life expectancy at birth")) %>%
  mutate(simd10 = case_when(simd10==1 ~ "D1 (most deprived)",
                            simd10==2 ~ "D2",
                            simd10==3 ~ "D3",
                            simd10==4 ~ "D4",
                            simd10==5 ~ "D5",
                            simd10==6 ~ "D6",
                            simd10==7 ~ "D7",
                            simd10==8 ~ "D8",
                            simd10==9 ~ "D9",
                            simd10==10 ~ "D10 (least deprived)",
                            simd10==125 ~ "All"))
write.csv(scenario_results_simd10_long, here("analysis", "scenario_results_simd10_long.csv"), row.names=F)




###############################################.
## calculate inequalities ----
###############################################.

# inequals data
scenario_inequals <- scenario_LE_results_simd10 %>%
  select(scenario, simd10, sex, scenario_LEx, pop_nrs) %>%
  mutate(scenario = gsub("_allsimd10", "", scenario)) %>% # remove the suffix from the aggregate results
  group_by(scenario, sex) %>%
  mutate(most_depr_value = scenario_LEx[simd10==1],
         least_depr_value = scenario_LEx[simd10==10],
         overall_value = scenario_LEx[simd10==125],
         total_pop = pop_nrs[simd10==125], # calculate the total population
         proportion_pop = pop_nrs/pop_nrs[simd10==125]) %>% # proportion of the population in each SIMD out of the total population.
  ungroup() %>%
  mutate(abs_range = most_depr_value - least_depr_value,
         rel_range = least_depr_value / most_depr_value) %>%
  filter(simd10 %in% c(1:10))  


###############################################.
## Slope of index of inequality (SII) ----
###############################################.
# The calculations below are those of the linear SII, you will have to amend the
# model if you wanted to calculate the Poisson SII
# This code will produce the results of the model, including confidence intervals
#Formula from: https://www.scotpho.org.uk/comparative-health/health-inequalities-tools/archive/triple-i-and-hits/
#https://pdfs.semanticscholar.org/14e0/c5ba25a4fdc87953771a91ec2f7214b2f00d.pdf

sii_model <- scenario_inequals %>%  
  group_by(scenario, sex) %>% 
  arrange(simd10, by_group=T) %>%
  mutate(cumulative_pro = cumsum(proportion_pop),  # cumulative proportion population for each area
         relative_rank = case_when(simd10 == 1 ~ 0.5*proportion_pop,
                                   simd10 != 1 ~ lag(cumulative_pro) + 0.5*proportion_pop),
         sqr_proportion_pop = sqrt(proportion_pop), #square root of the proportion of the population in each SIMD
         relrank_sqr_proppop = relative_rank * sqr_proportion_pop,
         value_sqr_proppop = sqr_proportion_pop * scenario_LEx) %>% #value based on population weights
  nest() %>% #creating one column called data with all the variables not in the grouping
  # Calculating linear regression for all the groups, then formatting the results
  # and calculating the confidence intervals
  mutate(model = map(data, ~ lm(value_sqr_proppop ~ sqr_proportion_pop + relrank_sqr_proppop + 0, data = .)),
         #extracting sii from model, a bit fiddly but it works
         sii = -1 * as.numeric(map(map(model, "coefficients"), "relrank_sqr_proppop")),
         conf.low = as.numeric(map(model, ~confint(., parm = "relrank_sqr_proppop")[1])),
         conf.high = as.numeric(map(model, ~confint(., parm = "relrank_sqr_proppop")[2]))) %>% #calculating confidence intervals
  ungroup() %>% 
  mutate(lowci_sii = -1 * conf.high, #fixing interpretation
         upci_sii = -1 * conf.low) %>% 
  select(scenario, sex, sii, lowci_sii, upci_sii)
# results for LEx are negative as lower is bad.


###############################################.
## Relative index of inequality (RII) ----
###############################################.
#This is the calculation of the linear RII which is based on the SII values
scenario_inequals_summary <- 
  left_join(scenario_inequals, sii_model, by = c("scenario", "sex")) %>% 
  mutate(rii = sii / overall_value,
         lowci_rii = lowci_sii / overall_value,
         upci_rii = upci_sii / overall_value,
         #Transforming RII into %. This way is interpreted as "most deprived areas are
         # xx% above the average" For example: Cancer mortality rate is around 55% higher
         # in deprived areas relative to the mean rate in the population
         rii_int = rii * 0.5 *100,
         lowci_rii_int = lowci_rii * 0.5*100,
         upci_rii_int = upci_rii * 0.5*100) %>%
  filter(simd10==1) %>%
  select(-simd10, -scenario_LEx, -pop_nrs)
write.csv(scenario_inequals_summary, here("analysis", "scenario_inequals_summary.csv"), row.names=F)

###############################################.
## Example comparison ----
###############################################.

scenario_inequals_summary %>%
  filter(scenario %in% c("sc_2025", "sc_2025_scpup") & sex %in% c("M", "F")) %>%
  select(scenario, sex, most_depr_value, least_depr_value, overall_value,
         abs_range, rel_range, sii, rii)

# # A tibble: 4 × 9
# scenario      sex   most_depr_value least_depr_value overall_value abs_range rel_range   sii    rii
# <chr>         <chr>           <dbl>            <dbl>         <dbl>     <dbl>     <dbl> <dbl>  <dbl>
# sc_2025_scpup F                76.6             86.3          81.7     -9.69      1.13 -10.2 -0.125
# sc_2025       F                76.4             86.3          81.7     -9.81      1.13 -10.4 -0.127
# sc_2025_scpup M                71.1             83.2          77.8    -12.1       1.17 -13.1 -0.168
# sc_2025       M                71.0             83.2          77.8    -12.3       1.17 -13.3 -0.171

# The 2025 SCPup scenario increased the Scottish Child Payment from £27.2/week to £50/week.
# The model estimates this would increase life exp in the most deprived SIMD decile by 0.2y for females and 0.1y for males.
# The table shows that this would narrow inequalities in life expectancy slightly, 
# although the gap between the most and least deprived deciles would still be over 9 years for females, and over 12 years for males. 