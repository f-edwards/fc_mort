#### read_ncands_agency.R 
#### project: fc_mort
#### Author: Frank Edwards
#### Email:  frank.edwards@rutgers.edu
#### repo: 
#
# log: read pop, mort, and FC data, output as panel
#----------------------------------------

### read and parse ncands agency files
# for mortality sub-analysis

library(tidyverse)
library(data.table)
library(readxl)
library(haven)

### pop
pop<-read_fwf("./data/us.1990_2023.singleages.through89.90plus.adjusted.txt",
              fwf_widths(c(4, 2, 2, 3, 2, 1, 1, 1, 2, 8),
                         c("year", "state", "st_fips", 
                           "cnty_fips", "reg", "race", 
                           "hisp", "sex", "age", "pop")),
              col_types = "ncccnnnnnn",
              col_select = c(year, state, age, pop)) |> 
  filter(year>2009) |> 
  group_by(year, state, age) |> 
  summarize(pop = sum(pop))

pop_inf<-pop |> 
  filter(age <= 1) |> 
  group_by(year, state) |> 
  summarize(pop_infant = sum(pop))

pop_child<-pop |> 
  filter(age<18) |> 
  group_by(year, state) |> 
  summarize(pop_child = sum(pop))

pop_join<-pop_inf |> 
  left_join(pop_child)


filenames<-paste(
  "./data/ncands_agency/",
  list.files("./data/ncands_agency/"),
  sep = "")

## read ncands files

n10<-read_dta(filenames[1]) |> 
  rename_all(tolower) |> 
  select(staterr, subyr,
         ftlfpscf:fatalcru,
         ndeaths) |> 
  mutate(subyr = as.numeric(subyr)) |> 
  rename(cfdeaths = ndeaths) |> 
  mutate(totdeaths = cfdeaths + fatality)

n11<-read_xlsx(filenames[2]) |> 
  rename_all(tolower) |> 
  select(staterr, subyr,
         ftlfpscf:fatalcru,
         ndeaths) |> 
  mutate(fatality = as.numeric(fatality)) |> 
  mutate(fatality = ifelse(is.na(fatality), 0, fatality)) |> 
  mutate(subyr = as.numeric(subyr)) |> 
  mutate(across(ftlfpscf:fatalcru, as.numeric)) |> 
  rename(cfdeaths = ndeaths) |> 
  mutate(totdeaths = cfdeaths + fatality)

n12<-read_xlsx(filenames[3]) |> 
  rename_all(tolower) |> 
  select(staterr, subyr,
         ftlfpscf:fatalcru,
         cfdeaths) |> 
  mutate(fatality = ifelse(is.na(fatality), 0, fatality)) |> 
  mutate(totdeaths = cfdeaths + fatality)

n13<-read_xlsx(filenames[4]) |> 
  rename_all(tolower) |> 
  select(staterr, subyr,
         ftlfpscf:fatalcru,
         cfdeaths) |> 
  mutate(fatality = ifelse(is.na(fatality), 0, fatality)) |> 
  mutate(totdeaths = cfdeaths + fatality)

n14<-read_xlsx(filenames[5])|> 
  rename_all(tolower) |> 
  select(staterr, subyr,
         ftlfpscf:fatalcru,
         cfdeaths) |> 
  mutate(fatality = ifelse(is.na(fatality), 0, fatality)) |> 
  mutate(totdeaths = cfdeaths + fatality)

n15<-read_xlsx(filenames[6])|> 
  rename_all(tolower) |> 
  select(staterr, subyr,
         ftlfpscf:fatalcru,
         cfdeaths) |> 
  mutate(fatality = ifelse(is.na(fatality), 0, fatality)) |> 
  mutate(totdeaths = cfdeaths + fatality)

n16<-read_xlsx(filenames[7])|> 
  rename_all(tolower) |> 
  select(staterr, subyr,
         ftlfpscf:fatalcru,
         cfdeaths, totdeaths)

n17<-read_xlsx(filenames[8])|> 
  rename_all(tolower) |> 
  select(staterr, subyr,
         ftlfpscf:fatalcru,
         cfdeaths, totdeaths)

n18<-read_xlsx(filenames[9])|> 
  rename_all(tolower) |> 
  select(staterr, subyr,
         ftlfpscf:fatalcru,
         cfdeaths, totdeaths)

n19<-read_xlsx(filenames[10])|> 
  rename_all(tolower)|> 
  select(staterr, subyr,
         ftlfpscf:fatalcru,
         cfdeaths, totdeaths)

n20<-read_xlsx(filenames[11])|> 
  rename_all(tolower)|> 
  select(staterr, subyr,
         ftlfpscf:fatalcru,
         cfdeaths, totdeaths)

n21<-read_xlsx(filenames[12])|> 
  rename_all(tolower)|> 
  select(staterr, subyr,
         ftlfpscf:fatalcru,
         cfdeaths, totdeaths)

n22<-read_xlsx(filenames[13])|> 
  rename_all(tolower)|> 
  select(staterr, subyr,
         ftlfpscf:fatalcru,
         cfdeaths, totdeaths)

n23<-read_xlsx(filenames[14])|> 
  rename_all(tolower)|> 
  select(staterr, subyr,
         ftlfpscf:fatalcru,
         cfdeaths, totdeaths)

nc_af<-bind_rows(n10, n11, n12, 
                 n13, n14, n15, n16,
                 n17, n18, n19, n20,
                 n21, n22, n23) |> 
  filter(!(is.na(staterr)),
         staterr!="PR",
         staterr!="DC") |> 
  rename(state = staterr,
         year = subyr) |> 
  mutate(mort_data_multi_source = 
           case_when(
             is.na(fatality) ~ F,
             fatality > 0 ~ T,
             (year > 2015) & !(is.na(fatality)) ~ T,
             (year<2016) & (fatality == 0) ~ F))

# export af time series for eda
nc_af |> 
  left_join(pop_child) |> 
  write_csv(file = "./data/ncands_af_fatalities_ts.csv")

# prefer nc_acf when version > 1, all years after 2014 
# read in CM report data 13-22
cm1<-read_xlsx("./data/State Child Fatality Data 2013-2022.xlsx",
               col_names = c("year", "state", "mort"),
               skip = 1,
               col_types = c("numeric", "text", "numeric"),
               na = "Missing")
# read in CM report data 10-12
cm2<-read_csv("./data/cr_mort_2010_2012.csv")
# xwalk state abb
library(usmap)
xwalk<-data.frame(state = state.name, 
                  state_abb = state.abb)
cm2<-left_join(cm2, xwalk) |> 
  select(-state) |> 
  rename(state = state_abb)
# bind
cm<-bind_rows(cm2, cm1) |> 
  filter(state!="National")
# join
mort<-nc_af |> 
  left_join(cm) 

# convert missings and prefer updated data
# CM>>AF prior to 2014, 2010-2013 AF all version 1

mort<-mort |> 
  mutate(cm_mort = 
           case_when(
             year < 2014 ~ mort,
             year >= 2014 ~ totdeaths)) |> 
  select(state, year, cm_mort, mort_data_multi_source) 

# read foster care --------------------------------------------------------

# foster care from AFCARS fc file 2009-2017; ab file 2018-2023
fc <- read_csv("./data/afcars_ts.csv") |> 
  rename(year = fy,
         state = st)

# read nhgis acs rolling 3-yr for pop data --------------------------------

pop2<-read_csv("./data/nhgis0074_csv/nhgis0074_ts_nominal_state.csv") |>
  rename(pop_tot = AV0AA,
         pop_edu_less9 = B69AA,
         pop_edu_lessBA = B69AB,
         pop_edu_BA = B69AC,
         pop_labor_InLabForce = B84AA,
         pop_labor_unemployed = B84AE,
         pop_poverty = CL6AA) |>
  mutate(pop_EduLess9 = pop_edu_less9/
           (pop_edu_less9 + pop_edu_lessBA + pop_edu_BA) * 100,
         pop_unemp_rt = pop_labor_unemployed/pop_labor_InLabForce * 100,
         pop_poverty_rt = pop_poverty / pop_tot * 100) |>
  mutate(year = str_sub(YEAR, -4, -1)) |>
  select(year, STATE, pop_EduLess9:pop_poverty_rt) 
# state abbreviations
xwalk<-data.frame(STATE = state.name,
                  state = state.abb)

pop2<-pop2 |>
  left_join(xwalk) |>
  select(-STATE) |>6/
  mutate(year = as.numeric(year)) |> 
  filter(!(is.na(state))) # remove DC

# join --------------------------------------------------------------------
dat<-pop_join |> 
  left_join(mort) |> 
  left_join(fc) |> 
  left_join(pop2) |> 
  mutate(mort_data_multi_source = ifelse(
    is.na(mort_data_multi_source), F, mort_data_multi_source)) |> 
  mutate(mort_rt = cm_mort/pop_child * 1e5,
         fc_entered_rt = fc_entered / pop_child * 1e3) |> 
  filter(state!="DC")

write_csv(dat, "./data/mort_fc.csv")
