# callouts and miscellaneous visuals --------------------------------------
library(tidyverse)
## pull imputed data for fig 1
source("make_figs.R")
## 

dat_plot |> 
  filter(year == 2023) |> 
  group_by(year) |> 
  summarize(mort_rt = mean(mort_rt_mn),
            mort_rt_sd = sd(mort_rt_mn),
            fc_rt = mean(fc_entered_rt),
            fc_sd = sd(fc_entered_rt))

diffs1<-dat_plot %>% 
  filter(year == 2010) %>% 
  select(state, mort_rt_mn, fc_entered_rt) %>% 
  rename(mort_rt_mn_13 = mort_rt_mn,
         fc_entered_rt_13 = fc_entered_rt)

diffs2<-dat_plot %>% 
  filter(year == 2023) %>% 
  select(state, mort_rt_mn, fc_entered_rt) %>% 
  rename(mort_rt_mn_22 = mort_rt_mn,
         fc_entered_rt_22 = fc_entered_rt)

diffs<-diffs1 %>% 
  left_join(diffs2) %>% 
  mutate(mort_diff = mort_rt_mn_22 - mort_rt_mn_13,
         fc_diff = fc_entered_rt_22 - fc_entered_rt_13) %>% 
  select(state, mort_diff:fc_diff) %>% 
  pivot_longer(cols = mort_diff:fc_diff,
               names_to = "type",
               values_to = "diff") 

ggplot(diffs,
       aes(x = diff,
           y = state)) + 
  geom_col() + 
  facet_wrap(~type)

diffs |> 
  group_by(type) |> 
  summarize(leq = sum(diff<=0),
            gr = sum(diff>0))


## plot of first differences
library(plm)
ts<-pdata.frame(dat_out %>%
                  filter(`_mi_m`==1) %>% 
                  select(state, year, fc_entered_rt, mort_rt) ,
                index = c("state", "year")) 

mort<-ts$mort_rt
mort_lag<-lag(mort)
mort_diff<-mort - mort_lag



fc<-ts$fc_entered_rt
fc_lag<-lag(fc)
fc_diff<-fc - fc_lag

ts_out<-data.frame(mort = mort, 
                   mort_lag = mort_lag, 
                   mort_diff = mort_diff,
                   fc = fc,
                   fc_lag = fc_lag,
                   fc_diff = fc_diff,
                   state_yr = names(mort))

ggplot(ts_out,
       aes(x = fc_diff,
           y = mort_diff,
           label = state_yr)) + 
  geom_point() + 
  geom_smooth(method = "lm")

### TS of medians, means, 10th/90th
t<-dat_out %>% 
  group_by(year) %>% 
  summarize(mort_rt_median = median(mort_rt),
            mort_rt_mean = mean(mort_rt),
            mort_rt_25 = quantile(mort_rt, 0.1),
            mort_rt_75 = quantile(mort_rt, 0.9))

ggplot(t,
       aes(x = year, y = mort_rt_median,
           ymax = mort_rt_75, 
           ymin = mort_rt_25)) + 
  geom_line() + 
  geom_errorbar() + 
  geom_line(aes(y = mort_rt_mean), color = "blue")

t1<-dat_out %>% 
  group_by(year) %>% 
  summarize(mort_rt_median = median(mort_rt),
            mort_rt_sd = sd(mort_rt),
            fc_rt_median = median(fc_entered_rt),
            fc_rt_sd = sd(fc_entered_rt))

# export t1? 

### callouts
# nat rates
dat %>% 
  group_by(year) %>% 
  summarize(c_mort = sum(mort, na.rm=T) / sum(pop) * 1e5)

diffs %>% 
  group_by(type) %>% 
  summarize(n_pos = sum(diff>0), n_neg = sum(diff<0),
            diff_mn = mean(diff))

dat |> 
  group_by(year) |> 
  summarize(fc_entered_rt = sum(fc_entered)/sum(pop_child) * 1e3)

dat |> 
  filter(!(is.na(cm_mort))) |> 
  group_by(year) |> 
  summarize(cm_mort_rt = sum(cm_mort)/sum(pop_child) * 1e5)


# agency file source analysis ---------------------------------------------
nc_af <-read_csv("./data/ncands_af_fatalities_ts.csv")

nc_af <- nc_af |> 
  mutate(cm_mort_rt = 
           case_when(
             is.na(totdeaths) ~ cfdeaths / pop_child * 1e5,
             !(is.na(totdeaths)) ~ totdeaths/pop_child * 1e5),
         source = ifelse(
           is.na(fatality), "Single", "Multiple"),
         maybe_missing = 
           ifelse(is.na(fatality) | fatality == 0,
                  T, F),
         more_than_cf = totdeaths>cfdeaths)
# clear difference in means           
nc_af |> 
  group_by(source) |> 
  summarize(cm_mort_rt_mean = mean(cm_mort_rt))
### missings coded as zeroes 2011-2015...
# ggplot(nc_af,
#        aes(x = year, 
#            y = cm_mort_rt)) + 
#   geom_point() + 
#   facet_wrap(~source)
# can't resolve sorting true zeroes from missing without audit of cm report

# subset to states that have multi source and > 0 non-cf fatalities 2016-2023
multi_st <- nc_af |> 
  filter(year>2015) |> 
  group_by(state) |> 
  summarize(maybe_missing = sum(maybe_missing)/n()) |> 
  filter(maybe_missing==0)

m1 <- lm(mort_rt ~ fc_entered_rt + 
                  pop_EduLess9 + pop_unemp_rt + 
                  pop_poverty_rt + 
                  factor(state) + factor(year),
                data = dat)

## run regression for only states consistently multi source
dat_t <- dat |> 
  filter(state %in% multi_st$state,
         year>2015)

m1_multi_st <- lm(mort_rt ~ fc_entered_rt + 
                    pop_EduLess9 + pop_unemp_rt + 
                    pop_poverty_rt + 
                    factor(state) + factor(year),
                  data = dat_t)
# then consistent single source


# then without FE but with source as predictor (re?)
dat_t <- dat |> 
  left_join(nc_af |> 
              select(state, year, source))

dat_t |> 
  group_by(source) |> 
  summarize(mort_rt = mean(mort_rt, na.rm = T))

m1_source <- lm(mort_rt ~ fc_entered_rt + 
                    pop_EduLess9 + pop_unemp_rt + 
                    pop_poverty_rt + factor(source) + 
                    factor(state) + factor(year),
                  data = dat_t)
## source clear negative correlation with outcome, 
## beta still positive

dat |> 
  group_by(year, mort_data_multi_source) |> 
  summarize(mort_rt = mean(mort_rt, na.rm=T)) |> 
  ggplot(aes(x = year, y = mort_rt, color = mort_data_multi_source)) + 
  geom_line()


### national results
dat |> 
  group_by(year) |> 
  summarize(mort_rt = sum(cm_mort)/sum(pop_child) * 1e5,
            fc_rt = sum(fc_entered)/sum(pop_child) * 1e3)

quantile(dat$fc_entered_rt, c(0.05,0.25, 0.5,0.75, .95))


# supplemental models etable4 ---------------------------------------------

dat <- read_csv("./data/mort_fc.csv")
#### UPDATE WITH NEW IMPUTATIONS
dat_out<-read_csv("./data/foster-care-imputed-20251028.csv") |> 
  rename(mort_rt = yrt,
         fc_entered_rt = xrt,
         imp = `_mi_m`,
         state = state_code) |> 
  select(state, year, mort_rt, mort_rt, fc_entered_rt, imp) |> 
  filter(imp>0)

mdat <- dat_out |> 
  left_join(dat |> 
              select(year, state, mort_data_multi_source))

mdat <- mdat |> 
  filter(imp==1)

library(plm)

mdat <- pdata.frame(mdat,
                    index = c("state", "year"))

m0 <- plm(mort_rt ~ fc_entered_rt,
          effect = "twoways",
          model = "pooling",
          data = mdat)

m0_1 <- plm(mort_rt ~ fc_entered_rt,
            effect = "individual",
            model = "within",
            data = mdat)

summary(m0_1, vcov = vcovHC(m0_2, cluster = c("group")))


m0_2 <- plm(mort_rt ~ fc_entered_rt,
            effect = "time",
            model = "within",
            data = mdat)

summary(m0_2, vcov = vcovHC(m0_2, cluster = "time"))


m0_3 <- plm(mort_rt ~ fc_entered_rt,
            effect = "twoways",
            model = "within",
            data = mdat)

summary(m0_3, vcov = vcovHC(m0_3, cluster = c("group", "time")))

m0_4 <- plm(mort_rt ~ fc_entered_rt + 
              pop_EduLess9 + pop_unemp_rt + pop_poverty_rt,
            effect = "twoways",
            model = "within",
            data = mdat)

m1 <- plm(mort_rt ~ fc_entered_rt + 
            pop_EduLess9 + pop_unemp_rt + pop_poverty_rt + 
            mort_data_multi_source,
          effect = "twoways",
          within = T,
          data = mdat)

summary(m1, vcov = vcovHC(m1, cluster = c("group", "time")))
