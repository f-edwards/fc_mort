# make fig 1
library(tidyverse)
library(patchwork)
library(haven)
library(geofacet)

dat <- read_csv("./data/mort_fc.csv")
dat_out<-read_csv("./data/foster-care-imputed-20251028.csv") |> 
  rename(mort_rt = yrt,
        fc_entered_rt = xrt,
        imp = `_mi_m`,
        state = state_code) |> 
  select(state, year, mort_rt, mort_rt, fc_entered_rt, imp) |> 
  filter(imp>0)
pop<-read_csv("./data/nhgis0074_csv/nhgis0074_ts_nominal_state.csv") %>%
  rename(pop_tot = AV0AA,
         pop_edu_less9 = B69AA,
         pop_edu_lessBA = B69AB,
         pop_edu_BA = B69AC,
         pop_labor_InLabForce = B84AA,
         pop_labor_unemployed = B84AE,
         pop_poverty = CL6AA) %>%
  mutate(pop_EduLess9 = pop_edu_less9/
           (pop_edu_less9 + pop_edu_lessBA + pop_edu_BA) * 100,
         pop_unemp_rt = pop_labor_unemployed/pop_labor_InLabForce * 100,
         pop_poverty_rt = pop_poverty / pop_tot * 100) %>%
  mutate(year = str_sub(YEAR, -4, -1)) %>%
  select(year, STATE, pop_EduLess9:pop_poverty_rt) %>%
  write_csv("./data/pop_ts.csv")

# state abbreviations
xwalk<-data.frame(STATE = state.name,
                  state = state.abb)

pop<-pop %>%
  left_join(xwalk) %>%
  select(-STATE) %>%
  mutate(year = as.numeric(year)) |> 
  filter(!(is.na(state))) # remove DC

dat_out<-dat_out %>%
  left_join(pop)

### add infant supplemental models
### add inf
pop<-read_fwf("./data/us.1990_2023.singleages.through89.90plus.adjusted.txt",
              fwf_widths(c(4, 2, 2, 3, 2, 1, 1, 1, 2, 8),
                         c("year", "state", "st_fips", "cnty_fips", "reg", "race", 
                           "hisp", "sex", "age", "pop"))) %>% 
  filter(year>2009)

fc_infants<-read_csv("./data/afcars_infants_13_21.csv") %>% 
  rename(year = FY, state = St)

pop_infants<-pop %>% 
  filter(age == "00"| age == "01") %>% 
  mutate(pop = as.numeric(pop)) %>% 
  group_by(year, state) %>% 
  summarize(infant_pop = sum(pop))

fc_infants_pop <- fc_infants %>% 
  left_join(pop_infants) %>% 
  mutate(infant_fc_entry_rt = n / infant_pop * 1e3)

dat_out<-dat_out %>% 
  mutate(year = as.numeric(year)) %>% 
  left_join(fc_infants_pop %>% 
              select(year, state, infant_fc_entry_rt))

pop_child<-pop %>% 
  mutate(age = as.numeric(age)) |> 
  filter(age<18) %>% 
  mutate(pop = as.numeric(pop)) %>% 
  group_by(year, state) %>% 
  summarize(child_pop = sum(pop))

### fig 1


dat_plot <- dat_out |> 
  group_by(state, year) |> 
  summarize(fc_entered_rt = mean(fc_entered_rt),
            mort_rt_mn = mean(mort_rt),
            mort_rt_lwr = quantile(mort_rt, 0.025),
            mort_rt_upr = quantile(mort_rt, 0.975))

nat_plot <- dat_out |> 
  left_join(pop_child) |> 
  mutate(fc_entered = fc_entered_rt * child_pop / 1000,
         mort_tot = mort_rt * child_pop / 1e5) |> 
  group_by(year, imp) |> 
  summarize(fc_entered = sum(fc_entered),
            mort = sum(mort_tot),
            pop = sum(child_pop)) |> 
  summarize(fc_entered = mean(fc_entered),
            mort_mn = mean(mort),
            mort_lwr = quantile(mort, 0.025),
            mort_upr = quantile(mort, 0.975),
            pop = max(pop)) |> 
  mutate(fc_entered_rt = fc_entered / pop * 1e3,
         mort_rt_mn = mort_mn / pop * 1e5,
         mort_rt_lwr = mort_lwr / pop * 1e5,
         mort_rt_upr = mort_upr / pop * 1e5)

p1<-ggplot(nat_plot, 
       aes(x = year, 
           y = mort_rt_mn,
           ymin = mort_rt_lwr,
           ymax = mort_rt_upr)) + 
  geom_line() + 
  geom_ribbon(alpha = 0.5) + 
  geom_line(aes(y = fc_entered_rt), color = "red") + 
  theme_classic() + 
  scale_x_continuous(breaks = c(2011, 2013, 2015, 2017, 2019, 2021, 2023)) +
  annotate("text",
           x = 2013.8, y = 3.3,
           label = "Foster care\nentry rate",
           color = "red") + 
  annotate("text",
           x = 2013.8, y = 2.3,
           label = "Maltreatment\nmortality rate",
           color = "black") + 
  labs(x = "Year",
       y = "Rate per 1,000 children (red); rate per 100,000 children (black)",
       subtitle = "(National)")

  
my_grid <- us_state_grid1
my_grid <- my_grid[1:50,]

p2<-ggplot(dat_plot,
       aes(y = mort_rt_mn,
           ymin = mort_rt_lwr,
           ymax = mort_rt_upr,
           x = as.numeric(year) - 2000,
           group = state)) + 
  geom_line() + 
  geom_line(aes(y = fc_entered_rt), color = "red") + 
  geom_ribbon(alpha = 0.5) + 
  facet_wrap(~state,
             ncol = 10,
             axes = "all",
             axis.labels = "margins") +
  labs(x = "", y = "",
       subtitle = "(By state)") + 
  theme_classic() + 
  theme(strip.background = element_blank(), 
        strip.placement = "outside",
        axis.ticks = element_blank(),
        axis.text.x = element_blank())



patch <- p1 + p2 + 
  plot_annotation(
    caption = "Note differing scales for foster care (entries per 1,000 children) and maltreatment mortality (deaths per 100,000).\nShaded area represents 95% post-imputation interval.\nFoster care data from AFCARS and mortality data from NCANDS 2010-2023")
patch
ggsave("./vis/fig1.png", height = 8, width = 16)
ggsave("./vis/fig1.pdf", height = 8, width = 16)
