# simulate measurement error and bias in mortality ------------------------
library(tidyverse)
library(Amelia)
library(brms)
library(tidybayes)

# set parameters ----------------------------------------------------------
# magnitude of bias on log scale
# use IQR of observed
x <- 1.97
gamma <- c(0, 0.1, 0.25, 0.5, 1, 1.5)
sim_dat <- expand_grid(x, gamma) |> 
  mutate(l_x = log1p(x),
         l_xg = log1p(x+gamma),
         x_g = expm1(l_xg),
         prop_change = x_g/x)

gamma <- c(0.1, 0.25, 0.5, 1, 1.5)
alpha<-c(0.05)
scen_tab<-expand_grid(gamma, alpha)
scen_tab$scen<-c(
  "1. Minimal bias",
  "2. Low bias",
  "3. Medium bias",
  "4. High bias",
  "5. Extreme bias")

# read in observed data ---------------------------------------------------
dat<-read_csv("./data/foster-care-imputed-20251028.csv") |> 
  rename(mort_rt = yrt,
         fc_entered_rt = xrt,
         pop_EduLess9 = z1_,
         pop_unemp_rt = z2_,
         pop_poverty_rt = z3_,
         imp = `_mi_m`,
         state = state_code) |> 
  select(state, year, imp, mort_rt:pop_poverty_rt) |> 
  filter(imp>0) |> 
  mutate(mort_rt = log1p(mort_rt)) |> 
  as.data.frame()
## need lognormal for this, remove 6 missing rows
# dat<-dat |>
#   as.data.frame() |>
#   mutate(across(mort_rt:pop_poverty_rt, log1p))

# run sims ----------------------------------------------------------------

k<-nrow(scen_tab)
a_out<-list()
m <- 0
for(h in 1:max(dat$imp)){
  for(i in 1:k){
    m <- m + 1
    gamma2<-scen_tab$gamma[i]
    alpha2<-scen_tab$alpha[i]
    temp <- dat |> 
      filter(imp==h) |> 
      select(-imp) 
    
    priors<-moPrep(temp,
                   formula = mort_rt ~ mort_rt,
                   error.sd = 0.1)
    ## shift mort_rt up from log1p(x) to log1p(x+gamma)
    priors$priors[,3]<-log1p(expm1(temp$mort_rt) + gamma2)
    # adjust sigma alpha * expm1(log(1 + x + gamma))
    priors$priors[,4] <- alpha2 * priors$priors[,3]
    # priors$priors[,4] <- ifelse(priors$priors[,4]==0,
    #                             alpha,
    #                             priors$priors[,4])
    
    a_temp<-amelia(priors,
                   cs = "state", 
                   ts = "year",
                   p2s = 0,
                   m = 10)
    
    t<-bind_rows(a_temp$imputations$imp1 |> 
                   mutate(imp = (h-1)*10 +  1),
                 a_temp$imputations$imp2|> 
                   mutate(imp = (h-1)*10 +  2),
                 a_temp$imputations$imp3|> 
                   mutate(imp = (h-1)*10 +  3),
                 a_temp$imputations$imp4|> 
                   mutate(imp = (h-1)*10 +  4),
                 a_temp$imputations$imp5|> 
                   mutate(imp = (h-1)*10 +  5),
                 a_temp$imputations$imp6|> 
                   mutate(imp = (h-1)*10 +  6),
                 a_temp$imputations$imp7|> 
                   mutate(imp = (h-1)*10 +  7),
                 a_temp$imputations$imp8|> 
                   mutate(imp = (h-1)*10 +  8),
                 a_temp$imputations$imp9|> 
                   mutate(imp = (h-1)*10 +  9),
                 a_temp$imputations$imp10|> 
                   mutate(imp = (h-1)*10 +  10)) |> 
      mutate(scen = scen_tab$scen[i])
    
    a_out[[m]]<-t
  }
}

# visualize imputed -------------------------------------------------------
p_dat<-bind_rows(a_out) |> 
  mutate(type = "Simulated") |> 
  bind_rows(
    dat |> 
      expand_grid(scen = unique(scen_tab$scen)) |> 
      mutate(type = "Observed",
             imp = imp + 200))

p_dat2 <- p_dat |> 
  filter(imp<=200) |> 
  group_by(scen) |> 
  summarize(mort_rt_mn = mean(mort_rt)) |> 
  mutate(type = "Simulated") |> 
  bind_rows(p_dat |> 
              filter(imp>200) |> 
              group_by(scen) |> 
              summarize(mort_rt_mn = mean(mort_rt)) |> 
              mutate(type = "Observed"))

pal <- RColorBrewer::brewer.pal(3, "Dark2")

ggplot(p_dat,
       aes(x = expm1(mort_rt),
           group = imp,
           color = type)) + 
  geom_line(alpha = 0.2,
            stat = "density") + 
  geom_vline(data = p_dat2, 
             aes(xintercept = expm1(mort_rt_mn),
                 color = type),
             lty = 2) + 
  facet_wrap(~scen, ncol = 1) + 
  labs(color = "Imputed",
       y = "Density",
       x = "Child maltreatment mortality rate per 100,000") + 
  theme_bw()  + 
  coord_cartesian(xlim=c(0, 8)) + 
  guides(colour = guide_legend(override.aes = list(alpha = 1))) + 
  labs(color = "") 

ggsave("./vis/figS1.png",
       width = 8,
       height = 6)
ggsave("./vis/figS1.pdf",
       width = 8,
       height = 6)

# estimate models ---------------------------------------------------------
p_dat<-p_dat |>
  mutate(mort_rt = expm1(mort_rt)) |>
  filter(imp<=200)
# initialize model
m1<-brm(mort_rt ~ fc_entered_rt + pop_EduLess9 +
          pop_unemp_rt + pop_poverty_rt +
          factor(state) + factor(year),
        data = p_dat |>
          filter(imp==1, scen == "1. Minimal bias"),
        cores = 4,
        iter = 5000)

m_out<-list()
l<-1
post_beta<-list()
for(k in 1:length(unique(p_dat$scen))){
  for(i in 1:max(p_dat$imp)){
    temp<-p_dat |>
      filter(imp==i, scen == unique(p_dat$scen)[k])
    print(c(i, k))

    m_temp<-update(m1,
                   newdata = temp,
                   iter = 5000,
                   cores = 4)
    ### then sample posterior beta
    b_temp<-data.frame(m_temp)$b_fc_entered_rt
    m_out[[l]]<-data.frame(b_post = b_temp,
                           scen = unique(p_dat$scen)[k],
                           imp = i)
    l<-l+1
  }
}

m_out <- bind_rows(m_out)

write_csv(m_out, "./data/sim_betas.csv")

# read in betas -----------------------------------------------------------
# m_out<-read_csv("./data/sim_betas.csv")
# fix labels

ggplot(m_out,
       aes(y = scen, 
           x = b_post)) + 
  stat_slabinterval(color = "black", 
            size = 0.5,
            .width = 0.95) + 
  geom_vline(xintercept = 0) + 
  geom_vline(xintercept = 0.17, lty = 2) + 
  labs(y = "",
       x = "Posterior distribution of Beta")

# make a two panel; one pooled one by imp

ggsave("./vis/figS2.pdf", width = 8, height =6)
ggsave("./vis/figS2.png", width = 8, height =6)

