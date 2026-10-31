###############################################################################
### Replication materials for The Institutional Order of Liberalization (BJPOLS)
# authors: AMANDA B. EDGELL, VANESSA A. BOESE, SERAPHINE F. MAERZ, 
#         PATRIK LINDENFORS, and STAFFAN I. LINDBERG
###############################################################################

## install packages::
devtools::install_github("jsks/seqR", build_vignettes = T)
devtools::install_github("vdeminstitute/vdemdata@V10")
devtools::install_github("vdeminstitute/ERT@V2.2")

# load required packages ----
library(seqR)
library(vdemdata)
library(ERT)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(ggrepel)
library(xtable)
install.packages("stargazer")
library(stargazer)
install.packages("psych")
library(psych)
library(modelr)

# house keeping ----

## clean up the workspace
rm(list = ls())

## here we store a vector of variables we are interested in 
vars <- c("v2mebias_ord", "v2mecrit_ord", "v2merange_ord", "v2elembaut_ord", 
          "v2elfrfair_ord", "v2elrgstry_ord", "v2elintim_ord", "v2elirreg_ord",
          "v2elembcap_ord", "v2elvotbuy_ord", "v2elpeace_ord", "v2cseeorgs_ord",
          "v2csreprss_ord", "v2elmulpar_ord", "v2psbars_ord", "v2psoppaut_ord",
          "v2psparban_ord", "v2meharjrn_ord", "v2cldiscm_ord", "v2clacfree_ord",
          "v2mecenefm_ord", "v2cldiscw_ord", "v2meslfcen_ord", "e_v2x_suffr_5C")

# loading required data ----

## here we load additional information about the indicators ----
if (file.exists("vars_subin.csv")) {
  indicators <- read.csv("vars_subin.csv")
} else {
  # fallback lookup table (used when vars_subin.csv is not in the working directory)
  # indicator = V-Dem variable code, varname = label shown on the graph,
  # subind = sub-index shown in the legend / used for point and label colors
  indicators <- data.frame(
    indicator = vars,
    varname = c("Media bias", "Media critical", "Media perspectives",
                "EMB autonomy", "Election free and fair", "Voter registry",
                "Election intimidation", "Other voting irregularities",
                "EMB capacity", "Vote buying", "Other electoral violence",
                "CSO entry and exit", "CSO repression", "Elections multiparty",
                "Barriers to parties", "Opposition parties autonomy",
                "Party ban", "Harassment of journalists", "Discussion for men",
                "Academic & cultural expression", "Government media censorship",
                "Discussion for women", "Media self\u2212censorship", "Suffrage"),
    subind = c(rep("Information", 3),
               rep("Clean elections", 5),
               "Clean elections", "Clean elections", "Clean elections",
               rep("Association", 6),
               rep("Expression", 6),
               "Suffrage"),
    stringsAsFactors = FALSE
  )
  indicators$subindvar <- indicators$subind
  indicators$subinda   <- indicators$subind
}

## here we load the data we need from the v-dem dataset ----

  # we start with the filled V-Dem dataset from the V-Dem package
edivars <- vdemdata::fill_vars(fill_na=T) %>%
  # then we select the columns we need for analysis
  select(year, country_text_id, all_of(vars), v2x_elecreg) %>% 
  
  ### we need to transform some variables into 0-4 scale
  mutate(e_v2x_suffr_5C = round(e_v2x_suffr_5C*4)) %>%    
  mutate(v2mecrit_ord = round(v2mecrit_ord*(4/3))) %>%
  mutate(v2merange_ord = round(v2merange_ord*(4/3))) %>%
  mutate(v2meslfcen_ord = round(v2meslfcen_ord*(4/3))) 
  

## then we get our episodes data ----

  # we start with the full sample from the ERT package
eps <- get_eps() %>%
  
  # here we subset into episodes of liberalizing autocracy, eliminating democratic years after any transition
  filter(dem_ep_prch==1 & dem_ep_ptr==0) %>%
  
  # then we select only the columns we need
  select(country_name, country_text_id, year, dem_ep_id, 
         dem_ep_start_year, dem_ep_end_year, dem_ep_outcome) %>%
  
  # and then join with the selected variables from the V-Dem dataset
  left_join(edivars, by=c("country_text_id", "year")) %>%
  ungroup()


# ## If you encounter problems with the V-Dem or ERT packages, here is the underlying data in a CSV file ----
  # uncomment the line below if you wish to load the data locally rather than compile
# eps <- read.csv("eps_data.csv")

## table with uncensored episodes ----
eps %>%
  filter(dem_ep_outcome !=6) %>%
  select(Country = country_name, Start=dem_ep_start_year, End=dem_ep_end_year, Outcome=dem_ep_outcome) %>%
  unique() %>%
  mutate(Outcome = case_when(Outcome == 1 ~ "democratic transition",
                             Outcome == 2 ~ "preempted transition",
                             Outcome == 3 ~ "stabilized electoral autocracy",
                             Outcome == 4 ~ "reverted liberalization")) %>%
  arrange(Country, Start) %>%
  xtable(digits=0) %>%
  print.xtable("TableA1.html", type="html")

## how many uncensored episodes do we have?
nrow(unique(select(filter(eps, dem_ep_outcome!=6), dem_ep_id)))



# now we generate our `collapsed` episodes datasets ----

## we will keep these in a list, so first let's make one
eps_col <- list()

## here we use a small function for efficiency

coldata <- function(df) {
  df %>%
    arrange(country_text_id, year) %>%
    select(all_of(vars)) %>%
    seqR::collapse()
}


## here we generate the collapsed dataframe for all episodes, and outcomes
eps_col$noncens <- coldata(filter(eps, dem_ep_outcome!=6)) 
eps_col$succ <- coldata(filter(eps, dem_ep_outcome==1)) 
eps_col$fail <- coldata(filter(eps, dem_ep_outcome>1 & dem_ep_outcome<6))
eps_col$censored <- coldata(filter(eps, dem_ep_outcome==6)) 
eps_col$preem <- coldata(filter(eps, dem_ep_outcome==2))
eps_col$stab <- coldata(filter(eps, dem_ep_outcome==3))
eps_col$rev <- coldata(filter(eps, dem_ep_outcome==4))


# generating percent tables based on collapsed data ----
## note: this is used for the main analysis

## create a list to store these  #
ptables <- list()


## then we can construct our percent tables for each episode type 
ptables <- map(eps_col, ~ ptable(.x))


# generating percent tables based on country-year data ----
## note: this is used for the robustness

## create a list to store these  #
ptables_cy <- list()


## then we can construct our percent tables for each episode type 
cptab <- function(df) {
  df %>%
    select(all_of(vars)) %>%
    ptable()
}

ptables_cy$noncens <- cptab(filter(eps, dem_ep_outcome !=6))
ptables_cy$succ <- cptab(filter(eps, dem_ep_outcome==1)) 
ptables_cy$fail <- cptab(filter(eps, dem_ep_outcome>1 & dem_ep_outcome<6))
ptables_cy$censored <- cptab(filter(eps, dem_ep_outcome==6)) 
ptables_cy$preem <- cptab(filter(eps, dem_ep_outcome==2)) 
ptables_cy$stab <- cptab(filter(eps, dem_ep_outcome==3)) 
ptables_cy$rev <- cptab(filter(eps, dem_ep_outcome==4)) 

# generating the domination tables from the ptables for collapsed data ----

## here we write a function to make the process more efficient
dtable <- function(df, p=50) {
  # we filter out cases where domination is less than 50%
  dom <- filter(df, `X>Y` > p)
  # we find the number of other indicators the indicator dominates
  n_x <- dom %>%
    group_by(X) %>%
    tally(name="d") %>%
    select(indicator=X, d)
  # we find the number of other indicators the indicator is dominated by
  n_y <- dom %>%
    group_by(Y) %>%
    tally(name="r") %>%
    select(indicator = Y, r)
  # we join these data together into one dataframe with info on index
  n_x %>%
    full_join(n_y, by="indicator") %>%
    full_join(indicators, by = "indicator") %>%
   # here we fill in missing values with zeros
    mutate(d = ifelse(is.na(d), 0, d), 
           r = ifelse(is.na(r), 0, r),
           score = r-d) %>%
    arrange(score)
}


## now we store domination tables for all episodes and outcomes in a list
dtables <- map(ptables, ~ dtable(.x))

# store domination tables for country-year data ----
dtables_cy <- map(ptables_cy, ~ dtable(.x))


# descriptive statistics ----

dsc_cy <- eps %>% 
  filter(dem_ep_outcome!=6) %>%
  select(all_of(vars)) %>%
  describe(fast=T) %>%
  tibble::rownames_to_column("indicator") %>%
  left_join(indicators, by="indicator") %>%
  select(indicator = varname,
         varname = indicator,
         subindex = subind, 
         obs = n,
         mean, sd) 

dsc_col <- eps_col$noncens %>%
  select(all_of(vars)) %>%
  describe(fast=T) %>%
  tibble::rownames_to_column("indicator") %>%
  left_join(indicators, by="indicator") %>%
  select(indicator = varname,
         varname = indicator,
         subindex = subind, 
         obs = n,
         mean, sd) 


dsc <- dsc_col %>%
  left_join(dsc_cy, by=c("varname", "indicator", "subindex")) 


print.xtable(xtable(dsc, digits=c(0,0,0,0,0,2,2,0,2,2)), 
             include.rownames =FALSE, file="TableA2.html", type="html")


# modeling democratic vs. nondemocratic outcomes (main results) ----

## a function to join dfs for outcomes
sfunc <- function(x,y) {
  x %>%
    left_join(y, by=c("indicator", "varname", "subindvar",
                         "subind", "subinda")) %>%
    select(indicator = varname, subind, d.x, r.x, score.x, d.y, r.y, score.y)
}


## join our main data
sf <- sfunc(dtables$succ, dtables$fail)


## make our main graph and export
set.seed(42)
ggplot(sf, aes(y=score.y, x=score.x)) +
  geom_hline(yintercept = 0, color="grey") +
  geom_vline(xintercept = 0, color= "grey") +
  stat_smooth(method="lm", color="grey", fullrange=T, level=.99) +
  geom_point(aes(color=subind)) +
  geom_text_repel(aes(label = indicator, color=subind), force = 10, show.legend=F, size = 4, segment.alpha=0.5) +
  scale_color_manual(values=c("#440154FF", "#35608DFF", "#21908CFF", "#5DC863FF", "#FDE725FF"), name="") +
  theme_light() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        text = element_text(size=12),
        legend.text=element_text(size=12)) +
  guides(color=guide_legend(nrow=1,byrow=TRUE,title.position = "top", override.aes = list(size=3))) +
  scale_x_continuous(name="Democratic transition", 
                     limits=c(-25,25), expand = c(0, 0),
                     sec.axis = sec_axis(trans=~.*1, 
                                         breaks=c(-10,10),
                                         labels=c("Earlier", "Later"))) +
  scale_y_continuous(name="Non-democratic outcome", 
                     limits=c(-29,29), expand = c(0, 0), 
                     sec.axis=sec_axis(trans=~.*1,
                                       breaks=c(-10,10),
                                       labels=c("Earlier", "Later"))) +
  coord_cartesian(xlim=c(-23,23),ylim=c(-23,23))

ggsave("Figure1.pdf", plot = last_plot(), width = 8, height = 8)


## get data for models and store it for later
fit1 <- lm(sf$score.y ~ sf$score.x)
summary(fit1)
sf <- sf %>% add_residuals(fit1) 
sf %>% select(indicator, d.x, r.x, score.x, d.y, r.y, score.y) %>%
  arrange(score.x) %>%
  xtable(digits=0) %>%
    print.xtable(include.rownames = F, file="TableA3.html", type="html")

# modeling the different types of outcomes ----

## preempted transitions----
spreem <- sfunc(dtables$succ, dtables$preem)
  
fit2 <- lm(spreem$score.y ~ spreem$score.x)
summary(fit2)
spreem <- spreem %>% add_residuals(fit2)

## stabilized electoral authoritarianism ----
sstab <- sfunc(dtables$succ, dtables$stab)

fit3 <- lm(sstab$score.y ~ sstab$score.x)
summary(fit3)
sstab <- sstab %>% add_residuals(fit3)

## stabilized electoral authoritarianism ----
srev <- sfunc(dtables$succ, dtables$rev)

fit4 <- lm(srev$score.y ~ srev$score.x)
summary(fit4)
srev <- srev %>% add_residuals(fit4)

## creating the regression table ----
regtab <- capture.output(stargazer(fit1, fit2, fit3, fit4, out="TableA4.html", align=T, no.space=T, digits=2))

## domination table for failure types ----
dtables$preem %>%
  select(indicator=varname, d, r, score) %>%
  left_join(select(dtables$stab, indicator=varname, d, r, score),
            by=c("indicator")) %>%
  left_join(select(dtables$rev, 
                   indicator=varname, d, r, score),
            by=c("indicator")) %>%
  xtable(digits=0) %>%
  print.xtable(include.rownames = F, file="TableA5.html", type="html")

## residuals table ---
sfres<- sf %>%
  select(indicator, all=resid) %>%
  left_join(select(spreem, indicator, preem=resid), by="indicator") %>%
  left_join(select(sstab, indicator, stab=resid), by="indicator") %>%
  left_join(select(srev, indicator, rev=resid), by="indicator") %>%
  arrange(all) 

print.xtable(xtable(sfres, digits=2), include.rownames = F, file="TableA6.html", type="html")

summary(sfres$all)

sfres <- sfres %>%
  select(-all) %>%
  gather(key=outcome, val=resid, preem:rev)

# now we replicate everything using the country-year data ----

## democratic v. all non-democratic outcomes 
sf_cy <- sfunc(dtables_cy$succ, dtables_cy$fail)

# get data for models and store it for later
fitcy1 <- lm(sf_cy$score.y ~ sf_cy$score.x)
summary(fitcy1)
sf_cy <- sf_cy %>% add_residuals(fitcy1) 

## democratic v. preem  ----
spreem_cy <- sfunc(dtables_cy$succ, dtables_cy$preem)

# get data for models and store it for later
fitcy2 <- lm(spreem_cy$score.y ~ spreem_cy$score.x)
summary(fitcy2)
spreem_cy <- spreem_cy %>% add_residuals(fitcy2) 

## democratic v. stab  ----
sstab_cy <- sfunc(dtables_cy$succ, dtables_cy$stab)

# get data for models and store it for later
fitcy3 <- lm(sstab_cy$score.y ~ sstab_cy$score.x)
summary(fitcy3)
sstab_cy <- sstab_cy %>% add_residuals(fitcy3) 

## democratic v. rev  ----
srev_cy <- sfunc(dtables_cy$succ, dtables_cy$rev)

# get data for models and store it for later
fitcy4 <- lm(srev_cy$score.y ~ srev_cy$score.x)
summary(fitcy4)
srev_cy <- srev_cy %>% add_residuals(fitcy4) 

## now we get our regression table ----
regtabcy <- capture.output(stargazer(fitcy1, fitcy2, fitcy3, fitcy4, out="TableA7.html", align=T, no.space=T, digits=2))

## residuals table ---
sfres_cy <- sf_cy %>%
  select(indicator, all=resid) %>%
  left_join(select(spreem_cy, indicator, preem=resid), by="indicator") %>%
  left_join(select(sstab_cy, indicator, stab=resid), by="indicator") %>%
  left_join(select(srev_cy, indicator, rev=resid), by="indicator") %>%
  arrange(all) 

print.xtable(xtable(sfres_cy, digits=2), include.rownames = F, file="TableA8.html", type="html")

summary(sfres_cy$all)

## domination table for country-year 
dtables_cy$succ %>%
  select(indicator=varname, dsucc=d, rsucc=r, scoresucc = score) %>%
  left_join(select(dtables_cy$fail, indicator=varname, dfail=d, rfail=r, scorefail = score),
            by=c("indicator")) %>%
  left_join(select(dtables_cy$preem, indicator=varname, dpreem = d, rpreem = r, scorepreem = score),
            by=c("indicator")) %>%
  left_join(select(dtables_cy$stab, indicator=varname, dstab = d, rstab = r, scorestab = score),
            by=c("indicator")) %>%
  left_join(select(dtables_cy$rev, 
                   indicator=varname, drev = d, rrev = r, scorerev = score),
            by=c("indicator")) %>%
  xtable(digits=0) %>%
  print.xtable(include.rownames = F, file="TableA9.html", type="html")

## now we replicate our collapsed finding using 66.6% ----
dtables66 <- map(ptables, ~ dtable(.x, p=(2/3*100)))

## democratic v. all non-democratic outcomes ----
sf66 <- sfunc(dtables66$succ, dtables66$fail)

# get data for models and store it for later
fit661 <- lm(sf66$score.y ~ sf66$score.x)
summary(fit661)
sf66 <- sf66 %>% add_residuals(fit661) 

## democratic v. preem  ----
spreem66 <- sfunc(dtables66$succ, dtables66$preem)

# get data for models and store it for later
fit662 <- lm(spreem66$score.y ~ spreem66$score.x)
summary(fit662)
spreem66 <- spreem66 %>% add_residuals(fit662) 

## democratic v. stab  ----
sstab66 <- sfunc(dtables66$succ, dtables66$stab)

# get data for models and store it for later
fit663 <- lm(sstab66$score.y ~ sstab66$score.x)
summary(fit663)
sstab66 <- sstab66 %>% add_residuals(fit663) 

## democratic v. rev  ----
srev66 <- sfunc(dtables66$succ, dtables66$rev)

# get data for models and store it for later
fit664 <- lm(srev66$score.y ~ srev66$score.x)
summary(fit664)
srev66 <- srev66 %>% add_residuals(fit664) 

## now we get our regression table ----
regtab66 <- capture.output(stargazer(fit661, fit662, fit663, fit664, out="TableA10.html", align=T, no.space=T, digits=2))

## residuals table ---
sfres66 <- sf66 %>%
  select(indicator, all=resid) %>%
  left_join(select(spreem66, indicator, preem=resid), by="indicator") %>%
  left_join(select(sstab66, indicator, stab=resid), by="indicator") %>%
  left_join(select(srev66, indicator, rev=resid), by="indicator") %>%
  arrange(all) 

print.xtable(xtable(sfres66, digits=2), include.rownames = F, file="TableA11.html", type="html")

summary(sfres66$all)

## domination table for 66% dominance  ----
dtables66$succ %>%
  select(indicator=varname, dsucc=d, rsucc=r, scoresucc = score) %>%
  left_join(select(dtables66$fail, indicator=varname, dfail=d, rfail=r, scorefail = score),
            by=c("indicator")) %>%
  left_join(select(dtables66$preem, indicator=varname, dpreem = d, rpreem = r, scorepreem = score),
            by=c("indicator")) %>%
  left_join(select(dtables66$stab, indicator=varname, dstab = d, rstab = r, scorestab = score),
            by=c("indicator")) %>%
  left_join(select(dtables66$rev, 
                   indicator=varname, drev = d, rrev = r, scorerev = score),
            by=c("indicator")) %>%
  xtable(digits=0) %>%
  print.xtable(include.rownames = F, file="TableA12.html", type="html")


## replicating with 80% is not feasible, domination becomes too small ----
dtables80 <- map(ptables, ~ dtable(.x, p=80))

