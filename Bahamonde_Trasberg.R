############################
#### CENSUS HEAT MAP
############################
cat("\014")
rm(list=ls())

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(haven)


# Load Census Data
# census <- haven::read_dta("/Users/hectorbahamonde/research/Inequality_State_Capacities/EJPE_data.dta") 
census <- haven::read_dta("/Users/hectorbahamonde/research/Inequality_State_Capacities/census_list_for_map.dta") 

# Subset
p_load(tidyverse)
census = census %>% mutate(year2 = as_factor(year2)) # year2 to factor
census$year2 = as.numeric(as.character(census$year2)) # year2 to numeric
census = census[census$year2 <= 2010, ] # keep up to 2010


p_load(tidyverse)
# census = data.frame(census %>% select(country, cum_census5, year2)) ## Drop everything else
census = data.frame(census %>% select(country, cum_census6, year2)) ## Drop everything else

census$country = as.character(as_factor(census$country)) # country to character
census$cum_census6 = as.numeric(as_factor(census$cum_census6)) # cum_census6 to numeric

p_load(scales) # rescaling
census$cum_census6 = rescale(census$cum_census6, to = c(0,6))


# Get Max Census
p_load(data.table)
census = data.frame(na.omit(data.frame(setDT(census)[, .SD[which.max(cum_census6)], by=country])))


# Add Manually countries
census <- rbind(census,  c("Congo, Dem. Rep.", 2, 2010))
census <- rbind(census,  c("Ivory Coast", 4, 2010))
census <- rbind(census,  c("Kyrgyzstan", 5, 2010))
census <- rbind(census,  c("Bosnia and Herzegovina", 5, 2010))
census <- rbind(census,  c("Czech Republic", 6, 2010))
census <- rbind(census,  c("Slovakia", 6, 2010))
census <- rbind(census,  c("Republic of Serbia", 5, 2010))
census <- rbind(census,  c("Central African Republic", 3, 2010))
census <- rbind(census,  c("South Sudan", 4, 2010)) # add



# Load World Data
p_load(cowplot, googleway, ggplot2, ggrepel, ggspatial, sf, rnaturalearth, rnaturalearthdata)
map <- ne_countries(scale = "medium", returnclass = "sf")

# change name
map$country = map$admin



# Change country names
map$country[map$country == "United States of America"] <- "United States"
map$country[map$country == "North Korea"] <- "Korea, North"
map$country[map$country == "South Korea"] <- "Korea, South"
map$country[map$country == "United Republic of Tanzania"] <- "Tanzania"
map$country[map$country == "Republic of Congo"] <- "Congo, Rep."
map$country[map$country == "Guinea Bissau"] <- "Guinea-Bissau"
map$country[map$country == "Democratic Republic of the Congo"] <- "Congo, Dem. Rep."


# Merge
dat <- merge(census, map, by=c("country"))
dat <- st_as_sf(dat)

# Rename "cum_census6" var. 
colnames(dat)[which(names(dat) == "cum_census6")] <- "Cumulative Census"

# Plot
p_load(ggplot2)
ggplot(data = dat) +
  geom_sf(aes(fill = `Cumulative Census`)) + 
  theme_bw() +
  scale_fill_viridis_d(
    alpha = 0.7,
    direction = -1,
    option = "plasma") # "magma", "inferno", "plasma", "viridis",  "cividis"





############################
#### Long-run Effects
############################

# Here we follow Dorsch and Maarek (2019, eq. 4). Based on our main equation:

# Inequality_i,t = α0 + β0Inequalityi_t-1 + β1Democracy_i,t-1 + β2StateCapacity_i,t-1 + β3Democracy*State Capacity_i,t- 1 +  Controls_i, t-1  +  γ_i + λ_t + µ_i,t

# State Cap levels
Stat.Cap.Max = 6
Stat.Cap.Med = 3

############
# Table 1
############

# Model 3
t1.m3.b1 = -0.337
t1.m3.b3 = 0.107
t1.m3.b0 = 0.917


# Model 4
t1.m4.b1 = -0.021
t1.m4.b3 = 0.007
t1.m4.b0 = 0.921

# Model 5
t1.m5.b1 = -0.187
t1.m5.b3 = 0.096
t1.m5.b0 = 0.937

# Model 6
m6.b1 = -0.022
m6.b3 = 0.008
m6.b0 = 0.941






# We define long-run effects such that (b1+b3*Stat.Cap)/(1-b0). Based on out main results table,

# long-run effects: m3
round((t1.m3.b1+(t1.m3.b3*Stat.Cap.Max))/(1-t1.m3.b0),2) # Max Lev of State Cap
round((t1.m3.b1+(t1.m3.b3*Stat.Cap.Med))/(1-t1.m3.b0),2) # Med Lev of State Cap

# long-run effects: m4
round((t1.m4.b1+(t1.m4.b3*Stat.Cap.Max))/(1-t1.m4.b0),2) # Max Lev of State Cap
round((t1.m4.b1+(t1.m4.b3*Stat.Cap.Med))/(1-t1.m4.b0),2) # Med Lev of State Cap

# long-run effects: m5
round((t1.m5.b1+(t1.m5.b3*Stat.Cap.Max))/(1-t1.m5.b0),2) # Max Lev of State Cap
round((t1.m5.b1+(t1.m5.b3*Stat.Cap.Med))/(1-t1.m5.b0),2) # Med Lev of State Cap

# long-run effects: m6
round((m6.b1+(m6.b3*Stat.Cap.Max))/(1-m6.b0),2) # Max Lev of State Cap
round((m6.b1+(m6.b3*Stat.Cap.Med))/(1-m6.b0),2) # Med Lev of State Cap



############
# Table 2
############

# Model 1
t2.m1.b1 = -0.117
t2.m1.b3 = 0.108
t2.m1.b0 = 0.937

# Model 3
t2.m3.b1 = -0.096
t2.m3.b3 = 0.085
t2.m3.b0 = 0.944
  
# Model 5
t2.m5.b1 = -0.216
t2.m5.b3 = 0.039
t2.m5.b0 = 0.995


# long-run effects: m1
round((t2.m1.b1+(t2.m1.b3*Stat.Cap.Max))/(1-t2.m1.b0),2) # Max Lev of State Cap
round((t2.m1.b1+(t2.m1.b3*Stat.Cap.Med))/(1-t2.m1.b0),2) # Med Lev of State Cap


# long-run effects: m3
round((t2.m3.b1+(t2.m3.b3*Stat.Cap.Max))/(1-t2.m3.b0),2) # Max Lev of State Cap
round((t2.m3.b1+(t2.m3.b3*Stat.Cap.Med))/(1-t2.m3.b0),2) # Med Lev of State Cap

# long-run effects: m5
round((t2.m5.b1+(t2.m5.b3*Stat.Cap.Max))/(1-t2.m5.b0),2) # Max Lev of State Cap
round((t2.m5.b1+(t2.m5.b3*Stat.Cap.Med))/(1-t2.m5.b0),2) # Med Lev of State Cap



############
# Table 3
############

# Model 1
t3.m1.b1 = -1.177
t3.m1.b3 = 0.515
t3.m1.b0 = 0.714

# Model 2
t3.m2.b1 = -0.082
t3.m2.b3 = 0.689
t3.m2.b0 = 0.041

# Model 3
t3.m3.b1 = -0.112
t3.m3.b3 = 0.080
t3.m3.b0 = 0.942

# Model 4
t3.m4.b1 = -0.024
t3.m4.b3 = 0.003
t3.m4.b0 = 0.935

# Model 5
t3.m5.b1 = -0.175
t3.m5.b3 = 0.093
t3.m5.b0 = 0.937

# Model 6
t3.m6.b1 = -0.003
t3.m6.b3 = 0.009
t3.m6.b0 = 0.936

# long-run effects: m1
round((t3.m1.b1+(t3.m1.b3*Stat.Cap.Max))/(1-t3.m1.b0),2) # Max Lev of State Cap
round((t3.m1.b1+(t3.m1.b3*Stat.Cap.Med))/(1-t3.m1.b0),2) # Med Lev of State Cap

# long-run effects: m2
round((t3.m2.b1+(t3.m2.b3*Stat.Cap.Max))/(1-t3.m2.b0),2) # Max Lev of State Cap
round((t3.m2.b1+(t3.m2.b3*Stat.Cap.Med))/(1-t3.m2.b0),2) # Med Lev of State Cap


# long-run effects: m3
round((t3.m3.b1+(t3.m3.b3*Stat.Cap.Max))/(1-t3.m3.b0),2) # Max Lev of State Cap
round((t3.m3.b1+(t3.m3.b3*Stat.Cap.Med))/(1-t3.m3.b0),2) # Med Lev of State Cap

# long-run effects: m4
round((t3.m4.b1+(t3.m4.b3*Stat.Cap.Max))/(1-t3.m4.b0),2) # Max Lev of State Cap
round((t3.m4.b1+(t3.m4.b3*Stat.Cap.Med))/(1-t3.m4.b0),2) # Med Lev of State Cap

# long-run effects: m5
round((t3.m5.b1+(t3.m5.b3*Stat.Cap.Max))/(1-t3.m5.b0),2) # Max Lev of State Cap
round((t3.m5.b1+(t3.m5.b3*Stat.Cap.Med))/(1-t3.m5.b0),2) # Med Lev of State Cap

# long-run effects: m6
round((t3.m6.b1+(t3.m6.b3*Stat.Cap.Max))/(1-t3.m6.b0),2) # Max Lev of State Cap
round((t3.m6.b1+(t3.m6.b3*Stat.Cap.Med))/(1-t3.m6.b0),2) # Med Lev of State Cap


############
# Table 4
############

# Model 1
t4.m1.b1 = -0.224
t4.m1.b3 = 0.066
t4.m1.b0 = 0.985

# Model 2
t4.m2.b1 = -0.023
t4.m2.b3 = 0.007
t4.m2.b0 = 0.986
  
# Model 3
t4.m3.b1 = -0.242
t4.m3.b3 = 0.028
t4.m3.b0 = 
  
# Model 4
t4.m4.b1 = -0.013
t4.m4.b3 = 0.001
t4.m4.b0 = 0.871


# long-run effects: m1
round((t4.m1.b1+(t4.m1.b3*Stat.Cap.Max))/(1-t4.m1.b0),2) # Max Lev of State Cap
round((t4.m1.b1+(t4.m1.b3*Stat.Cap.Med))/(1-t4.m1.b0),2) # Med Lev of State Cap

# long-run effects: m2
round((t4.m2.b1+(t4.m2.b3*Stat.Cap.Max))/(1-t4.m2.b0),2) # Max Lev of State Cap
round((t4.m2.b1+(t4.m2.b3*Stat.Cap.Med))/(1-t4.m2.b0),2) # Med Lev of State Cap

# long-run effects: m3
round((t4.m3.b1+(t4.m3.b3*Stat.Cap.Max))/(1-t4.m3.b0),2) # Max Lev of State Cap
round((t4.m3.b1+(t4.m3.b3*Stat.Cap.Med))/(1-t4.m3.b0),2) # Med Lev of State Cap

# long-run effects: m4
round((t4.m4.b1+(t4.m4.b3*Stat.Cap.Max))/(1-t4.m4.b0),2) # Max Lev of State Cap
round((t4.m4.b1+(t4.m4.b3*Stat.Cap.Med))/(1-t4.m4.b0),2) # Med Lev of State Cap


############
# Table 4A
############

# Model 1
t4a.m1.b1 = -0.251
t4a.m1.b3 = 0.127
t4a.m1.b0 = 0.943
  
# Model 1
t4a.m2.b1 = -0.183
t4a.m2.b3 = 0.096
t4a.m2.b0 = 0.935

# long-run effects: m1
round((t4a.m1.b1+(t4a.m1.b3*Stat.Cap.Max))/(1-t4a.m1.b0),2) # Max Lev of State Cap
round((t4a.m1.b1+(t4a.m1.b3*Stat.Cap.Med))/(1-t4a.m1.b0),2) # Med Lev of State Cap

# long-run effects: m2
round((t4a.m2.b1+(t4a.m2.b3*Stat.Cap.Max))/(1-t4a.m2.b0),2) # Max Lev of State Cap
round((t4a.m2.b1+(t4a.m2.b3*Stat.Cap.Med))/(1-t4a.m2.b0),2) # Med Lev of State Cap


############################
#### Loadings
############################


## ---- loadings:data ----
# Load the census data
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(foreign)

## Load Census data 
census.d <- read.dta("../Inequality_State_Capacities/Data/Census_data_until_2015.dta") 


## replaces NAs with 0s
census.d$census_full[is.na(census.d$census_full)] <- 0

## generate cumulative census variable by country
census.d = census.d[with(census.d, order(country, year)),]
census.d$cum.census <- as.numeric(ave(census.d$census_full, census.d$country, FUN=cumsum))

## keep just important variables
census.d <- census.d[, c("country", "year", "census_full", "cum.census")]





if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(data.table) 
census.d = merge(data.frame(setDT(census.d[ which(census.d$cum.census > 0),])[, .(country_num_of_census = uniqueN(cum.census)), by = country]),
                 census.d,
                 by=c("country"),
                 all.x=T
)
### generate variable with number of years by country
census.d = merge(
        data.frame(setDT(census.d)[, .(country_tot_years = uniqueN(year)), by = country]),
        census.d,
        by=c("country"),
        all.x=T
)

# average census per year
census.d$ave_census = census.d$country_num_of_census/census.d$country_tot_years


# Load Inequality data
inequality.d <- read.dta("../Inequality_State_Capacities/Data/SWIID/dataverse_files/swiid6_0/swiid6_0.dta") 

### creating DISP
inequality.disp.d = as.data.frame(inequality.d[ , grepl("gini_disp", names(inequality.d))]) # generating another DF with DISP only
inequality.d$gini_disp.mean = rowMeans(inequality.disp.d[,-1]) # attaching "gini_disp.mean" with mean of 100 columns
### creating MKT
inequality.mkt.d = as.data.frame(inequality.d[ , grepl("gini_mkt", names(inequality.d))]) # generating another DF with mkt only
inequality.d$gini_mkt.mean = rowMeans(inequality.mkt.d[,-1]) # attaching "gini_mkt.mean" with mean of 100 columns
### creating abs_red
inequality.abs_red.d = as.data.frame(inequality.d[ , grepl("_abs_red", names(inequality.d))]) # generating another DF with abs_red only
inequality.d$gini_abs_red.mean = rowMeans(inequality.abs_red.d[,-1]) # attaching "gini_abs_red.mean" with mean of 100 columns
### creating rel_red
inequality.rel_red.d = as.data.frame(inequality.d[ , grepl("rel_red", names(inequality.d))]) # generating another DF with rel_red only
inequality.d$gini_rel_red.mean = rowMeans(inequality.rel_red.d[,-1]) # attaching "gini_rel_red.mean" with mean of 100 columns
### keeping just these variables (and country, and year) and dropping everything else
inequality.d <- inequality.d[, c("country", "year", "gini_disp.mean", "gini_mkt.mean", "gini_abs_red.mean", "gini_rel_red.mean")]
## change some country names to be consistent with the other datasets
inequality.d$country[inequality.d$country == "Saint Kitts and Nevis"] <- "St Kitts and Nevis"
inequality.d$country[inequality.d$country == "Saint Vincent and the Grenadines"] <- "St. Vincent"
inequality.d$country[inequality.d$country == "Saint Lucia"] <- "St. Lucia"


# Load Boix's democracy variable

democracy.d <- read.dta("../Inequality_State_Capacities/Data/Boix_Dem_Variable/democracy.dta") 

##change the the type of variable from numeric to character
democracy.d$country = as.character(democracy.d$country)
## change some country names to be consistent with the other datasets
democracy.d$country[democracy.d$country == "UNITED STATES OF AMERICA"] <- "UNITED STATES"
democracy.d$country[democracy.d$country == "TRINIDAD&TOBAGO"] <- "Trinidad and Tobago"
democracy.d$country[democracy.d$country == "CAPE VERDE IS."] <- "Cape Verde"
democracy.d$country[democracy.d$country == "CONGO, DEM REP (ex Zaire)"] <- "CONGO"
democracy.d$country[democracy.d$country == "CENTRAL AFR.R."] <- "Central African Republic"
democracy.d$country[democracy.d$country == "BURKINA FASO"] <- "Burkina Faso"
democracy.d$country[democracy.d$country == "ETHIOPIA  (INCL. ERIT)"] <- "Ethiopia"
democracy.d$country[democracy.d$country == "ST.KITTS&NEVIS"] <- "St Kitts and Nevis"
democracy.d$country[democracy.d$country == "ST.VINCENT&GRE"] <- "St. Vincent"
democracy.d$country[democracy.d$country == "SOLOMON IS."] <- "Solomon Islands"
democracy.d$country[democracy.d$country == "YEMEN PEOPLE REPUBLIC"] <- "Yemen" # In 1990 it changes from "Yemen People Republic" to just "Yemen." This is the one that we will use.
# democracy.d$country[democracy.d$country == "YEMEN ARAB REPUBLIC"] <- "Yemen" # This is before the unification.
democracy.d$country[democracy.d$country == "GUINEA-BISS"] <- "Guinea-Bissau"
democracy.d$country[democracy.d$country == "DOMINICAN REP."] <- "Dominican Republic"
democracy.d$country[democracy.d$country == "UNITED ARAB E."] <- "United Arab Emirates"

# Boix's dataset contains an error: Yugoslavia - 1991 is repeated.
# drop Yugoslavia democracy.d$year == 1991 & democracy.d$democracy_breakdowns == 1
democracy.d$country[democracy.d$country == "YUGOSLAVIA" & democracy.d$year == 1991 & democracy.d$democracy_breakdowns == 1] <- NA



## to merge with the other datasets, change country names from all uppercase, to just first letter uppercase
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(stringi)
democracy.d$country <- stri_trans_totitle(dput(as.character(democracy.d$country)))



# merge census.d, inequality.d, democracy.d
df <- merge(census.d, inequality.d, by=c("country", "year"), all.x=T)
df <- merge(df, democracy.d, by=c("country", "year"), all.x=T)


# merging vdem and df datasets
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(readxl)
vdem <- read_excel("../Inequality_State_Capacities/Data/VDem/vdem2.xlsx") 

## clean VDEM and keep what we need.
vdem <- vdem[c("v2x_polyarchy", "v2x_libdem", "country_name", "year", "v2x_partipdem", "v2x_delibdem", "v2x_egaldem")]
## change name of country variable
colnames(vdem)[3] <- "country"

## from charct to numeric
vdem$v2x_polyarchy = scan(text=vdem$v2x_polyarchy, dec=",", sep=".")
vdem$v2x_libdem = scan(text=vdem$v2x_libdem, dec=",", sep=".")
vdem$v2x_partipdem = scan(text=vdem$v2x_partipdem, dec=",", sep=".")
vdem$v2x_delibdem = scan(text=vdem$v2x_delibdem, dec=",", sep=".")
vdem$v2x_egaldem = scan(text=vdem$v2x_egaldem, dec=",", sep=".")

# merge DF with vdem
df <- merge(df, vdem, by=c("country", "year"), all.x=T) 



# WDI data added 
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(readxl)
wdi <- read_excel("Data/WDI/wdi.xlsx")

colnames(wdi)[3] <- "country"

## changing some country names in wdi dataset to be consistent with the df dataset
wdi$country[wdi$country == "Bahamas, The"] <- "Bahamas"
wdi$country[wdi$country == "Czech Republic"] <- "Czechoslovakia"
wdi$country[wdi$country == "Egypt, Arab Rep."] <- "Egypt"
wdi$country[wdi$country == "Gambia, The"] <- "Gambia"
wdi$country[wdi$country == "Iran, Islamic Rep."] <- "Iran"

## renaming with shorter variables -- use first 15 characters of the long variables, kill white spaces and special characters.
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(stringr)
names(wdi)[5:ncol(wdi)] <- paste("wdi.", str_replace_all(stri_trans_totitle(dput(as.character(substr(colnames(wdi[,5:ncol(wdi)]), start = 1, stop = 15)))), "[^[:alnum:]]", ""), sep="")

## loop to change from charct to numeric // it also rounds to the first 10 digits.
for(i in c(5:ncol(wdi))) {
        wdi[,i] <- round(as.numeric(as.character(unlist(wdi[,i]))), 10)
}



## Adding WDI New Variables
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(readxl)
wdi.new <- read_excel("../Inequality_State_Capacities/Data/WDI/WDI_new_variables.xlsx") 

## renaming
names(wdi.new)[names(wdi.new) == 'Country Name'] <- 'country'
names(wdi.new)[names(wdi.new) == 'Time'] <- 'year'

names(wdi.new)[names(wdi.new) == 'General government final consumption expenditure (% of GDP)'] <- 'wdi.govt.exp.gdp'
names(wdi.new)[names(wdi.new) == 'General government final consumption expenditure (constant 2010 US$)'] <- 'wdi.govt.exp.const.2010'
names(wdi.new)[names(wdi.new) == 'General government final consumption expenditure (current US$)'] <- 'wdi.govt.exp.current.us.dollar'
names(wdi.new)[names(wdi.new) == 'Health expenditure, total (% of GDP)'] <- 'wdi.health.exp.gdp'


## keep just important variables
wdi.new <- wdi.new[, c("country", "year", "wdi.govt.exp.gdp", "wdi.govt.exp.const.2010", "wdi.govt.exp.current.us.dollar", "wdi.health.exp.gdp")]

wdi.new$wdi.health.exp.gdp = as.numeric(wdi.new$wdi.health.exp.gdp)
wdi.new$wdi.govt.exp.gdp = as.numeric(wdi.new$wdi.govt.exp.gdp)


## merging wdi with wdi.new
wdi <- merge(wdi, wdi.new, by=c("country", "year"), all.x=T) 

# merging df and wdi datasets, created a new one called dataset
df <- merge(df, wdi, by=c("country", "year"), all.x=T) 

## drop Yogoslavia (as it is not considered in the VDEM dataset)
df <- df[!(df$country=="Yugoslavia"),]





# New GDP from WDI (longer series)
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(readxl)
wdi.gdp.d <- read_excel("../Inequality_State_Capacities/Data/WDI/WDI_GDP_per_ capita_new.xlsx") 
## changing name of variables
names(wdi.gdp.d)[names(wdi.gdp.d) == "GDP per capita (constant 2010 US$) [NY.GDP.PCAP.KD]"] <- "wdi.gdp"
names(wdi.gdp.d)[names(wdi.gdp.d) == "Country Name"] <- "country"
names(wdi.gdp.d)[names(wdi.gdp.d) == "Time"] <- "year"
## keeping what we need.
wdi.gdp.d <- wdi.gdp.d[c("country", "year", "wdi.gdp")]
## deleting weird characters in wdi.gdp and replacing them with NA
wdi.gdp.d$wdi.gdp = as.numeric(wdi.gdp.d$wdi.gdp)
## merging with df
df <- merge(df, wdi.gdp.d, by=c("country", "year"), all.x=T) 

# Polity
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(readxl)
polity.d <- read_excel("Data/Polity/Polity_data.xlsx") 
## changing name of variables
names(polity.d)[names(polity.d) == "country_name"] <- "country"
names(polity.d)[names(polity.d) == "Polity2_revised"] <- "polity"

## keeping what we need.
polity.d <- polity.d[c("country", "year", "polity")]


## cum.polity
polity.d$polity.na <- is.na(polity.d$polity)
polity.d$polity.2 <- polity.d$polity # duplicates polity score
polity.d$polity.2[is.na(polity.d$polity.2)] <- 0 # missings in polity duplicate are 0
polity.d$cum.polity <- as.numeric(ave(polity.d$polity.2, polity.d$country, FUN=cumsum)) # cumsum of polity
polity.d <- polity.d[c("country", "year", "polity", "cum.polity")] # deletes unused vars.
polity.d$cum.polity <- ifelse(is.na(polity.d$polity) & polity.d$cum.polity==0, NA, polity.d$cum.polity) # condition 1: NAs for all missing Polity scores.
polity.d$cum.polity <- ifelse(is.na(polity.d$polity), NA, polity.d$cum.polity) # condition 2: NAs for all missing Cum Polity scores (before, they were mainting the last score, which wrongly assumes that countries keep their polity score.)
### Note: this assumes that for the year we don't have data, countries maintain their Polity scores, and then, as soon as we do have scores again, we can take the last measurement. 


## merging with df
# merging df and wdi datasets, created a new one called dataset
df <- merge(df, polity.d, by=c("country", "year"), all.x=T) 



# State Antiquity Merging
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(readxl)
state.ant.d <- read_excel("../Inequality_State_Capacities/Data/State_Antiquity/State_Antiquity_data.xlsx")
## drop NAs
state.ant.d = subset(state.ant.d, !is.na(`State hist 5`) | !is.na(`State hist 10`) | !is.na(`State hist 50`))
## change var names
colnames(state.ant.d)[3] <- "state.history.5"
colnames(state.ant.d)[4] <- "state.history.10"
colnames(state.ant.d)[5] <- "state.history.50"
## keeping what we need
state.ant.d <- state.ant.d[c("country", "state.history.5", "state.history.10", "state.history.50")]
## generating a data.table object
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(data.table)
state.ant.d = data.table(state.ant.d)
## merging
df <- merge(df, state.ant.d, by = "country", all = TRUE, sort = FALSE)




# regional dummies
# Load the census data
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(readstata13)

## Load Census data 
regions.d <- read.dta13("../Inequality_State_Capacities/Data/Census_Data_Hanson/Census_data_until_2015.dta") 

## recoding
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(car)
regions.d$region <- recode(as.factor(regions.d$region), 
                           "1 = 'South/Central America' ; 
                           2 = 'Middle East' ; 
                           3 = 'East Europe' ; 
                           4 = 'Africa' ; 
                           5 = 'Central Asia' ; 
                           6 = 'South East Asia' ;
                           7 = 'Europe/U.S.' ")
## keeping what we need
regions.d <- regions.d[c("country", "year", "region")]
## merging
df <- merge(df, regions.d, by=c("country", "year"), all.x=T)


# Merge Fraser dataset
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(readxl)
fraser.d <- read_excel("../Inequality_State_Capacities/Data/Fraser/fraser.xlsx", sheet = "dataR") 

colnames(fraser.d) <- paste("fraser", colnames(fraser.d), sep = "_") # adds prefix to identify the source data

names(fraser.d)[names(fraser.d) == 'fraser_year'] <- 'year' # renames
names(fraser.d)[names(fraser.d) == 'fraser_country'] <- 'country' # renames
names(fraser.d)[names(fraser.d) == 'fraser_ISO_Code'] <- 'ISO' # renames

names(fraser.d) <- gsub(x = names(fraser.d), pattern = " ", replacement = "_") # replace blank with _ in var names

df <- merge(df, fraser.d, by=c("country", "year"), all.x=T) ## merging


# Merge GCID
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(readxl)
gcid.d <- read_excel("../Inequality_State_Capacities/Data/GCID/GCID_2006_2016.xlsx", sheet = "dataR") 

gcid.d <- gcid.d[!names(gcid.d) %in% c("Table 1", "X__1", "v3") ] # dropping vars

gcid.d = data.frame(t(gcid.d)) # transposing df

gcid.d = data.frame(reshape(gcid.d, # reshaping df
                            varying = c("X2", "X3","X4","X5","X6","X7","X8","X9","X10","X11"), 
                            v.names = "gcid_index",
                            timevar = "year", 
                            times =c("2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016"), 
                            direction = "long")); rownames(gcid.d) <- NULL

names(gcid.d)[names(gcid.d) == 'X1'] <- 'country' # renames
gcid.d <- gcid.d[!names(gcid.d) %in% c("id") ] # dropping vars
gcid.d$year = as.factor(gcid.d$year)
options(digits=16) # sets precision
gcid.d$gcid_index = as.numeric(as.character(gcid.d$gcid_index)) # transforms character to numeric
df <- merge(df, gcid.d, by=c("country", "year"), all.x=T) # merging 


# Merging Asal_Conrad_Toronto
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(foreign)

## Load Asal Conrad Toronto data 
act.d <- read.dta("../Inequality_State_Capacities/Data/Asal_Conrad_Toronto/Asal_Conrad_Toronto.dta") 

## renaming with shorter variables -- use first 15 characters of the long variables, kill white spaces and special characters// Capitalizes first character // adds "act" to identify variables.
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(stringr)
names(act.d)[4:ncol(act.d)] <- paste("act.", str_replace_all(stri_trans_totitle(dput(as.character(substr(colnames(act.d[,4:ncol(act.d)]), start = 1, stop = 15)))), "[^[:alnum:]]", ""), sep="")

options(digits=16) # sets precision
df <- merge(df, act.d, by=c("ccode", "year"), all.x=T) # merging 


# Merging Good_For_The_Money
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(foreign)

## Load Good_For_The_Money
gfm.d <- read.dta("../Inequality_State_Capacities/Data/Good_For_The_Money/Good_For_The_Money.dta") 

gfm.d <- gfm.d[3:ncol(gfm.d)] # select columns we want

## renaming with shorter variables -- use first 15 characters of the long variables, kill white spaces and special characters// Capitalizes first character // adds "gfm" to identify variables.
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(stringr)
names(gfm.d)[3:ncol(gfm.d)] <- paste("gfm.", str_replace_all(stri_trans_totitle(dput(as.character(substr(colnames(gfm.d[3:ncol(gfm.d)]), start = 1, stop = 15)))), "[^[:alnum:]]", ""), sep="")

options(digits=16) # sets precision
df <- merge(df, gfm.d, by=c("ccode", "year"), all.x=T) # merging 



# The DF below HAS country-year duplicates


# financial system variables merge
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(foreign)

## load financial_system
financial.system.df <- read.dta("../Inequality_State_Capacities/Data/financial_system_variables/financial_system_variables_fixed.dta") 

## add prefix to identify the data.
colnames(financial.system.df) <- paste("financial_system", colnames(financial.system.df), sep = "_") # adds prefix to identify the source data

## rename before merging
names(financial.system.df)[names(financial.system.df) == 'financial_system_year'] <- 'year' # renames
names(financial.system.df)[names(financial.system.df) == 'financial_system_country'] <- 'country' # renames

## financial.system.df$country to character
financial.system.df$country = as.character(financial.system.df$country)

# dropping "country" variable
financial.system.df$financial_system_code <- NULL

## merging
df <- merge(df, financial.system.df, by=c("country", "year"), all.x=T, all.y=T) # merging 

# fix(df)
# colnames(df)

# Data Recoding
########################

# same DF but with our variables lagged and differenced BY COUNTRY

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(plm)

p.df <- pdata.frame(df, index = c("country", "year"), row.names = FALSE) # declare df as panel df. Panel is country.

p.df$lag.gini_disp.mean = lag(p.df$gini_disp.mean, k=1) 
p.df$lag.v2x_polyarchy = lag(p.df$v2x_polyarchy, k=1) 
p.df$lag.v2x_libdem = lag(p.df$v2x_libdem, k=1) 
p.df$lag.democracy = lag(p.df$democracy, k=1) 
p.df$lag.cum.census = lag(p.df$cum.census, k=1) 
p.df$lag.wdi.GdpPerCapita = lag(p.df$wdi.GdpPerCapita, k=1) 
p.df$lag.wdi.InflationGdp = lag(p.df$wdi.InflationGdp, k=1) 
p.df$lag.wdi.ForeignDirect = lag(p.df$wdi.ForeignDirect, k=1) 
p.df$lag.wdi.Manufacturing = lag(p.df$wdi.Manufacturing, k=1) 
p.df$lag.wdi.HealthExpendit = lag(p.df$wdi.HealthExpendit, k=1) 
p.df$lag.wdi.AgricultureVa = lag(p.df$wdi.AgricultureVa, k=1) 
p.df$lag.wdi.PopulationAges = lag(p.df$wdi.PopulationAges, k=1) 
p.df$lag.wdi.GovernmentExpe = lag(p.df$wdi.GovernmentExpe, k=1) 
p.df$lag.wdi.TaxesOnIncome = lag(p.df$wdi.TaxesOnIncome, k=1) 
p.df$lag.polity = lag(p.df$polity, k=1) 
p.df$lag.wdi.gdp = lag(p.df$wdi.gdp, k=1) 
p.df$lag.wdi.TradeOfGdp = lag(p.df$wdi.TradeOfGdp, k=1) 
p.df$lag.gini_mkt.mean = lag(p.df$gini_mkt.mean, k=1) 
p.df$lag.gini_rel_red.mean = lag(p.df$gini_rel_red.mean, k=1) 
p.df$lag.wdi.TaxRevenue = lag(p.df$wdi.TaxRevenue, k=1) 
p.df$lag.wdi.OilRentsOf = lag(p.df$wdi.OilRentsOf, k=1) 
p.df$lag.wdi.UrbanPopulatio = lag(p.df$wdi.UrbanPopulatio, k=1) 
p.df$lag.wdi.PopulationGrow = lag(p.df$wdi.PopulationGrow, k=1) 
p.df$lag.fraser_econ_fred_sum = lag(p.df$fraser_econ_fred_sum, k=1) 
p.df$lag.fraser_Protection_of_property_rights= lag(p.df$fraser_Protection_of_property_rights, k=1) 
p.df$lag.fraser_Integrity_of_the_legal_system= lag(p.df$fraser_Integrity_of_the_legal_system, k=1) 
p.df$lag.fraser_Legal_enforcement_of_contracts= lag(p.df$fraser_Legal_enforcement_of_contracts, k=1) 
p.df$lag.gcid_index= lag(p.df$gcid_index, k=1) 
p.df$lag.gcid_index= lag(p.df$gcid_index, k=1) 
p.df$lag.gfm.Cim1= lag(p.df$gfm.Cim1, k=1) 
p.df$lag.gfm.Iiavg1= lag(p.df$gfm.Iiavg1, k=1) 
p.df$lag.act.Thomrival= lag(p.df$act.Thomrival, k=1) 
p.df$lag.act.Inter= lag(p.df$act.Inter, k=1) 
p.df$lag.act.Intra= lag(p.df$act.Intra, k=1) 
p.df$lag.wdi.health.exp.gdp = lag(p.df$wdi.health.exp.gdp, k=1) 
p.df$lag.wdi.govt.exp.gdp = lag(p.df$wdi.govt.exp.gdp, k=1)
p.df$lag.financial_system_dbcba = lag(p.df$financial_system_dbcba, k=1)
p.df$lag.financial_system_llgdp = lag(p.df$financial_system_llgdp, k=1)                                   
p.df$lag.financial_system_cbgdp = lag(p.df$financial_system_cbgdp, k=1)                                     
p.df$lag.financial_system_dbagdp = lag(p.df$financial_system_dbagdp, k=1)                                    
p.df$lag.financial_system_ofagdp = lag(p.df$financial_system_ofagdp, k=1)                                  
p.df$lag.financial_system_pcrdbofgdp = lag(p.df$financial_system_pcrdbofgdp, k=1)                                
p.df$lag.financial_system_bdgdp = lag(p.df$financial_system_bdgdp, k=1) 
p.df$lag.financial_system_fdgdp = lag(p.df$financial_system_fdgdp, k=1)                                     
p.df$lag.financial_system_bcbd = lag(p.df$financial_system_bcbd, k=1)                                    
p.df$lag.financial_system_ll_usd = lag(p.df$financial_system_ll_usd, k=1)
p.df$lag.financial_system_stock = lag(p.df$financial_system_stock, k=1)
p.df$lag.democracy_duration = lag(p.df$democracy_duration, k=1)



# differentiating variables
p.df$diff.gini_disp.mean =  diff(p.df$gini_disp.mean, k=1)
p.df$diff.v2x_polyarchy =  diff(p.df$v2x_polyarchy, k=1)
p.df$diff.v2x_libdem =  diff(p.df$v2x_libdem, k=1)
p.df$diff.democracy =  diff(p.df$democracy, k=1)
p.df$diff.cum.census =  diff(p.df$cum.census, k=1)
p.df$diff.wdi.GdpPerCapita =  diff(p.df$wdi.GdpPerCapita, k=1)
p.df$diff.wdi.InflationGdp =  diff(p.df$wdi.InflationGdp, k=1)
p.df$diff.wdi.ForeignDirect =  diff(p.df$wdi.ForeignDirect, k=1)
p.df$diff.wdi.Manufacturing =  diff(p.df$wdi.Manufacturing, k=1)
p.df$diff.wdi.TaxesOnIncome =  diff(p.df$wdi.TaxesOnIncome, k=1)
p.df$diff.wdi.HealthExpendit =  diff(p.df$wdi.HealthExpendit, k=1)
p.df$diff.wdi.AgricultureVa =  diff(p.df$wdi.AgricultureVa, k=1)
p.df$diff.wdi.PopulationAges =  diff(p.df$wdi.PopulationAges, k=1)
p.df$diff.wdi.GovernmentExpe =  diff(p.df$wdi.GovernmentExpe, k=1)
p.df$diff.polity =  diff(p.df$polity, k=1)
p.df$diff.wdi.gdp =  diff(p.df$wdi.gdp, k=1)
p.df$diff.wdi.TradeOfGdp =  diff(p.df$wdi.TradeOfGdp, k=1)
p.df$diff.gini_mkt.mean =  diff(p.df$gini_mkt.mean, k=1)
p.df$diff.gini_rel_red.mean =  diff(p.df$gini_rel_red.mean, k=1)
p.df$diff.wdi.TaxRevenue =  diff(p.df$wdi.TaxRevenue, k=1)
p.df$diff.wdi.OilRentsOf =  diff(p.df$wdi.OilRentsOf, k=1)
p.df$diff.wdi.UrbanPopulatio =  diff(p.df$wdi.UrbanPopulatio, k=1)
p.df$diff.fraser_econ_fred_sum =  diff(p.df$fraser_econ_fred_sum, k=1)
p.df$diff.fraser_Protection_of_property_rights=  diff(p.df$fraser_Protection_of_property_rights, k=1)
p.df$diff.fraser_Integrity_of_the_legal_system=  diff(p.df$fraser_Integrity_of_the_legal_system, k=1)
p.df$diff.fraser_Legal_enforcement_of_contracts=  diff(p.df$fraser_Legal_enforcement_of_contracts, k=1)
p.df$diff.gcid_index =  diff(p.df$gcid_index, k=1)
p.df$diff.gfm.Cim1 = diff(p.df$gfm.Cim1, k=1)
p.df$diff.gfm.Iiavg1 =  diff(p.df$gfm.Iiavg1, k=1)
p.df$diff.wdi.health.exp.gdp =  diff(p.df$wdi.health.exp.gdp, k=1)
p.df$diff.wdi.govt.exp.gdp =  diff(p.df$wdi.govt.exp.gdp, k=1)
p.df$diff.financial_system_dbcba = diff(p.df$financial_system_dbcba, k=1)
p.df$diff.financial_system_llgdp = diff(p.df$financial_system_llgdp, k=1)                                   
p.df$diff.financial_system_cbgdp = diff(p.df$financial_system_cbgdp, k=1)                                     
p.df$diff.financial_system_dbagdp = diff(p.df$financial_system_dbagdp, k=1)                                    
p.df$diff.financial_system_ofagdp = diff(p.df$financial_system_ofagdp, k=1)                                  
p.df$diff.financial_system_pcrdbofgdp = diff(p.df$financial_system_pcrdbofgdp, k=1)                                
p.df$diff.financial_system_bdgdp = diff(p.df$financial_system_bdgdp, k=1) 
p.df$diff.financial_system_fdgdp = diff(p.df$financial_system_fdgdp, k=1)                                     
p.df$diff.financial_system_bcbd = diff(p.df$financial_system_bcbd, k=1)                                    
p.df$diff.financial_system_ll_usd = diff(p.df$financial_system_ll_usd, k=1)
p.df$diff.financial_system_stock = diff(p.df$financial_system_stock, k=1)
p.df$diff.democracy_duration = diff(p.df$democracy_duration, k=1)


#df <- data.frame(p.df) # convert panel df to regular df.
df <- p.df # convert panel df to regular df.


# for fixed effects
#df$country = as.factor(df$country)
# df$year = as.factor(df$year)


# dataset with country ISO codes (for graphs)
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(foreign)

## Load Good_For_The_Money
iso.ccode.d <- read.csv("../Inequality_State_Capacities/Data/ISO_ccode/iso_ccode.csv") 

iso.ccode.d <- iso.ccode.d[c("alpha.2", "alpha.3", "region.code", "name")]

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(stringr)
names(iso.ccode.d) <- paste("iso.", str_replace_all(stri_trans_totitle(dput(as.character(substr(colnames(iso.ccode.d), start = 1, stop = 15)))), "[^[:alnum:]]", ""), sep="")
names(iso.ccode.d)[names(iso.ccode.d) == 'iso.Name'] <- 'country' # renames

iso.ccode.d$country = as.character(iso.ccode.d$country)
iso.ccode.d$iso.Alpha2 = as.character(iso.ccode.d$iso.Alpha2)
iso.ccode.d$iso.Alpha3 = as.character(iso.ccode.d$iso.Alpha3)

df <- merge(df, iso.ccode.d, by=c("country"), all.x=T) # merging 


# save dataset
# save(df, file = "C:/Users/tmart/OneDrive/Other Research Projects/Inequality_State_Capacities/df.RData") # Mart
save(df, file = "../Inequality_State_Capacities/df.RData") # Hector

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(foreign)

# export df to STATA format
write.dta(df, 
          "../Inequality_State_Capacities/df.dta", 
          version = 7L,
          convert.dates = TRUE, tz = "GMT",
          convert.factors = c("labels", "string", "numeric", "codes")
)


load("../Inequality_State_Capacities/df.RData") 
## ---- 



########################
# TS Test
########################


########################
# All models
########################
cat("\014")
rm(list=ls())

# Regions are
# "Africa", "Central Asia", "East Europe", "Europe/U.S.", "Middle East", "South East Asia" #"South/Central America". // != means does NOT equal to, == means equals to


# load data
load("../Inequality_State_Capacities/df.RData") 




########################################
######  MAIN MODELS
########################################


## ---- main:models ----

# complete cases // this is to input numbers in the paper
## it uses the covariates in the benchmark model (model.1)
completeFun <- function(data, desiredCols) {
        completeVec <- complete.cases(data[, desiredCols])
        return(data[completeVec, ])
}

df.complete.model.1 = completeFun(df, c("iso.Alpha3", "diff.gini_disp.mean", "lag.gini_disp.mean", "lag.polity", "lag.cum.census", "lag.wdi.gdp", "diff.wdi.gdp", "lag.wdi.InflationGdp", "diff.wdi.InflationGdp", "lag.wdi.ForeignDirect", "diff.wdi.ForeignDirect", "lag.wdi.TradeOfGdp", "diff.wdi.TradeOfGdp", "lag.wdi.AgricultureVa", "diff.wdi.AgricultureVa", "lag.wdi.PopulationAges", "diff.wdi.PopulationAges", "lag.wdi.UrbanPopulatio", "diff.wdi.UrbanPopulatio", "country", "year")) # variables used in model.1


# to introduce right number of countries (N), and years. Only complete cases.
complete.n.country = length(unique(levels(df.complete.model.1$country)))
complete.n.years.max = max(as.numeric(as.character(df.complete.model.1$year)))
complete.n.years.min = min(as.numeric(as.character(df.complete.model.1$year)))

# Model 0
# model without interaction effects - shows that there is no significant relationship between Polity and inequality, which means that we replicate results form other studies, allowing to show that there is nothing weird about our dataset
options(scipen=9999999)
model.0 = lm(diff.gini_disp.mean ~ 
                     lag.gini_disp.mean +
                     lag.polity + lag.cum.census +
                     diff.polity + lag.cum.census +
                     lag.wdi.gdp + diff.wdi.gdp +
                     lag.wdi.InflationGdp + diff.wdi.InflationGdp + 
                     lag.wdi.ForeignDirect + diff.wdi.ForeignDirect +
                     lag.wdi.TradeOfGdp + diff.wdi.TradeOfGdp +
                     lag.wdi.AgricultureVa + diff.wdi.AgricultureVa +
                     lag.wdi.PopulationAges + diff.wdi.PopulationAges +
                     lag.wdi.UrbanPopulatio + diff.wdi.UrbanPopulatio + 
                     factor(country) +  
                     factor(year),
             data = df)

#summary(model.0)


# Model 1
options(scipen=9999999)
model.1 = lm(diff.gini_disp.mean ~ 
                     lag.gini_disp.mean +
                     lag.polity * lag.cum.census +
                     diff.polity * lag.cum.census +
                     lag.wdi.gdp + diff.wdi.gdp +
                     lag.wdi.InflationGdp + diff.wdi.InflationGdp + 
                     lag.wdi.ForeignDirect + diff.wdi.ForeignDirect +
                     lag.wdi.TradeOfGdp + diff.wdi.TradeOfGdp +
                     lag.wdi.AgricultureVa + diff.wdi.AgricultureVa +
                     lag.wdi.PopulationAges + diff.wdi.PopulationAges +
                     lag.wdi.UrbanPopulatio + diff.wdi.UrbanPopulatio + 
                     factor(country) +  
                     factor(year),
             data = df)



# summary(model.1)

# if (!require("pacman")) install.packages("pacman"); library(pacman) 
# p_load(DAMisc)
# DAintfun2(model.1, c("lag.polity", "lag.cum.census"), rug = T, hist = T, scale.hist=.7,
#           xlab = c("State Capacity", "Polity"), 
#           nclass = c(16,20),
#           border=NA,
#           ylab = c("Conditional Effect of Democracy", "Conditional Effect of State Capacity"))

# this below tells us whether the interaction term is significant at the 0.05 level or not.
significance.level.interaction.term.model.1= as.numeric(if (coef(summary(model.1))["lag.polity:lag.cum.census","Pr(>|t|)"] < 0.05) {
        0.05
} else {
        print("Error:Non-Significant")
}
)

# this below (building on Brambor et al 2006, eq. 13) constructs a dataset with the marginal effects (left panel), year and country. I'm using this to give more substantive meaning to the intepretation of the plot.
conditional.effects.df.left = data.frame(
        effect = coefficients(model.1)["lag.polity"]+(coefficients(model.1)["lag.polity:lag.cum.census"]*df.complete.model.1$lag.cum.census),
        year = df.complete.model.1$year, 
        country = df.complete.model.1$country,
        iso.Alpha3 = df.complete.model.1$iso.Alpha3,
        dem = df.complete.model.1$lag.polity,
        stat.cap = df.complete.model.1$lag.cum.census,
        ineq = df.complete.model.1$lag.gini_disp.mean
)


# this is just a subsection of conditional.effects.df.left, but with the highest dem and state capacity. their effect is positive, and trend of inequality increases.
conditional.effects.df.left.dem.high.state.high = data.frame(
        country = conditional.effects.df.left$country[conditional.effects.df.left$dem==max(conditional.effects.df.left$dem) & conditional.effects.df.left$stat.cap==max(conditional.effects.df.left$stat.cap)],
        year = conditional.effects.df.left$year[conditional.effects.df.left$dem==max(conditional.effects.df.left$dem) & conditional.effects.df.left$stat.cap==max(conditional.effects.df.left$stat.cap)],
        effect = conditional.effects.df.left$effect[conditional.effects.df.left$dem==max(conditional.effects.df.left$dem) & conditional.effects.df.left$stat.cap==max(conditional.effects.df.left$stat.cap)], 
        ineq = conditional.effects.df.left$ineq[conditional.effects.df.left$dem==max(conditional.effects.df.left$dem) & conditional.effects.df.left$stat.cap==max(conditional.effects.df.left$stat.cap)]
)



# this below (building on Brambor et al 2006, eq. 13) constructs a dataset with the marginal effects (right panel), year and country. I'm using this to give more substantive meaning to the intepretation of the plot.
conditional.effects.df.right = data.frame(
        effect = coefficients(model.1)["lag.cum.census"]+(coefficients(model.1)["lag.polity:lag.cum.census"]*df.complete.model.1$lag.polity),
        year = df.complete.model.1$year, 
        country = df.complete.model.1$country,
        dem = df.complete.model.1$lag.polity,
        stat.cap = df.complete.model.1$lag.cum.census,
        ineq = df.complete.model.1$lag.gini_disp.mean
)

# this is just a subsection of conditional.effects.df.right, but with the dem <= 3. 
conditional.effects.df.right.dem.low.state.low = data.frame(
        country = conditional.effects.df.right$country[conditional.effects.df.right$dem<=-3],
        year = conditional.effects.df.right$year[conditional.effects.df.right$dem<=-3],
        effect = conditional.effects.df.right$effect[conditional.effects.df.right$dem<=-3], 
        ineq = conditional.effects.df.right$ineq[conditional.effects.df.right$dem<=-3],
        dem = conditional.effects.df.right$dem[conditional.effects.df.right$dem<=-3],
        stat.cap = df.complete.model.1$lag.cum.census[conditional.effects.df.right$dem<=-3]
        
)





# Model 2 - baseline model, with V-dem Polyarchy
options(scipen=9999999)
model.2 = lm(diff.gini_disp.mean ~ 
                     lag.gini_disp.mean +
                     lag.v2x_polyarchy*lag.cum.census +
                     diff.v2x_polyarchy*lag.cum.census +
                     lag.wdi.gdp + diff.wdi.gdp +
                     lag.wdi.InflationGdp + diff.wdi.InflationGdp + 
                     lag.wdi.ForeignDirect + diff.wdi.ForeignDirect +
                     lag.wdi.TradeOfGdp + diff.wdi.TradeOfGdp +
                     lag.wdi.AgricultureVa + diff.wdi.AgricultureVa +
                     lag.wdi.PopulationAges + diff.wdi.PopulationAges +
                     lag.wdi.UrbanPopulatio + diff.wdi.UrbanPopulatio +
                     factor(country) +  
                     factor(year),
             data = df)

# summary(model.2)

# Model 3 - excluding US and Europe, Polity
options(scipen=9999999)
model.3 = lm(diff.gini_disp.mean ~ 
                     lag.gini_disp.mean + 
                     lag.polity*lag.cum.census +
                     diff.polity*lag.cum.census + 
                     lag.wdi.gdp + diff.wdi.gdp +
                     lag.wdi.InflationGdp + diff.wdi.InflationGdp +
                     lag.wdi.ForeignDirect + diff.wdi.ForeignDirect +
                     lag.wdi.TradeOfGdp + diff.wdi.TradeOfGdp +
                     lag.wdi.AgricultureVa + diff.wdi.AgricultureVa +
                     lag.wdi.PopulationAges + diff.wdi.PopulationAges +
                     lag.wdi.UrbanPopulatio + diff.wdi.UrbanPopulatio +
                     factor(country) +  
                     factor(year),
             subset(df, df$region != "Europe/U.S." ))

# summary(model.3)


# Model 4 - excluding US and Europe, with V-dem Polyarchy 
options(scipen=9999999)
model.4 = lm(diff.gini_disp.mean ~ 
                     lag.gini_disp.mean + 
                     lag.v2x_polyarchy*lag.cum.census +
                     diff.v2x_polyarchy*lag.cum.census +
                     lag.wdi.gdp + diff.wdi.gdp +
                     lag.wdi.InflationGdp + diff.wdi.InflationGdp +
                     lag.wdi.ForeignDirect + diff.wdi.ForeignDirect +
                     lag.wdi.TradeOfGdp + diff.wdi.TradeOfGdp +
                     lag.wdi.AgricultureVa + diff.wdi.AgricultureVa +
                     lag.wdi.PopulationAges + diff.wdi.PopulationAges + 
                     lag.wdi.UrbanPopulatio + diff.wdi.UrbanPopulatio +
                     factor(country) +  
                     factor(year),
             subset(df, df$region != "Europe/U.S." ))

# summary(model.4)


# Model 5 - baseline model, but 5-year census cycle countries excluded 
options(scipen=9999999)
model.5 = lm(diff.gini_disp.mean ~ 
                     lag.gini_disp.mean +                     
                     lag.polity*lag.cum.census + 
                     diff.polity*lag.cum.census + 
                     lag.wdi.gdp + diff.wdi.gdp +
                     lag.wdi.InflationGdp + diff.wdi.InflationGdp + 
                     lag.wdi.ForeignDirect + diff.wdi.ForeignDirect +
                     lag.wdi.TradeOfGdp + diff.wdi.TradeOfGdp +
                     lag.wdi.AgricultureVa + diff.wdi.AgricultureVa +
                     lag.wdi.PopulationAges + diff.wdi.PopulationAges +
                     lag.wdi.UrbanPopulatio + diff.wdi.UrbanPopulatio +
                     factor(country) +  
                     factor(year),
             subset(df, df$country != "Ireland" & df$country != "Denmark" & df$country != "New Zealand"  & df$country != "Australia" & df$country != "Canada" & df$country != "Germany" & df$country != "Turkey" & df$country != "South Korea" & df$country != "Japan" & df$country != "Mexico" & df$country != "Brazil"))

# summary(model.5)

# Model 6 - baseline model with V-dem Polyarchy, but 5-year census cycle countries excluded 
options(scipen=9999999)
model.6 = lm(diff.gini_disp.mean ~ 
                     lag.gini_disp.mean +                     
                     lag.v2x_polyarchy*lag.cum.census + 
                     diff.v2x_polyarchy*lag.cum.census + 
                     lag.wdi.gdp + diff.wdi.gdp +
                     lag.wdi.InflationGdp + diff.wdi.InflationGdp + 
                     lag.wdi.ForeignDirect + diff.wdi.ForeignDirect +
                     lag.wdi.TradeOfGdp + diff.wdi.TradeOfGdp +
                     lag.wdi.AgricultureVa + diff.wdi.AgricultureVa +
                     lag.wdi.PopulationAges + diff.wdi.PopulationAges +
                     lag.wdi.UrbanPopulatio + diff.wdi.UrbanPopulatio +
                     factor(country) +  
                     factor(year),
             subset(df, df$country != "Ireland" & df$country != "Denmark" & df$country != "New Zealand"  & df$country != "Australia" & df$country != "Canada" & df$country != "Germany" & df$country != "Turkey" & df$country != "South Korea" & df$country != "Japan" & df$country != "Mexico" & df$country != "Brazil"))
# summary(model.6)
## ---- 



## ---- long:run:effect:d ----
# long-run effects
## Based on Taking Time Seriously p. 191

lrm = (round(summary(model.1)$coefficients["lag.polity:lag.cum.census",1],5))/(round(summary(model.1)$coefficients["lag.gini_disp.mean",1],5)) # define long run multiplier
ecr = (round(summary(model.1)$coefficients["lag.gini_disp.mean",1],5)) # define error correction rate
long.run.effect.d = data.frame(
        'Periods' = c('t+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7', 't+8', 't+9', 't+10', 't+11', 't+12', 't+13', 't+14', 't+15', 't+16', 't+17', 't+18', 't+19', 't+20'),
        'Inequality'  = c((lrm*ecr)/1, (lrm*ecr)/2, (lrm*ecr)/3, (lrm*ecr)/4, (lrm*ecr)/5, (lrm*ecr)/6, (lrm*ecr)/7, (lrm*ecr)/8, (lrm*ecr)/9, (lrm*ecr)/10, (lrm*ecr)/11, (lrm*ecr)/12, (lrm*ecr)/13, (lrm*ecr)/14, (lrm*ecr)/15, (lrm*ecr)/16, (lrm*ecr)/17, (lrm*ecr)/18, (lrm*ecr)/19, (lrm*ecr)/20)
)

## t+x to factor, ordered
long.run.effect.d$Periods <- factor(long.run.effect.d$Periods, levels = long.run.effect.d$Periods[order(as.numeric(row.names(long.run.effect.d)))])


## ---- long:run:effect:p1 ----
# plot
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ggplot2)
long.run.effect.p = ggplot(long.run.effect.d, aes(Periods, Inequality, group = 1)) + 
        geom_line() + 
        geom_point() +
        theme_bw() +
        labs(title="") +
        theme(
                axis.text.y = element_text(size=7), 
                axis.text.x = element_text(size=7,angle = 45, hjust = 1), 
                axis.title.y = element_text(size=7), 
                axis.title.x = element_text(size=7), 
                legend.text=element_text(size=7), 
                legend.title=element_text(size=7),
                plot.title = element_text(size=7),
                legend.position="bottom")
## ----

## ---- long:run:effect:p2 ----
long.run.effect.p
long.run.effect.p.note <-   paste(
  "{\\bf Overtime Effect of Increases in State Capacity and Democracy on Inequality (Gini Points)}.",
  "\\\\\\hspace{\\textwidth}", 
  "{\\bf Note}: The figure shows that the long-run effect of higher state capacity and democracy levels, increases inequality overtime, dissipating at a rate dictated by the error correction rate $\\beta_{1} \\;=\\;", round(summary(model.1)$coefficients["lag.gini_disp.mean",1],5),paste("$", "Note", sep = ". "), "that given the long run multiplier, defined by $\\frac{\\rho}{\\beta_{1}} \\;=\\; \\frac{", round(summary(model.1)$coefficients["lag.polity:lag.cum.census",1],5), "}{", round(summary(model.1)$coefficients["lag.gini_disp.mean",1],5), "}$, inequality increases during the first periods, reequilibrating over future time periods.", 
  "\n")
## ----

## ---- main:models:table ----
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(texreg) 

options(scipen=99999, digits=5)

texreg( # use "screenreg" or "texreg" // BUT DO KEEP IT IN texreg FOR THE PAPER
        list(model.0, model.1, model.2, model.3, model.4, model.5, model.6), # list all the saved models here
        caption = "Error Correction Models: Long-term Effects of State Capacity and Regime Type on Economic Inequality.", 
        custom.coef.names = c(
          "Gini$_{t-1}$", 
          "Polity$_{t-1}$", 
          "Cesus$_{t-1}$", 
          "Polity$_{\\Delta}$", 
          "GDP$_{t-1}$", 
          "GDP$_{\\Delta}$", 
          "Inflation$_{t-1}$", 
          "Inflation$_{\\Delta}$", 
          "FDI$_{t-1}$", 
          "FDI$_{\\Delta}$", 
          "Trade$_{t-1}$", 
          "Trade$_{\\Delta}$", 
          "Agriculture$_{t-1}$", 
          "Agriculture$_{\\Delta}$", 
          "Pop. Age$_{t-1}$", 
          "Pop. Age$_{\\Delta}$", 
          "Urban Pop.$_{t-1}$", 
          "Urban Pop.$_{\\Delta}$", 
          "Polity$_{t-1}$\\;$\\times$\\;Census$_{t-1}$", 
          "Census$_{t-1}$\\;$\\times$\\;Polity$_{\\Delta}$", 
          "Polyarchy$_{t-1}$", 
          "Polyarchy$_{\\Delta}$", 
          "Polyarchy$_{t-1}$\\;$\\times$\\;Census$_{t-1}$", 
          "Census$_{t-1}$\\;$\\times$\\;Polyarchy$_{\\Delta}$"
          ),
        custom.model.names = c(
                "(0)",# Model Name
                "(1)",# Model Name
                "(2)", # Model Name
                "(3)", # Model Name
                "(4)", # Model Name
                "(5)", # Model Name
                "(6)" # Model Name
        ),
        label = "main:models:table",
        custom.note = "%stars. Year and country fixed effects in all models (not shown). Intercept was excluded from the table. All models are OLS.",
        scalebox = 0.7,
        #fontsize = "tiny",
        center = TRUE,
        use.packages = FALSE,
        dcolumn = TRUE,
        #booktabs = TRUE,
        omit.coef = "(factor)|((Intercept))",
        reorder.coef = c(19:20,23:24,2,4,21,22,3,5:18,1),
        digits = 5,
        stars = c(0.01, 0.05, 0.1),
        no.margin = TRUE,
        groups = list("{\\bf Lagged Dependent Variable}" = 24)
        )
## ---- 



########################################
######  ROBUSTNESS TESTS 
########################################

# load data
load("../Inequality_State_Capacities/df.RData") 


## ---- robustness:models ----
# Model 7 - baseline model, with Education spending (wdi.GovernmentExpe) added as control 
options(scipen=999)
model.7 = lm(diff.gini_disp.mean ~ 
                     lag.gini_disp.mean +
                     lag.polity*lag.cum.census + diff.polity*lag.cum.census +
                     lag.wdi.gdp + diff.wdi.gdp +
                     lag.wdi.InflationGdp + diff.wdi.InflationGdp + 
                     lag.wdi.ForeignDirect + diff.wdi.ForeignDirect +
                     lag.wdi.TradeOfGdp + diff.wdi.TradeOfGdp +
                     lag.wdi.AgricultureVa + diff.wdi.AgricultureVa +
                     lag.wdi.PopulationAges + diff.wdi.PopulationAges +
                     lag.wdi.GovernmentExpe + diff.wdi.GovernmentExpe +
                     lag.wdi.UrbanPopulatio + diff.wdi.UrbanPopulatio +
                     factor(country) +  
                     factor(year),
             data = df)

# summary(model.7)

# Model 8 - baseline model, with Government Health spending (wdi.health) added as control (but we dont have that variable yet - can you check lag.wdi.health seem to be operating as dummy variables here, and diff command didn't work for some reason // it is working now -- it's not a dummy variable, it's continuous. I added the lagged and diff versions. Both work.
options(scipen=999)
model.8 = lm(diff.gini_disp.mean ~ 
                     lag.gini_disp.mean +
                     lag.polity*lag.cum.census + diff.polity*lag.cum.census +
                     lag.wdi.gdp + diff.wdi.gdp +
                     lag.wdi.InflationGdp + diff.wdi.InflationGdp + 
                     lag.wdi.ForeignDirect + diff.wdi.ForeignDirect +
                     lag.wdi.TradeOfGdp + diff.wdi.TradeOfGdp +
                     lag.wdi.AgricultureVa + diff.wdi.AgricultureVa +
                     lag.wdi.PopulationAges + diff.wdi.PopulationAges + 
                     lag.wdi.health.exp.gdp + diff.wdi.health.exp.gdp +
                     lag.wdi.UrbanPopulatio + diff.wdi.UrbanPopulatio +
                     factor(country) +  
                     factor(year),
             data = df)

# summary(model.8)


# Model 9 - baseline model, with Income Taxes as % of GDP added as control (but we dont have that variable yet - can you check 
options(scipen=999)
model.9 = lm(diff.gini_disp.mean ~ 
               lag.gini_disp.mean +
               lag.polity*lag.cum.census + 
               diff.polity*lag.cum.census +
               lag.wdi.gdp + diff.wdi.gdp +
                     lag.wdi.InflationGdp + diff.wdi.InflationGdp + 
                     lag.wdi.ForeignDirect + diff.wdi.ForeignDirect +
                     lag.wdi.TradeOfGdp + diff.wdi.TradeOfGdp +
                     lag.wdi.AgricultureVa + diff.wdi.AgricultureVa +
                     lag.wdi.PopulationAges + diff.wdi.PopulationAges +
                     lag.wdi.TaxesOnIncome + diff.wdi.TaxesOnIncome + # fixed; weren't included before
                     lag.wdi.UrbanPopulatio +
                     factor(country) +  
                     factor(year),
             data = df)

# summary(model.9)


# Model 10 - baseline model (Model 1), but democracy_duration added as additional control variable (Wong 2015). 
## HB: I changed it to the lag and diff format we've been using (i.e. "lag.democracy_duration + diff.democracy_duration"). I added these two variables in line 439 and 488 (see commit b16374)
options(scipen=999)
model.10 = lm(diff.gini_disp.mean ~ 
                      lag.gini_disp.mean +
                      lag.polity*lag.cum.census + diff.polity*lag.cum.census +
                      lag.wdi.gdp + diff.wdi.gdp +
                      lag.wdi.InflationGdp + diff.wdi.InflationGdp + 
                      lag.wdi.ForeignDirect + diff.wdi.ForeignDirect +
                      lag.wdi.TradeOfGdp + diff.wdi.TradeOfGdp +
                      lag.wdi.AgricultureVa + diff.wdi.AgricultureVa +
                      lag.wdi.PopulationAges + diff.wdi.PopulationAges +
                      lag.wdi.UrbanPopulatio + diff.wdi.UrbanPopulatio +
                      lag.democracy_duration + diff.democracy_duration +
                      factor(country) +  
                      factor(year),
              data = df)

# summary(model.10)


# Model 11 - Model with State Antiquity as a measure of state capacity 
options(scipen=999)
model.11 = lm(diff.gini_disp.mean ~ 
                      lag.gini_disp.mean +
                      lag.polity*state.history.5 + diff.polity*state.history.5 +
                      lag.wdi.gdp + diff.wdi.gdp +
                      lag.wdi.InflationGdp + diff.wdi.InflationGdp + 
                      lag.wdi.ForeignDirect + diff.wdi.ForeignDirect +
                      lag.wdi.TradeOfGdp + diff.wdi.TradeOfGdp +
                      lag.wdi.AgricultureVa + diff.wdi.AgricultureVa +
                      lag.wdi.PopulationAges + diff.wdi.PopulationAges + 
                      lag.wdi.UrbanPopulatio + diff.wdi.UrbanPopulatio +
                      factor(country) +
                      factor(year), 
              data = df)

# summary(model.11)

# Model 12 - Model with Income taxes as a measure of state capacity 
options(scipen=999)
model.12 = lm(diff.gini_disp.mean ~ 
                      lag.gini_disp.mean +
                      lag.polity*lag.wdi.TaxesOnIncome + diff.polity*lag.wdi.TaxesOnIncome +
                      lag.wdi.gdp + diff.wdi.gdp +
                      lag.wdi.InflationGdp + diff.wdi.InflationGdp + 
                      lag.wdi.ForeignDirect + diff.wdi.ForeignDirect +
                      lag.wdi.TradeOfGdp + diff.wdi.TradeOfGdp +
                      lag.wdi.AgricultureVa + diff.wdi.AgricultureVa +
                      lag.wdi.PopulationAges + diff.wdi.PopulationAges +
                      lag.wdi.UrbanPopulatio + diff.wdi.UrbanPopulatio +
                      factor(country) + 
                      factor(year),
              data = df)

# summary(model.12)

# Model 13 - Model with Tax Revenue as a measure of state capacity. Insignificant, but has a positive sign, so I guess that works  
options(scipen=999)
model.13 = lm(diff.gini_disp.mean ~ 
                      lag.gini_disp.mean +
                      lag.polity*lag.wdi.TaxRevenue + diff.polity*lag.wdi.TaxRevenue +
                      lag.wdi.gdp + diff.wdi.gdp +
                      lag.wdi.InflationGdp + diff.wdi.InflationGdp + 
                      lag.wdi.ForeignDirect + diff.wdi.ForeignDirect +
                      lag.wdi.TradeOfGdp + diff.wdi.TradeOfGdp +
                      lag.wdi.PopulationAges + diff.wdi.PopulationAges +
                      lag.wdi.UrbanPopulatio + diff.wdi.UrbanPopulatio +
                      factor(country) +  
                      factor(year),
              data = df)

# summary(model.13)
## ---- 


## ---- robustness:models:table ----
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(texreg) 

options(scipen=99999, digits=5)

texreg( # use "screenreg" or "texreg" // BUT DO KEEP IT IN texreg FOR THE PAPER
        list(model.7, model.8, model.9, model.10, model.11, model.12, model.13), # list all the saved models here
        caption = "Error Correction Models (Robustness Checks): Long-term Effects of State Capacity and Regime Type on Economic Inequality.",
        # BELOW IS TO CUSTOMIZE THE TABLE ONCE WE HAVE MORE MODELS WORKOUT OUT
        custom.coef.names = c(
                "Gini$_{t-1}$",
                "Polity$_{t-1}$",
                "Census$_{t-1}$",
                "Polity$_{\\Delta}$",
                "GDP$_{t-1}$",
                "GDP$_{\\Delta}$",
                "Inflation$_{t-1}$",
                "Inflation$_{\\Delta}$",
                "FDI$_{t-1}$",
                "FDI$_{\\Delta}$",
                "Trade$_{t-1}$",
                "Trade$_{\\Delta}$",
                "Agriculture$_{t-1}$",
                "Agriculture$_{\\Delta}$",
                "Pop. Age$_{t-1}$",
                "Pop. Age$_{\\Delta}$",
                "Govt. Expenditure$_{t-1}$",
                "Govt. Expenditure$_{\\Delta}$",
                "Urban Pop$_{t-1}$",
                "Urban Pop$_{\\Delta}$",
                "Polity$_{t-1}$$\\;\\times\\;$Census$_{t-1}$",
                "Census$_{t-1}$$\\;\\times\\;$Polity$_{\\Delta}$",
                "Health Expenditure$_{t-1}$",
                "Health Expenditure$_{\\Delta}$",
                "Income Tax$_{t-1}$",
                "Income Tax$_{\\Delta}$",
                "Democracy Duration$_{t-1}$",
                "Democracy Duration$_{\\Delta}$",
                "State History (5)",
                "Polity$_{t-1}$$\\;\\times\\;$State History (5)$_{t-1}$",
                "State History (5)$_{t-1}$$\\;\\times\\;$Polity$_{\\Delta}$",
                "Polity$_{t-1}$$\\;\\times\\;$Income Tax$_{t-1}$",
                "Income Tax$_{t-1}$ $\\;\\times\\;$ Polity$_{\\Delta}$",
                "Tax Revenue$_{t-1}$",
                "Polity$_{t-1}$$\\;\\times\\;$Tax Revenue$_{t-1}$",
                "Tax Revenue$_{t-1}$$\\;\\times\\;$Polity$_{\\Delta}$"
                ),
        custom.model.names = c(
                "(7)",# Model Name
                "(8)", # Model Name
                "(9)", # Model Name
                "(10)", # Model Name
                "(11)", # Model Name
                "(12)", # Model Name
                "(13)"
        ),
        label = "robustness:models:table",
        custom.note = "%stars. Year and country fixed effects in all models. Intercepted was excluded from the table. All models OLS.",
        #scalebox = 0.5,
        fontsize = "tiny",
        center = TRUE,
        use.packages = FALSE,
        dcolumn = TRUE,
        booktabs = TRUE,
        omit.coef = "(factor)|((Intercept))",
        reorder.coef = c(21,22,30,31,32,33,35,36,29,34,3,2,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,23,24,25,26,27,28,1),
        digits = 5,
        stars = c(0.001, 0.01, 0.05, 0.1),
        no.margin = TRUE,
        groups = list("{\\bf Lagged Dependent Variable}" = 36)
        )
## ---- 



########################################
######  MECHANISMS
########################################


# load data
load("../Inequality_State_Capacities/df.RData") 


## ---- mechanisms:models ----
# Model 14 - Dem and state capacity interactive effect on impact on IAAVG
# interaction closely misses significance in (.12)
options(scipen=999)
model.14 = lm(diff.gfm.Iiavg1  ~  # Institutional Investor Credit Ratings
                      lag.gfm.Iiavg1 + # lagged DV
                lag.polity*lag.cum.census +
                diff.polity*lag.cum.census +
                lag.wdi.gdp + diff.wdi.gdp +
                lag.wdi.InflationGdp + diff.wdi.InflationGdp + 
                lag.wdi.TradeOfGdp + diff.wdi.TradeOfGdp +
                factor(country) +  
                factor(year),
              data = df)
#summary(model.14)

# Model 15 - Dem and state capacity interactive effect on impact on FDI
# interaction insignficant, but see Model 17
options(scipen=999)
model.15 = lm(diff.wdi.ForeignDirect ~ 
                      lag.wdi.ForeignDirect + # lagged DV
                lag.polity*lag.cum.census +
                diff.polity*lag.cum.census +
                lag.wdi.gdp + diff.wdi.gdp +
                lag.wdi.InflationGdp + diff.wdi.InflationGdp + 
                lag.wdi.TradeOfGdp + diff.wdi.TradeOfGdp +
                factor(country) +  
                factor(year),
              data = df)
#summary(model.15)




# Model 16 - Dem and state capacity interactive effect Private credit as GDP
options(scipen=999)
model.16 = lm(diff.financial_system_pcrdbofgdp ~  # Private Credit
                      lag.financial_system_pcrdbofgdp + # lagged DV
                lag.polity*lag.cum.census +
                diff.polity*lag.cum.census +
                lag.wdi.gdp + diff.wdi.gdp +
                lag.wdi.InflationGdp + diff.wdi.InflationGdp + 
                lag.wdi.TradeOfGdp + diff.wdi.TradeOfGdp +
                lag.wdi.ForeignDirect + diff.wdi.ForeignDirect +
                factor(country) +  
                factor(year),
              data = df)
#summary(model.16)

# Model 17 - IIAVG impact on FDI 
options(scipen=999)
model.17 = lm(diff.wdi.ForeignDirect ~ lag.wdi.ForeignDirect + 
                lag.gfm.Iiavg1 + diff.gfm.Iiavg1 +
                lag.wdi.gdp + diff.wdi.gdp +
                lag.wdi.InflationGdp + diff.wdi.InflationGdp + 
                lag.wdi.TradeOfGdp + diff.wdi.TradeOfGdp +
                factor(country),
              data = df)
#summary(model.17)


# Model 18 - IAAVG impact on Private credit as GDP
options(scipen=999)
model.18 = lm(diff.financial_system_pcrdbofgdp ~ # Private Credit
                lag.financial_system_pcrdbofgdp +
                lag.gfm.Iiavg1 +  diff.gfm.Iiavg1 +
                lag.wdi.gdp + diff.wdi.gdp +
                lag.wdi.InflationGdp + diff.wdi.InflationGdp + 
                lag.wdi.TradeOfGdp + diff.wdi.TradeOfGdp +
                factor(country) +  
                factor(year),
              data = df)
#summary(model.18)

# Model 19 - IIAVG impact on Inequality
# significant, but not robust to country FE
options(scipen=999)
model.19 = lm(diff.gini_disp.mean ~ 
                lag.gini_disp.mean +
                lag.gfm.Iiavg1 + diff.gfm.Iiavg1 +
                lag.wdi.gdp + diff.wdi.gdp +
                lag.wdi.InflationGdp + diff.wdi.InflationGdp + 
                lag.wdi.TradeOfGdp + diff.wdi.TradeOfGdp +
                lag.wdi.AgricultureVa + diff.wdi.AgricultureVa +
                lag.wdi.UrbanPopulatio + diff.wdi.UrbanPopulatio +
                #factor(country) +  
                factor(year),
              data = df)
#summary(model.19)


# Model 20- FDI impact to inequality model 
options(scipen=999)
model.20 = lm(diff.gini_disp.mean ~ 
                lag.gini_disp.mean +
                lag.wdi.ForeignDirect + diff.wdi.ForeignDirect +
                lag.wdi.gdp + diff.wdi.gdp +
                lag.wdi.InflationGdp + diff.wdi.InflationGdp + 
                lag.wdi.TradeOfGdp + diff.wdi.TradeOfGdp +
                lag.wdi.PopulationAges + diff.wdi.PopulationAges +
                lag.wdi.AgricultureVa + diff.wdi.AgricultureVa +
                lag.wdi.UrbanPopulatio + diff.wdi.UrbanPopulatio +
                factor(country) +  
                factor(year),
              data = df)
#summary(model.20)

# Model 21 - Private sector deposits as GDP and Inequality
options(scipen=9999999)
model.21 = lm(diff.gini_disp.mean ~ 
                      # lagged DV
                      lag.gini_disp.mean +
                      # main QI
                      lag.financial_system_pcrdbofgdp + diff.financial_system_pcrdbofgdp +
                      # control vars
                      lag.wdi.gdp + diff.wdi.gdp +
                      lag.wdi.InflationGdp + diff.wdi.InflationGdp + 
                      lag.wdi.ForeignDirect + diff.wdi.ForeignDirect +
                      lag.wdi.TradeOfGdp + diff.wdi.TradeOfGdp +
                      lag.wdi.PopulationAges + diff.wdi.PopulationAges +
                      lag.wdi.AgricultureVa + diff.wdi.AgricultureVa +
                      lag.wdi.UrbanPopulatio + diff.wdi.UrbanPopulatio +
                      factor(country) +  
                      factor(year),
              data = df)
#summary(model.21)
## ---- 




## ---- mechanisms:models:table ----
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(texreg) 
options(scipen=99999, digits=5)
texreg( # use "screenreg" or "texreg" // BUT DO KEEP IT IN texreg FOR THE PAPER
        list(model.14,model.15,model.16, model.17, model.18,model.19, model.20,model.21), # list all the saved models here
        caption = "Error Correction Models: Long-term Effects of Business Confidence on Economic Inequality.",
        # BELOW IS TO CUSTOMIZE THE TABLE ONCE WE HAVE MORE MODELS
        custom.coef.names = c(
                "Institutional Investor Credit Ratings$_{t-1}$", 
                "Polity$_{t-1}$", 
                "Census$_{t-1}$", 
                "Polity$_{\\Delta}$", 
                "GPD$_{t-1}$", 
                "GPD$_{\\Delta}$", 
                "Inflation$_{t-1}$", 
                "Inflation$_{\\Delta}$", 
                "Trade$_{t-1}$", 
                "Trade$_{\\Delta}$", 
                "Polity$_{t-1}$$\\;\\times\\;$Census$_{t-1}$", 
                "Census$_{t-1}$$\\;\\times\\;$Polity$_{\\Delta}$", 
                "FDI$_{t-1}$(only M15 y M17; covariate in M16)", 
                "Private Credit$_{t-1}$", 
                "FDI$_{\\Delta}$", 
                "Institutional Investor Credit Ratings$_{\\Delta}$", 
                "Gini$_{t-1}$", 
                "Agriculture$_{t-1}$", 
                "Agriculture$_{\\Delta}$", 
                "Urban Pop.$_{t-1}$", 
                "Urban Pop.$_{\\Delta}$", 
                "Pop. Age$_{t-1}$", 
                "Pop. Age$_{\\Delta}$", 
                "Private Credit$_{\\Delta}$"
                ),
        custom.model.names = c(
                "(14)", # Model Name
                "(15)", # Model Name
                "(16)",
                "(17)",
                "(18)",
                "(19)",
                "(20)",
                "(21)"
                ),
        label = "mechanisms:models:table",
        custom.note = "%stars. Year and country FE in all models. Intercept excluded. All models OLS.",
        scalebox = 0.6, # FYI: or scalebox or fontsize
        #fontsize = "tiny", # FYI: or scalebox or fontsize
        center = TRUE,
        use.packages = FALSE, # should stay FALSE
        dcolumn = TRUE,
        #booktabs = TRUE,
        omit.coef = "(factor)|((Intercept))",
        reorder.coef = c(11, 12, 16, 1, 14, 2, 4, 3, 15, 24, 5, 6, 7, 8, 9, 10, 18, 19, 20, 21, 22, 23, 13, 17),
        digits = 5,
        symbol = "\\cdot",
        stars = c(0.001, 0.01, 0.05, 0.1),
        no.margin = TRUE,
        groups = list("{\\bf Lagged Dependent Variable}" = 23:24)
        )
## ---- 



## ---- interaction:model:14:plot ----
library(gsl) # install.packages("gsl") # Source? N
library(DAMisc) # install.packages("DAMisc")



# plot
DAintfun2(model.14, c("lag.polity", "lag.cum.census"), rug = T, hist = T, scale.hist=.7,
          xlab = c("State Capacity\n(Cumulative Census)", "Democracy\n(Polity)"), 
          nclass = c(16,20),
          border=NA,
          ylab = c("Conditional Effect of Democracy", "Conditional Effect of State Capacity")
          #name.stem = c("cond_eff","cond_eff"), 
          #plot.type = "pdf"
)

## ---- 



## ---- interaction:model:14:plot:note ----
# note
interaction.model.14.plot.note <- paste(
        "{\\bf Causal Mechanisms: Conditional Effect of State Capacity and Democracy on Business Confidence (Fraser Institute's)}.",
        "\\\\\\hspace{\\textwidth}", 
        "{\\bf Note}: Following the advice of \\textcite{Brambor2006,Berry2012a}, the estimations of model 14 in \\autoref{mechanisms:models:table} were used to compute the conditional effects of the constitutive variables of the interaction term. The figure shows that as state capacity increases, the conditional effect of democracy on business confidence is positive but non-significant (left panel). The figure also shows that, as countries become more democratic, the conditional effect of state capacity on business confidence is positive and significant (right panel).",
        "\n")
## ---- 






## ---- interaction:model:15:plot ----
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(DAMisc)

# plot
DAintfun2(model.15, c("lag.polity", "lag.cum.census"), rug = T, hist = T, scale.hist=.7,
          xlab = c("State Capacity\n(Cumulative Census)", "Democracy\n(Polity)"), 
          nclass = c(16,20),
          border=NA,
          ylab = c("Conditional Effect of Democracy", "Conditional Effect of State Capacity")
          #name.stem = c("cond_eff","cond_eff"), 
          #plot.type = "pdf"
)

## ---- 






## ---- interaction:model:15:plot:note ----
# note
interaction.model.15.plot.note <- paste(
        "{\\bf Causal Mechanisms: Conditional Effect of State Capacity and Democracy on Business Confidence (CIM)}.",
        "\\\\\\hspace{\\textwidth}", 
        "{\\bf Note}: Following the advice of \\textcite{Brambor2006,Berry2012a}, the estimations of model 15 in \\autoref{mechanisms:models:table} were used to compute the conditional effects of the constitutive variables of the interaction term. The figure shows that as state capacity increases, the conditional effect of democracy on business confidence is positive and non-significant (left panel). The figure also shows that, as countries become more democratic, the conditional effect of state capacity on business confidence is positive and significant (right panel).",
        "\n")
## ---- 






########################
# Plots of Model 1 (baseline model) ONLY
########################


## ---- interaction:model:1:plot ----
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(DAMisc)

# plot
DAintfun2(model.1, c("lag.polity", "lag.cum.census"), rug = T, hist = T, scale.hist=.7,
          xlab = c("State Capacity\n(Cumulative Census)", "Democracy\n(Polity)"), 
          nclass = c(16,20),
          border=NA,
          ylab = c("Conditional Effect of Democracy", "Conditional Effect of State Capacity")
          #name.stem = c("cond_eff","cond_eff"), 
          #plot.type = "pdf"
          )


## ---- 


## ---- interaction:model:1:plot:note ----
# note
interaction.model.1.plot.note <- paste(
        "{\\bf Conditional Effect of State Capacity and Democracy on Inequality.}",
        "\\\\\\hspace{\\textwidth}", 
        "{\\bf Note}: Following the advice of \\textcite{Brambor2006,Berry2012a}, the estimations of model 1 in \\autoref{main:models:table} were used to compute the conditional effects of the constitutive variables of the interaction term. The figure shows that as state capacity increases, the conditional effect of democracy on inequality is positive and significant (left panel). The figure also shows that, as countries become more democratic, the conditional effect of state capacity on inequality is positive and significant too (right panel).",
        "\n")
## ---- 


# model.1
## effects library

# effect package does not handle factors in the variables (the ones we're using in the fixed effects specification), so I copied the df, and converted both vars into f.e., and entered them in the model without using "factor(variable)."
df.fact = df
df.fact$country.f = as.factor(df.fact$country)
df.fact$year.f = as.factor(df.fact$year)

# same as model.1
model.1.fact = lm(diff.gini_disp.mean ~ 
                          lag.gini_disp.mean +
                          lag.polity*lag.cum.census +
                          lag.wdi.gdp + diff.wdi.gdp +
                          lag.wdi.InflationGdp + diff.wdi.InflationGdp + 
                          lag.wdi.ForeignDirect + diff.wdi.ForeignDirect +
                          lag.wdi.TradeOfGdp + diff.wdi.TradeOfGdp +
                          lag.wdi.AgricultureVa + diff.wdi.AgricultureVa +
                          lag.wdi.PopulationAges + diff.wdi.PopulationAges +
                          lag.wdi.UrbanPopulatio + diff.wdi.UrbanPopulatio + 
                          country.f +   # but these are entered as factor without calling the factor function
                          year.f, # but these are entered as factor without calling the factor function
                  data = df.fact)

# effects
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(effects,ggplot2)
effects.model.1 <- effect("lag.polity*lag.cum.census", model.1.fact)
effects.model.1.d = data.frame(effects.model.1)

# effects
ggplot(effects.model.1.d, aes(lag.polity, fit))+
        geom_line(data=effects.model.1.d)+
        geom_ribbon(data=effects.model.1.d,aes(ymin=lower,ymax=upper),alpha=0.3) +
        facet_grid(. ~ lag.cum.census) +
        geom_hline(yintercept = 0, colour = "red", linetype = 2) +
        theme_bw() +
        xlab("Polity") + 
        ylab("Inequality") + 
        ggtitle("State Capacity (Cumulative Census)") +
        theme(plot.title = element_text(hjust = 0.5))



## rockchalk library
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(rockchalk) 
rockchalk.model.1 <- plotSlopes(model.1, "lag.polity", "lag.cum.census") 
testSlopes(rockchalk.model.1)

## DAMisc library
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(DAMisc)
DAintfun2(model.1, c("lag.polity", "lag.cum.census"), rug = T, hist = T, scale.hist=.3,
          xlab = c("State Capacity", "Polity"), 
          #nclass = 3,
          ylab = c("Polity | State Capacity", "State Capacity | Polity")
)

## DAMisc library: 3D Plot
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(rgl,rpanel) 
DAintfun(model.1, c("lag.polity", "lag.cum.census"),
         theta=-50, phi=5)

# Berry, Golder and Milton (2012)
BGMtest(model.1,c("lag.polity", "lag.cum.census"), digits = 7)




########################
# Interaction Plots Other Models
########################
dev.off();
# plot.new() ## clean up device


## Model 2
if (!require("pacman")) install.packages("pacman"); library(pacman)  
p_load(DAMisc) 

DAintfun2(
        model.2, c("lag.v2x_polyarchy", "lag.cum.census"), 
        rug = T, 
        hist = T, 
        scale.hist=.2, 
        xlab = c("", ""),  
        nclass = c(4,4), 
        ticksize = 0,
        border=NA, 
        ylab = c("", "")#, 
        #name.stem = c("cond_eff","cond_eff"), 
        #plot.type = "pdf" 
)
interaction.plot.model.2 <- recordPlot()
plot.new() ## clean up device



## Model 3
if (!require("pacman")) install.packages("pacman"); library(pacman)  
p_load(DAMisc) 

DAintfun2(model.3, c("lag.polity", "lag.cum.census"), 
          rug = T, 
          hist = T, 
          scale.hist=.2, 
          xlab = c("", ""),  
          nclass = c(4,4), 
          ticksize = 0,
          border=NA, 
          ylab = c("", "")#, 
          #name.stem = c("cond_eff","cond_eff"), 
          #plot.type = "pdf" 
)

interaction.plot.model.3 <- recordPlot()
plot.new() ## clean up device


## Model 4
if (!require("pacman")) install.packages("pacman"); library(pacman)  
p_load(DAMisc) 
DAintfun2(model.4, c("lag.v2x_polyarchy", "lag.cum.census"), 
          rug = T, 
          hist = T, 
          scale.hist=.2, 
          xlab = c("", ""),  
          nclass = c(4,4), 
          ticksize = 0,
          border=NA, 
          ylab = c("", "")#, 
          #name.stem = c("cond_eff","cond_eff"), 
          #plot.type = "pdf" 
)

interaction.plot.model.4 <- recordPlot()
plot.new() ## clean up device



## Model 5
if (!require("pacman")) install.packages("pacman"); library(pacman)  
p_load(DAMisc) 
DAintfun2(model.5, c("lag.polity", "lag.cum.census"),           
          rug = T, 
          hist = T, 
          scale.hist=.2, 
          xlab = c("", ""),  
          nclass = c(4,4), 
          ticksize = 0,
          border=NA, 
          ylab = c("", "")#, 
          #name.stem = c("cond_eff","cond_eff"), 
          #plot.type = "pdf" 
)


interaction.plot.model.5 <- recordPlot()
plot.new() ## clean up device



## Model 6
if (!require("pacman")) install.packages("pacman"); library(pacman)  
p_load(DAMisc) 
DAintfun2(model.6, c("lag.v2x_polyarchy", "lag.cum.census"), 
          rug = T, 
          hist = T, 
          scale.hist=.2, 
          xlab = c("", ""),  
          nclass = c(4,4), 
          ticksize = 0,
          border=NA, 
          ylab = c("", "")#, 
          #name.stem = c("cond_eff","cond_eff"), 
          #plot.type = "pdf" 
)


interaction.plot.model.6 <- recordPlot()
plot.new() ## clean up device




## Model 9
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(DAMisc)
DAintfun2(model.9, c("lag.polity", "lag.cum.census"), 
          rug = T, 
          hist = T, 
          scale.hist=.2, 
          xlab = c("", ""),  
          nclass = c(4,4), 
          ticksize = 0,
          border=NA, 
          ylab = c("", "")#, 
          #name.stem = c("cond_eff","cond_eff"), 
          #plot.type = "pdf" 
)

interaction.plot.model.9 <- recordPlot()
plot.new() ## clean up device



# merge all plots in one
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(cowplot,gridGraphics)

plot_grid(
        interaction.plot.model.2, 
        interaction.plot.model.3,  
        interaction.plot.model.4,
        interaction.plot.model.5,
        interaction.plot.model.6,
        interaction.plot.model.9,
        ncol = 3,
        nrow = 2,
        labels = "auto", scale = 0.8)



########################
# Plots of Different Measurements, to show robustness
########################
cat("\014")
rm(list=ls())


## ---- diff:measurements:state:plot:data ----
## load data
load("../Inequality_State_Capacities/df.RData") 


# get mean cum.census df
mean.cum.census.df = data.frame(
        aggregate(df$cum.census, 
                  by=list(Country=df$country), mean))

# get mean state.history.5 df
mean.state.history.5.df = data.frame(
        aggregate(df$state.history.5, 
                  by=list(Country=df$country), mean))

## bind cum census and state hist
mean.state.hist.and.cum.census.df = na.omit(data.frame(cbind(mean.cum.census.df[,2],mean.state.history.5.df[,2])))


## scale df
mean.state.hist.and.cum.census.df = as.data.frame(scale(mean.state.hist.and.cum.census.df))

## change col names
colnames(mean.state.hist.and.cum.census.df) <- c("cum.census", "state.hist.5")

## vector of country names
country.names.state.hist.and.cum.census.df = na.omit(data.frame(cbind(mean.cum.census.df,mean.state.history.5.df)))[,1]



## vector of regions by country
regions.names.state.hist.and.cum.census.df = na.omit(
        aggregate(df$state.history.5, by=list(Country=df$country, Region=df$region), mean))[,2]


## attach
mean.state.hist.and.cum.census.df$country <- country.names.state.hist.and.cum.census.df
#mean.state.hist.and.cum.census.df$Region <- regions.names.state.hist.and.cum.census.df

## change name
colnames(mean.state.hist.and.cum.census.df)

## Cum.Census with State Hist05 
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(ggplot2,ggrepel)

set.seed(1007)
diff.measurements.state.plot = ggplot(mean.state.hist.and.cum.census.df, aes(x=state.hist.5, y=cum.census)) +
        geom_point(fill = "red", color="black", size=1, shape=21, position = "jitter") +
        geom_text(label=mean.state.hist.and.cum.census.df$country, angle = 45, size=1, position = position_nudge(y = -0.15)) + 
        # geom_text_repel(aes(state.hist.5, cum.census, label = mean.state.hist.and.cum.census.df$country), point.padding = NA) + Too messy?
        xlab("State History (scaled)") +
        ylab("Cumulative Census (scaled)") +
        theme_bw() +
        labs(title="") +
        theme(
                axis.text.y = element_text(size=7), 
                axis.text.x = element_text(size=7), 
                axis.title.y = element_text(size=7), 
                axis.title.x = element_text(size=7), 
                legend.text=element_text(size=7), 
                legend.title=element_text(size=7),
                plot.title = element_text(size=7),
                legend.position="bottom")
## ----


## ---- diff:measurements:state:plot ----
diff.measurements.state.plot
diff.measurements.state.plot.note  <- paste(
        "{\\bf Title}",
        "\\\\\\hspace{\\textwidth}", 
        "{\\bf Note}: ",
        "\n")
## ----



########################
# Country Plots
########################
cat("\014")
rm(list=ls())

## load data
load("../Inequality_State_Capacities/df.RData") 


###############################
# Standarized Data
###############################


## Generate Standarized Version of Our Three Variables of Interest, plus their corresponding years and country
std.df = data.frame(
        gini_disp.mean = df$gini_disp.mean,
        v2x_polyarchy = df$v2x_polyarchy,
        polity = df$polity,
        cum.census = df$cum.census
)

## scale df.
std.df <- as.data.frame(scale(std.df))
## checking everything is normalized
# colMeans(std.df, na.rm = T)


## include country and year
std.df$country <- df$country
std.df$year <- df$year
std.df$region <- df$region


###############################
####Country Plot (South Africa)
###############################

## Getting data ready

## ---- south:africa:plot:data ----
## load data
load("../Inequality_State_Capacities/df.RData") 



## getting mean cum.census of developing world
cum.census.mean.dev.world = data.frame(aggregate(df$cum.census, by=list(Region=df$region, Year=as.numeric(levels(df$year))[df$year]), mean)); cum.census.mean.dev.world$Variable = "Cumulative Census"

## subsetting for developing world
cum.census.mean.dev.world <- cum.census.mean.dev.world[ which(
        cum.census.mean.dev.world$Region == "Africa" | 
                cum.census.mean.dev.world$Region == "Central Asia" |
                cum.census.mean.dev.world$Region == "South East Asia" |
                cum.census.mean.dev.world$Region == "Middle East" |
                cum.census.mean.dev.world$Region == "South/Central America"), ]

## taking the average of all developing regions.
cum.census.mean.dev.world = data.frame(aggregate(cum.census.mean.dev.world$x, by=list(Year=cum.census.mean.dev.world$Year), mean))

## drop values beyond 1980
cum.census.mean.dev.world <- cum.census.mean.dev.world[ which(cum.census.mean.dev.world$Year>=1980), ]

## now, include this to merge with the other df's
cum.census.mean.dev.world$Factor = "State Capacities"
colnames(cum.census.mean.dev.world)[2] = "Value"
colnames(cum.census.mean.dev.world)[1] = "Year"
cum.census.mean.dev.world$Year = as.numeric(as.character(cum.census.mean.dev.world$Year))
cum.census.mean.dev.world$Region = "DevelopingWorld"



## subsetting
df.south.africa = subset(df,country == "South Africa")

df.south.africa.state.cap <- df.south.africa[c("cum.census", "year")]
df.south.africa.state.cap$Factor = "State Capacities"
names(df.south.africa.state.cap)[names(df.south.africa.state.cap) == 'cum.census'] <- 'Value'
df.south.africa.state.cap$Region = "South Africa"

df.south.africa.dem <- df.south.africa[c("polity", "year")]
df.south.africa.dem$Factor = "Democracy"
names(df.south.africa.dem)[names(df.south.africa.dem) == 'polity'] <- 'Value'
df.south.africa.dem$Region = "South Africa"

df.south.africa.ineq <- df.south.africa[c("gini_disp.mean", "year")]
df.south.africa.ineq$Factor = "Inequality"
names(df.south.africa.ineq)[names(df.south.africa.ineq) == 'gini_disp.mean'] <- 'Value'
df.south.africa.ineq$Region = "South Africa"

## rbinding all three df's
df.south.africa = data.frame(rbind(df.south.africa.state.cap, df.south.africa.dem, df.south.africa.ineq))
df.south.africa$year = as.numeric(as.character(df.south.africa$year))

## Renaming
names(df.south.africa)[names(df.south.africa) == 'year'] <- 'Year'

## subsetting > 1980.  
df.south.africa <- df.south.africa[ which(as.numeric(as.character(df.south.africa$Year))>=1980), ]


### Plot
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(ggplot2)

south.africa.plot = ggplot(na.omit(df.south.africa), aes(x=Year, y=Value, group = Factor, colour = Factor)) + facet_wrap( ~Factor,  ncol = 1, scales = "free_y") +
        geom_line() + 
        geom_line(data=data.frame(cum.census.mean.dev.world,Factor="StateCapacities"),aes(Year,Value), colour= "brown", linetype = "dashed") +
        geom_vline(xintercept=1994, linetype="dashed", color = "grey56") +
        xlab("Year") +
        ylab("Value") +
        theme_bw() +
        scale_y_continuous(labels=function(x) sprintf("%.2f", x)) +
        labs(title="") +
        theme(
                axis.text.y = element_text(size=7), 
                axis.text.x = element_text(size=7, angle = 45, hjust=1), 
                axis.title.y = element_text(size=7), 
                axis.title.x = element_text(size=7), 
                legend.text=element_text(size=7), 
                legend.title=element_text(size=7),
                plot.title = element_text(size=7),
                legend.position="none") +
        geom_text(data = data.frame(label = c("Mean Developing World"),Factor   = c("State Capacities"),x= 2009,y= c(3.5)), mapping = aes(x = x, y = y, label = label), colour="brown")
## ----


## ---- south:africa:plot ----
south.africa.plot
south.africa.plot.note <- paste(
        "{\\bf Democracy, Inequality, and State Capacity in South Africa.}",
        "\\\\\\hspace{\\textwidth}", 
        "{\\bf Note}: This figure shows overtime levels of democracy, inequality, and state capacity in South Africa. For reference, it also includes mean levels of state capacity for all developing countries in our dataset.",
        "\n")
## ----


###############################
## By Region
###############################

## ---- cen:gini:polity:ts:plot:data ----
## load data
load("../Inequality_State_Capacities/df.RData") 

#### Getting Data Ready 

### Census Data
cum.census.by.region.plot.d = data.frame(aggregate(df$cum.census, by=list(Region=df$region, Year=as.numeric(levels(df$year))[df$year]), mean)); cum.census.by.region.plot.d$Variable = "Cumulative Census"

### Gini Data
gini.by.region.plot.d = data.frame(aggregate(df$gini_disp.mean[!is.na(df$gini_disp.mean)], by=list(Region=df$region[!is.na(df$gini_disp.mean)], Year=as.numeric(levels(df$year[!is.na(df$gini_disp.mean)]))[df$year[!is.na(df$gini_disp.mean)]]),mean)); gini.by.region.plot.d$Variable = "Gini"; #gini.by.region.plot.d$x = (gini.by.region.plot.d$x)*10

### Polity Data
polity.by.region.plot.d = data.frame(aggregate(df$polity[!is.na(df$polity)], by=list(Region=df$region[!is.na(df$polity)], Year=as.numeric(levels(df$year[!is.na(df$polity)]))[df$year[!is.na(df$polity)]]), mean)); polity.by.region.plot.d$Variable = "Polity"

### CBinding the three DFs
region.plot.d = rbind(cum.census.by.region.plot.d, gini.by.region.plot.d, polity.by.region.plot.d)

### Plot
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(ggplot2)
cen.gini.polity.ts.plot = ggplot(region.plot.d, aes(Year, x, colour = Region)) + facet_wrap(~Variable,  ncol = 3, scales = "free") +
        geom_line() + 
        xlab("Year") +
        ylab("Cumulative State Capacities") +
        theme_bw() +
        labs(title="") +
        theme(
                axis.text.y = element_text(size=7), 
                axis.text.x = element_text(size=7), 
                axis.title.y = element_text(size=7), 
                axis.title.x = element_text(size=7), 
                legend.text=element_text(size=7), 
                legend.title=element_text(size=7),
                plot.title = element_text(size=7),
                legend.position="bottom")
## ----


## ---- cen:gini:polity:ts:plot ----
cen.gini.polity.ts.plot
cen.gini.polity.ts.plot.note <- paste(
        "{\\bf Regional Levels of Cumulative State Capacity, Inequality, and Democracy.}",
        "\\\\\\hspace{\\textwidth}", 
        "{\\bf Note}: The figure shows different trends of state capacity, inequality and democracy broken by different regions.",
        "\n")
## ----
