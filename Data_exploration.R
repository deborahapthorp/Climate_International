library(googlesheets4)
library(tidyverse)
library(ggplot2)
library(janitor)
library(psych)
library(ggrain)
library(raincloudplots)
library(afex)
library(emmeans)

## Load data from Google Sheets and clean names

climateData_HUN <- read_sheet("https://docs.google.com/spreadsheets/d/1trLrX54D4PD9pJP6dS-1YaYVDMRDlhRQ_1TrmuhYJaQ/edit?gid=1733083915#gid=1733083915", sheet="HUN-english")

climateData_HUN <- clean_names(climateData_HUN)

climateData_CL <- read_sheet("https://docs.google.com/spreadsheets/d/1trLrX54D4PD9pJP6dS-1YaYVDMRDlhRQ_1TrmuhYJaQ/edit?gid=1733083915#gid=1733083915", sheet="CL-english")

climateData_CL <- clean_names(climateData_CL)

climateData_CL <- climateData_CL %>%
    mutate(across(dass1:dass21, ~  . - 1)) # correct DASS scoring 

climateData_CL <- climateData_CL %>%
  mutate(across(dass_mean:dass_stress, ~  . - 1)) # correct DASS scoring 


climateData_AUS_2020 <- read_sheet("https://docs.google.com/spreadsheets/d/1trLrX54D4PD9pJP6dS-1YaYVDMRDlhRQ_1TrmuhYJaQ/edit?gid=1733083915#gid=1733083915", sheet="Australia-english 2020")

climateData_AUS_2020 <- clean_names(climateData_AUS_2020)

climateData_AUS_2022 <- read_sheet("https://docs.google.com/spreadsheets/d/1trLrX54D4PD9pJP6dS-1YaYVDMRDlhRQ_1TrmuhYJaQ/edit?gid=1733083915#gid=1733083915", sheet="Australia-english 2022")

climateData_AUS_2022 <- clean_names(climateData_AUS_2022)



hist(climateData_AUS_2020$age)

climateData_HUN<- climateData_HUN %>% 
  filter(age < 26)

hist(climateData_HUN$age)

climateData_CL<- climateData_CL %>% 
  filter(age < 26)

hist(climateData_CL$age)


# Create a data frame for occupation
occupation_data <- as.data.frame(table(climateData_HUN$occupation))
colnames(occupation_data ) <- c("occupation", "count")

p1 <- ggplot(occupation_data, aes(x = occupation, y = count, fill = occupation)) +
  geom_bar(stat = "identity") +
  labs(y = "Number of participants", title = "Distribution by Occupation - Hungary") +
  theme_classic()

p1

occupation_data_CL <- as.data.frame(table(climateData_CL$occupation))
colnames(occupation_data_CL ) <- c("occupation", "count")

p2 <- ggplot(occupation_data_CL, aes(x = occupation, y = count, fill = occupation)) +
  geom_bar(stat = "identity") +
  labs(y = "Number of participants", title = "Distribution by Occupation - Chile") +
  theme_classic()

p2


# Score CCAS for Chilean sample (scores were recorded as integers in the sheet)
# To do: Tidy this up into a function

keys_ccas <-  rep(1, 22)
keys_ccas_cog <- rep(1,8)
keys_ccas_fun <- rep(1,5)
keys_ccas_exp <- rep(1,3)
keys_ccas_beh <- rep(1,6)

ccas_CL <- select(climateData_CL , ccas1:ccas22) #each number refers to the column
ccas_CL_scores <- scoreItems(keys_ccas, ccas_CL)

ccas_CL_cognitive <- select(climateData_CL , ccas1:ccas8) #each number refers to the column
ccas_CL_cognitive_scores <- scoreItems(keys_ccas_cog, ccas_CL_cognitive)

ccas_CL_functional <- select(climateData_CL , ccas9:ccas13) #each number refers to the column
ccas_CL_functional_scores <- scoreItems(keys_ccas_fun, ccas_CL_functional)

ccas_CL_experience <- select(climateData_CL , ccas14:ccas16) #each number refers to the column
ccas_CL_experience_scores <- scoreItems(keys_ccas_exp, ccas_CL_experience)

ccas_CL_beh <- select(climateData_CL , ccas17:ccas22) #each number refers to the column
ccas_CL_beh_scores <- scoreItems(keys_ccas_beh, ccas_CL_beh)

## AU 2022 data
ccas_AU_22 <- select(climateData_AUS_2022 , ccas1:ccas22) #each number refers to the column
ccas_AU_22_scores <- scoreItems(keys_ccas, ccas_AU_22)

ccas_AU_22_cognitive <- select(climateData_AUS_2022 , ccas1:ccas8) #each number refers to the column
ccas_AU_22_cognitive_scores <- scoreItems(keys_ccas_cog, ccas_AU_22_cognitive)

ccas_AU_22_functional <- select(climateData_AUS_2022 , ccas9:ccas13) #each number refers to the column
ccas_AU_22_functional_scores <- scoreItems(keys_ccas_fun, ccas_AU_22_functional)

ccas_AU_22_experience <- select(climateData_AUS_2022 , ccas14:ccas16) #each number refers to the column
ccas_AU_22_experience_scores <- scoreItems(keys_ccas_exp, ccas_AU_22_experience)

ccas_AU_22_beh <- select(climateData_AUS_2022 , ccas17:ccas22) #each number refers to the column
ccas_AU_22_beh_scores <- scoreItems(keys_ccas_beh, ccas_AU_22_beh)


#### Add revised means to CL dataset and check by plotting
climateData_CL$ccas_cognitive <- ccas_CL_cognitive_scores$scores[,1]
hist(climateData_CL$ccas_cognitive)

climateData_CL$ccas_functional <- ccas_CL_functional_scores$scores[,1]
hist(climateData_CL$ccas_functional)

climateData_CL$ccas_experience <- ccas_CL_experience_scores$scores[,1]
hist(climateData_CL$ccas_experience)

climateData_CL$ccas_peb <- ccas_CL_beh_scores$scores[,1]
hist(climateData_CL$ccas_peb)

climateData_CL$ccas_mean <- ccas_CL_scores$scores[,1]
hist(climateData_CL$ccas_mean)

#### Add CCAS subscale scores to AU dataset and check by plotting

climateData_AUS_2022$ccas_cognitive <- ccas_AU_22_cognitive_scores$scores[,1]
hist(climateData_AUS_2022$ccas_cognitive)

climateData_AUS_2022$ccas_functional <- ccas_AU_22_functional_scores$scores[,1]
hist(climateData_AUS_2022$ccas_functional)

climateData_AUS_2022$ccas_experience <- ccas_AU_22_experience_scores$scores[,1]
hist(climateData_AUS_2022$ccas_experience)

climateData_AUS_2022$ccas_peb <- ccas_AU_22_beh_scores$scores[,1]
hist(climateData_AUS_2022$ccas_peb)

###### Compare to other countries' data

hist(climateData_AUS_2022$ccas_mean)

hist(climateData_HUN$ccas_mean)

hist(climateData_CL$ccas_mean)


### Psychological Distance Scale (subscales)

## Geographical distance

keys_pds_geo <- c(1, 1, 1, 1, -1, -1) # geographic distance - items 1:6
keys_pds_temp <- c(-1, -1, -1, 1, -1, -1, 1, 1) # temporal distance - items 7:14
keys_pds_soc <- c(-1, -1, -1, 1, 1) # social distance - items 15:19
keys_pds_unc <- c(1, 1, 1, -1, 1, -1, 1) # uncertainty - items 20:26


pds_HUN_geo <- select(climateData_HUN , pds1:pds5, pds6) #each number refers to the column
pds_HUN_geo_scores <- scoreItems(keys_pds_geo, pds_HUN_geo)
climateData_HUN$pds_geo <- pds_HUN_geo_scores$scores[,1]

## Chile only has 1,2, 3 and 5
#pds_CL_geo <- select(climateData_CL , pds1:pds5, pds6) #each number refers to the column
#pds_CL_geo_scores <- scoreItems(keys_pds_geo, pds_CL_geo)
#climateData_CL$pds_geo <- pds_CL_geo_scores$scores[,1]

pds_AU_20_geo <- select(climateData_AUS_2020 , pds1:pds5, pds6) #each number refers to the column
pds_AU_20_geo_scores <- scoreItems(keys_pds_geo, pds_AU_20_geo)
climateData_AUS_2020$pds_geo <- pds_AU_20_geo_scores$scores[,1]


pds_AU_22_geo <- select(climateData_AUS_2022 , pds1:pds5, pds6) #each number refers to the column
pds_AU_22_geo_scores <- scoreItems(keys_pds_geo, pds_AU_22_geo)
climateData_AUS_2022$pds_geo <- pds_AU_22_geo_scores$scores[,1]


## Temporal distance

###  Hungary
pds_HUN_temp <- select(climateData_HUN , pds7, pds8, pds9, pds10, pds11, pds12, pds13, pds14)
pds_HUN_temp_scores <- scoreItems(keys_pds_temp, pds_HUN_temp)
climateData_HUN$pds_temp <- pds_HUN_temp_scores$scores[,1]


### Chile
## Chile only has 7,8, 9 and 14


### Australia 2020

pds_AU_20_temp <- select(climateData_AUS_2020 , pds7, pds8, pds9, pds10, pds11, pds12, pds13, pds14)
pds_AU_20_temp_scores <- scoreItems(keys_pds_temp, pds_AU_20_temp)
climateData_AUS_2020$pds_temp <- pds_AU_20_temp_scores$scores[,1]

### Australia 2022

pds_AU_22_temp <- select(climateData_AUS_2022 , pds7, pds8, pds9, pds10, pds11, pds12, pds13, pds14)
pds_AU_22_temp_scores <- scoreItems(keys_pds_temp, pds_AU_22_temp)
climateData_AUS_2022$pds_temp <- pds_AU_22_temp_scores$scores[,1]


## Nature relatedness scale (NRS-6)


keys_nrs <-  rep(1, 6)

nrs_CL <- select(climateData_CL , nrs1:nrs6) #each number refers to the column
nrs_CL_scores <- scoreItems(keys_nrs, nrs_CL)
nrs_CL_alpha <- nrs_CL_scores$alpha[,1]

climateData_CL$nrs_mean <- nrs_CL_scores$scores[,1] # dataset already has these, but now we've scored everything properly

nrs_HUN <- select(climateData_HUN , nrs1:nrs6) #each number refers to the column
nrs_HUN_scores <- scoreItems(keys_nrs, nrs_HUN)
nrs_HUN_alpha <- nrs_HUN_scores$alpha[,1]

climateData_HUN$nrs_mean <- nrs_HUN_scores$scores[,1] # dataset already has these, but now we've scored everything properly

nrs_AU_20 <- select(climateData_AUS_2020 , nrs1:nrs6) #each number refers to the column
nrs_AU_20_scores <- scoreItems(keys_nrs, nrs_AU_20)
nrs_AU_20_alpha <- nrs_AU_20_scores$alpha[,1]

climateData_AUS_2020$nrs_mean <- nrs_AU_20_scores$scores[,1] # dataset already has these, but now we've scored everything properly

## Brief Resilience Scale (BRS) for Australian datasets

keys_brs <-  c(1, -1, 1, -1,-1)
brs_AU_20 <- select(climateData_AUS_2020 , brs1, brs2, brs3, brs4, brs6) #each number refers to the column
brs_AU_20_scores <- scoreItems(keys_brs, brs_AU_20)
brs_AU_20_alpha <- brs_AU_20_scores$alpha[,1]

climateData_AUS_2020$brs_mean <- brs_AU_20_scores$scores[,1]

brs_AU_22 <- select(climateData_AUS_2022 , brs1, brs2, brs3, brs4, brs6) #each number refers to the column
brs_AU_22_scores <- scoreItems(keys_brs, brs_AU_22)
brs_AU_22_alpha <- brs_AU_22_scores$alpha[,1]

climateData_AUS_2022$brs_mean <- brs_AU_22_scores$scores[,1]

## Combine datasets for analyses 

hun_selected <- select(climateData_HUN, age, gender, dass_mean, ccas_mean, ccas_cognitive, ccas_functional, ccas_experience, ccas_peb, brs_mean)

cl_selected <- select(climateData_CL, age, gender, dass_mean, ccas_mean, ccas_cognitive, ccas_functional, ccas_experience, ccas_peb, brs_mean)

au_22_selected<- select(climateData_AUS_2022, age, gender, dass_mean, ccas_mean, ccas_cognitive, ccas_functional, ccas_experience, ccas_peb, brs_mean)


hun_selected$country <- "hungary"
cl_selected$country <- "chile"
au_22_selected$country <- "australia_22"

all_data <- rbind(hun_selected, cl_selected, au_22_selected)


###
# Add IDs to the data as they don't seem to have been included and are needed for various statistics
# Function from https://samfirke.com/2018/08/22/generating-unique-ids-using-r/

create_unique_ids <- function(n, seed_no = 1, char_len = 10){
  set.seed(seed_no)
  pool <- c(letters, LETTERS, 0:9)
  
  res <- character(n) # pre-allocating vector is much faster than growing it
  for(i in seq(n)){
    this_res <- paste0(sample(pool, char_len, replace = TRUE), collapse = "")
    while(this_res %in% res){ # if there was a duplicate, redo
      this_res <- paste0(sample(pool, char_len, replace = TRUE), collapse = "")
    }
    res[i] <- this_res
  }
  res
}

all_data$id <- create_unique_ids(nrow(all_data), seed = 23)

all_data$age_c <- all_data$age - mean(all_data$age, na.rm = TRUE)

all_data$dass_mean<- suppressWarnings(as.numeric(all_data$dass_mean))

###
## Plot mean scores on CCAS and DASS

ccas_mean_plot <- ggplot(all_data, aes(x = country, y = ccas_mean, fill = 	country)) +
  geom_rain(rain.side = 'l', alpha = .7) + scale_fill_brewer(palette = 'Set1', guide = 'none')+ylab('CCAS mean score') + theme_classic()

dass_mean_plot <- ggplot(all_data, aes(x = country, y = dass_mean, fill = 	country)) +
  geom_rain(rain.side = 'l', alpha = .7) + scale_fill_brewer(palette = 'Set1', guide = 'none')+ylab('DASS mean score') + theme_classic()

ccas_mean_plot

# I think this shows that the DASS scores in the Chile dataset are calculated wrong! (Fixed now)
dass_mean_plot

## ANOVA for CCAS mean

ccas_res<- aov_ez(id = "id", dv = "ccas_mean",
              between = c("country"),
              covariate = c("age_c"), factorize = FALSE,
              data = all_data)

ccas_res

emmeans(ccas_res, "country", contr = "pairwise")


dass_res<- aov_ez(id = "id", dv = "dass_mean",
                  between = c("country"),
                  covariate = c("age_c"), factorize = FALSE,
                  data = all_data)

dass_res

emmeans(dass_res, "country", contr = "pairwise")