#########################################################
# Poisson Model
# Implementation in STAN
# Code for the manuscript:
# Modelling COVID-19 With More Disaggregation and Less Nomothetic Parameterisation: UK and India Examples
# Olsen et al.
#########################################################
# Authors:
# Jihye Kim
# Social Statistics Department, UOM


library("tidyverse")
library("readstata13")


#########download data#########
# not available on GitHub
data0 <- read.csv("death_data_1806.csv")
data1 <- read.dta13("couple.dta")
data_member0 <- read.dta13("members.12Jun.dta")
pop0 <- read.csv("population_1806.csv")
age <- read.csv("census_age.csv")
colnames(age) <- c("district_no", "pop_65plus", "pop_65plus_f", "pop_65plus_m")
mig <- read.csv("census_migration.csv")
colnames(mig) <- c("district_no", "pop_mig")

#########data clean##############
data.couple <- data1 %>% mutate(age.group=as.factor(recode(age_category4, `1`="age1519",`2`="age2029",`3`="age2029",`4`="age3039",`5`="age3039",`6`="age4049",`7`="age4049",`8`="age5059")), 
  urban=as.numeric(urbanrural=="urban"), 
  bmi_f=as.numeric(replace(bmi_women, bmi_women==9998, NA))/100,
  sc=as.numeric(scheduledcaste=="schedule caste"), 
  st=as.numeric(scheduledcaste=="schedule tribe"),
  kilograms1=(as.numeric(replace(kilograms, kilograms>9990, NA)))/10,
  underweight1=as.numeric(bmi<1800),
  obesity1=as.numeric(bmi>=3000), 
  smoke=as.numeric(smoke_non=="no"),
  migration=as.numeric(awayfromhome_12mths=="yes"),
  urban=as.numeric(urbanrural=="urban"),
  bp=as.numeric(bp_diagnosed=="yes"), 
  diabetes=as.numeric(diabetes=="yes"), 
  asthma=as.numeric(asthma=="yes"),
  heart=as.numeric(heart=="yes"),
  cancer=as.numeric(cancer=="yes"),
  q1=as.numeric(wealth=="poorest"),
  q2=as.numeric(wealth=="poorer"),
  q4=as.numeric(wealth=="richer"),
  q5=as.numeric(wealth=="richest"),
  district_no=as.numeric(district), state_no=as.numeric(state))

data.couple$sc[is.na(data.couple$sc)] <- 0
data.couple$st[is.na(data.couple$st)] <- 0
data.couple$bp[is.na(data.couple$bp)] <- 0
data.couple$age.group <- as.numeric(data.couple$age.group)-1
data.couple$migration[is.na(data.couple$migration)] <- 0

newdata_total <- data.couple %>% filter(!is.na(obesity1) & bmi!=9998) %>%  
  dplyr::select(underweight1, obesity1, female, age.group, urban, state, sc, st, district, weight, smoke, bp, diabetes, asthma, district_no, state_no, heart, cancer, health, q1,q2,q4,q5, migration)
summary(newdata_total)
length(unique(newdata_total$district_no))


#########individual ############################

pop <- pop0 %>% mutate(pop=TOT_P_2020)
pop <- subset(pop, select=c(district_no, pop, growth))

df0 <- data0 %>% mutate(state=ï..state)%>%
  group_by(state, state_no, district, district_no) %>% 
  summarise_at(vars(cases, deaths), sum)
df0$district_id <- 1:345

newdata <- merge(df0, newdata_total, by= "district_no", all = TRUE)
newdata <- newdata %>% filter(!is.na(cases))
newdata <- merge(pop, newdata, by= "district_no")
newdata <- merge(age, newdata, by= "district_no")
newdata <- merge(mig, newdata, by= "district_no")

newdata$migration2 <- newdata$pop_mig/newdata$pop
newdata$age65up <- newdata$pop_65plus/newdata$pop
newdata$pop_10000 <- round(newdata$pop/10000, 0)

######### 2020 population adjusted fwt##########

newdata$fwt <- round(newdata$weight*newdata$growth/mean(newdata$weight*newdata$growth), digits=0)
newdata$fwt <- replace(newdata$fwt, newdata$fwt==0, 1)

######### 2020 weighted obesity ################
data_member1 <- data_member0  %>% 
  mutate(bmi=as.numeric(replace(bmi, bmi==9998, NA))/100,
  district_no=as.numeric(district), 
  state_no=as.numeric(state))

data_member1$obesity <- as.numeric(data_member1$bmi >=30)
data_member1$underweight <- as.numeric(data_member1$bmi <18)

data_member <- data_member1 %>% filter(!is.na(obesity)) %>% 
  dplyr::select(weight, obesity, underweight, district_no)
summary(data_member)

bmi <- merge(data_member, pop, by= "district_no")
bmi$weight2020 <- bmi$weight*bmi$growth
bmi <- bmi %>% filter(!is.na(growth))
summary(bmi)

bmi <- bmi %>% 
  group_by(district_no) %>% 
  summarise_at(vars(obesity, underweight), funs(weighted.mean(., weight2020)))

newdata <- merge(bmi, newdata, by= "district_no")
summary(newdata)

write.csv(print(newdata), file="newdata.1806.csv")
length(unique(newdata$district_id))

######### (weighted) district aggregates in 11 states##############


df1 <- newdata_total
df1 <- merge(df1, pop, by= "district_no")
df1$weight2020 <- df1$weight*df1$growth
df1 <- df1 %>% 
  group_by(district_no) %>% 
  summarise_at(vars(urban, sc, st, smoke, health,migration), funs(weighted.mean(., weight2020)))

districtmean <- merge(df0, df1, by= "district_no", all = TRUE)
dim(districtmean)
districtmean <- districtmean %>% filter(!is.na(cases))
districtmean <- merge(pop, districtmean, by= "district_no")
dim(districtmean)
districtmean <- merge(mig, districtmean, by= "district_no")
dim(districtmean)
districtmean <- merge(age, districtmean, by= "district_no")
dim(districtmean)
districtmean <- merge(bmi, districtmean, by= "district_no")
dim(districtmean)

districtmean$migration2 <- districtmean$pop_mig/districtmean$pop
districtmean$age65up <- districtmean$pop_65plus/districtmean$pop
districtmean$pop_1000 <- round(districtmean$pop/1000, 0)
summary(districtmean)
write.csv(print(districtmean), file="combineddata_1806.csv")


