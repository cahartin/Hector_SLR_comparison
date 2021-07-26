
# Rcode to implement Kopp 2016 sea level rise semi-empirical projections
# currently uses only a single value of each parameter, rather than an uncertain distribution
# unclear where to set initial equilibrium temperature...
# https://www.pnas.org/content/113/11/E1434

#Equation 10
#dh/dt = b[T(t) -Te(t)]+phi(t)
#DTe/dt = [T(t)-Te(t)]/p1
#dphi/dt = -phi/p2

# created by M.Sarofim
# additional edits C. Hartin
# 5/11/2021


#############################################

library(ggplot2)
library(dplyr)
library(tidyr)

phicalc <- function(phi0, p2, numyears)
{
  t <- 1:numyears
  y1 <- phi0 * exp(-t/p2)
  return(y1)
}

equiltempcalc <- function(equiltemp0, p1, temp)
  {
  y1 <- numeric(numyears)
  for(t in 1:numyears)
    {
    ifelse(t == 1, 
               y1[t] <- equiltemp0,
               y1[t] <- y1[t-1]+(temp[t]-y1[t-1])/p1)
  }
  return(y1)
}

sealevelcalc <- function(bscalar, equiltemp, phi, temp)
{
  y1 <- numeric(numyears)
  for(t in 1:numyears)
  {
    ifelse(t==1, 
               y1[t] <- 0,
               y1[t] <- y1[t-1]+bscalar*(temp[t]-equiltemp[t])+phi[t])
  }
  return(y1)
}

###### Parameters

phi0 <- 0.14 #mm/year: multi-millenial contribution. 
#Kopp: "order 0.1 mm/y in 2000 CE"
bscalar <- 4.0 #scalar: sensitivity of sea level to temperature difference
#Kopp: eyeballing mode from Figure S5 for "a"
p1 <- 174 #timescale: how quickly does equilibrium temp reach current temp
#Kopp: eyeballing mode from FIgure S5 for "tau"
p2 <- 4175 #timescale: e-folding decay rate for phi 
#Kopp: eyeballing mode for "tau-c"
equiltemp0 <-  -0.05 # or 0.25
#tempscen$temperature[1] - 0.5 #initial equiltemp

######

# reading in the temperature trajectory from Hector rcp26, 45, 85 scenarios
# relative to 2000 baseline

tempscen <- read.csv("Data/sample_outputstream_rcp26.csv", sep = ",", skip = 1) %>%
  select(-component) %>% 
  filter(year > 1849) %>% 
  filter(variable == "Tgav") %>% 
  rename("temperature" = "value") %>% 
  select(year, temperature) 

numyears <- length(tempscen$year)


tempscen$equiltemp <- equiltempcalc(equiltemp0, p1, tempscen$temperature)
tempscen$phi <- phicalc(phi0, p2, numyears)
tempscen$sealevel <- sealevelcalc(bscalar, tempscen$equiltemp, tempscen$phi, 
                                  tempscen$temperature)
tempscen_rcp26 <- tempscen %>% 
  mutate(run_name = "SE_rcp26")

avg <- as.numeric(summarise(filter(tempscen_rcp26, year %in% c(2000)), SLR = mean(sealevel)))

tempscen_rcp26 <- mutate(tempscen_rcp26, SLR = sealevel - avg)

#### 
tempscen <- read.csv("Data/sample_outputstream_rcp45.csv", sep = ",", skip = 1) %>%
  select(-component) %>% 
  filter(year > 1849) %>% 
  filter(variable == "Tgav") %>% 
  rename("temperature" = "value") %>% 
  select(year, temperature) 

tempscen$equiltemp <- equiltempcalc(equiltemp0, p1, tempscen$temperature)
tempscen$phi <- phicalc(phi0, p2, numyears)
tempscen$sealevel <- sealevelcalc(bscalar, tempscen$equiltemp, tempscen$phi, 
                                  tempscen$temperature)
tempscen_rcp45 <- tempscen %>% 
  mutate(run_name = "SE_rcp45")

avg <- as.numeric(summarise(filter(tempscen_rcp45, year %in% c(2000)), SLR = mean(sealevel)))

tempscen_rcp45 <- mutate(tempscen_rcp45, SLR = sealevel - avg)


####
tempscen <- read.csv("Data/sample_outputstream_rcp85.csv", sep = ",", skip = 1) %>%
  select(-component) %>% 
  filter(year > 1849) %>% 
  filter(variable == "Tgav") %>% 
  rename("temperature" = "value") %>% 
  select(year, temperature) 

tempscen$equiltemp <- equiltempcalc(equiltemp0, p1, tempscen$temperature)
tempscen$phi <- phicalc(phi0, p2, numyears)
tempscen$sealevel <- sealevelcalc(bscalar, tempscen$equiltemp, tempscen$phi, 
                                  tempscen$temperature)
tempscen_rcp85 <- tempscen %>% 
  mutate(run_name = "SE_rcp85")

avg <- as.numeric(summarise(filter(tempscen_rcp85, year %in% c(2000)), SLR = mean(sealevel)))

tempscen_rcp85 <- mutate(tempscen_rcp85, SLR = sealevel - avg)

se_slr <- rbind(tempscen_rcp26, tempscen_rcp45, tempscen_rcp85) %>% 
  select(-equiltemp, -phi) %>% 
  rename("value" = "SLR") %>% 
  mutate(value = value/10) %>% 
  filter(year<2100) %>% 
  mutate(model = "SE")

ggplot(se_slr, aes(year, value, color=run_name)) +
  geom_line() +
  geom_point() +
  geom_segment(aes(x = 2103, y = 25, xend = 2103, yend = 59), color = "red", size = 1.5) +
  geom_point(aes(x = 2103, y = 38), color = "red", shape = 1, size = 3) +
  
  geom_segment(aes(x = 2106, y = 34, xend = 2106, yend = 81), color = "light green", size = 1.5) +
  geom_point(aes(x = 2106, y = 51), color = "light green", shape = 3, size = 3) +
  
  geom_segment(aes(x = 2109, y = 52, xend = 2109, yend = 121), color = "blue", size = 1.5) +
  geom_point(aes(x = 2109, y = 75), shape = 2, color = "blue", size = 3) +
  xlim(1850,2110) +
  ylab("gmslr in cm") +
  ggtitle("GSL comparison to Kopp et al 2016")


#############################
# What does it look like when i read in a temp time series that is realtive to 1986-2005?
tempscen <- read.csv("C:/Users/chartin/OneDrive - Environmental Protection Agency (EPA)/Documents/GitHub/hector//output/Hector_GCAMv5.3_ref.csv", 
  sep = ",", skip = 2) %>%
  filter(variable == "Tgav") %>% 
  filter(parameter_value == "ECS_3") %>% 
  rename("temperature" = "value") %>% 
  select(year, temperature) 


numyears <- length(tempscen$year)


tempscen$equiltemp <- equiltempcalc(equiltemp0, p1, tempscen$temperature)
tempscen$phi <- phicalc(phi0, p2, numyears)
tempscen$sealevel <- sealevelcalc(bscalar, tempscen$equiltemp, tempscen$phi, 
                                  tempscen$temperature)

tempscen_gcam <- tempscen %>%  mutate(sealevel = sealevel/10)

avg <- as.numeric(summarise(filter(tempscen_gcam, year %in% c(1986:2005)), SLR = mean(sealevel)))

tempscen_gcam <- mutate(tempscen_gcam, SLR = sealevel - avg)

ggplot(tempscen_gcam, aes(year, SLR)) + 
  geom_point() + 
  ggtitle("GCAM reference run (1986-2005) SLR in cm T0 = -0.05")

