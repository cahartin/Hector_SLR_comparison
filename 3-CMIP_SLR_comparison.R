# Script to compare Kopps semi-empirical SLR code 
# https://www.pnas.org/content/113/11/E1434
# to projections of SLR from CMIP5/6 from 
# https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2020GL092064

# C.Hartin & M.Sarofim
# 5/14/2021


library(ggplot2)
library(dplyr)
library(tidyr)

### Run Kopp SLR code
# modify output to compare to CMIP

# RCP 45

# reading in the temperature trajectory from Hector rcp26 scenario
tempscen <- read.csv("sample_outputstream_rcp26.csv", sep = ",", skip = 1) %>%
  select(-component) %>% 
  filter(year > 1849) %>% 
  filter(variable == "Tgav") %>% 
  rename("temperature" = "value") %>% 
  select(year, temperature) 

equiltemp0 <- tempscen$temperature[1] - 0.5 #initial equiltemp

numyears <- length(tempscen$year)


tempscen$equiltemp <- equiltempcalc(equiltemp0, p1, tempscen$temperature)
tempscen$phi <- phicalc(phi0, p2, numyears)
tempscen$sealevel <- sealevelcalc(bscalar, tempscen$equiltemp, tempscen$phi, 
                                  tempscen$temperature)
tempscen_rcp26 <- tempscen %>% 
  mutate(run_name = "SE_rcp26")

avg <- as.numeric(summarise(filter(tempscen_rcp26, year %in% c(1986:2005)), SLR = mean(sealevel)))

tempscen_rcp26 <- mutate(tempscen_rcp26, SLR = sealevel - avg)

######
# RCP 45
#tempscen <- read.csv("tempexample.csv", header=TRUE)
# reading in the temperature trajectory from Hector rcp26 scenario
tempscen <- read.csv("sample_outputstream_rcp45.csv", sep = ",", skip = 1) %>%
  select(-component) %>% 
  filter(year > 1849) %>% 
  filter(variable == "Tgav") %>% 
  rename("temperature" = "value") %>% 
  select(year, temperature) 

equiltemp0 <- tempscen$temperature[1] - 0.5 #initial equiltemp

numyears <- length(tempscen$year)


tempscen$equiltemp <- equiltempcalc(equiltemp0, p1, tempscen$temperature)
tempscen$phi <- phicalc(phi0, p2, numyears)
tempscen$sealevel <- sealevelcalc(bscalar, tempscen$equiltemp, tempscen$phi, 
                                  tempscen$temperature)
tempscen_rcp45 <- tempscen %>% 
  mutate(run_name = "SE_rcp45")
avg <- as.numeric(summarise(filter(tempscen_rcp45, year %in% c(1986:2005)), SLR = mean(sealevel)))

tempscen_rcp45 <- mutate(tempscen_rcp45, SLR = sealevel - avg)

######
# RCP 85
#tempscen <- read.csv("tempexample.csv", header=TRUE)
# reading in the temperature trajectory from Hector rcp85 scenario
tempscen <- read.csv("sample_outputstream_rcp85.csv", sep = ",", skip = 1) %>%
  select(-component) %>% 
  filter(year > 1849) %>% 
  filter(variable == "Tgav") %>% 
  rename("temperature" = "value") %>% 
  select(year, temperature)

equiltemp0 <- tempscen$temperature[1] - 0.5 #initial equiltemp

numyears <- length(tempscen$year)


tempscen$equiltemp <- equiltempcalc(equiltemp0, p1, tempscen$temperature)
tempscen$phi <- phicalc(phi0, p2, numyears)
tempscen$sealevel <- sealevelcalc(bscalar, tempscen$equiltemp, tempscen$phi, 
                                  tempscen$temperature)
tempscen_rcp85 <- tempscen %>% 
  mutate(run_name = "SE_rcp85")

avg <- as.numeric(summarise(filter(tempscen_rcp85, year %in% c(1986:2005)), SLR = mean(sealevel)))

tempscen_rcp85 <- mutate(tempscen_rcp85, SLR = sealevel - avg)

se_slr <- rbind(tempscen_rcp26, tempscen_rcp45, tempscen_rcp85) %>% 
  select(-equiltemp, -phi) %>% 
  rename("value" = "SLR") %>% 
  mutate(value = value/1000) %>% 
  filter(year<2100) %>% 
  mutate(model = "SE")


###
# Run the processed projections

x<- full_join(se_slr, gmslr) 

ggplot(x, aes(year, value, color=run_name)) +
  geom_line() +
  geom_point(data = filter(x, run_name %in% c("SE_rcp26", "SE_rcp45", "SE_rcp85"))) +
  xlim(2000,2100) +
  ylab("gmslr in meters")
