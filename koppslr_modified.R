
# Rcode to implement Kopp 2016 sea level rise semi-empirical projections
# currently uses only a single value of each parameter, rather than an uncertain distribution
# unclear where to set initial equilibrium temperature...
# https://www.pnas.org/content/113/11/E1434

#dh/dt = b[T(t) -Te(t)]+phi(t)
#DTe/dt = [T(t)-Te(t)]/p1
#dphi/dt = -phi/p2

# created by M.Sarofim
# additional edits C. Hartin
# 5/11/2021

# milimeters

#############################################
# Something is off with units. Also need to compare to common baseline. 
# observations mm
# hector cm
# semi-emp mm

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

#tempscen <- read.csv("tempexample.csv", header=TRUE)
# reading in the temperature trajectory from Hector rcp26 scenario
tempscen <- read.csv("sample_outputstream_rcp26.csv", sep = ",", skip = 1) %>%
  select(-component) %>% 
  filter(year > 1849) %>% 
  filter(variable == "Tgav") %>% 
  rename("temperature" = "value") %>% 
  select(year, temperature)


numyears <- length(tempscen$year)

phi0 <- 0.1 #mm/year: multi-millenial contribution. 
#Kopp: "order 0.1 mm/y in 2000 CE"
bscalar <- 0.4 #scalar: sensitivity of sea level to temperature difference
#Kopp: eyeballing mode from Figure S5 for "a"
p1 <- 150 #timescale: how quickly does equilibrium temp reach current temp
#Kopp: eyeballing mode from FIgure S5 for "tau"
p2 <- 300 #timescale: e-folding decay rate for phi 
#Kopp: eyeballing mode for "tau-c"
equiltemp0 <- tempscen$temperature[1] - 0.5 #initial equiltemp


tempscen$equiltemp <- equiltempcalc(equiltemp0, p1, tempscen$temperature)
tempscen$phi <- phicalc(phi0, p2, numyears)
tempscen$sealevel <- sealevelcalc(bscalar, tempscen$equiltemp, tempscen$phi, 
                                      tempscen$temperature) 


# Read in historical observations

obs <- read.csv("CSIRO_Recons_gmsl_yr_2015.csv", sep = ",") %>% 
  rename("SLR" = "GMSL..mm.") %>% 
  rename("Error" = "GMSL.uncertainty..mm.") %>% 
  rename("year" = "Time") %>% 
  mutate(run_name = "observations")

# combine observations and model output
semi_emp <- rename(tempscen, "SLR" = "sealevel") %>% # so we can match observations column name
  mutate(run_name = "semi-empirical") %>% 
  select(year, SLR, run_name)

df1 <- full_join(obs, semi_emp)


# Import Hector data for comparison
# cm units

rcp26 <- read.csv("sample_outputstream_rcp26.csv", sep = ",", skip = 1) %>%
  select(-component) %>% 
  filter(year > 1849) %>% 
  filter(variable == "slr") %>% 
  rename("SLR" = "value")

df2 <- full_join(df1, rcp26) 

ggplot(df2, aes(x=year, y=SLR, color = run_name)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=SLR-Error, ymax=SLR+Error)) +
  xlim(1850,2110)+
  ylim(-250,250)
  #geom_segment(aes(x = 2100, y = .55, xend = 2100, yend = 0.85), color = "blue") +
  #geom_segment(aes(x = 2105, y = .8, xend = 2105, yend = 1.25), color = "red") 

