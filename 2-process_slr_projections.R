# Step 2 in SLR script
# Process CMIP data for SLR from
# https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2020GL092064
# Data located: 
# https://data.4tu.nl/articles/dataset/Supporting_data_for_Projecting_Global_Mean_Sea-Level_Change_Using_CMIP6_Models/12958079

# C.Hartin 5/31/2021

library(ncdf4)

year <- 2006:2099

nc <- nc_open("CMIP_SLR/Data/cmip6_mc_projections/ssp126_GMSLR_mid.nc")
gmslr_mid <- ncvar_get(nc, var="GMSLR") %>%
  as.data.frame() %>% 
  rename("value" = ".") %>% 
  mutate(run_name = "SSP126_mid") %>% 
  cbind(year)

nc <- nc_open("CMIP_SLR/Data/cmip6_mc_projections/ssp126_GMSLR_lower.nc")
gmslr_lower <- ncvar_get(nc, var="GMSLR") %>% 
  as.data.frame() %>% 
  rename("value" = ".") %>% 
  mutate(run_name = "SSP126_lower")%>% 
  cbind(year)

nc <- nc_open("CMIP_SLR/Data/cmip6_mc_projections/ssp126_GMSLR_upper.nc")
gmslr_upper <- ncvar_get(nc, var="GMSLR") %>% 
  as.data.frame() %>% 
  rename("value" = ".") %>% 
  mutate(run_name = "SSP126_upper")%>% 
  cbind(year)

gmslr_ssp126 <- rbind(gmslr_mid, gmslr_lower, gmslr_upper)

####
nc <- nc_open("CMIP_SLR/Data/cmip6_mc_projections/ssp245_GMSLR_mid.nc")
gmslr_mid <- ncvar_get(nc, var="GMSLR") %>%
  as.data.frame() %>% 
  rename("value" = ".") %>% 
  mutate(run_name = "SSP245_mid") %>% 
  cbind(year)

nc <- nc_open("CMIP_SLR/Data/cmip6_mc_projections/ssp245_GMSLR_lower.nc")
gmslr_lower <- ncvar_get(nc, var="GMSLR") %>% 
  as.data.frame() %>% 
  rename("value" = ".") %>% 
  mutate(run_name = "SSP245_lower")%>% 
  cbind(year)

nc <- nc_open("CMIP_SLR/Data/cmip6_mc_projections/ssp245_GMSLR_upper.nc")
gmslr_upper <- ncvar_get(nc, var="GMSLR") %>% 
  as.data.frame() %>% 
  rename("value" = ".") %>% 
  mutate(run_name = "SSP245_upper")%>% 
  cbind(year)

gmslr_ssp245 <- rbind(gmslr_mid, gmslr_lower, gmslr_upper)

######
nc <- nc_open("CMIP_SLR/Data/cmip6_mc_projections/ssp585_GMSLR_mid.nc")
gmslr_mid <- ncvar_get(nc, var="GMSLR") %>%
  as.data.frame() %>% 
  rename("value" = ".") %>% 
  mutate(run_name = "SSP585_mid") %>% 
  cbind(year)

nc <- nc_open("CMIP_SLR/Data/cmip6_mc_projections/ssp585_GMSLR_lower.nc")
gmslr_lower <- ncvar_get(nc, var="GMSLR") %>% 
  as.data.frame() %>% 
  rename("value" = ".") %>% 
  mutate(run_name = "SSP585_lower")%>% 
  cbind(year)

nc <- nc_open("CMIP_SLR/Data/cmip6_mc_projections/ssp585_GMSLR_upper.nc")
gmslr_upper <- ncvar_get(nc, var="GMSLR") %>% 
  as.data.frame() %>% 
  rename("value" = ".") %>% 
  mutate(run_name = "SSP585_upper")%>% 
  cbind(year)

gmslr_ssp585 <- rbind(gmslr_mid, gmslr_lower, gmslr_upper)


gmslr <- rbind(gmslr_ssp126, gmslr_ssp245, gmslr_ssp585) %>% 
  mutate(model = "CMIP")
