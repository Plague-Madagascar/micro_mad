##### 04_checking_potential_error_in_soil_temp #####
#rm(list=ls())

# I have found that the soil temperature values produced through microclimc and nichemapr vary dependent on the above
# ground height specified, test and plot this using consistent land cover and soil type 

# Load the needed packages
library(devtools)
library(microclima) # devtools::install_github("ilyamaclean/microclima")
library(NicheMapR)  # devtools::install_github('mrke/NicheMapR')
library(microclimc) # devtools::install_github("ilyamaclean/microclimc")
library(microctools)
library(lubridate) # https://ehsanx.github.io/intro2R/date-time-data-with-lubridate.html
library(tidyverse)
library(mcera5)   # devtools::install_github("dklinges9/mcera5", dependencies=T)
library(purrr)

##### Load the climate data #####

#era_5_files <- list.files(path = "C:/Spatial_data/", full.names = T)

# set a location within the extracted data

loc <- c(47.982193, -17.528319) # lon, lat

st_time <- lubridate::ymd("2010:01:1")
en_time <- lubridate::ymd("2010:12:31")

# gather all hourly variables at that point and time period from the selected year (2010)

#point_out <- extract_clim(nc = era_5_files[[50]], long = loc[[1]], lat = loc[[2]],
#                          start_time = st_time,  end_time = en_time)

# Write the point data for use in sharing code

# write_rds(point_out, file = "./results/Depth_error/point_clim.rds")

point_out <- readRDS("./results/Depth_error/point_clim.rds")

# convert for use in microclimc

climdata <- hourlyncep_convert(climdata = point_out, loc[[1]], lat = loc[[2]])

# Get hourly time steps from the climate data

tme_mad <- as.POSIXlt(climdata$obs_time, format = "%Y-%m-%d %H:%M", tz = "Etc/GMT+3")

# Daily time steps
year_2010 <- seq(st_time, en_time, by = "day")

# Generate Habitat PAIs for each of the habitats
micro_hab_num <- c(2,4,6,7,8,9,10,11,13,14,16) # Open water removed

vary_hab <- lapply(micro_hab_num, function(x){
  microctools::habitatvars(habitat = x, 
                           long = loc[[1]], 
                           lat = loc[[2]], 
                           year_2010,  
                           m = 100)
})

# Name the habitats

hab_names <- c("Evergreen_broadleaf_forest",
                  "Deciduous_broadleaf_forest",
                  "Closed_shrublands",
                  "Open_shrublands",
                  "Woody_savannas",
                  "Savannas",
                  "Short_grasslands",
                  "Tall_grasslands",
                  "Croplands",
                  "Urban_and_built_up",
                  "Barren_or_sparsely_vegetated") 

names(vary_hab) <- hab_names

# View

vary_hab$Evergreen_broadleaf_forest

## Use Evergreen_broadleaf_forest for testing throughout ##

#### Run through models varying the height ####

# Set the soil parameters

soilp <- microclimc::soilinit(soiltype = "Loamy sand", m = 10, sdepth = 2)

# Set the precipitation parameters (this previously failed but is currently running)

prec_mad <- dailyprecipNCEP(lat = loc[[2]], long = loc[[1]], tme_mad)

# Set heights

height <- seq(0.1, 2, 0.1)

# create output list files

lu_lc_nmr <- list()

# Run model across all the heights
tic()
for (j in 1:length(height)) {
  lu_lc_nmr[[j]] <- runwithNMR(climdata, prec_mad, vary_hab[[1]], soilp, reqhgt = height[[j]], lat = loc[[2]], long = loc[[1]])
}
toc()

# List of 20 outputs across a range of reqhgt
# Get soil temperatures from these for annual data

# Should use mapply

soil_10cm_temp <- lapply(lu_lc_nmr, function(x){
  as.data.frame(x$nmrout$soil) %>%
    select(D10cm) %>%
    mutate(reqhgt = NA)
})

for (i in 1:length(soil_10cm_temp)) {
  soil_10cm_temp[[i]] <- soil_10cm_temp[[i]] %>%
    mutate(reqhgt = height[[i]])
}

soil_10cm_temp[[1]]

# Combine into one tibble

soil_10cm_temp_ann <- map_dfr(soil_10cm_temp, bind_rows) 

ggplot(data = soil_10cm_temp_ann,
       mapping = aes(x = reqhgt, y = D10cm, group = reqhgt)) +
  geom_boxplot() +
  labs(y = "Mean Annual Temp 10cm Soil Depth",
       x = "Required Height (reqhgt)",
       title = "Annual soil temp variation (10cm) with required height") +
  theme_classic()

# Save plot

ggsave("./results/Depth_error/10cm_annual_soil_vary.png", plot = last_plot())

#### Get soil temperature for the first day only and compare between depth #### 

soil_10cm_temp <- lapply(lu_lc_nmr, function(x){
  as.data.frame(x$nmrout$soil) %>%
    filter(DOY == 1)
})

## Check if the height is offset by 10cm 

soil_10cm_temp[[20]]$D10cm # 2m
soil_10cm_temp[[19]]$D0cm # 1.9m
soil_10cm_temp[[19]]$D20cm


# No the heights are not offset by 10cm

