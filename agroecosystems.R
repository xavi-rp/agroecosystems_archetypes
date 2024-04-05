


if(Sys.info()[4] == "D01RI1700308") {
  wd <- "D:/xavi_rp/D3_NRL_agroecosystems/"
}else if(Sys.info()[4] == "S-JRCIPRAP320P") {
  wd <- "D:/rotllxa/D3_NRL_agroecosystems/"
}else if(Sys.info()[4] %in% c("jeodpp-terminal-jd001-03", "jeodpp-terminal-03", "jeodpp-terminal-dev-12", 
                              "jeodpp-terminal-jd002-03", "jeodpp-terminal-jd004-03.cidsn.jrc.it",
                              "jeodpp-terminal-dev-jd002-12.cidsn.jrc.it")) {
  if(!dir.exists("/eos/jeodpp/home/users/rotllxa/NRL_agroecosystems/")) 
    dir.create("/eos/jeodpp/home/users/rotllxa/NRL_agroecosystems/")
  wd <- "/eos/jeodpp/home/users/rotllxa/NRL_agroecosystems/"
  WhereAmI <- "bdap"
}else if(Sys.info()[4] == "MacBook-MacBook-Pro-de-Xavier.local"){
  wd <- "/Users/xavi_rp/Documents/D3_NRL/NRL_agroecosystems/"
  WhereAmI <- "mac"
}else{
  wd <- "C:/Users/rotllxa/NRL_agroecosystems/"
}

setwd(wd)


library(terra)
library(sf)
library(data.table)
library(tidyverse)
library(ggplot2)





## Crop Systems ####
## Rega's crop types (Rega et al. 2020)

list.files("/eos/jeodpp/data/projects/REFOCUS/data/BIODIVERSITY/Rega/")
list.files("/eos/jeodpp/data/projects/REFOCUS/data/BIODIVERSITY/Rega/Energy_input_raw data")

Crop_management_systems_dom50_def <- rast("/eos/jeodpp/data/projects/REFOCUS/data/BIODIVERSITY/Rega/Crop_management_systems_dom50_def.tif")
Crop_management_systems_dom50_def   # 100m x 100m

Crop_management_systems_dom50_def_vals <- values(Crop_management_systems_dom50_def, dataframe = TRUE)
Crop_management_systems_dom50_def_unique <- unique(Crop_management_systems_dom50_def_vals$Cropmgmt50)
sort(levels(Crop_management_systems_dom50_def_unique))


Crop_management_systems_dom50_def


#rcl_df <- data.frame(is = sort(levels(Crop_management_systems_dom50_def_unique)),
#                     becomes = sort(levels(Crop_management_systems_dom50_def_unique)))
#
#rcl_df$becomes <- gsub("Grasslands and meadows" , "Grasslands_meadows", rcl_df$becomes)
#rcl_df$becomes <- gsub("Mixed systems with prevalence of arable crops" , "Mixed_Prevalence_Arable_Crops", rcl_df$becomes)
#rcl_df$becomes <- gsub("Mixed systems with prevalence of grasslands" , "Mixed_Prevalence_Grasslands", rcl_df$becomes)
#rcl_df$becomes <- gsub("Mixed systems with prevalence of permanent crops" , "Mixed_Prevalence_Permanent_Crops", rcl_df$becomes)
#rcl_df$becomes <- gsub("No Data" , "No_Data", rcl_df$becomes)
#rcl_df$becomes <- gsub("Non-agricultural areas" , "Non-agricultural", rcl_df$becomes)
#rcl_df$becomes <- gsub("Specialist field crops - cereals" , "Specialist_Cereals", rcl_df$becomes)
#rcl_df$becomes <- gsub("Specialist field crops - industrial crops" , "Specialist_Industrial_Crops", rcl_df$becomes)
#rcl_df$becomes <- gsub("Specialist Forage crops" , "Specialist_Forage", rcl_df$becomes)
#rcl_df$becomes <- gsub("Specialist fruits and citrus fruits" , "Specialist_Fruits_Citrus", rcl_df$becomes)
#rcl_df$becomes <- gsub("Specialist Olives" , "Specialist_Olives", rcl_df$becomes)
#rcl_df$becomes <- gsub("Specialist Vegetables, flowers and horticulture" , "Specialist_Horticulture", rcl_df$becomes)
#rcl_df$becomes <- gsub("Specialist Vineyards" , "Specialist_Vineyards", rcl_df$becomes)
#rcl_df

#Crop_systems <- classify(Crop_management_systems_dom50_def, 
#                         rcl = rcl_df[, c(1, 3)], 
#                         filename = "crop_systems_from_Rega.tif")




## Biogeographical areas ####
biogeoregions <- read_sf(dsn = "/eos/jeodpp/home/users/rotllxa/BiogeographicalRegions/eea_v_3035_1_mio_biogeo-regions_p_2016_v01_r00/", 
                         layer = "BiogeoRegions2016")
biogeoregions


biogeoregions_rast <- rasterize(biogeoregions, Crop_management_systems_dom50_def, field = "short_name")
biogeoregions_rast



## Merging biogeoregions ####

Crop_management_systems_dom50_def
#plot(Crop_management_systems_dom50_def)
#plot(biogeoregions_rast)


cropSystems_df <- as.data.frame(Crop_management_systems_dom50_def, cells = TRUE, na.rm = FALSE)
head(cropSystems_df)
nrow(cropSystems_df)

cropSystems_dt <- setDT(cropSystems_df)
cropSystems_dt


biogeoregions_rast_df <- as.data.frame(biogeoregions_rast, cells = TRUE, na.rm = FALSE)
head(biogeoregions_rast_df)
nrow(biogeoregions_rast_df)

biogeoregions_rast_dt <- setDT(biogeoregions_rast_df)
biogeoregions_rast_dt


## Merging data tables
cropSystems_biogeoregion <- merge(cropSystems_dt, biogeoregions_rast_dt, by = "cell", all.x = TRUE)
cropSystems_biogeoregion

nrow(cropSystems_biogeoregion)
sum(is.na(cropSystems_dt$Cropmgmt50))
sum(is.na(cropSystems_biogeoregion$short_name))

setnames(cropSystems_biogeoregion, c("Cropmgmt50", "short_name"), c("crop_system","biogeographical_region"))
cropSystems_biogeoregion

cropSystems <- Crop_management_systems_dom50_def
#values(cropSystems) <- cropSystems_biogeoregion[, "crop_system"]
cropSystems$crop_system <- cropSystems_biogeoregion$crop_system
cropSystems$biogeographical_region <- cropSystems_biogeoregion$biogeographical_region
#cropSystems <- setValues(cropSystems, values = cropSystems_biogeoregion, keepnames = TRUE)
cropSystems

writeRaster(cropSystems, filename = "cropSystems_biogeoregions.tif", overwrite = TRUE)
#cropSystems <- rast("cropSystems_biogeoregions.tif")
#cropSystems_biogeoregion <- as.data.frame(cropSystems, cells = FALSE, na.rm = FALSE)
#cropSystems_biogeoregion <- as.data.table(cropSystems_biogeoregion)

#cropSystems_df <- as.data.frame(cropSystems, cells = FALSE, na.rm = FALSE)
#head(cropSystems_df)
#identical(cropSystems_df$Cropmgmt50, cropSystems_df$crop_system)
#plot(cropSystems[[c("Cropmgmt50", "crop_system")]])

sum(is.na(cropSystems_biogeoregion$crop_system))
sum(is.na(cropSystems_biogeoregion$biogeographical_region))

cropSystems_biogeoregion_1 <- cropSystems_biogeoregion[!is.na(cropSystems_biogeoregion$crop_system), ]
cropSystems_biogeoregion_1
nrow(cropSystems_biogeoregion_1)
sum(is.na(cropSystems_biogeoregion_1$biogeographical_region))

cropSystems_biogeoregion_1 <- cropSystems_biogeoregion_1[!is.na(cropSystems_biogeoregion_1$biogeographical_region), ]
cropSystems_biogeoregion_1
#write.csv(cropSystems_biogeoregion_1, "cropSystems_biogeoregion_1.csv", row.names = FALSE)


## Splitting crop system and intenity in separated columns

#library(tidyverse)
#cropSystems_biogeoregion_2 <- separate(cropSystems_biogeoregion_1, 
#                                       col = "crop_system",
#                                       into = c("CropSystem", "Intensity"),
#                                       sep = " - ",
#                                       remove = FALSE)
#cropSystems_biogeoregion_2
#

cropSystems_biogeoregion_2 <- cropSystems_biogeoregion_1
cropSystems_biogeoregion_2
sort(unique(cropSystems_biogeoregion_2$crop_system))
     
cropSystems_biogeoregion_2 <- cropSystems_biogeoregion_2[, CropSystem := crop_system]
cropSystems_biogeoregion_2 <- cropSystems_biogeoregion_2[, Intensity := crop_system]
cropSystems_biogeoregion_2

cropSystems_biogeoregion_2$Intensity <- gsub("No Data - ",
                                             "",
                                             cropSystems_biogeoregion_2$Intensity)

sort(unique(cropSystems_biogeoregion_2$Intensity))



cropSystems_biogeoregion_2$CropSystem <- gsub(" - High",
                                             "",
                                             cropSystems_biogeoregion_2$CropSystem)

sort(unique(cropSystems_biogeoregion_2$CropSystem))


cropSystems_biogeoregion_2
nrow(cropSystems_biogeoregion_2)
cropSystems_biogeoregion_2[, c("cell", "CropSystem", "Intensity", "biogeographical_region")]

#write.csv(cropSystems_biogeoregion_2, "cropSystems_biogeoregion_2.csv", row.names = FALSE)
fread()

cropSystems_biogeoregion_tosave <- merge(cropSystems_biogeoregion,
                                         cropSystems_biogeoregion_2[, c("cell", "CropSystem", "Intensity", "biogeographical_region")],
                                         all.x = TRUE,
                                         by = "cell")
cropSystems_biogeoregion_tosave

names(cropSystems)
cropSystems[["CropSystem"]] <- cropSystems_biogeoregion_tosave$CropSystem
cropSystems[["Intensity"]] <- cropSystems_biogeoregion_tosave$Intensity

writeRaster(cropSystems, filename = "cropSystems_rast.tif", overwrite = TRUE)  
Sys.time()

#cropSystems <- rast("cropSystems_rast.tif")
cropSystems
names(cropSystems)


## Plotting ####

cropSystems_biogeoregion_2_sel <- sample_n(cropSystems_biogeoregion_2[, c("cell", "CropSystem", "Intensity", "biogeographical_region")], 10^4) 
cropSystems_biogeoregion_2_sel <- cropSystems_biogeoregion_2  # all points 
cropSystems_biogeoregion_2_sel  
str(cropSystems_biogeoregion_2_sel)

cropSystems_biogeoregion_2_sel <- cropSystems_biogeoregion_2_sel %>%
  mutate(Intensity = fct_relevel(Intensity, "Low", "Medium", "High"))

table(cropSystems_biogeoregion_2_sel$biogeographical_region)


## plots by biogeographical region
cropSystems_biogeoregion_2_sel %>%
  group_by(CropSystem, Intensity, biogeographical_region) %>%
  summarise(n = n()) %>%
  filter(biogeographical_region == "outside") #%>%
  View()
  

cropSystems_biogeoregion_2_sel %>%
  group_by(CropSystem, Intensity, biogeographical_region) %>%
  summarise(n = n()) %>%
  group_by(biogeographical_region) %>%
  mutate(freq = n / sum(n)) %>% 
  #summarise(sum = sum(freq)) %>%
  #View()
  #
  group_by(CropSystem, biogeographical_region)  %>% 
  summarise(sum = round(sum(freq), 3)) %>%
  filter(biogeographical_region == "mediterranean") #%>%
  #group_by(biogeographical_region) %>%
  #summarise(sum(sum))
  View()

  
cropSystems_biogeoregion_2_sel %>%
    group_by(CropSystem, Intensity, biogeographical_region) %>%
    summarise(n = n()) %>%
    group_by(biogeographical_region) %>%
    mutate(freq = n / sum(n)) %>% 
    filter(biogeographical_region == "alpine") %>% 
    filter(CropSystem == "Grasslands and meadows")  
  
  
  

d <- ggplot(cropSystems_biogeoregion_2_sel, 
            aes(x = Intensity, y = CropSystem, col = biogeographical_region))
d1 <- d + geom_count(
  aes(
    size = after_stat(prop), 
    #size = after_stat(n), 
    group = 1)) +
    #group = CropSystem)) +
  scale_size_area(max_size = 10, "Proportion") + 
  scale_colour_viridis_d() +
  facet_wrap(~ biogeographical_region, nrow = 2) +  
  guides(colour = "none") 

d1 

png("CropSystem_Intensity_biogeogr.png", width = 25, height = 20, units = "cm", res = 150)
d1
dev.off()




## plots all regions

cropSystems_biogeoregion_2_sel %>%
  group_by(CropSystem, Intensity) %>%
  summarise(n = n()) %>%
  #mutate(freq = n / sum(n)) %>% #View()
  mutate(freq = n / nrow(cropSystems_biogeoregion_2_sel)) %>% View()
  #summarise(sum(freq)) %>%
  #summarise(sum(`sum(freq)`))
  



d0 <- ggplot(cropSystems_biogeoregion_2_sel, 
            aes(x = Intensity, y = CropSystem))
d01 <- d0 + geom_count(
  aes(
    size = after_stat(prop), 
    #size = after_stat(n), 
    group = 1)) +
    #group = c("Intensity"))) +
  scale_size_area(max_size = 10, "Proportion") 

d01 


png("CropSystem_Intensity.png", width = 20, height = 20, units = "cm", res = 150)
d01
dev.off()



## Same, including values close to the circles
cropSystems_biogeoregion_2_sel %>%
  group_by(CropSystem, Intensity) %>%
  summarise(n = n()) %>%
  #mutate(freq = n / sum(n)) %>% #View()
  mutate(freq = n / nrow(cropSystems_biogeoregion_2_sel))  %>%  # View()
  group_by(CropSystem) %>%
  summarise(sum = round(sum(freq), 3))  #%>%  #View()
  #summarise(sum(sum))


d0 <- ggplot(cropSystems_biogeoregion_2_sel %>%
               group_by(CropSystem, Intensity) %>%
               summarise(n = n()) %>%
               #mutate(freq = n / sum(n)) %>% #View()
               mutate(freq = n / nrow(cropSystems_biogeoregion_2_sel)) , 
             aes(x = Intensity, y = CropSystem))
d01 <- d0 + geom_count(
  aes(
    size = freq, 
    #size = after_stat(n), 
    group = 1), shape = 21, fill = "lightblue", color = "blue") +
  #group = c("Intensity"))) +
  scale_size_area(max_size = 10, "Proportion")  +
  geom_text(aes(label = sprintf('%.3f', freq)),
            color = "black",
            hjust = -0.5)

d01

png("CropSystem_Intensity_values.png", width = 20, height = 20, units = "cm", res = 150)
d01
dev.off()



## Combined archetypes by biogeographical region

#cropSystems_biogeoregion_2_sel <- sample_n(cropSystems_biogeoregion_2, 10^4) 
#cropSystems_biogeoregion_2_sel

d00 <- ggplot(cropSystems_biogeoregion_2_sel %>%
               group_by(crop_system, biogeographical_region) %>%
               summarise(n = n()) %>%
               #mutate(freq = n / sum(n)) %>% #View()
               mutate(freq = n / nrow(cropSystems_biogeoregion_2_sel)), 
             aes(x = biogeographical_region, y = factor(crop_system, level = sort(as.vector(unique(cropSystems_biogeoregion_2_sel$crop_system))))))
d001 <- d00 + geom_count(
  aes(
    size = freq, 
    #size = after_stat(n), 
    group = 1), shape = 21, fill = "lightblue", color = "blue") +
  #group = c("Intensity"))) +
  scale_size_area(max_size = 10, "Proportion")  +
  geom_text(aes(label = sprintf('%.3f', freq)),
            color = "black",
            hjust = -0.5) +
  xlab("Biogeographical region") +
  ylab("Archetypes (crop system + intensity)")

d001 

png("archetypes_biogeographical_values.png", width = 30, height = 15, units = "cm", res = 150)
d001
dev.off()


## without the values in the plot
d00 <- ggplot(cropSystems_biogeoregion_2_sel %>%
               group_by(crop_system, biogeographical_region) %>%
               summarise(n = n()) %>%
               #mutate(freq = n / sum(n)) %>% #View()
               mutate(freq = n / nrow(cropSystems_biogeoregion_2_sel)), 
             aes(x = biogeographical_region, y = factor(crop_system, level = sort(as.vector(unique(cropSystems_biogeoregion_2_sel$crop_system))))))
d001 <- d00 + geom_count(
  aes(
    size = freq, 
    #size = after_stat(n), 
    group = 1), shape = 21, fill = "lightblue", color = "blue") +
  #group = c("Intensity"))) +
  scale_size_area(max_size = 10, "Proportion")  +
  #geom_text(aes(label = sprintf('%.3f', freq)),
  #          color = "blue",
  #          hjust = -0.5) +
  xlab("Biogeographical region") +
  ylab("Archetypes (crop system + intensity)")

d001

png("archetypes_biogeographical.png", width = 30, height = 15, units = "cm", res = 150)
d001
dev.off()




## Archetypes against ES variables ####

cropSystems <- rast("cropSystems_rast.tif")  # 100m
cropSystems
names(cropSystems)


## ES from https://ecosystem-accounts.jrc.ec.europa.eu/data-catalogue/output-data

### Pollination demand ####

ES_pollin_dir <- "/eos/jeodpp/home/users/rotllxa/KIPINCA/CROP_POLLINATION/"

# Description: Demand for pollination expressed as hectare of pollinator-dependent crops per km^2.
# Spatial data derived from the CAPRI model has been used to quantify the demand.

list.files(ES_pollin_dir)
list.files(paste0(ES_pollin_dir, "/demand"))

demand_hectare <- rast(paste0(ES_pollin_dir, "/demand", "/demand_hectare.tif"))
demand_hectare  # bands 1-4 = 2000, 2006, 2012, 2018
res(demand_hectare)  # 1000 x 1000 m
terra::minmax(demand_hectare)  # max = 100 ha/km2  (the whole cell is pollinator dependent)

demand_hectare <- demand_hectare[[2]]   # taking 2006 as it's the closest to the base year for the archetypes data (2008)
demand_hectare


## Differences among Crop Systems
## As this variable is directly calculated as area of the crop system, 
## it makes no sense to calculate differences among Crop Systems.

Crop_systems_1km_char <- rast("/eos/jeodpp/home/users/rotllxa/Birds_Map_Indicators/crop_systems_from_Rega_1km_char.tif")
Crop_systems_1km_char

Crop_systems_1km <- rast("/eos/jeodpp/home/users/rotllxa/Birds_Map_Indicators/crop_systems_from_Rega_1km.tif")
Crop_systems_1km



## Differences among Intensity classes
list.files("/eos/jeodpp/data/projects/REFOCUS/data/BIODIVERSITY/Rega")
list.files("/eos/jeodpp/data/projects/REFOCUS/data/BIODIVERSITY/Rega/Energy_input_raw data")
Energy_input_2008_fille04_no_labour <- rast("/eos/jeodpp/data/projects/REFOCUS/data/BIODIVERSITY/Rega/Energy_input_raw data/Energy_input_2008_fille04_no_labour.tif")
Energy_input_2008_fille04_no_labour  # like fig 3A (absolute intensity) but continuous (1km)
                                     # fig 3A (Absolute_intensity_5_clas_Fig3A): absolute intensity by 5 classes (1km) 
str(levels(Energy_input_2008_fille04_no_labour))
plot(Energy_input_2008_fille04_no_labour)

Energy_input_2008_fille04_no_labour_vals <- values(Energy_input_2008_fille04_no_labour)
str(Energy_input_2008_fille04_no_labour_vals)
is.matrix(Energy_input_2008_fille04_no_labour_vals)
range(unique(Energy_input_2008_fille04_no_labour_vals), na.rm = TRUE)   # 43 - 886724




demand_hectare
Energy_input_2008_fille04_no_labour

# cut
demand_hectare_1 <- crop(demand_hectare, Energy_input_2008_fille04_no_labour, mask = TRUE)
plot(demand_hectare)
plot(demand_hectare_1)
plot(Energy_input_2008_fille04_no_labour)


# scatter plot
demand_hectare_1_vals <- values(demand_hectare_1)
str(demand_hectare_1_vals)
range(demand_hectare_1_vals, na.rm = TRUE)

data2plot <- data.table(Energy_input_2008_fille04_no_labour_vals,
                        demand_hectare_1_vals)

data2plot

data2plot <- na.omit(data2plot)
data2plot

R2 <- cor(data2plot)^2
R2

p <- ggplot(data = data2plot, aes(x = demand_hectare_2, y = Intens_cla)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red", formula = y ~ x) 
 
p  # there is no correlation between Intensity and Pollination Demand

p + ggpubr::stat_cor(label.y = 4e5, label.x = 50)



# boxplots
list.files("/eos/jeodpp/data/projects/REFOCUS/data/BIODIVERSITY/Rega/Fig3A-Absolute_intensity_5_clas_.tif")

Absolute_intensity_5_clas_Fig3A <- rast("/eos/jeodpp/data/projects/REFOCUS/data/BIODIVERSITY/Rega/Fig3A-Absolute_intensity_5_clas_.tif/Absolute_intensity_5_clas_Fig3A.tif")
Absolute_intensity_5_clas_Fig3A 
plot(Absolute_intensity_5_clas_Fig3A)

Absolute_intensity_5_clas_Fig3A_vals <- values(Absolute_intensity_5_clas_Fig3A)
unique(Absolute_intensity_5_clas_Fig3A_vals)

data2plot_1 <- data.table(Absolute_intensity_5_clas_Fig3A_vals,
                          demand_hectare_1_vals)

data2plot_1

data2plot_1 <- na.omit(data2plot_1)
data2plot_1

p1 <- ggplot(data2plot_1, aes(x = factor(Range_valu), y = demand_hectare_2)) + 
  geom_boxplot() +
  labs(y = 'Pollination Demand', x = 'Intensity class (MJ/ha)') +
  scale_x_discrete("Intensity class (MJ/ha)", labels = c("<= 5000", 
                                                 "> 5000 - 10000", 
                                                 "> 10000 - 15000", 
                                                 "> 15000 - 20000", 
                                                 "> 20000"))
p1

png("boxplot_PollinationDemand-IntensityClasses.png")
p1
dev.off()


#aov <- anova(lm(demand_hectare_2 ~ factor(Range_valu), data = data2plot_1))
#aov

aov <- lm(demand_hectare_2 ~ factor(Range_valu), data = data2plot_1)
summary(aov)

data2plot_1 %>%
  group_by(Range_valu) %>%
  summarise(mean(demand_hectare_2))

aov <- aov(demand_hectare_2 ~ factor(Range_valu), data = data2plot_1)
summary(aov)
aov
aov$coefficients
TukeyHSD(aov)




### Crop Provision - Use ####

list.files("/eos/jeodpp/home/users/rotllxa/KIPINCA/CROP_PROVISION")
list.files("/eos/jeodpp/home/users/rotllxa/KIPINCA/CROP_PROVISION/use")

# Description:
# Contribution of ecosystem to crop production expressed by tonnes per km^3.
# It has been estimated disentangling the ecosystem contribution to crop production for each crop type separately
# based on CAPRI model data.

use_1000_tonne <- rast("/eos/jeodpp/home/users/rotllxa/KIPINCA/CROP_PROVISION/use/use_1000-tonne.tif")

use_1000_tonne <- use_1000_tonne[[2]]
use_1000_tonne



## Differences among Crop Systems

Crop_systems_1km_char <- rast("/eos/jeodpp/home/users/rotllxa/Birds_Map_Indicators/crop_systems_from_Rega_1km_char.tif")
Crop_systems_1km_char

Crop_systems_1km <- rast("/eos/jeodpp/home/users/rotllxa/Birds_Map_Indicators/crop_systems_from_Rega_1km.tif")
Crop_systems_1km


Crop_systems_1km_vals <- values(Crop_systems_1km)
unique(Crop_systems_1km_vals)
nrow(Crop_systems_1km_vals)


# cut
use_1000_tonne_1 <- project(use_1000_tonne, Crop_systems_1km_char)
use_1000_tonne_1 <- crop(use_1000_tonne_1, Crop_systems_1km_char, mask = TRUE, extend = TRUE)
use_1000_tonne_1

use_1000_tonne_1_vals <- values(use_1000_tonne_1)
use_1000_tonne_1_vals
summary(use_1000_tonne_1_vals)
sum(!is.na(use_1000_tonne_1_vals))
sum(is.na(use_1000_tonne_1_vals))


data2plot_1 <- data.table(Crop_systems_1km_vals,
                          use_1000_tonne_1_vals)

data2plot_1

data2plot_1 <- na.omit(data2plot_1)
data2plot_1

p1 <- ggplot(data2plot_1, aes(x = factor(Crop_systems_num), y = `use_1000-tonne_2`)) + 
  geom_boxplot() +
  labs(y = 'Crop Provision - Use (tonnes/km^3)', x = 'Crop System') +
  scale_x_discrete("Crop System", labels = c("Grasslands_meadows", "Mixed_Prevalence_Arable_Crops", "Mixed_Prevalence_Grasslands", "Mixed_Prevalence_Permanent_Crops",
                                                         "No_Data", "Specialist_Cereals", "Specialist_Industrial_Crops", "Specialist_Forage",               
                                                         "Specialist_Fruits_Citrus", "Specialist_Olives", "Specialist_Horticulture", "Specialist_Vineyards"            
                                                         )) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

p1

png("boxplot_CropProvision_Use-CropSystems.png")
p1
dev.off()


aov <- lm(`use_1000-tonne_2` ~ factor(Crop_systems_num), data = data2plot_1)
summary(aov)

data2plot_1 %>%
  group_by(Crop_systems_num) %>%
  summarise(mean(`use_1000-tonne_2`))

aov <- aov(`use_1000-tonne_2` ~ factor(Crop_systems_num), data = data2plot_1)
summary(aov)
aov
aov$coefficients
TukeyHSD(aov)
View(TukeyHSD(aov)$`factor(Crop_systems_num)`)



## Differences among Intensity classes
# cut
use_1000_tonne_1 <- project(use_1000_tonne, Absolute_intensity_5_clas_Fig3A)
use_1000_tonne_1 <- crop(use_1000_tonne_1, Absolute_intensity_5_clas_Fig3A, mask = TRUE, extend = TRUE)
use_1000_tonne_1

use_1000_tonne_1_vals <- values(use_1000_tonne_1)
use_1000_tonne_1_vals


data2plot_1 <- data.table(Absolute_intensity_5_clas_Fig3A_vals,
                          use_1000_tonne_1_vals)

data2plot_1

data2plot_1 <- na.omit(data2plot_1)
data2plot_1

p1 <- ggplot(data2plot_1, aes(x = factor(Range_valu), y = `use_1000-tonne_2`)) +
  geom_boxplot() +
  labs(y = 'Crop Provision - Use (tonnes/km3)', x = 'Intensity class (MJ/ha)') +
  scale_x_discrete("Intensity class (MJ/ha)", labels = c("<= 5000", 
                                                         "> 5000 - 10000", 
                                                         "> 10000 - 15000", 
                                                         "> 15000 - 20000", 
                                                         "> 20000"))
p1

png("boxplot_CropProvision_Use-IntensityClasses.png")
p1
dev.off()



aov <- lm(`use_1000-tonne_2` ~ factor(Range_valu), data = data2plot_1)
summary(aov)

data2plot_1 %>%
  group_by(Range_valu) %>%
  summarise(mean(`use_1000-tonne_2`))

aov <- aov(`use_1000-tonne_2` ~ factor(Range_valu), data = data2plot_1)
summary(aov)
aov
aov$coefficients
TukeyHSD(aov)
