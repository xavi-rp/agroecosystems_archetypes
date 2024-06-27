



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
  wd <- "/Users/xavi_rp/Documents/D3_iBLUA/"
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



## Rega's crop types (Rega et al. 2020) ####

## Rega's intensity classes (3)
if(WhereAmI == "mac"){
  Energy_input_2008_fille04_no_labour <- rast(paste0("/Users/xavi_rp/Documents/D5_FFGRCC/Rega/Energy_input_raw data/", "Energy_input_2008_fille04_no_labour.tif"))
}else{
  Energy_input_2008_fille04_no_labour <- rast(paste0("/eos/jeodpp/data/projects/REFOCUS/data/BIODIVERSITY/Rega/Energy_input_raw data/",
                                                     "Energy_input_2008_fille04_no_labour.tif"))
}
Energy_input_2008_fille04_no_labour # 1km; 3 intensity classes (1 = low, 2 = medium, 3 = high)
plot(Energy_input_2008_fille04_no_labour)

cats(Energy_input_2008_fille04_no_labour)  ## This is the attribute table of the raster; it includes all the unique
                                           ## combination of attributes
head(cats(Energy_input_2008_fille04_no_labour)[[1]])
nrow(cats(Energy_input_2008_fille04_no_labour)[[1]])  

#Energy_input_2008_fille04_no_labour_num <- raster::raster(paste0("/eos/jeodpp/data/projects/REFOCUS/data/BIODIVERSITY/Rega/Energy_input_raw data/",
#                                                       "Energy_input_2008_fille04_no_labour.tif"))
#Energy_input_2008_fille04_no_labour_num # 
#sum(!is.na(unique(values(Energy_input_2008_fille04_no_labour_num))))

attribs <- cats(Energy_input_2008_fille04_no_labour)[[1]]
head(attribs)
unique(attribs$Intensity)
unique(attribs[, c("Intens_cla", "Intensity")])


## Rega's crop types

if(WhereAmI == "mac"){
  crop_systems_from_Rega_1km_char <- rast(paste0("/Users/xavi_rp/Documents/D5_FFGRCC/Rega/crop_systems_from_Rega/",
                                                 "crop_systems_from_Rega_1km_char.tif"))
}else{
  crop_systems_from_Rega_1km_char <- rast(paste0("/eos/jeodpp/home/users/rotllxa/Birds_Map_Indicators/",
                                                 "crop_systems_from_Rega_1km_char.tif"))
}
crop_systems_from_Rega_1km_char   # crop systems aggregated to 1km
cats(crop_systems_from_Rega_1km_char)[[1]]

#plot(crop_systems_from_Rega_1km_char)  # crop systems aggregated to 1km

crp_syst <- ggplot() + 
  tidyterra::geom_spatraster(data = crop_systems_from_Rega_1km_char, aes(fill = code_char)) +
  viridis::scale_fill_viridis(discrete = TRUE, na.translate = FALSE, name = "Crop system") +
  labs(title = "Archetypes: crop system component") +
  theme(plot.title = element_text(hjust = 0.5))


jpeg("ArchetypesCropsystemComponent.jpg", width = 20, height = 15, units = "cm", res = 150)
crp_syst
dev.off()

## Merging both -> archetypes (1km, 3 intensity classes)
Energy_input_2008_fille04_no_labour <- project(Energy_input_2008_fille04_no_labour, crop_systems_from_Rega_1km_char)
Energy_input_2008_fille04_no_labour
plot(Energy_input_2008_fille04_no_labour)

intsty <- ggplot() + 
  tidyterra::geom_spatraster(data = Energy_input_2008_fille04_no_labour, aes(fill = Intens_cla)) +
  viridis::scale_fill_viridis(discrete = TRUE, na.translate = FALSE, name = "Intensity",
                              option = "plasma", direction = - 1) +
  labs(title = "Archetypes: intensity component") +
  theme(plot.title = element_text(hjust = 0.5))

jpeg("ArchetypesIntensityComponent.jpg", width = 20, height = 15, units = "cm", res = 150)
intsty
dev.off()

# concatenating both layers
#archetypes <- concats(crop_systems_from_Rega_1km_char, Energy_input_2008_fille04_no_labour,
#                      filename = "archetypes_rega_1km.tif")
#archetypes
#plot(archetypes)

# two separated layers
archetypes_1 <- crop_systems_from_Rega_1km_char

archetypes_1$intensity <- Energy_input_2008_fille04_no_labour
archetypes_1
names(archetypes_1) <- c("Crop_system", "Intensity")
archetypes_1




### Thresholds for intensity classes (Rega) ####

Energy_input_2008_fille04_no_labour

attribs <- cats(Energy_input_2008_fille04_no_labour)[[1]]
head(attribs)

attribs %>%
  group_by(Intensity) %>%
  #reframe(range(Value))
  summarise(Min = min(Value), Max = max(Value)) %>%
  arrange(Max)

# Intensity                      Min      Max
# (level 1) Low-Intensity         43     8419
# (level 2) Medium-Intensity    8420    16137
# (level 3) High-Intensity     16138   886724





##  Carbon sequestration - Use ####
if(WhereAmI == "mac"){
  list.files("/Users/xavi_rp/Documents/D5_FFGRCC/CARBON_SEQUESTRATION/use")
}else{
  list.files("/eos/jeodpp/home/users/rotllxa/KIPINCA/CARBON_SEQUESTRATION/use/")
}

# Description:
# CO_2 uptake for all ecosystems expressed in tonnes per km^2 based on reported LULUCF statistics.
# 

if(WhereAmI == "mac"){
  carb_seq_use_tonnes <- rast("/Users/xavi_rp/Documents/D5_FFGRCC/CARBON_SEQUESTRATION/use/use_tonnes.tif")
}else{
  carb_seq_use_tonnes <- rast("/eos/jeodpp/home/users/rotllxa/KIPINCA/CARBON_SEQUESTRATION/use/use_tonnes.tif")
}


#carb_seq_use_tonnes_2 <- carb_seq_use_tonnes[[2]]  # bands 1-4 = 2000, 2006, 2012, 2018
#carb_seq_use_tonnes_3 <- carb_seq_use_tonnes[[3]]  # bands 1-4 = 2000, 2006, 2012, 2018
#
#carb_seq_use_tonnes_dif <- carb_seq_use_tonnes_2 - carb_seq_use_tonnes_3
#carb_seq_use_tonnes_dif <- abs(carb_seq_use_tonnes_2 - carb_seq_use_tonnes_3)
#summary(values(carb_seq_use_tonnes_dif))
#summary(abs(values(carb_seq_use_tonnes_dif)))
#quantile(abs(values(carb_seq_use_tonnes_dif)), seq(0, 1, 0.1), na.rm = TRUE)
#quantile(abs(values(carb_seq_use_tonnes_dif)), seq(0, 1, 0.05), na.rm = TRUE)
#mean(abs(values(carb_seq_use_tonnes_dif)), na.rm = TRUE)
#sd(abs(values(carb_seq_use_tonnes_dif)), na.rm = TRUE)
#
#
#ggplot() + 
#  tidyterra::geom_spatraster(data = carb_seq_use_tonnes_dif)  +
#  viridis::scale_fill_viridis(discrete = FALSE,
#                              option = "turbo", direction = - 1,
#                              na.value = "transparent")
#  


carb_seq_use_tonnes <- carb_seq_use_tonnes[[2]]  # bands 1-4 = 2000, 2006, 2012, 2018

carb_seq_use_tonnes <- project(carb_seq_use_tonnes, archetypes_1)
plot(archetypes_1[[1]])
plot(carb_seq_use_tonnes)
carb_seq_use_tonnes


crb_seq <- ggplot() + 
  tidyterra::geom_spatraster(data = carb_seq_use_tonnes, aes(fill = use_tonnes_2)) +
  viridis::scale_fill_viridis(discrete = FALSE, name = "Tonnes per km^2",
                              option = "cividis", direction = - 1,
                              na.value = "transparent") +
  labs(title = "Carbon sequestration (INCA)") +
  theme(plot.title = element_text(hjust = 0.5))


jpeg("CarbonSequestrationInca.jpg", width = 20, height = 15, units = "cm", res = 150)
crb_seq
dev.off()



archetypes_CarbonSeq <- archetypes_1
archetypes_CarbonSeq$Carbon_seq <- carb_seq_use_tonnes

archetypes_CarbonSeq


archetypes_CarbonSeq_dt <- data.table(values(archetypes_CarbonSeq, dataframe = TRUE), 
                                      keep.rownames = TRUE)
archetypes_CarbonSeq_dt 
archetypes_CarbonSeq_dt <- na.omit(archetypes_CarbonSeq_dt)   # rn is row names (cell number of the original raster)
archetypes_CarbonSeq_dt 


archetypes_CarbonSeq_results <- archetypes_CarbonSeq_dt %>%
  group_by(Crop_system, Intensity) %>%
  summarize(
    n = n(),
    Min = min(Carbon_seq), Max = max(Carbon_seq), 
    Mean = mean(Carbon_seq)) # 

View(archetypes_CarbonSeq_results) 

png("Archetypes_Carbon_Sequestration_summary.png", height = 25*nrow(archetypes_CarbonSeq_results), width = 100*ncol(archetypes_CarbonSeq_results))
gridExtra::grid.table(archetypes_CarbonSeq_results)
dev.off()




p1 <- ggplot(archetypes_CarbonSeq_results, aes(fill = Intensity, x = Crop_system, y = Mean)) + 
  geom_bar(position="dodge", stat="identity") +
  viridis::scale_fill_viridis(discrete = T) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title = "Archetypes and Carbon Sequestration",
       y ="Carbon Sequestration Average (tonnes/km^2)", x = "Crop System") +
  theme(plot.title = element_text(hjust = 0.5))

jpeg("Archetypes_Carbon_Sequestration.jpg", width = 20, height = 15, units = "cm", res = 150)
p1
dev.off()



## NUTS3 regions ####
  library(giscoR)
# Gisco maps
# https://ropengov.github.io/giscoR/

nuts3 <- gisco_get_nuts(
  year = "2021",
  epsg = "3035",
  resolution = "10",
  nuts_level = "3")

nuts3
#plot(nuts3[nuts3$NUTS_ID == "TR621", ][1])

nuts3_rast <- rasterize(nuts3, archetypes_CarbonSeq, field = "NUTS_ID")
nuts3_rast
#nuts3_rast <- mask(nuts3_rast, mask = archetypes_CarbonSeq["Crop_system"])
#

nuts2 <- gisco_get_nuts(
  year = "2021",
  epsg = "3035",
  resolution = "10",
  nuts_level = "2")

nuts2
#plot(nuts2[nuts2$NUTS_ID == "TR621", ][1])

nuts2_rast <- rasterize(nuts2, archetypes_CarbonSeq, field = "NUTS_ID")
nuts2_rast
#nuts2_rast <- mask(nuts2_rast, mask = archetypes_CarbonSeq["Crop_system"])
#

### Baseline ####

archetypes_CarbonSeq$NUTS2 <- nuts2_rast
archetypes_CarbonSeq

plot(archetypes_CarbonSeq[["NUTS2"]])
plot(archetypes_CarbonSeq[["Intensity"]], col = c("green", "yellow", "orange"))


archetypes_CarbonSeq_dt <- data.table(values(archetypes_CarbonSeq, dataframe = TRUE), 
                                      keep.rownames = TRUE)
archetypes_CarbonSeq_dt 
archetypes_CarbonSeq_dt <- na.omit(archetypes_CarbonSeq_dt)   # rn is row names (cell number of the original raster)
archetypes_CarbonSeq_dt 

Carbon_seq_NUTS2 <- archetypes_CarbonSeq_dt %>%
  group_by(NUTS2) %>%
  summarise(Carbon_seq_NUTS2 = sum(Carbon_seq, na.rm = TRUE)) #%>%
#  mutate(Carbon_seq_NUTS2 = sum(Carbon_seq, na.rm = TRUE)) %>%
#  mutate(Carbon_seq_NUTS2 = replace(Carbon_seq_NUTS2, is.na(NUTS2), NA))

Carbon_seq_NUTS2
sum(is.na(Carbon_seq_NUTS2))

plot(archetypes_CarbonSeq["Crop_system"])
plot(archetypes_CarbonSeq["NUTS2"])
length(unique(values(archetypes_CarbonSeq["NUTS2"])))
length(unique(Carbon_seq_NUTS2$NUTS2))
#archetypes_CarbonSeq$Carbon_seq_NUTS2 <- Carbon_seq_NUTS2$Carbon_seq_NUTS2
#archetypes_CarbonSeq
#
#ggplot() +
#  tidyterra::geom_spatraster(data = archetypes_CarbonSeq, aes(fill = Carbon_seq_NUTS2)) +
#  viridis::scale_fill_viridis(direction = -1, na.value = "transparent") +
#  labs(fill = "Total carbon \nsequestration \n(tonnes/km^2)")
#
#View(Carbon_seq_NUTS2)
#
nuts2_carbSeqTotal <- merge(nuts2["NUTS_ID"], Carbon_seq_NUTS2, 
                            by.x = "NUTS_ID", by.y = "NUTS2")
nuts2_carbSeqTotal
sum(is.na(nuts2_carbSeqTotal$Carbon_seq_NUTS2))
length(unique(nuts2_carbSeqTotal$NUTS_ID))

#plot(nuts2_carbSeqTotal["Carbon_seq_NUTS2"])

p2 <- ggplot() +
  tidyterra::geom_spatvector(data = nuts2_carbSeqTotal, aes(fill = Carbon_seq_NUTS2)) + 
  viridis::scale_fill_viridis(direction = -1, na.value = "transparent") +
  labs(fill = "Total carbon \nsequestration \n(tonnes/km^2)") +
  labs(title = "Baseline") +
  theme(plot.title = element_text(hjust = 0.5))

jpeg("Carbon_Seq_NUTS2_Baseline.jpg", width = 20, height = 15, units = "cm", res = 150)
p2
dev.off()


View(Carbon_seq_NUTS2)


### Scenario 1: Change of land composition ####

## 20% increase 'Specialist_Fruits_Citruss'

archetypes_CarbonSeq
unique(archetypes_CarbonSeq$Crop_system)

plot(archetypes_CarbonSeq["Crop_system"])


archetypes_CarbonSeq %>%
  slice_sample(prop = 0.2)






## Linear model ####

str(data.frame(archetypes_CarbonSeq))
archetypes_CarbonSeq_dt

mdl <- lm(Carbon_seq ~ Crop_system * Intensity,
          data = archetypes_CarbonSeq_dt)
anova(mdl)
summary(mdl)
coef(mdl)
car::qqPlot(resid(mdl))
hist(resid(mdl))
hist(data.frame(archetypes_CarbonSeq)$Carbon_seq)


#mdl1 <- lm(log10(Carbon_seq) ~ Crop_system * Intensity,
#          data = data.frame(archetypes_CarbonSeq))
#car::qqPlot(resid(mdl1))
#hist(resid(mdl1))


#mdl2 <- glm(Carbon_seq ~ Crop_system * Intensity,
#           data = data.frame(archetypes_CarbonSeq), family = Gamma(link = "identity"))
#anova(mdl2)
#summary(mdl2)
#car::qqPlot(resid(mdl2))
#hist(resid(mdl2))

#mdl3 <- lm(Carbon_seq ~ Crop_system + Intensity,
#           data = data.frame(archetypes_CarbonSeq))
#summary(mdl3)


## Removing Outliers

ggplot(archetypes_CarbonSeq_dt, aes(x = Carbon_seq)) +
  geom_boxplot() 

# Z-score method
z_scrs <- as.vector(scale(archetypes_CarbonSeq_dt$Carbon_seq))
outlrs <- abs(z_scrs) > 3  # Common threshold is 3
archetypes_CarbonSeq_dt[outlrs, ]

#archetypes_CarbonSeq_dt_backup <- archetypes_CarbonSeq_dt
archetypes_CarbonSeq_dt <- archetypes_CarbonSeq_dt_backup
archetypes_CarbonSeq_dt <- archetypes_CarbonSeq_dt[!outlrs, ]

mdl4 <- lm(Carbon_seq ~ Crop_system * Intensity,
           data = archetypes_CarbonSeq_dt)
anova(mdl4)
summary(mdl4)
smry <- summary(mdl4)
smry$coefficients

sink("mdl4.txt")
print(summary(mdl4))
sink() 

### Computing estimates ####

contrsts_crop <- emmeans::emmeans(mdl, pairwise ~ Crop_system)
contrsts_crop
plot(contrsts_crop, comparisons = FALSE)

contrsts_intensity <- emmeans::emmeans(mdl, pairwise ~ Intensity)
contrsts_intensity
plot(contrsts_intensity, comparisons = FALSE)
pairs(contrsts_intensity, type = "response")

#emmeans::emmeans(mdl, ~ Crop_system * Intensity)
contrsts <- emmeans::emmeans(mdl, pairwise ~ Crop_system * Intensity)
contrsts  # estimates is directly the change of carbon sequestration from the new archetype 
          # to the reference (grasslands intensity 1 in the model; from the second to the first in the 
          # notation of emmeans::emmeans$contrasts)
          # coeff of crop syst + coeff of intensity + coeff of interaction 
          # (negative for the emmeans estimates)  
view(pairs(contrsts, type = "response"))  # to see only estimates (no estimated marginal means)
plot(contrsts, comparisons = TRUE)


contrasts_estimates <- data.frame(pairs(contrsts, type = "response"))
write.csv(contrasts_estimates, "Archetypes_Carbon_Sequestration_LModelEstimates.csv", row.names = FALSE)

sink("mdl4_estimates.txt")
print(contrasts_estimates)
sink() 

#







# ES: 2006, 2012
# intensity: 2008 (average of data of 2007, 2008 and 2009)
# crop system: 2010







