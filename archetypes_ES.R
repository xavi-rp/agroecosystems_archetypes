



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



## Rega's crop types (Rega et al. 2020) ####

## Rega's intensity classes (3)
Energy_input_2008_fille04_no_labour <- rast(paste0("/eos/jeodpp/data/projects/REFOCUS/data/BIODIVERSITY/Rega/Energy_input_raw data/",
                                                   "Energy_input_2008_fille04_no_labour.tif"))
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

crop_systems_from_Rega_1km_char <- rast(paste0("/eos/jeodpp/home/users/rotllxa/Birds_Map_Indicators/",
                                               "crop_systems_from_Rega_1km_char.tif"))
crop_systems_from_Rega_1km_char   # crop systems aggregated to 1km
cats(crop_systems_from_Rega_1km_char)[[1]]

#plot(crop_systems_from_Rega_1km_char)  # crop systems aggregated to 1km
ggplot() + tidyterra::geom_spatraster(data = crop_systems_from_Rega_1km_char, aes(fill = code_char)) 


## Merging both -> archetypes (1km, 3 intensity classes)
Energy_input_2008_fille04_no_labour <- project(Energy_input_2008_fille04_no_labour, crop_systems_from_Rega_1km_char)
Energy_input_2008_fille04_no_labour
plot(Energy_input_2008_fille04_no_labour)

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

# Intensity          Min    Max
# Low-Intensity       43   8419
# Medium-Intensity  8420  16137
# High-Intensity   16138 886724





##  Carbon sequestration - Use ####

list.files("/eos/jeodpp/home/users/rotllxa/KIPINCA/CARBON_SEQUESTRATION/use/")

# Description:
# CO_2 uptake for all ecosystems expressed in tonnes per km^2 based on reported LULUCF statistics.
# 

carb_seq_use_tonnes <- rast("/eos/jeodpp/home/users/rotllxa/KIPINCA/CARBON_SEQUESTRATION/use/use_tonnes.tif")

carb_seq_use_tonnes <- carb_seq_use_tonnes[[2]]  # bands 1-4 = 2000, 2006, 2012, 2018
carb_seq_use_tonnes

carb_seq_use_tonnes <- project(carb_seq_use_tonnes, archetypes_1)
plot(archetypes_1[[1]])
plot(carb_seq_use_tonnes)
carb_seq_use_tonnes


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
    Mean = mean(Carbon_seq)) # archetypes_CarbonSeq_results %>% View()

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

mdl <- lm(Carbon_seq ~ Crop_system * Intensity,
          data = data.frame(archetypes_CarbonSeq))
anova(mdl)
summary(mdl)
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

emmeans::emmeans(mdl, pairwise ~ Crop_system)
emmeans::emmeans(mdl, pairwise ~ Crop_system * Intensity)
#emmeans::emmeans(mdl, ~ Crop_system * Intensity)
contrsts <- emmeans::emmeans(mdl, pairwise ~ Crop_system * Intensity)
contrsts
view(pairs(contrsts, type = "response"))
plot(contrsts, comparisons = TRUE)
