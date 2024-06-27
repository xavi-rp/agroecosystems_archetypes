
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



## Rega's intensity classes (3)
if(WhereAmI == "mac"){
  Energy_input_2008_fille04_no_labour <- rast(paste0("/Users/xavi_rp/Documents/D5_FFGRCC/Rega/Energy_input_raw data/", "Energy_input_2008_fille04_no_labour.tif"))
}else{
  Energy_input_2008_fille04_no_labour <- rast(paste0("/eos/jeodpp/data/projects/REFOCUS/data/BIODIVERSITY/Rega/Energy_input_raw data/",
                                                     "Energy_input_2008_fille04_no_labour.tif"))
}
Energy_input_2008_fille04_no_labour # 1km; 3 intensity classes (1 = low, 2 = medium, 3 = high)


## Rega's crop types

if(WhereAmI == "mac"){
  crop_systems_from_Rega_1km_char <- rast(paste0("/Users/xavi_rp/Documents/D5_FFGRCC/Rega/crop_systems_from_Rega/",
                                                 "crop_systems_from_Rega_1km_char.tif"))
}else{
  crop_systems_from_Rega_1km_char <- rast(paste0("/eos/jeodpp/home/users/rotllxa/Birds_Map_Indicators/",
                                                 "crop_systems_from_Rega_1km_char.tif"))
}
crop_systems_from_Rega_1km_char   # crop systems aggregated to 1km


## Merging both -> archetypes (1km, 3 intensity classes)
Energy_input_2008_fille04_no_labour <- project(Energy_input_2008_fille04_no_labour, crop_systems_from_Rega_1km_char)
Energy_input_2008_fille04_no_labour


# two separated layers
archetypes_1 <- crop_systems_from_Rega_1km_char

archetypes_1$intensity <- Energy_input_2008_fille04_no_labour
archetypes_1
names(archetypes_1) <- c("Crop_system", "Intensity")
archetypes_1



##  Condition    ####
if(WhereAmI == "mac"){
  list.files("/Users/xavi_rp/Documents/D3_iBLUA/Good_condition_crop_v1_eu27_1000m")
}else{
  list.files("")
}

# Description:
# 

if(WhereAmI == "mac"){
  condition_1km <- rast("/Users/xavi_rp/Documents/D3_iBLUA/Good_condition_crop_v1_eu27_1000m/Good_condition_crop_v1_eu27_1000m.tif")
}else{
  condition_1km <- rast("")
}

condition_1km

condition_1km <- project(condition_1km, archetypes_1)
condition_1km
plot(condition_1km)



p_cond <- ggplot() + 
  tidyterra::geom_spatraster(data = condition_1km, aes(fill = Band_1)) +
  viridis::scale_fill_viridis(discrete = FALSE, name = "Condition",
                              option = "cividis", direction = - 1,
                              na.value = "transparent") +
  labs(title = "Agroecosystems condition") +
  theme(plot.title = element_text(hjust = 0.5))


jpeg("Agroecosystems_condition.jpg", width = 20, height = 15, units = "cm", res = 150)
p_cond
dev.off()


archetypes_Cond <- archetypes_1
archetypes_Cond$Condition <- condition_1km

archetypes_Cond


archetypes_Cond_dt <- data.table(values(archetypes_Cond, dataframe = TRUE), 
                                      keep.rownames = TRUE)
archetypes_Cond_dt 
archetypes_Cond_dt <- na.omit(archetypes_Cond_dt)   # rn is row names (cell number of the original raster)
archetypes_Cond_dt 


archetypes_Cond_results <- archetypes_Cond_dt %>%
  group_by(Crop_system, Intensity) %>%
  summarize(
    n = n(),
    Min = round(min(Condition), 2), Max = round(max(Condition), 2), 
    Mean = round(mean(Condition), 2)) # 

archetypes_Cond_results


png("Archetypes_Condition_summary.png", height = 25*nrow(archetypes_Cond_results), width = 100*ncol(archetypes_Cond_results))
gridExtra::grid.table(archetypes_Cond_results)
dev.off()


p1 <- ggplot(archetypes_Cond_results, aes(fill = Intensity, x = Crop_system, y = Mean)) + 
  geom_bar(position="dodge", stat="identity") +
  viridis::scale_fill_viridis(discrete = T) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title = "Archetypes and Ecosystems Condition",
       y ="Condition", x = "Crop System") +
  theme(plot.title = element_text(hjust = 0.5))

jpeg("Archetypes_Condition.jpg", width = 20, height = 15, units = "cm", res = 150)
p1
dev.off()




## Linear model ####

str(data.frame(archetypes_Cond))
archetypes_Cond_dt

mdl <- lm(Condition ~ Crop_system * Intensity,
          data = archetypes_Cond_dt)
anova(mdl)
summary(mdl)
coef(mdl)
car::qqPlot(resid(mdl))
hist(resid(mdl))
hist(data.frame(archetypes_Cond)$condition)


## Removing Outliers

ggplot(archetypes_Cond_dt, aes(x = Condition)) +
  geom_boxplot() 

# Z-score method
z_scrs <- as.vector(scale(archetypes_Cond_dt$Condition))
outlrs <- abs(z_scrs) > 3  # Common threshold is 3
archetypes_Cond_dt[outlrs, ]

#archetypes_Cond_dt_backup <- archetypes_Cond_dt
archetypes_Cond_dt <- archetypes_Cond_dt_backup
archetypes_Cond_dt <- archetypes_Cond_dt[!outlrs, ]

mdl4 <- lm(Condition ~ Crop_system * Intensity,
           data = archetypes_Cond_dt)
anova(mdl4)
summary(mdl4)
smry <- summary(mdl4)
mean(smry$coefficients[-1, 1])
sd(smry$coefficients[-1, 1])







