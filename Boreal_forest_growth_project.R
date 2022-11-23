#******************************************************************************
# Purpose: R script for making projection
#
# Author: Jiejie Wang (2022-11-23)
#
#******************************************************************************

# generate the dataset on the most recent observation of each tree
# merge with NFI dataset
summary(tree_proj)


# make projection for each tree of each target species


#  balsamfir projection--------------------
balsam_proj=tree_proj[tree_proj$SpeciesFIA==12,]



# prediction set for baseline
balsam_base=balsam_proj[,c("BA9","TreeBA","Moist","Slope","Aspect_Cat", "Max_ST_base","Min_WT_base","FFP_base","MSP_base","PAS_base","CO2_base")]

colnames(balsam_base)[6:11]=c( "Max_ST", "Min_WT", "FFP", "MSP", "PAS","CO2")
balsam_predict_base_1=predict.gbm(balsam_model_1, newdata =balsam_base,n.trees = 1000)
balsam_predict_base_1=as.matrix(balsam_predict_base_1)

balsam_data_proj=cbind(balsam_proj, balsam_predict_base_1)
names(balsam_data_proj)[names(balsam_data_proj)=="V1"]="Project_base_1"


# predicton on 2050s RCP4.5-------------------------
balsam_50s_RCP45=balsam_proj[,c("BA9","TreeBA","Moist","Slope","Aspect_Cat", "Max_ST_50_45","Min_WT_50_45","FFP_50_45","MSP_50_45","PAS_50_45","CO2_50_45")]

colnames(balsam_50s_RCP45)[6:11]=c( "Max_ST", "Min_WT", "FFP", "MSP", "PAS","CO2")
balsam_predict_50s_RCP45_1=predict.gbm(balsam_model_1, newdata =balsam_50s_RCP45,n.trees = 1000)
balsam_predict_50s_RCP45_1=as.matrix(balsam_predict_50s_RCP45_1)


balsam_data_proj=cbind(balsam_data_proj, balsam_predict_50s_RCP45_1)
names(balsam_data_proj)[names(balsam_data_proj)=="V1"]="Project_50s_RCP45_1"



# predicton on 2050s RCP8.5-------------------------
balsam_50s_RCP85=balsam_proj[,c("BA9","TreeBA","Moist","Slope","Aspect_Cat","Max_ST_50_85","Min_WT_50_85","FFP_50_85","MSP_50_85","PAS_50_85","CO2_50_85")]

colnames(balsam_50s_RCP85)[6:11]=c( "Max_ST", "Min_WT", "FFP", "MSP", "PAS","CO2")
balsam_predict_50s_RCP85_1=predict.gbm(balsam_model_1, newdata =balsam_50s_RCP85,n.trees = 1000)
balsam_predict_50s_RCP85_1=as.matrix(balsam_predict_50s_RCP85_1)


balsam_data_proj=cbind(balsam_data_proj, balsam_predict_50s_RCP85_1)
names(balsam_data_proj)[names(balsam_data_proj)=="V1"]="Project_50s_RCP85_1"


# sum per plot scale per hectare
balsam_data_proj_PlotSum_ha=aggregate(cbind(balsam_data_proj$Project_base_1*balsam_data_proj$TreeFactor, 
                                            balsam_data_proj$Project_50s_RCP45_1 * balsam_data_proj$TreeFactor,
                                            balsam_data_proj$Project_50s_RCP85_1 * balsam_data_proj$TreeFactor), 
                                      by=list(balsam_data_proj$UniquePlotID), FUN=sum)
colnames(balsam_data_proj_PlotSum_ha)=c("UniquePlotID","Proj_base_PlotSum_ha","Proj_RCP45_PlotSum_ha","Proj_RCP85_PlotSum_ha")

balsam_data_proj_PlotSum_ha$Species=12






#  blackspruce projection--------------------
black_proj=tree_proj[tree_proj$SpeciesFIA==95,]


# prediction set for baseline
black_base=black_proj[,c("BA9","TreeBA","Moist","Slope","Aspect_Cat", "Max_ST_base","Min_WT_base","FFP_base","MSP_base","PAS_base","CO2_base")]

colnames(black_base)[6:11]=c( "Max_ST", "Min_WT", "FFP", "MSP", "PAS","CO2")
black_predict_base_1=predict.gbm(black_model_1, newdata =black_base,n.trees = 1000)
black_predict_base_1=as.matrix(black_predict_base_1)

black_data_proj=cbind(black_proj, black_predict_base_1)
names(black_data_proj)[names(black_data_proj)=="V1"]="Project_base_1"


# predicton on 2050s RCP4.5-------------------------
black_50s_RCP45=black_proj[,c("BA9","TreeBA","Moist","Slope","Aspect_Cat", "Max_ST_50_45","Min_WT_50_45","FFP_50_45","MSP_50_45","PAS_50_45","CO2_50_45")]

colnames(black_50s_RCP45)[6:11]=c( "Max_ST", "Min_WT", "FFP", "MSP", "PAS","CO2")
black_predict_50s_RCP45_1=predict.gbm(black_model_1, newdata =black_50s_RCP45,n.trees = 1000)
black_predict_50s_RCP45_1=as.matrix(black_predict_50s_RCP45_1)


black_data_proj=cbind(black_data_proj, black_predict_50s_RCP45_1)
names(black_data_proj)[names(black_data_proj)=="V1"]="Project_50s_RCP45_1"



# predicton on 2050s RCP8.5-------------------------
black_50s_RCP85=black_proj[,c("BA9","TreeBA","Moist","Slope","Aspect_Cat","Max_ST_50_85","Min_WT_50_85","FFP_50_85","MSP_50_85","PAS_50_85","CO2_50_85")]

colnames(black_50s_RCP85)[6:11]=c( "Max_ST", "Min_WT", "FFP", "MSP", "PAS","CO2")
black_predict_50s_RCP85_1=predict.gbm(black_model_1, newdata =black_50s_RCP85,n.trees = 1000)
black_predict_50s_RCP85_1=as.matrix(black_predict_50s_RCP85_1)


black_data_proj=cbind(black_data_proj, black_predict_50s_RCP85_1)
names(black_data_proj)[names(black_data_proj)=="V1"]="Project_50s_RCP85_1"


# sum per plot scale per hectare
black_data_proj_PlotSum_ha=aggregate(cbind(black_data_proj$Project_base_1*black_data_proj$TreeFactor, 
                                            black_data_proj$Project_50s_RCP45_1 * black_data_proj$TreeFactor,
                                            black_data_proj$Project_50s_RCP85_1 * black_data_proj$TreeFactor), 
                                      by=list(black_data_proj$UniquePlotID), FUN=sum)
colnames(black_data_proj_PlotSum_ha)=c("UniquePlotID","Proj_base_PlotSum_ha","Proj_RCP45_PlotSum_ha","Proj_RCP85_PlotSum_ha")

black_data_proj_PlotSum_ha$Species=95






#  whitespruce projection--------------------
white_proj=tree_proj[tree_proj$SpeciesFIA==94,]


# prediction set for baseline
white_base=white_proj[,c("BA9","TreeBA","Moist","Slope","Aspect_Cat", "Max_ST_base","Min_WT_base","FFP_base","MSP_base","PAS_base","CO2_base")]

colnames(white_base)[6:11]=c( "Max_ST", "Min_WT", "FFP", "MSP", "PAS","CO2")
white_predict_base_1=predict.gbm(white_model_1, newdata =white_base,n.trees = 1000)
white_predict_base_1=as.matrix(white_predict_base_1)

white_data_proj=cbind(white_proj, white_predict_base_1)
names(white_data_proj)[names(white_data_proj)=="V1"]="Project_base_1"


# predicton on 2050s RCP4.5-------------------------
white_50s_RCP45=white_proj[,c("BA9","TreeBA","Moist","Slope","Aspect_Cat", "Max_ST_50_45","Min_WT_50_45","FFP_50_45","MSP_50_45","PAS_50_45","CO2_50_45")]

colnames(white_50s_RCP45)[6:11]=c( "Max_ST", "Min_WT", "FFP", "MSP", "PAS","CO2")
white_predict_50s_RCP45_1=predict.gbm(white_model_1, newdata =white_50s_RCP45,n.trees = 1000)
white_predict_50s_RCP45_1=as.matrix(white_predict_50s_RCP45_1)


white_data_proj=cbind(white_data_proj, white_predict_50s_RCP45_1)
names(white_data_proj)[names(white_data_proj)=="V1"]="Project_50s_RCP45_1"



# predicton on 2050s RCP8.5-------------------------
white_50s_RCP85=white_proj[,c("BA9","TreeBA","Moist","Slope","Aspect_Cat","Max_ST_50_85","Min_WT_50_85","FFP_50_85","MSP_50_85","PAS_50_85","CO2_50_85")]

colnames(white_50s_RCP85)[6:11]=c( "Max_ST", "Min_WT", "FFP", "MSP", "PAS","CO2")
white_predict_50s_RCP85_1=predict.gbm(white_model_1, newdata =white_50s_RCP85,n.trees = 1000)
white_predict_50s_RCP85_1=as.matrix(white_predict_50s_RCP85_1)


white_data_proj=cbind(white_data_proj, white_predict_50s_RCP85_1)
names(white_data_proj)[names(white_data_proj)=="V1"]="Project_50s_RCP85_1"


# sum per plot scale per hectare
white_data_proj_PlotSum_ha=aggregate(cbind(white_data_proj$Project_base_1*white_data_proj$TreeFactor, 
                                           white_data_proj$Project_50s_RCP45_1 * white_data_proj$TreeFactor,
                                           white_data_proj$Project_50s_RCP85_1 * white_data_proj$TreeFactor), 
                                     by=list(white_data_proj$UniquePlotID), FUN=sum)
colnames(white_data_proj_PlotSum_ha)=c("UniquePlotID","Proj_base_PlotSum_ha","Proj_RCP45_PlotSum_ha","Proj_RCP85_PlotSum_ha")

white_data_proj_PlotSum_ha$Species=94






#  whitebirch projection--------------------
birch_proj=tree_proj[tree_proj$SpeciesFIA==375,]


# prediction set for baseline
birch_base=birch_proj[,c("BA9","TreeBA","Moist","Slope","Aspect_Cat", "Max_ST_base","Min_WT_base","FFP_base","MSP_base","PAS_base","CO2_base")]

colnames(birch_base)[6:11]=c( "Max_ST", "Min_WT", "FFP", "MSP", "PAS","CO2")
birch_predict_base_1=predict.gbm(birch_model_1, newdata =birch_base,n.trees = 1000)
birch_predict_base_1=as.matrix(birch_predict_base_1)

birch_data_proj=cbind(birch_proj, birch_predict_base_1)
names(birch_data_proj)[names(birch_data_proj)=="V1"]="Project_base_1"


# predicton on 2050s RCP4.5-------------------------
birch_50s_RCP45=birch_proj[,c("BA9","TreeBA","Moist","Slope","Aspect_Cat", "Max_ST_50_45","Min_WT_50_45","FFP_50_45","MSP_50_45","PAS_50_45","CO2_50_45")]

colnames(birch_50s_RCP45)[6:11]=c( "Max_ST", "Min_WT", "FFP", "MSP", "PAS","CO2")
birch_predict_50s_RCP45_1=predict.gbm(birch_model_1, newdata =birch_50s_RCP45,n.trees = 1000)
birch_predict_50s_RCP45_1=as.matrix(birch_predict_50s_RCP45_1)


birch_data_proj=cbind(birch_data_proj, birch_predict_50s_RCP45_1)
names(birch_data_proj)[names(birch_data_proj)=="V1"]="Project_50s_RCP45_1"



# predicton on 2050s RCP8.5-------------------------
birch_50s_RCP85=birch_proj[,c("BA9","TreeBA","Moist","Slope","Aspect_Cat","Max_ST_50_85","Min_WT_50_85","FFP_50_85","MSP_50_85","PAS_50_85","CO2_50_85")]

colnames(birch_50s_RCP85)[6:11]=c( "Max_ST", "Min_WT", "FFP", "MSP", "PAS","CO2")
birch_predict_50s_RCP85_1=predict.gbm(birch_model_1, newdata =birch_50s_RCP85,n.trees = 1000)
birch_predict_50s_RCP85_1=as.matrix(birch_predict_50s_RCP85_1)


birch_data_proj=cbind(birch_data_proj, birch_predict_50s_RCP85_1)
names(birch_data_proj)[names(birch_data_proj)=="V1"]="Project_50s_RCP85_1"


# sum per plot scale per hectare
birch_data_proj_PlotSum_ha=aggregate(cbind(birch_data_proj$Project_base_1*birch_data_proj$TreeFactor, 
                                           birch_data_proj$Project_50s_RCP45_1 * birch_data_proj$TreeFactor,
                                           birch_data_proj$Project_50s_RCP85_1 * birch_data_proj$TreeFactor), 
                                     by=list(birch_data_proj$UniquePlotID), FUN=sum)
colnames(birch_data_proj_PlotSum_ha)=c("UniquePlotID","Proj_base_PlotSum_ha","Proj_RCP45_PlotSum_ha","Proj_RCP85_PlotSum_ha")

birch_data_proj_PlotSum_ha$Species=375







#  jack pine projection--------------------
jack_proj=tree_proj[tree_proj$SpeciesFIA==105,]


# prediction set for baseline
jack_base=jack_proj[,c("BA9","TreeBA","Moist","Slope","Aspect_Cat", "Max_ST_base","Min_WT_base","FFP_base","MSP_base","PAS_base","CO2_base")]

colnames(jack_base)[6:11]=c( "Max_ST", "Min_WT", "FFP", "MSP", "PAS","CO2")
jack_predict_base_1=predict.gbm(jack_model_1, newdata =jack_base,n.trees = 1000)
jack_predict_base_1=as.matrix(jack_predict_base_1)

jack_data_proj=cbind(jack_proj, jack_predict_base_1)
names(jack_data_proj)[names(jack_data_proj)=="V1"]="Project_base_1"


# predicton on 2050s RCP4.5-------------------------
jack_50s_RCP45=jack_proj[,c("BA9","TreeBA","Moist","Slope","Aspect_Cat", "Max_ST_50_45","Min_WT_50_45","FFP_50_45","MSP_50_45","PAS_50_45","CO2_50_45")]

colnames(jack_50s_RCP45)[6:11]=c( "Max_ST", "Min_WT", "FFP", "MSP", "PAS","CO2")
jack_predict_50s_RCP45_1=predict.gbm(jack_model_1, newdata =jack_50s_RCP45,n.trees = 1000)
jack_predict_50s_RCP45_1=as.matrix(jack_predict_50s_RCP45_1)


jack_data_proj=cbind(jack_data_proj, jack_predict_50s_RCP45_1)
names(jack_data_proj)[names(jack_data_proj)=="V1"]="Project_50s_RCP45_1"



# predicton on 2050s RCP8.5-------------------------
jack_50s_RCP85=jack_proj[,c("BA9","TreeBA","Moist","Slope","Aspect_Cat","Max_ST_50_85","Min_WT_50_85","FFP_50_85","MSP_50_85","PAS_50_85","CO2_50_85")]

colnames(jack_50s_RCP85)[6:11]=c( "Max_ST", "Min_WT", "FFP", "MSP", "PAS","CO2")
jack_predict_50s_RCP85_1=predict.gbm(jack_model_1, newdata =jack_50s_RCP85,n.trees = 1000)
jack_predict_50s_RCP85_1=as.matrix(jack_predict_50s_RCP85_1)


jack_data_proj=cbind(jack_data_proj, jack_predict_50s_RCP85_1)
names(jack_data_proj)[names(jack_data_proj)=="V1"]="Project_50s_RCP85_1"


# sum per plot scale per hectare
jack_data_proj_PlotSum_ha=aggregate(cbind(jack_data_proj$Project_base_1*jack_data_proj$TreeFactor, 
                                           jack_data_proj$Project_50s_RCP45_1 * jack_data_proj$TreeFactor,
                                           jack_data_proj$Project_50s_RCP85_1 * jack_data_proj$TreeFactor), 
                                     by=list(jack_data_proj$UniquePlotID), FUN=sum)
colnames(jack_data_proj_PlotSum_ha)=c("UniquePlotID","Proj_base_PlotSum_ha","Proj_RCP45_PlotSum_ha","Proj_RCP85_PlotSum_ha")

jack_data_proj_PlotSum_ha$Species=105







#  aspen projection--------------------
aspen_proj=tree_proj[tree_proj$SpeciesFIA==746,]


# prediction set for baseline
aspen_base=aspen_proj[,c("BA9","TreeBA","Moist","Slope","Aspect_Cat", "Max_ST_base","Min_WT_base","FFP_base","MSP_base","PAS_base","CO2_base")]

colnames(aspen_base)[6:11]=c( "Max_ST", "Min_WT", "FFP", "MSP", "PAS","CO2")
aspen_predict_base_1=predict.gbm(aspen_model_1, newdata =aspen_base,n.trees = 1000)
aspen_predict_base_1=as.matrix(aspen_predict_base_1)

aspen_data_proj=cbind(aspen_proj, aspen_predict_base_1)
names(aspen_data_proj)[names(aspen_data_proj)=="V1"]="Project_base_1"


# predicton on 2050s RCP4.5-------------------------
aspen_50s_RCP45=aspen_proj[,c("BA9","TreeBA","Moist","Slope","Aspect_Cat", "Max_ST_50_45","Min_WT_50_45","FFP_50_45","MSP_50_45","PAS_50_45","CO2_50_45")]

colnames(aspen_50s_RCP45)[6:11]=c( "Max_ST", "Min_WT", "FFP", "MSP", "PAS","CO2")
aspen_predict_50s_RCP45_1=predict.gbm(aspen_model_1, newdata =aspen_50s_RCP45,n.trees = 1000)
aspen_predict_50s_RCP45_1=as.matrix(aspen_predict_50s_RCP45_1)


aspen_data_proj=cbind(aspen_data_proj, aspen_predict_50s_RCP45_1)
names(aspen_data_proj)[names(aspen_data_proj)=="V1"]="Project_50s_RCP45_1"



# predicton on 2050s RCP8.5-------------------------
aspen_50s_RCP85=aspen_proj[,c("BA9","TreeBA","Moist","Slope","Aspect_Cat","Max_ST_50_85","Min_WT_50_85","FFP_50_85","MSP_50_85","PAS_50_85","CO2_50_85")]

colnames(aspen_50s_RCP85)[6:11]=c( "Max_ST", "Min_WT", "FFP", "MSP", "PAS","CO2")
aspen_predict_50s_RCP85_1=predict.gbm(aspen_model_1, newdata =aspen_50s_RCP85,n.trees = 1000)
aspen_predict_50s_RCP85_1=as.matrix(aspen_predict_50s_RCP85_1)


aspen_data_proj=cbind(aspen_data_proj, aspen_predict_50s_RCP85_1)
names(aspen_data_proj)[names(aspen_data_proj)=="V1"]="Project_50s_RCP85_1"


# sum per plot scale per hectare
aspen_data_proj_PlotSum_ha=aggregate(cbind(aspen_data_proj$Project_base_1*aspen_data_proj$TreeFactor, 
                                          aspen_data_proj$Project_50s_RCP45_1 * aspen_data_proj$TreeFactor,
                                          aspen_data_proj$Project_50s_RCP85_1 * aspen_data_proj$TreeFactor), 
                                    by=list(aspen_data_proj$UniquePlotID), FUN=sum)
colnames(aspen_data_proj_PlotSum_ha)=c("UniquePlotID","Proj_base_PlotSum_ha","Proj_RCP45_PlotSum_ha","Proj_RCP85_PlotSum_ha")

aspen_data_proj_PlotSum_ha$Species=746




# bind growth projection of all target species
treeSpecies_Plots_Proj=rbind(balsam_data_proj_PlotSum_ha,black_data_proj_PlotSum_ha,
                                 white_data_proj_PlotSum_ha,birch_data_proj_PlotSum_ha,
                                 jack_data_proj_PlotSum_ha,aspen_data_proj_PlotSum_ha)







# limit projection to boreal forest only
# add plot coordinates (latitude and longitude)
# then convert to spatial file


coordinates(treeSpecies_Plots_Proj) <- c('Longitude', 'Latitude')
proj4string(treeSpecies_Plots_Proj)=CRS("+proj=longlat +datum=WGS84 +no_defs")

#subset to boreal zone (from "The extent of the North American boreal zone"
#Authors: J.P. Brandt
#Publication: Environmental Reviews . 12 June 2009 . https://doi.org/10.1139/A09-004

boreal <- readOGR("C:/Users/NABoreal.shp")

#subset to boreal zone (exclude hemiboreal, south of boreal)
boreal=subset(boreal, TYPE %in% c("BOREAL"))
#convert to same projection
boreal <-  spTransform(boreal,crs(Plot_growth))

#subset to boreal zone 
Plot_growth_boreal <- treeSpecies_Plots_Proj[boreal, ] 
