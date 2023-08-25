### Get Mapdata

# setup environment
require(openxlsx)
require(rgdal)

# set path
wd <- "C:/Envimaster/DWA_Compare"
dat <- file.path(wd,"Data")
path_res <- file.path(wd,"Results")
path_out <-file.path(wd,"GIS")

# load functions
source(file.path(wd,"R/DWA_Compare_functions.R"))

# load data
dat1 <- read.xlsx(file.path(path_res,"Ameise_compare.xlsx"))
dat2 <- read.xlsx(file.path(path_res,"Begräbnis_compare.xlsx"))
dat3 <- read.xlsx(file.path(path_res,"Ziege_compare.xlsx"))
dat4 <- read.xlsx(file.path(path_res,"Deichsel_compare.xlsx"))
dat5 <- read.xlsx(file.path(path_res,"Gurke_compare.xlsx"))
dat6 <- read.xlsx(file.path(path_res,"Hagebutte_compare.xlsx"))


################################################################################

# set lists
ls <- list(dat1,dat2,dat3,dat4,dat5,dat6)
ls_name <- c("Ameise","Begräbnis","Ziege","Deichsel","Gurke","Hagebutte")
# define projection
src <- "+proj=longlat +datum=WGS84 +no_defs" # WGS 84 (Long / Lat)


# get shp for all places loop
for(i in 1:length(ls)){
  # select item
  df1 <-ls[[i]]
  df1$LONG <- as.numeric(df1$LONG)
  df1$LAT <- as.numeric(df1$LAT)
  # select rows without NA
  dfc <-  df1[which(is.na(df1$phontype_cleanLex)==F),]
  # convert to spatial obj
  df_spat <-tab2spat(dfc,5,6,src)  
  # write shp
  writeOGR(df_spat,file.path(path_out,paste0(ls_name[i],".shp")),driver="ESRI Shapefile",layer="1")
}

# get all places for background and NA
nas <- tab2spat(dat1,5,6,src)
writeOGR(nas,file.path(path_out,"NAs.shp"),driver="ESRI Shapefile",layer="1")


### coherence Testing Area ######################################################
require(LinguGeo)
dcoh <- read.xlsx(file.path(wd,"Results/Ziege_compare.xlsx"))
head(dcoh)


dcoh$LONG <- as.numeric(dcoh$LONG)
dcoh$LAT <- as.numeric(dcoh$LAT)
coh2 <- LinguGeo::coherence(dat = dcoh,cl = "reliability",xcord = 5,ycord = 6,nk = 5,develop = F)
coh$coh[1] <- 0.123
require(ggplot2)
require(viridis)
plot_coh(coh)

head(dat1)
ggplot2::ggplot(dat1,aes(LONG,LAT))+ geom_point(size = 3)+ theme(legend.position = "left") + labs(x = "", 
                                                                y = "", caption = ". NA") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank() )
# plot needs discrete=F warum auch immer
dat1 <-dat4
plt <- ggplot2::ggplot(dat1, aes(LONG, LAT, col = lextype_compare_abs, 
                               )) + geom_point(size = 3) + theme_void()

plt

aggregate_coh(ls = list(coh,coh2))
