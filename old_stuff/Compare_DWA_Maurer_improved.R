### DWA Maurer Compare

# setup environment
require(openxlsx)
require(LinguGeo)
require(stringr)
require(stringdist)
wd <- "C:/Envimaster/DWA_Compare"
dat <- file.path(wd,"Data")

# source function
source("C:/Envimaster/DWA/R/Bracket_cleansing.R")

# load data
df <- read.xlsx(file.path(dat,"DWA_Maurer_Begräbnis_03_2023.xlsx"))
df2 <- read.xlsx(file.path(dat,"DWA_Maurer_Ameise_clean_org_0504.xlsx"))
df3 <- read.xlsx(file.path(dat,"DWA_Maurer_Ziege_1204_2023.xlsx"))
colnames(df)
colnames(df2)

# convert new data into old format #############################################

df2 <- df2[,c(1:10,12:15,17,16)]
colnames(df2) <-c("ID","Datum","Bearbeiten/in", "Digi_Index","GID","LONG","LAT",
                  "Buchstaben","Ort","Provinz","item","phontype","lextype","Belegnr","Korrektur","Datensatz" )
df <- df2
################################################################################

# convert df3 to old format ####################################################
colnames (df3)
colnames(df2)
# add dummy cols
df3$d1 <- 999
df3$d2<- 999
df3$d3<- 999
df3$d4<- 999
df3$d5<- 999

df3 <- df3[,c(1,17,16,2,4,5,6,15,3,14,8,9,10,12,13,11)]
colnames(df3) <-c("ID","Datum","Bearbeiten/in", "Digi_Index","GID","LONG","LAT",
                  "Buchstaben","Ort","Provinz","item","phontype","lextype","Belegnr","Korrektur","Datensatz" )
df3$Datensatz[which(df3$Datensatz=="maurer")]<-"Maurer"
df<-df3
################################################################################

# remove unneeded column
df <- df[,1:16]

# check phonotyp and lextyp for NA and rfeplace by 0
df$lextype[which(is.na(df$lextype))] <-0
df$phontype[which(is.na(df$phontype))] <-0
df[which(is.na(df[,11])),] <-0

# split df by Datensatz
dwa <- df[which(df$Datensatz=="DWA"),]
mau <- df[which(df$Datensatz=="Maurer"),]

head(dwa)
# clean DWA
#dwa <-clean_dwa(df = dwa,11)

head(dwa)
# merge multi places
dwa_mp <- LinguGeo::mergeMultiPlaces(df = dwa,pos_x = 6,pos_y = 7,col_ls = c(5,11,12,13),class_col_ls = c(5,11,12,13))
mau_mp <- LinguGeo::mergeMultiPlaces(df = mau,pos_x = 6,pos_y = 7,col_ls = c(5,11,12,13),class_col_ls = c(5,11,12,13))

# reduce data to matching places !!! dones not work by ortsanem or GID 
#dwa_mau <- dwa_mp[which(dwa_mp$GID%in%mau_mp$GID),]
#mau_dwa <- mau_mp[which(mau_mp$GID%in%dwa_mau$GID),]
#
#dwa_mau <- dwa_mp[which(dwa_mp$Ort%in%mau_mp$Ort),]
#mau_dwa <- mau_mp[which(mau_mp$Ort%in%dwa_mau$Ort),]

# add unique coordinates
dwa_mp$xyID <- paste0(dwa_mp$LONG,"_",dwa_mp$LAT)
mau_mp$xyID <- paste0(mau_mp$LONG,"_",mau_mp$LAT)

dwa_mau <- dwa_mp[which(dwa_mp$xyID%in%mau_mp$xyID),]
mau_dwa <- mau_mp[which(mau_mp$xyID%in%dwa_mau$xyID),]

# order dfs
dwa_srt <- dwa_mau[order(dwa_mau$xyID),]
mau_srt <- mau_dwa[order(mau_dwa$xyID),]

head(dwa_srt)
head(mau_srt)
colnames(dwa_srt)
colnames(mau_srt)
# merge tables by cbind
df_compare <- cbind(dwa_srt[,c(1:13,18)],mau_srt[,c(11:13,18)])
# rename
colnames(df_compare)[11:ncol(df_compare)] <- c("dwa_item","dwa_phonotyp","dwa_lextype","dwa_id",
                                               "mau_item","mau_phonotyp","mau_lextype","mau_id")

colnames(df_compare)

# add vector for results
df_compare$item_compare <-999
df_compare$item_levensthein <-999
df_compare$phtype_compare <-999
df_compare$lextype_compare <-999
df_compare$id_compare <-999

# rearrange
df_compare <- df_compare[,c(1:10,11,15,19,20,12,16,21,13,17,22,14,18,23)]
################################################################################

# compare
for(i in 1:nrow(df_compare)){
  print(i)
  
  ### compare item
  
  # set variables
  items_dwa <- str_split(df_compare$dwa_item[i],", ")[[1]]
  items_mau <- str_split(df_compare$mau_item[i],", ")[[1]]
  
  # calculate simirarity of nn and dat
  if(length(items_dwa)>=length(items_mau)){
    df_compare$item_compare[i] <- length(which(items_dwa%in%items_mau))/length(items_dwa)
  }
  if (length(items_dwa)<length(items_mau)){
    df_compare$item_compare[i] <- length(which(items_dwa%in%items_mau))/length(items_mau)
  }
  
  ### Compare item with levensthein
  df_compare$item_levensthein[i] <- stringsim(df_compare$dwa_item[i],df_compare$mau_item[i])

  ### compare phonotyp
  
  # set variables
  items_dwa <- str_split(df_compare$dwa_phonotyp[i],", ")[[1]]
  items_mau <- str_split(df_compare$mau_phonotyp[i],", ")[[1]]
  
  # calculate simirarity of nn and dat
  if(length(items_dwa)>=length(items_mau)){
    df_compare$phtype_compare[i] <- length(which(items_dwa%in%items_mau))/length(items_dwa)
  }
  if (length(items_dwa)<length(items_mau)){
    df_compare$phtype_compare[i] <- length(which(items_dwa%in%items_mau))/length(items_mau)
  }
  ### compare lextype
  
  # set variables
  items_dwa <- str_split(df_compare$dwa_lextype[i],", ")[[1]]
  items_mau <- str_split(df_compare$mau_lextype[i],", ")[[1]]
  
  # calculate simirarity of nn and dat
  if(length(items_dwa)>=length(items_mau)){
    df_compare$lextype_compare[i] <- length(which(items_dwa%in%items_mau))/length(items_dwa)
  }
  if (length(items_dwa)<length(items_mau)){
    df_compare$lextype_compare[i] <- length(which(items_dwa%in%items_mau))/length(items_mau)
  }

  ### compare geographic id
  if(df_compare$dwa_id[i]==df_compare$mau_id[i]){
   df_compare$id_compare[i] <- 1
  } else {
    df_compare$id_compare[i] <- 0
  }
  
  }# end loop i

# levensthein correction by item compare
df_compare$levensthein_correction <-df_compare$item_levensthein
df_compare$levensthein_correction[which(df_compare$item_compare>0)] <-1

# results
sum(df_compare$item_compare)/nrow(df_compare)
sum(df_compare$item_levensthein)/nrow(df_compare)
sum(df_compare$levensthein_correction)/nrow(df_compare)
sum(df_compare$phtype_compare)/nrow(df_compare)
sum(df_compare$lextype_compare)/nrow(df_compare)
table(df_compare$id_compare)

# show not matching
colnames(df_compare)
df_compare[which(df_compare$item_compare<0.5),11:13]

df_compare[which(df_compare$phtype_compare<0.5),15:17]

df_compare[which(df_compare$lextype_compare<0.5),18:20]

write.xlsx(df_compare,"C:/Envimaster/DWA_Compare/Ameise_compare_result.xlsx")

# plot with mapview
require(mapview)
require(raster)
# get spatial object
compare <- sp::SpatialPointsDataFrame(df_compare[,6:7],df_compare)
sp::proj4string(compare) <-"+proj=longlat +datum=WGS84 +no_defs" # WGS 84 (Long / Lat)

mapview(compare[which(compare$phtype_compare<0.5),])# does not run, need to save in new var
item <-compare[which(compare$item_levensthein<0.5),]
phonotyp <-compare[which(compare$phtype_compare<0.5),]
lextype <-compare[which(compare$lextype_compare<0.5),]
mapview(item)
mapview(phonotyp)
mapview(lextype)
