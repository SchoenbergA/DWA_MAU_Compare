### DWA Maurer Compare

# setup environment
require(openxlsx)
require(LinguGeo)
wd <- "C:/Envimaster/DWA_Compare"
dat <- file.path(wd,"Data")

# load data
df <- read.xlsx(file.path(dat,"DWA_Maurer_Begräbnis_03_2023.xlsx"))

# remove unneeded column
df <- df[,1:16]

# check phonotyp and lextyp for NA and rfeplace by 0
df$lextype[which(is.na(df$lextype))] <-0
df$phontype[which(is.na(df$phontype))] <-0
df$`12..Begräbnis`[which(is.na(df$`12..Begräbnis`))] <-0

# split df by Datensatz
dwa <- df[which(df$Datensatz=="DWA"),]
mau <- df[which(df$Datensatz=="Maurer"),]

head(dwa)
# merge multi places
dwa_mp <- LinguGeo::mergeMultiPlaces(df = dwa,pos_x = 6,pos_y = 7,col_ls = c(5,11,12,13),class_col_ls = c(5,11,12,13))
mau_mp <- LinguGeo::mergeMultiPlaces(df = mau,pos_x = 6,pos_y = 7,col_ls = c(5,11,12,13),class_col_ls = c(5,11,12,13))

# clean DWA
#dwa_mp$`12..Begräbnis` <-gsub("\\s*\\([^\\)]+\\)", "", dwa_mp$`12..Begräbnis`)

# reduce data to matching places !!! dones not work by ortsanem or GID 
dwa_mau <- dwa_mp[which(dwa_mp$GID%in%mau_mp$GID),]
mau_dwa <- mau_mp[which(mau_mp$GID%in%dwa_mau$GID),]

dwa_mau <- dwa_mp[which(dwa_mp$Ort%in%mau_mp$Ort),]
mau_dwa <- mau_mp[which(mau_mp$Ort%in%dwa_mau$Ort),]

# add unique coordinates
dwa_mp$xyID <- paste0(dwa_mp$LONG,"_",dwa_mp$LAT)
mau_mp$xyID <- paste0(mau_mp$LONG,"_",mau_mp$LAT)

dwa_mau <- dwa_mp[which(dwa_mp$xyID%in%mau_mp$xyID),]
mau_dwa <- mau_mp[which(mau_mp$xyID%in%dwa_mau$xyID),]

# order dfs
dwa_srt <- dwa_mau[order(dwa_mau$xyID),]
mau_srt <- mau_dwa[order(mau_dwa$xyID),]

# add vector fpr results
dwa_srt$phtype_compare <- 999
dwa_srt$lxtype_compare <- 999
dwa_srt$equal <- 999


# compare
for(i in 1:nrow(dwa_srt)){
print(i)
    if(dwa_srt$phontype[i]==mau_srt$phontype[i]){
    dwa_srt$phtype_compare[i] <- 1
  } else {
    dwa_srt$phtype_compare[i] <- 0
  }
  if(dwa_srt$lextype[i]==mau_srt$lextype[i]){
    dwa_srt$lxtype_compare[i] <- 1
  } else {
    dwa_srt$lxtype_compare[i] <- 0
  }
  if(dwa_srt$`12..Begräbnis`[i]==mau_srt$`12..Begräbnis`[i]){
    dwa_srt$equal[i] <- 1 
  } else {
    dwa_srt$equal[i] <- 0
  }
  
}
require(stringdist)
dwa_srt$lev <-stringsim(dwa_srt$`12..Begräbnis`,mau_srt$`12..Begräbnis`)

# results
length(which(dwa_srt$phtype_compare==1))
length(which(dwa_srt$lxtype_compare==1))    
length(which(dwa_srt$equal==1))
length(which(dwa_srt$lev==1))

length(which(dwa_srt$phtype_compare==1))/nrow(dwa_srt)
length(which(dwa_srt$lxtype_compare==1))/nrow(dwa_srt)
length(which(dwa_srt$equal==1))/nrow(dwa_srt)
sum(dwa_srt$equal)/nrow(dwa_srt)
sum(df_compare$item_compare)/nrow(dwa_srt)

test <-cbind(dwa_srt$`12..Begräbnis`,mau_srt$`12..Begräbnis`,dwa_srt$equal)
# levenstein
plot(dwa_srt$lev)
sum(dwa_srt$lev)/nrow(dwa_srt)

# add mau to dwa
compare <- dwa_srt[11]
compare$mau <- mau_srt[11]
compare$lev <- dwa_srt[ncol(dwa_srt)]

compare
