### DWA Compare

# setup environment
require(openxlsx)
require(LinguGeo)
require(stringr)
require(stringdist)
require(stringi)

# set path
wd <- "C:/Envimaster/DWA_Compare"
dat <- file.path(wd,"Data")
path_res <-file.path(wd,"Results")

# load functions
source(file.path(wd,"R/DWA_Compare_functions.R"))

# load data
dat1 <- read.xlsx(file.path(dat,"DWA_Maurer_Ameise_new.xlsx"))
dat2 <- read.xlsx(file.path(dat,"DWA_Maurer_Begräbnis_new.xlsx"))
dat3 <- read.xlsx(file.path(dat,"DWA_Maurer_Ziege_new.xlsx"))
dat4 <- read.xlsx(file.path(dat,"DWA_Maurer_Deichsel.xlsx"))
dat5 <- read.xlsx(file.path(dat,"DWA_Maurer_Gurke.xlsx"))
dat6 <- read.xlsx(file.path(dat,"DWA_Maurer_Hagebutte.xlsx"))
dat7 <- read.xlsx(file.path(dat,"DWA_Maurer_Ameise2.xlsx"),sheet = "Sheet 1")

# compare ameise items
res1 <- compare_dwa_mau(df=dat1,item_name = "Ameise",path_res,write_res=F)
res2 <- compare_dwa_mau(df=dat7,item_name = "Ameise2",path_res,write_res=F)
df=dat3
item_name="test"
path_output=wd
write_res=F
n_table=5
res2 <- compare_dwa_mau(df=dat2,item_name = "Begräbnis",path_res,write_res=F,n_table = 5)
dff3 <- res2$df_compare
res2$phn_lex
# get full result table
res <-rbind(res1$res,res2$res)
write.xlsx(res2$phn_lex,file.path(path_res,"Begräbnis_phn_lex.xlsx"))
res2$res

res1$phn_lex
res2$phn_lex

test <- compare_dwa_mau(df=dat3,item_name = "test",path_res,write_res=F,n_table = 150) 
test$phn_lex
################################################################################
# set lists
ls <- list(dat1,dat2,dat3,dat4,dat5,dat6)
ls_name <- c("Ameise","Begräbnis","Ziege","Deichsel","Gurke","Hagebutte")

# forloop
for(i in 1:length(ls)){
  com <-compare_dwa_mau(df=ls[[i]],item_name = ls_name[[i]],path_res,write_res=F)
  res <- com$res
  df <- com$df_compare
  phn <- com$phn_lex
  
  if(i==1){
    ls_res <- res
    ls_df <- list(df)
    ls_phn <- list(phn)
  } else {
    ls_res <- rbind(ls_res,res)
    ls_df <- c(ls_df,list(df))
    ls_phn <- c(ls_phn,list(phn))
  }
  #return(list(ls_res=ls_res,ls_df=ls_df))
}

ls_res
ls_df
ls_phn


# get n places by amount of data
nrow(Reduce(function(x, y) merge(x, y, by = "xyID"), ls_df[1:2]))

ls_res$plc_NA_aggregated <-999
ls_res$plc_NA_aggregated[2] <-nrow(Reduce(function(x, y) merge(x, y, by = "xyID"), ls_df[1:2]))
ls_res$plc_NA_aggregated[3] <-nrow(Reduce(function(x, y) merge(x, y, by = "xyID"), ls_df[1:3]))

ls_res

# loop
ls_res$plc_NA_aggregated <-999
for(i in 1:nrow(ls_res)){
  ls_res$plc_NA_aggregated[i] <-nrow(Reduce(function(x, y) merge(x, y, by = "xyID"), ls_df[1:i]))
}

ls_res <- ls_res[,c(1:3,10,4:9)]
ls_res

write.xlsx(ls_res,file.path(wd,"Ergebnis_neu2.xlsx"))
write.xlsx(ls_phn,file.path(wd,"Ergebnis_neu2_phn.xlsx"))
