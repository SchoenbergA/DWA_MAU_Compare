### DWA Compare

# setup environment
require(openxlsx)
require(LinguGeo)
require(stringr)
require(stringdist)

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


res1 <- compare_dwa_mau(df=dat1,item_name = "Ameise",path_res)
res2 <- compare_dwa_mau(df=dat2,item_name = "Begräbnis",path_res)
res3 <- compare_dwa_mau(df=dat3,item_name = "Ziege",path_res)
                                                                                  
res <-rbind(res1,res2,res3)
res
