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
dat1 <- read.xlsx(file.path(dat,"DWA_Maurer_Ameise2.xlsx"),sheet = "Sheet 1")
dat2 <- read.xlsx(file.path(dat,"DWA_Maurer_Begräbnis_new.xlsx"))
dat3 <- read.xlsx(file.path(dat,"DWA_Maurer_Ziege_new.xlsx"))
dat4 <- read.xlsx(file.path(dat,"DWA_Maurer_Deichsel.xlsx"))
dat5 <- read.xlsx(file.path(dat,"DWA_Maurer_Gurke.xlsx"))
dat6 <- read.xlsx(file.path(dat,"DWA_Maurer_Pflaume.xlsx"))
dat7 <- read.xlsx(file.path(dat,"DWA_Maurer_Hebamme.xlsx"))
dat8 <- read.xlsx(file.path(dat,"DWA_Maurer_Hagebutte_v3.xlsx"))


################################################################################

# run compare for specific dataset
cmp1 <- compare_dwa_mau(dat1,"Ameise_improved",path_res,F,mode = 1)

phn <-cmp1$phn_lex
dfr <-cmp1$df

