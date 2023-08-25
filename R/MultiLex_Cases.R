### MultiLex DWA Compare
# check amoumnt of MultiLex and Get MultiLex cases in data

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

comp1 <- read.xlsx(file.path(path_res,"Ameise_compare.xlsx"))
comp2 <- read.xlsx(file.path(path_res,"Begräbnis_compare.xlsx"))
comp3 <- read.xlsx(file.path(path_res,"Ziege_compare.xlsx"))
comp4 <- read.xlsx(file.path(path_res,"Deichsel_compare.xlsx"))
comp5 <- read.xlsx(file.path(path_res,"Gurke_compare.xlsx"))
comp6 <- read.xlsx(file.path(path_res,"Pflaume_compare.xlsx"))
comp7 <- read.xlsx(file.path(path_res,"Hebamme_compare.xlsx"))
comp8 <- read.xlsx(file.path(path_res,"Hagebutte_v3_compare.xlsx"))

################################################################################
# set lists
ls <- list(comp1,comp2,comp3,comp4,comp5,comp6,comp7,comp8)

# get amount of cases
lng <-lapply(1:length(ls), function(x){
  lng <- nrow(ls[[x]])

})
sum(unlist(lng))



ls <- list(dat1,dat2,dat3,dat4,dat5,dat6,dat7,dat8)
ls_name <- c("Ameise","Begräbnis","Ziege","Deichsel","Gurke","Pflaume","Hebamme","Hagebutte")

CheckMultiLex(ls,ls_name)

# get MultiLex Cases
ml1 <- getMultiLex(comp1)
ml2 <- getMultiLex(comp2)
ml3 <- getMultiLex(comp3)
ml4 <- getMultiLex(comp4)
ml5 <- getMultiLex(comp5)
ml6 <- getMultiLex(comp6)
ml7 <- getMultiLex(comp7)
ml8 <- getMultiLex(comp8)

lsc <- list(comp1,comp2,comp3,comp4,comp5,comp6,comp7,comp8)
test <-lapply(lsc,FUN = function(x){
  getMultiLex(x)
})
test
Multi_Lex_Cases <-do.call(rbind,test)
Multi_Lex_Cases$Lextype_Format <- NA
Multi_Lex_Cases$LV_Methode <- NA

Multi_Lex_Cases
write.xlsx(Multi_Lex_Cases,file.path(wd,"Multi_Lex_Cases.xlsx"))
