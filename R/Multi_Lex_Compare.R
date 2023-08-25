### Develop MUliLex cleaning

# setup environment
require(openxlsx)
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

### compare resuölts for differnet methods for only "einduetig" cases
test1 <-compare_dwa_mau(df = dat1,item_name = "Ameise",path_output = wd,F,mode = 1)# best fit
test2 <-compare_dwa_mau(df = dat1,item_name = "Ameise",path_output = wd,F,mode = 2)# bypos
test3 <-compare_dwa_mau(df = dat1,item_name = "Ameise",path_output = wd,F,mode = 3)

res <-cbind(test1$phn_lex,test2$phn_lex,test3$phn_lex[,c(1,2,4)])
write.xlsx(res,file.path(wd,"res_compare_methods.xlsx"),overwrite = T)

res$diff <- as.numeric(res[,3])-as.numeric(res[,6])
res

df_test <- test1$df
write.xlsx(df_test,file.path(wd,"Ameise_Lexclass_bestFIT.xlsx"),overwrite = T)
