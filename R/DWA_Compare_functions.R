### Functions for DWA Compare ##################################################

# handle UniCode
hndl_UC <- function(data,cols){
  
  # set container
  count_ncol <-0
  count_non  <-0
  count_ncolu<-0
  count_nonu <-0
  #loop
  
  for(i in cols){
    # sum up
    if(length(which(stri_trans_isnfc(data[,i])==F))>0){
      count_ncol <- count_ncol+1}
    count_non <- count_non+length(which(stri_trans_isnfc(data[,i])==F))
    if(length(unique(data[,i]))-length(unique(stri_trans_nfc(data[,i])))>0){
      count_ncolu <- count_ncolu+1}
    count_nonu <- count_nonu+length(unique(data[,i]))-length(unique(stri_trans_nfc(data[,i])))
    
    # transform all columns
    data[,i] <-stri_trans_nfc(data[,i])
    
  }# end loop
  cat(paste0(count_ncol," columns contain a total of ",count_non, " non UFC strings, leading to a total of ",
             count_nonu, " lesser uniques in ",count_ncolu, " columns."),sep = "\n")
  # return
  return(data)
}# end function

### compare dwa and maurer items and types #####################################
compare_dwa_mau <- function(df,item_name,path_output,write_res=NULL,mode=1){

  df <- hndl_UC(df,8:10)
  # check input data
  check_colnames <-c("lfd","Fragebogen","Ort","GID","LONG","LAT",
                     "org","item","phontype","lextype","belegnr.",
                     "erhebung","korrektur")
  
  if(all.equal(colnames(df),check_colnames)!=T){
    stop("Incorrect colnames")
  }
  # handle 'na' and 999
  df[which(df[8]=="na"),8] <- 999
  df[which(df[9]=="na"),9] <- 999
  df[which(df[10]=="na"),10] <-999
  
  # split df by Datensatz
  dwa <- df[which(df$erhebung=="DWA"),]
  mau <- df[which(df$erhebung=="maurer"),]
  
  colnames(dwa)
  # merge multi places

  dwa_mp <- LinguGeo::mergeMultiPlaces(df = dwa,pos_x = 5,pos_y = 6,col_ls = c(1:ncol(dwa)),class_col_ls = c(1:ncol(dwa)))
  mau_mp <- LinguGeo::mergeMultiPlaces(df = mau,pos_x = 5,pos_y = 6,col_ls = c(1:ncol(mau)),class_col_ls = c(1:ncol(mau)))

  # get unique coordinates
  dwa_mp$xyID <- paste0(dwa_mp$LONG,"_",dwa_mp$LAT)
  mau_mp$xyID <- paste0(mau_mp$LONG,"_",mau_mp$LAT)
  
  # get places which occure in both datasets
  dwa_mau <- dwa_mp[which(dwa_mp$xyID%in%mau_mp$xyID),]
  mau_dwa <- mau_mp[which(mau_mp$xyID%in%dwa_mau$xyID),]
  
  # order dfs
  dwa_srt <- dwa_mau[order(dwa_mau$xyID),]
  mau_srt <- mau_dwa[order(mau_dwa$xyID),]
  
  head(dwa_srt)
  head(mau_srt)
  colnames(dwa_srt)
  colnames(mau_srt)
  
  # cbind datasets
  
  df_compare <- cbind(dwa_srt[,c(1:10,13)],mau_srt[,c(7:10,13)])
  plc_match <- nrow(df_compare)
  # rename
  colnames(df_compare)[7:ncol(df_compare)] <- c("dwa_org","dwa_item","dwa_phontype","dwa_lextype","dwa_korrektur",
                                                "mau_org","mau_item","mau_phontype","mau_lextype","mau_korrektur")
  # add vector for results
  df_compare$item_compare <-999
  df_compare$item_compare_abs <-999
  df_compare$item_levensthein <-999
  df_compare$phontype_compare <-999
  df_compare$phontype_compare_abs <-999
  df_compare$lextype_compare <-999
  df_compare$lextype_compare_abs <-999
  
  # rearrange
  colnames(df_compare)
  df_compare <- df_compare[,c(1:6,7,12,11,16,8,13,17,18,19,9,14,20,21,10,15,22,23)]
  ################################################################################
  
  ### compare datasets and save results
  for(i in 1:nrow(df_compare)){

    
    # if any item or type has 999 set results to NA
    if(any(df_compare[i,c(11,12,16,17,20,21)]==999)){
      df_compare$item_compare[i] <-NA
      df_compare$item_compare_abs[i] <-NA
      df_compare$item_levensthein[i] <-NA
      df_compare$phontype_compare[i] <-NA
      df_compare$phontype_compare_abs[i] <-NA
      df_compare$lextype_compare[i] <-NA
      df_compare$lextype_compare_abs[i] <-NA
    } else {
      
      # compare item
      
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
      
      # set compare item to 1 if any variation matches
      if(df_compare$item_compare[i]>0){
        df_compare$item_compare_abs[i] <-1
      } else {
        df_compare$item_compare_abs[i] <-0
      }
      
      ### Compare item with levensthein
      df_compare$item_levensthein[i] <- stringsim(df_compare$dwa_item[i],df_compare$mau_item[i])
      
      ### compare phontype
      
      # set variables
      items_dwa <- str_split(df_compare$dwa_phontype[i],", ")[[1]]
      items_mau <- str_split(df_compare$mau_phontype[i],", ")[[1]]
      
      # calculate simirarity of nn and dat
      if(length(items_dwa)>=length(items_mau)){
        df_compare$phontype_compare[i] <- length(which(items_dwa%in%items_mau))/length(items_dwa)
      }
      if (length(items_dwa)<length(items_mau)){
        df_compare$phontype_compare[i] <- length(which(items_dwa%in%items_mau))/length(items_mau)
      }
      
      # set compare phontyp to 1 if any variation matches
      if(df_compare$phontype_compare[i]>0){
        df_compare$phontype_compare_abs[i] <-1
      } else {
        df_compare$phontype_compare_abs[i] <-0
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
      
      # set compare lextyp to 1 if any variation matches
      if(df_compare$lextype_compare[i]>0){
        df_compare$lextype_compare_abs[i] <-1
      } else {
        df_compare$lextype_compare_abs[i] <-0
      }
    }# end else
  }# end loop i
  
  # remove places with any NA in items
  df_compare <- df_compare[-which(is.na(df_compare$item_compare)),]
  
  # n places matching (incluing NA)
  plc_match
  plc_NA_remove <- nrow(df_compare)
  
  # Clean Phontype
  # set all phontype to NA if Lextype == 0
  colnames(df_compare)
  df_compare$phontype_cleanLex <- 999
  for(i in 1:nrow(df_compare)){
    if(df_compare$lextype_compare_abs[i]==0){
      df_compare$phontype_cleanLex[i] <- NA
    } else {
      df_compare$phontype_cleanLex[i] <- df_compare$phontype_compare_abs[i]
    }
  }
  
  # calc levensthein for cleanLex
  df_compare$LVDist_cleanLex <- 999
  for(l in 1:nrow(df_compare)){
    if(is.na(df_compare$phontype_cleanLex[l])==F){
      df_compare$LVDist_cleanLex[l] <-stringsim(df_compare$dwa_phontype[l],df_compare$mau_phontype[l])
    } else {
      df_compare$LVDist_cleanLex[l] <- NA
    }
  }
  
  # get result table #############################################################
  
  # save results
  item_compare <-round(sum(df_compare$item_compare)/nrow(df_compare),digits = 4)
  item_compare_abs <-round(sum(df_compare$item_compare_abs)/nrow(df_compare),digits = 4)
  
  phontype_compare <-round(sum(df_compare$phontype_compare)/nrow(df_compare),digits = 4)
  phontype_compare_abs <-round(sum(df_compare$phontype_compare_abs)/nrow(df_compare),digits = 4)
  
  lextype_compare <-round(sum(df_compare$lextype_compare)/nrow(df_compare),digits = 4)
  lextype_compare_abs <-round(sum(df_compare$lextype_compare_abs)/nrow(df_compare),digits = 4)
  
  phontype_cleanLex <- round(sum(df_compare$phontype_cleanLex[which(is.na(df_compare$phontype_cleanLex)==F)])/length(which(is.na(df_compare$phontype_cleanLex)==F)),digits = 4)
  nrow_clean <-paste0(length(which(is.na(df_compare$phontype_cleanLex)==F)), " (",round(length(which(is.na(df_compare$phontype_cleanLex)==F))/nrow(df_compare),digits = 4)," %)")
  
  phontype_cleanLex_LV <- round(sum(df_compare$LVDist_cleanLex[which(is.na(df_compare$LVDist_cleanLex)==F)])/length(which(is.na(df_compare$LVDist_cleanLex)==F)),digits = 4)

  #round(sum(df_compare$phontype_compare_abs[which(is.na(df_compare$phontype_cleanLex)==T)])/nrow(df_compare),digits = 4)
  
  # get result table
  res <-cbind(
    plc_match,
    plc_NA_remove,
    item_compare,
    item_compare_abs,
    
    phontype_compare,
    phontype_compare_abs,
    
    lextype_compare,
    lextype_compare_abs,
    phontype_cleanLex,
    nrow_clean,
    phontype_cleanLex_LV
  )
  
  res <-data.frame(item_name,res)
  print(res)
  
  
  
  # add xyID
  df_compare$xyID <- paste0(df_compare$LONG,"_",df_compare$LAT)
  
  # get similarity for phontype by lextype classes
  if(mode==1){
  phn_lex <- compare_PhonByLexCl_bestFIT(df = df_compare)
  }
  if(mode==2){
    phn_lex <- compare_PhonByLexCl_POS(df = df_compare)
  }
  if(mode==3){
    phn_lex <- compare_PhonByLexCl2(df = df_compare)
  }
  if(write_res == T){
    # write df_compare
    write.xlsx(phn_lex$df,file.path(path_output,paste0(item_name,"_compare.xlsx")),overwrite = T)
  }
  # return
  return(list(res=res,phn_lex=phn_lex$res2,df=phn_lex$df))
  #return(df_compare)
} # end of function

# get similarity for phontype by lextype classes ###############################
compare_PhonByLexCl <- function(df){

  # get unique lextypes in DWA
  string <- paste0(df$dwa_lextype,collapse = ", ")
  lexclass <-unique(unlist((str_split(string,", "))))
  lexclass <-sort(table(unlist((str_split(string,", ")))),decreasing = T)

  # iteration for all lexclass
  for(lc in 1:length(lexclass)){
    
    # add res column for lexclass
    df$new <- NA


    # get position for lexclass 
  df$lexclass <- 999
  for(i in 1:nrow(df)){
    if(any(unlist(str_split(df$dwa_lextype[i],", "))==  names(lexclass[lc]))==T &
       any(unlist(str_split(df$mau_lextype[i],", "))==  names(lexclass[lc]))==T){
      df$lexclass[i] <- 1
    } else {
      df$lexclass[i] <- 0
    }
  }
  
  # save row positions to vector
  vec_row <-which(df$lexclass==1)
  ##############################################################################
  ###
  # add col
  df$reliability <- NA

    # check for LexClass
    if(length(vec_row)>0){
    # loop over all vec_rows
    for(v in 1:length(vec_row)){
      
      ### solve all cases

      # case 1: lextype
      # split string
      strs1 <- unlist(str_split(df$dwa_phontype[vec_row[v]],", "))
      strs2 <- unlist(str_split(df$mau_phontype[vec_row[v]],", "))
      
      # get matrix
      mat <-matrix(nrow = length(strs1),ncol = length(strs2))
      rownames(mat) <-strs1
      colnames(mat) <-strs2
      mat
      
      # loop over all obj
      for(r in 1:length(strs1)){
        for(c in 1:length(strs2)){
          mat[r,c]<-(stringsim(strs1[r],strs2[c]))  
        }
      }
      
      # save best results
      df$new[vec_row[v]] <-max(mat)  

      } # end loop v
    
    lex_name <-   names(lexclass[lc])# need iteration
    similarity <- round(sum(df$phontype_compare_abs[vec_row])/length( vec_row),digits = 4)
    n_entries<-length(vec_row)
    reliability <-round(sum(df$reliability[vec_row])/length(vec_row),digits = 4)
    # save results
    res <-cbind(lex_name,n_entries,similarity,reliability)
    
  

    if(lc==1){
      res2 <-  as.data.frame(res)
      
    }  else {
      res2 <- rbind(res2,res)
    }
    } # else (vec_row ==0) was passiert dann?
  colnames(df)[which(colnames(df)=="new")]<- paste0("LexCl_",names(lexclass[lc]))
  }# end loop lc ################################################################
#return(res2)  
  df <- df[1:(ncol(df)-2)]
  return(list(res2=res2,df=df))
}# end function

# compare phontyp v2 add columns for LV ###############################
compare_PhonByLexCl2 <- function(df){
  
  # get unique lextypes in DWA
  string <- paste0(df$dwa_lextype,collapse = ", ")
  lexclass <-unique(unlist((str_split(string,", "))))
  lexclass <-sort(table(unlist((str_split(string,", ")))),decreasing = T)
  
  # iteration for all lexclass
  for(lc in 1:length(lexclass)){
    
    # add res column for lexclass
    df$new <- NA
    
    
    # get position for lexclass 
    df$lexclass <- 999
    for(i in 1:nrow(df)){
      if(any(unlist(str_split(df$dwa_lextype[i],", "))==  names(lexclass[lc]))==T &
         any(unlist(str_split(df$mau_lextype[i],", "))==  names(lexclass[lc]))==T){
        df$lexclass[i] <- 1
      } else {
        df$lexclass[i] <- 0
      }
    }
    
    # save row positions to vector
    vec_row <-which(df$lexclass==1)
    ##############################################################################
    ###
    # add col
    df$reliability <- NA
    
    # check for LexClass
    if(length(vec_row)>0){
      # loop over all vec_rows
      for(v in 1:length(vec_row)){
        lex_pos_dwa <- which(unlist(str_split(df$dwa_lextype[vec_row[v]],", "))==names(lexclass[lc]) )
        lex_pos_mau <- which(unlist(str_split(df$mau_lextype[vec_row[v]],", "))==names(lexclass[lc]) )
        
        df$new[vec_row[v]] <- stringsim(
          unlist(str_split(df$dwa_phontype[vec_row[v]],", "))[lex_pos_dwa],
          unlist(str_split(df$mau_phontype[vec_row[v]],", "))[lex_pos_mau]
        )
      }
      
      lex_name <-   names(lexclass[lc])# need iteration
      similarity <- round(sum(df$phontype_compare_abs[vec_row])/length( vec_row),digits = 4)
      n_entries<-length(vec_row)
      reliability <-round(sum(df$new[vec_row])/length(vec_row),digits = 4)
      # save results
      res <-cbind(lex_name,n_entries,similarity,reliability)
      
      
      
      if(lc==1){
        res2 <-  as.data.frame(res)
        
      }  else {
        res2 <- rbind(res2,res)
      }
    } # else (vec_row ==0) was passiert dann?
    colnames(df)[which(colnames(df)=="new")]<- paste0("LexCl_",names(lexclass[lc]))
  }# end loop lc ################################################################
  return(list(res2=res2,df=df))
}# end function

GetResLs <- function(ls,ls_name,path_output,write_res=T){
  # forloop
  for(i in 1:length(ls)){
    com <-compare_dwa_mau(df=ls[[i]],item_name = ls_name[[i]],path_output = path_output,write_res=write_res)
    res <- com$res
    df <- com$df
    phn <- com$phn_lex
    phn$item <- ls_name[i]
    
    if(i==1){
      ls_res <- res
      ls_df <- list(df)
      ls_phn <- list(phn)
    } else {
      ls_res <- rbind(ls_res,res)
      ls_df <- c(ls_df,list(df))
      ls_phn <- c(ls_phn,list(phn))
    }
    
  }
  # rbind phn res
  ls_phn <-do.call(rbind,ls_phn)
  
  # aggregate NAs get n places by amount of data
  ls_res$plc_NA_aggregated <-999
  for(i in 1:nrow(ls_res)){
    ls_res$plc_NA_aggregated[i] <-nrow(Reduce(function(x, y) merge(x, y, by = "xyID"), ls_df[1:i]))
  }
  return(list(ls_res=ls_res,ls_df=ls_df,ls_phn=ls_phn))
  
}# end of function

tab2spat <- function(df,pos_x,pos_y,src_proj){
  
  # check for NA
  if(any(is.na(df[,pos_x]))){
    stop("NA detected in X coordinates")
  }
  if(any(is.na(df[,pos_y]))){
    stop("NA detected in Y coordinates")
  }
  # check for numeric coordinates
  if(class(df[,pos_x])!="numeric"){
    df[,pos_x] <- as.numeric(df[,pos_x])
    cat("Converting X coordinates to numeric",sep="\n")
  } else {
    cat("X coordinates are numeric",sep="\n")
  }
  if(class(df[,pos_y])!="numeric"){
    df[,pos_y] <- as.numeric(df[,pos_y])
    cat("Converting Y coordinates to numeric",sep="\n")
  } else {
    cat("Y coordinates are numeric",sep="\n")
  }
  
  # get spatial object
  df_spt <- sp::SpatialPointsDataFrame(df[,pos_x:pos_y],df)
  # set projection
  sp::proj4string(df_spt) <- src_proj
  return(df_spt)
}# end of function

CheckMultiLex <- function(ls,ls_name){
  # forloop
  for(i in 1:length(ls)){
    df <- ls[[i]]
    df <- hndl_UC(df,8:10)
    # check input data
    check_colnames <-c("lfd","Fragebogen","Ort","GID","LONG","LAT",
                       "org","item","phontype","lextype","belegnr.",
                       "erhebung","korrektur")
    
    if(all.equal(colnames(df),check_colnames)!=T){
      stop("Incorrect colnames")
    }
    # handle 'na' and 999
    df[which(df[8]=="na"),8] <- 999
    df[which(df[9]=="na"),9] <- 999
    df[which(df[10]=="na"),10] <-999
    
    # split df by Datensatz
    dwa <- df[which(df$erhebung=="DWA"),]
    mau <- df[which(df$erhebung=="maurer"),]
    
    colnames(dwa)
    # merge multi places
    
    dwa_mp <- LinguGeo::mergeMultiPlaces(df = dwa,pos_x = 5,pos_y = 6,col_ls = c(1:ncol(dwa)),class_col_ls = c(1:ncol(dwa)))
    mau_mp <- LinguGeo::mergeMultiPlaces(df = mau,pos_x = 5,pos_y = 6,col_ls = c(1:ncol(mau)),class_col_ls = c(1:ncol(mau)))
    
    # get unique coordinates
    dwa_mp$xyID <- paste0(dwa_mp$LONG,"_",dwa_mp$LAT)
    mau_mp$xyID <- paste0(mau_mp$LONG,"_",mau_mp$LAT)
    
    # get places which occure in both datasets
    dwa_mau <- dwa_mp[which(dwa_mp$xyID%in%mau_mp$xyID),]
    mau_dwa <- mau_mp[which(mau_mp$xyID%in%dwa_mau$xyID),]
    
    # order dfs
    dwa_srt <- dwa_mau[order(dwa_mau$xyID),]
    mau_srt <- mau_dwa[order(mau_dwa$xyID),]
    
    head(dwa_srt)
    head(mau_srt)
    colnames(dwa_srt)
    colnames(mau_srt)
    
    # cbind datasets
    
    df_compare <- cbind(dwa_srt[,c(1:10,13)],mau_srt[,c(7:10,13)])
    plc_match <- nrow(df_compare)
    # rename
    colnames(df_compare)[7:ncol(df_compare)] <- c("dwa_org","dwa_item","dwa_phontype","dwa_lextype","dwa_korrektur",
                                                  "mau_org","mau_item","mau_phontype","mau_lextype","mau_korrektur")
    
    # get amount of multi entires in Lextyp for both datasets
    n <-length(grep(",",df_compare$dwa_lextype)[grep(",",df_compare$dwa_lextype)%in%grep(",",df_compare$mau_lextype)])
    item<- ls_name[i]

    
    if(i==1){
      ls_n <- n
      ls_item <- item
    } else {
      ls_n <- c(ls_n,n)
      ls_item <- c(ls_item,item)
    }
    
  }
#
  res <-data.frame(ls_item,ls_n)
print(res)
return(res)
  
}# end of function



getMultiLex <- function(df_compare){
# get amount of multi entires in Lextyp for both datasets
n<- length(grep(",",df_compare$dwa_lextype)[grep(",",df_compare$dwa_lextype)%in%grep(",",df_compare$mau_lextype)])
df_compare$MuliLex_dwa <- NA
df_compare$MuliLex_mau <- NA
for(i in 1:nrow(df_compare)){

  df_compare$MuliLex_dwa[i] <-  length(unlist(str_split(c(df_compare$dwa_lextype[i]),",")))
  df_compare$MuliLex_mau[i] <-  length(unlist(str_split(c(df_compare$mau_lextype[i]),",")))
}

# check occurance of multi Lex
dm2 <-length(which(df_compare$MuliLex_dwa>1&df_compare$MuliLex_mau>1))
d3m2 <-length(which(df_compare$MuliLex_dwa>2&df_compare$MuliLex_mau>1))
d2m3 <-length(which(df_compare$MuliLex_dwa>1&df_compare$MuliLex_mau>2))
over3 <-length(which(df_compare$MuliLex_dwa>2&df_compare$MuliLex_mau>2))

res <-rbind(dm2,d3m2,d2m3,over3)
colnames(res) <- "n_multiLex"
rownames(res) <- c(">1 in both",">2 in dwa",">2 in mau",">2 in both")

print(res)


df_multiLex <- df_compare[grep(",",df_compare$dwa_lextype)[grep(",",df_compare$dwa_lextype)%in%grep(",",df_compare$mau_lextype)],c(20,21,16,17)]
return(df_multiLex)
}# end of function


# get similarity for phontype by lextype classes ###############################
compare_PhonByLexCl_bestFIT <- function(df){
  
  # get unique lextypes in DWA
  string <- paste0(df$dwa_lextype,collapse = ", ")
  lexclass <-unique(unlist((str_split(string,", "))))
  lexclass <-sort(table(unlist((str_split(string,", ")))),decreasing = T)
  
  # iteration for all lexclass
  for(lc in 1:length(lexclass)){
    
    # add res column for lexclass
    df$new <- NA
    
    
    # get position for lexclass 
    df$lexclass <- 999
    for(i in 1:nrow(df)){
      if(any(unlist(str_split(df$dwa_lextype[i],", "))==  names(lexclass[lc]))==T &
         any(unlist(str_split(df$mau_lextype[i],", "))==  names(lexclass[lc]))==T){
        df$lexclass[i] <- 1
      } else {
        df$lexclass[i] <- 0
      }
    }
    
    # save row positions to vector
    vec_row <-which(df$lexclass==1)
    ##############################################################################
    ###
    # add col
    df$reliability <- NA
    
    # check for LexClass
    if(length(vec_row)>0){
      # loop over all vec_rows
      for(v in 1:length(vec_row)){
        
        if(any(c( length(unlist(str_split(df$dwa_lextype[vec_row[v]],", "))),
                  length(unlist(str_split(df$mau_lextype[vec_row[v]],", "))) )==1)){
        ### solve all cases
        
        # case 1: lextype
        # split string
        strs1 <- unlist(str_split(df$dwa_phontype[vec_row[v]],", "))
        strs2 <- unlist(str_split(df$mau_phontype[vec_row[v]],", "))
        
        # get matrix
        mat <-matrix(nrow = length(strs1),ncol = length(strs2))
        rownames(mat) <-strs1
        colnames(mat) <-strs2
        mat
        
        # loop over all obj
        for(r in 1:length(strs1)){
          for(c in 1:length(strs2)){
            mat[r,c]<-(stringsim(strs1[r],strs2[c]))  
          }
        }
        
        # save best results
        df$new[vec_row[v]] <-max(mat)  
        } else {
          df$new[vec_row[v]] <- NA
        }
      } # end loop v
      
      lex_name <-   names(lexclass[lc])# need iteration
      
      n_entries<-length(df$new[which(is.na(df$new)==F)])
      reliability <-round(sum(df$new[which(is.na(df$new)==F)])/length(which(is.na(df$new)==F)),digits = 8)
      # save results
      res <-cbind(lex_name,n_entries,reliability)
      
      
      
      if(lc==1){
        res2 <-  as.data.frame(res)
        
      }  else {
        res2 <- rbind(res2,res)
      }
    } # else (vec_row ==0) was passiert dann?
    colnames(df)[which(colnames(df)=="new")]<- paste0("LexCl_",names(lexclass[lc]))
  }# end loop lc ################################################################
  #return(res2)  
  df <- df[1:(ncol(df)-2)]
  return(list(res2=res2,df=df))
}# end function

# compare phontyp v2 add columns for LV ###############################
compare_PhonByLexCl_POS <- function(df){
  
  # get unique lextypes in DWA
  string <- paste0(df$dwa_lextype,collapse = ", ")
  lexclass <-unique(unlist((str_split(string,", "))))
  lexclass <-sort(table(unlist((str_split(string,", ")))),decreasing = T)
  
  # iteration for all lexclass
  for(lc in 1:length(lexclass)){
    
    # add res column for lexclass
    df$new <- NA
    
    
    # get position for lexclass 
    df$lexclass <- 999
    for(i in 1:nrow(df)){
      if(any(unlist(str_split(df$dwa_lextype[i],", "))==  names(lexclass[lc]))==T &
         any(unlist(str_split(df$mau_lextype[i],", "))==  names(lexclass[lc]))==T){
        df$lexclass[i] <- 1
      } else {
        df$lexclass[i] <- 0
      }
    }
    
    # save row positions to vector
    vec_row <-which(df$lexclass==1)
    ##############################################################################
    ###
    # add col
    df$reliability <- NA
    
    # check for LexClass
    if(length(vec_row)>0){
      # loop over all vec_rows
      for(v in 1:length(vec_row)){
        if(any(c( length(unlist(str_split(df$dwa_lextype[vec_row[v]],", "))),
                  length(unlist(str_split(df$mau_lextype[vec_row[v]],", "))) )==1)){
        lex_pos_dwa <- which(unlist(str_split(df$dwa_lextype[vec_row[v]],", "))==names(lexclass[lc]) )
        lex_pos_mau <- which(unlist(str_split(df$mau_lextype[vec_row[v]],", "))==names(lexclass[lc]) )
        
        df$new[vec_row[v]] <- stringsim(
          unlist(str_split(df$dwa_phontype[vec_row[v]],", "))[lex_pos_dwa],
          unlist(str_split(df$mau_phontype[vec_row[v]],", "))[lex_pos_mau]
        )
      
      } else {
        df$new[vec_row[v]] <- NA
      }
      } # end loop v
      lex_name <-   names(lexclass[lc])# need iteration
      
      n_entries<-length(df$new[which(is.na(df$new)==F)])
      reliability <-round(sum(df$new[which(is.na(df$new)==F)])/length(which(is.na(df$new)==F)),digits = 8)
      # save results
      res <-cbind(lex_name,n_entries,reliability)

      
      
      if(lc==1){
        res2 <-  as.data.frame(res)
        
      }  else {
        res2 <- rbind(res2,res)
      }
      
      
    } # else (vec_row ==0) was passiert dann?
    colnames(df)[which(colnames(df)=="new")]<- paste0("LexCl_",names(lexclass[lc]))
  }# end loop lc ################################################################
  return(list(res2=res2,df=df))
}# end function
