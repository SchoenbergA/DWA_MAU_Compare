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
    print(i)
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
compare_dwa_mau <- function(df,item_name,path_output,write_res=NULL,n_table=5){
  
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
    nrow_clean
  )
  
  res <-data.frame(item_name,res)
  print(res)
  
  if(write_res == T){
  # write df_compare
  write.xlsx(df_compare,file.path(path_output,paste0(item_name,"_compare.xlsx")))
  }
  
  # add xyID
  df_compare$xyID <- paste0(df_compare$LONG,"_",df_compare$LAT)
  
  # get similarity for phontype by lextype classes
  phn_lex <- compare_PhonByLexCl(df = df_compare,n_freq = n_table)
  # return
  return(list(res=res,df_compare=phn_lex$df_compare,phn_lex=phn_lex$res2))
  #return(df_compare)
} # end of function

# get similarity for phontype by lextype classes ###############################
compare_PhonByLexCl <- function(df,n_freq){
  # get frequency of lextypes (based on DWA & in Mau)
  sort(table(df$dwa_lextype),decreasing = T)
  tbl <- sort(table(df$dwa_lextype),decreasing = T)
  
  # add col
  df$phn_class_dwa <- NA
  df$phn_class_mau <- NA
  df$phn_class_abs <- NA
  df$lev_scale_phn_LexClean <- NA
  df$lev_abs_phn_LexClean <- NA
  df$reliability <- NA
  
  # loop over first i entries in table()
  for(i in 1:n_freq){
    lex_name <- names(tbl[i])
    n_entries <- length(which(df$dwa_lextype==names(tbl[i]) & df$lextype_compare_abs==1))
    similarity <- round(sum(df$phontype_compare_abs[which(df$dwa_lextype==names(tbl[i]) & df$lextype_compare_abs==1)])/
                          length( which(df$dwa_lextype==names(tbl[i]) & df$lextype_compare_abs==1)),digits = 4)
    # save results
    res <-cbind(lex_name,n_entries,similarity)
    
    if(i==1){
      res2 <-  as.data.frame(res)
      
    }  else {
      res2 <- rbind(res2,res)
    }
    
    # get vector with row positions ############################################
    vec_row <-which(df$dwa_lextype==names(tbl[i]) & df$lextype_compare_abs==1)
    
    # loop over all vec_rows
    for(v in 1:length(vec_row)){
    # print(paste0(df$dwa_phontype[vec_row[v]], "_" ,df$mau_phontype[vec_row[v]]))
    
      stringsim(df$dwa_phontype[vec_row[v]],df$mau_phontype[vec_row[v]])
      stringdist(df$dwa_phontype[vec_row[v]],df$mau_phontype[vec_row[v]])
      
      # split string
      strs1 <- unlist(str_split(df$dwa_phontype[vec_row[v]],", "))
      strs2 <- unlist(str_split(df$mau_phontype[vec_row[v]],", "))
      
      # get matrix
      mat <-matrix(nrow = length(strs1),ncol = length(strs2))
      rownames(mat) <-strs1
      colnames(mat) <-strs2
      mat
      
      # loop over all obj
      for(i in 1:length(strs1)){
        for(k in 1:length(strs2)){
          mat[i,k]<-(stringsim(strs1[i],strs2[k]))  
        }
      }
      
      # get matrix
      mat2 <-matrix(nrow = length(strs1),ncol = length(strs2))
      rownames(mat2) <-strs1
      colnames(mat2) <-strs2
      mat2
      
      # loop over all obj
      for(i in 1:length(strs1)){
        for(k in 1:length(strs2)){
          mat2[i,k]<-(stringdist(strs1[i],strs2[k]))  
        }
      }
      
      mat
      mat2
      
      # which item has more characters
      maxchar <-max(nchar(rownames(mat2)[which(mat2==min(mat2),arr.ind = T)[1]]),# select rowname from which array
      nchar(colnames(mat2)[which(mat2==min(mat2),arr.ind = T)[2]]) )# select colname from which array
      
      df$reliability [vec_row[v]]<-abs((min(mat2)/maxchar)-1)
      
      # get stringdist for max similar pair
      max(mat)
      
      
      df$phn_class_dwa[vec_row[v]]  <- df$dwa_phontype[vec_row[v]]
      df$phn_class_mau[vec_row[v]]  <- df$mau_phontype[vec_row[v]]
      df$phn_class_abs[vec_row[v]]  <- df$phontype_compare_abs[vec_row[v]]
      df$lev_scale_phn_LexClean[vec_row[v]] <-max(mat)  

      df$lev_abs_phn_LexClean[vec_row[v]] <-min(mat2)  
      
      } # end loop v
    
    
    
    
  }# end loop i ################################################################
  return(list(res2=res2,df_compare=df))
}# end function

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
