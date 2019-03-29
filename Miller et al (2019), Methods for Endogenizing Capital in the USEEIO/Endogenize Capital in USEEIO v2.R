#Code pertaining to: "Methods for Endogenizing Capital in the USEEIO".
# T. Reed Miller1*, Peter Berrill2, Paul Wolfram2, Ranran Wang2, Yookyung Kim2, Xinzhu Zheng2, Edgar G. Hertwich1,2
# 1 Department of Chemical and Environmental Engineering, Yale University, New Haven, USA 
# 2 Center for Industrial Ecology, School of Forestry and Environmental Studies, Yale University, New Haven, USA 
# * Corresponding author: reed.miller@yale.edu


#load necessary packages (install first if needed)
library(readxl); library(dplyr); library(reshape2); library(tidyr); library(stringr)

# Run the code for 2007 and then 2012; the two years are calculated independently
for(year in c('2007','2012')){
  # year <- '2012'  #for diagnosis
    
#BEFORE YOU BEGIN USING THE CODE, REPLACE MY WORKING FILE DIRECTORY PATH WITH YOURS, BE SURE TO USE / NOT \: ----
  yourwd<-"C:/Users/trm43/Box Sync/EGH Retreat Group Files/Fixed Assets/R approach/Miller et al (2019), Methods for Endogenizing Capital in the USEEIO"
  
#The code below follows the structure of the Methods section, under "Overall approach". See Figure 4.

#(a)	Create a detailed CFC table for each Y^K category ----
#Y^K final demand investment categories:
# Nonresidential Private (nrp)
  # 1	Nonresidential private fixed investment in structures 
  # 2	Nonresidential private fixed investment in equipment
  # 3	Nonresidential private fixed investment in IPP
# Residential Private (rp)
  # 4	Residential private fixed investment
# Government (gov)
  # 5	Federal national defense: Gross investment in structures
  # 6	Federal national defense: Gross investment in equipment
  # 7	Federal national defense: Gross investment in IPP
  # 8	Federal nondefense: Gross investment in structures
  # 9	Federal nondefense: Gross investment in equipment
  # 10	Federal nondefense: Gross investment in IPP
  # 11	State and local: Gross investment in structures
  # 12	State and local: Gross investment in equipment
  # 13	State and local: Gross investment in IPP

# Asset Types:
  # Structures (stu) # to disambiguate with the R command "str"
  # Equipment (equ)
  # Intellectual Property Products (ipp)

##############################################################################################################################################################

#(a)i.	Create IO-FAA commodity concordance (con) in purchasers' value ----

#Here we load in previously constructed investment concordances in purchasers' value
# setwd(file.path(yourwd,"final input files"))

# The following concordance tables match the changes in detailed BEA codes from the 2007 to the 2012 benchmark versions:
  # con_use <-read.csv("con_use.csv")
  # con_make <-read.csv("con_make.csv")


#Note that in the Equations in the text, the concordance tables are in chi x h, suggesting all 401 DIGs chi; these tables actually are the subset of pertinent DIGs.  Later in the process the 4 unused DIG commodities will be merged in.
#(a)i.1-3: Nonresidential private fixed investment 

#Prep to create the concordance files: 
  # #create table summing the fixed asset investment across industries
  # inv_nrp <-read.csv("inv_nrp.csv", check.names = F)
  # 
  # inv_nrp <- separate(inv_nrp,code, c("skip1","sig_code","skip2","asset_code", "skip3"),  sep=c(3,7, 8, 12))
  # inv_nrp$skip1<-NULL;inv_nrp$skip2<-NULL; inv_nrp$skip3<-NULL #Not necessary
  # 
  # #get asset descriptions, combine  with CFC data
  # nrp_faa_codes <-read.csv("nrp_faa_codes.csv")
  #   nrp_faa_codes$asset_code <- as.character(nrp_faa_codes$asset_code)
  # 
  # # combine asset descriptions with inv data
  # inv_nrp <- left_join(inv_nrp,nrp_faa_codes, by="asset_code")
  # inv_nrp <- inv_nrp[,c(1:2,120:121,3:119)]  
  # 
  # inv_nrp <- inv_nrp[!is.na(inv_nrp$asset_cat),] #this removes subtotal categories, which don't have a matching asset category (by design)
  
  #subset wide-format inv data for each year
    # setwd(file.path(yourwd,"final intermed files"))
    # for (i in c('2007','2012','2016')){
    #   assign(paste("inv_nrp",i,sep="_"), aggregate(inv_nrp[,i], by=list(asset_cat=inv_nrp$asset_cat, asset_code = inv_nrp$asset_code, asset_desc = inv_nrp$asset_desc),FUN=sum))
    #   write.csv(get(paste("inv_nrp",i,sep="_")),paste("inv_nrp_",i,".csv",sep=""), row.names = F)   
    # }
  # Use these investment files to manually create con_nrp_ tables by matching assets and commodities in Excel

setwd(file.path(yourwd,"final input files",paste(year,"_files",sep="")))

#(a)i. 1	Nonresidential private fixed investment in structures ----
con_nrp_str <-read.csv("con_nrp_str.csv") #long format
   # note, 'Local Transit Structures' is 0 in investment data, but small in CFC data.
   # nominal value of "1" assigned to 'Local Transit Structures'
# codes not affected by update
   

#convert to wide format
con_nrp_str <- dcast(con_nrp_str, dig_code + dig_desc ~ asset_nipa, value.var = "inv", fun.aggregate = sum )
  #

#(a)i. 2	Nonresidential private fixed investment in equipment ----
  # This is based on the PEQ bridge #Steps (a)i and (a)ii combined due to many-to-many table; see below in Step (a)ii


#(a)i. 3	Nonresidential private fixed investment in IPP ----
con_nrp_ipp <-read.csv("con_nrp_ipp.csv", check.names = F) #long format

#convert to wide format 
con_nrp_ipp <- dcast(con_nrp_ipp, dig_code + dig_desc ~ asset_nipa, value.var = "inv", fun.aggregate = sum )


# codes not affected by update

#(a)i. 4	Residential private fixed investment ----
# codes ARE SUBSTANTIALLY affected by update; split into owner/tenant occupied
#rp concordance happens in steps:
#the fixed asset (fa) types are assigned one rp category
con_rp_fa <-read.csv("con_rp_fa.csv", check.names =  F) #long format
  con_rp_fa[is.na(con_rp_fa)]<-0

#under new codes, differentiate owner (HSO) from tenant (HST) 
con_rp_fa_hso <-dcast(con_rp_fa,rp_fa_code + rp_fa_type  ~ rp_cat, value.var = "531HSO", fun.aggregate = sum )
con_rp_fa_hst <-dcast(con_rp_fa,rp_fa_code + rp_fa_type  ~ rp_cat, value.var = "531HST", fun.aggregate = sum )
  rownames(con_rp_fa_hso)<-con_rp_fa_hso$rp_fa_code  #in the BEA, three digits preceed this code depending on investment / CFC/ net stock
  rownames(con_rp_fa_hst)<-con_rp_fa_hst$rp_fa_code 
  
#the rp categories are distributed across the detail industry group (dig) commodities
con_rp_dig <-read.csv("con_rp_dig.csv", check.names = F) #wide format

  # check that it's parsed:
  # identical(colnames(con_rp_dig)[-c(1:2)],colnames(con_rp_fa_hso)[-c(1:2)]) # true

#combine 
con_rp_hso <- cbind(con_rp_dig[,c(1,2)],data.matrix(con_rp_dig[,-c(1,2)])%*%t(data.matrix(con_rp_fa_hso[,-c(1:2)])))
con_rp_hst <- cbind(con_rp_dig[,c(1,2)],data.matrix(con_rp_dig[,-c(1,2)])%*%t(data.matrix(con_rp_fa_hst[,-c(1:2)])))

  
#(a)i. 5-13	Government fixed investment ----

# # need to update tables to match new BEA tables. using R to melt the wide format in Use table. Concordances made in Excel for convenience.
  # setwd(file.path(yourwd,"final intermed files"))
  #   inv_gov_07 <-read.csv("inv_gov_2007.csv")
  #   inv_gov_07_long <-melt(inv_gov_07,id=c("dig_code","dig_desc"), value.name ="inv_07")
  #   write.csv(inv_gov_07_long,"inv_gov_07_long.csv")
  #   
  #   inv_gov_12 <-read.csv("inv_gov_2012.csv")
  #   inv_gov_12_long <-melt(inv_gov_12,id=c("dig_code","dig_desc"), value.name ="inv_12")
  #   write.csv(inv_gov_12_long,"inv_gov_12_long.csv")

#since government FA data from single tables, reading it in together
con_gov_line <-read.csv("con_gov_line.csv") #adds attributes to Tables 7.3 and 7.5 lines 

#associates the lines in Tables 7.3 and 7.5 with final demand categories and DIG commodities  
con_gov_comm <- read.csv("con_gov_comm.csv", check.names = F)
  con_gov_comm <- melt(con_gov_comm, id=c("dig_code", "dig_desc", "yk_code"), variable.name = "line", value.name = "inv")
  con_gov_comm <- con_gov_comm[which(con_gov_comm$inv>0),]
  con_gov_comm$line <- as.numeric(as.character(con_gov_comm$line)) #convert from factor to match con_gov_line
  con_gov_comm <- left_join(con_gov_comm,con_gov_line) #consider reordering
  con_gov_comm <-  con_gov_comm[,c("dig_code", "dig_desc","yk","yk_code","yk_desc", "line", "item","inv")]
  
  #The goal of the concordance is to assign DIGs to assets; if a DIG is spread across multiple assets within a Yk, that is ok; no need to make proportional across assets
  
for(i in 5:13){
  assign(paste("con_gov_comm",i,sep="_"),dcast(con_gov_comm[which(con_gov_comm$yk==i),],dig_code + dig_desc ~ line, value.var = "inv", fun.aggregate = sum))
 }


##############################################################################################################################################################

#(a)ii.	Convert from purchasers' to producers' value  ----

#read in excerpt of BEA Margins table (P) for Y^K categories
#note that not all DIG commodities are listed, generally only those which pertain to the Y^K
P<-read.csv("margins.csv") #doesn't have yk 
  con_yk <-  read.csv("con_yk.csv")
  P <- left_join(P, con_yk)
  P <- P[,c(10,1:9)] #reorder
  
#remove Used, and Scrap, and Noncomparable Imports DIGs
P<-P[which(P$dig_code != "S00401" & P$dig_code != "S00402" & P$dig_code!="S00300"),]

# remove several small Asset-Dig combinations which were not assigned (due to unsolveable mismatch in underlying data)
  P<-P[which( !(P$dig_code== "3219A0" & P$yk==1) ),]  #did not assign "All other wood product manufacturing" to nrp_str
  P<-P[which( !(P$dig_code== "5111A0" & P$yk==3) ),]  #did not assign "Directory, mailing list, and other publishers" to nrp_ipp
  P<-P[which( !(P$dig_code== "33721A" & P$yk==6) ),]  #only present in 2012, very small "Office furniture and custom architectural woodwork and millwork manufacturing"
 
#remove irrelevant entries with both Producers' and Purchasers' zero
P<-P[which((P$pro + P$pur)!=0),]

#to create transformation matrices T, want to remove double-counted margins in the DIGs, which have purchasers' value ==0. Pseudo-margins table in Purchasers' (alpha)
P_alpha <-P[which(P$pur!=0),]

#replace "pur" with sum of "pro" and margins, to address any minor rounding error
P_alpha[,10]<-rowSums(P_alpha[,6:9])
  P_alpha <-P_alpha[which(P_alpha$pur!=0),] #in case some "pur" became 0 as a result of the sum

#create transformation matrices (Trans) by normalizing 4 components of Producers' value by the purchasers' value
#see Equation S1
Trans<-P_alpha #same structure as P_alpha
Trans<-Trans[order(Trans$yk,Trans$dig_code),] 

  # This approach works (with additional steps to recombine dataframes; Sweep is used for convenience) 
    # P_alpha_pur <-solve(diag(P_alpha$pur)) #matrix to normalize other components
    # Trans_o<-P_alpha$pro %*% P_alpha_pur
    # Trans_w<-P_alpha$who %*% P_alpha_pur
    # Trans_r<-P_alpha$ret %*% P_alpha_pur
    # Trans_t<-P_alpha$tra %*% P_alpha_pur

#in R, can achieve same result of Equation S1 with Sweep function, which has the benefit of retained dataframe structure
  Trans[,6:10] <-sweep(Trans[,6:10], 1, Trans[,10], FUN= "/")

#to distribute the margin totals to the margin DIGs, extract them from the P matrix. Note that there are no margins for government structures, and no retail margins for other government investment.
  # this approach assumes that the margin DIGs are distributed across the assets in the same way. Could instead manually match the retail or wholesale type to the asset type, though it would not be straightforward.
  P_mar <-P[which(P$pur==0),1:6] #margins have pro>0 but pur==0
  
    # create con_P_mar, below
    # P_mar_codes <-unique(P_mar[,c("dig_code", "dig_desc")])
    # setwd(file.path(yourwd,"final intermed files"))
    # write.csv(P_mar_codes,"P_mar_codes.csv", row.names=F)
    # setwd(file.path(yourwd,"final input files",paste(year,"_files",sep="")))
    
  #cast from long to wide format
  P_mar <-dcast(P_mar,yk + yk_code + yk_desc ~ dig_code, value.var = "pro", fun.aggregate = sum)
  
  #read in binary concordance between DIG and margin type
  con_P_mar <-read.csv("con_P_mar.csv")
    con_P_mar[is.na(con_P_mar)]<-0
    con_P_mar <- con_P_mar[order(con_P_mar$dig_code),]
    
    # check that this is parsed with P_mar
    # P_mar_check <-data.frame(cbind(colnames(P_mar)[-c(1:3)],as.character(con_P_mar[,"dig_code"]))) #parsed, after 'ordering' con_P_mar
      # identical(P_mar_check$X1,P_mar_check$X2) #true
  
  #this is effectively performing Equations S3 & S4, but simultaneous for all Y^K. Note: Equations S2, S5, S6 below for each YK
    P_mar_sum <- data.matrix(P_mar[,-c(1:3)]) %*% data.matrix(con_P_mar[,3:5]) #effectively Equation S3
    
    P_mar_who <-P_mar
    P_mar_who[,-c(1:3)] <-data.matrix(P_mar[,-c(1:3)]) %*% diag(con_P_mar[,"who"])
    P_mar_who[,-c(1:3)] <-solve(diag(P_mar_sum[,"who"]))%*% data.matrix(P_mar_who[,-c(1:3)]) #effectively Equation S4
    rownames(P_mar_who)<-P_mar_who$yk
    P_mar_who<- data.frame(t(P_mar_who[,-c(1:3)]),check.names = F)
    P_mar_who$dig_code <- rownames(P_mar_who)
    
    P_mar_ret <-P_mar
    P_mar_ret[,-c(1:3)] <-data.matrix(P_mar[,-c(1:3)]) %*% diag(con_P_mar[,"ret"])
    P_mar_ret[2:4,-c(1:3)] <-solve(diag(P_mar_sum[2:4,"ret"]))%*% data.matrix(P_mar_ret[2:4,-c(1:3)]) #limit to non-zero rows 2:4 for matrix to invert
    rownames(P_mar_ret)<-P_mar_ret$yk
    P_mar_ret<- data.frame(t(P_mar_ret[,-c(1:3)]),check.names = F)
    P_mar_ret$dig_code <- rownames(P_mar_ret)
    
    P_mar_tra <-P_mar
    P_mar_tra[,-c(1:3)] <-data.matrix(P_mar[,-c(1:3)]) %*% diag(con_P_mar[,"tra"])
    P_mar_tra[,-c(1:3)] <-solve(diag(P_mar_sum[,"tra"]))%*% data.matrix(P_mar_tra[,-c(1:3)])
    rownames(P_mar_tra)<-P_mar_tra$yk
    P_mar_tra<- data.frame(t(P_mar_tra[,-c(1:3)]),check.names = F)
    P_mar_tra$dig_code <- rownames(P_mar_tra)
    
  #read in full list of 401 DIG commodity codes 
  dig_codes_com <-read.csv("dig_codes_com.csv") #this excludes the 4 not assigned capital
  
  #Defined a function for Equations S5-S9 ----
  equations5_s9 <- function(con_alpha, Y_k){
    
    # diagnose non-conformable issues
      # con_alpha<-con_gov_comm_9
      # Y_k <-9
      # parse_chk <-  cbind(Trans_yk[,c(4:5)], con_alpha[,c(1:2)])
      
      # con_alpha<-con_nrp_str
      # Y_k <-1
      # parse_chk <-  cbind(Trans_yk[,c(4:5)], con_alpha[,c(1:2)])
    con_alpha$dig_code <- as.character(con_alpha$dig_code) #in the case that all codes are numeric
     
    Trans_yk<-Trans[which(Trans$yk==Y_k),]
    pro <- cbind(con_alpha[,1:2],diag(Trans_yk[,"pro"]) %*% data.matrix(con_alpha[,-(1:2)]))
      colnames(pro)<-colnames(con_alpha)
    
    #take the margin sum across DIGs, Equation S5
    tra <- matrix(1,1,NROW(Trans_yk)) %*% data.matrix((diag(Trans_yk[,7]) %*% data.matrix(con_alpha[,-(1:2)])))
    who <- matrix(1,1,NROW(Trans_yk)) %*% (diag(Trans_yk[,8]) %*% data.matrix(con_alpha[,-(1:2)]))
    ret <- matrix(1,1,NROW(Trans_yk)) %*% (diag(Trans_yk[,9]) %*% data.matrix(con_alpha[,-(1:2)]))
    
    #allocate margin sum to appropriate DIG commodity from P_mar_, Equation S6
    tra2<- data.frame(cbind(P_mar_tra[,"dig_code"],data.matrix(P_mar_tra[,paste(Y_k)])%*%tra), check.names = F)
      tra2[,-1] <- sapply(tra2[,-1], function(x) as.numeric(as.character(x)))
      colnames(tra2)<-c("dig_code",colnames(con_alpha)[-c(1:2)])
    who2<- data.frame(cbind(P_mar_who[,"dig_code"],P_mar_who[,paste(Y_k)]%*%who), check.names = F)
      who2[,-1] <- sapply(who2[,-1], function(x) as.numeric(as.character(x)))
      colnames(who2)<-c("dig_code",colnames(con_alpha)[-c(1:2)])
    ret2<- data.frame(cbind(P_mar_ret[,"dig_code"],P_mar_ret[,paste(Y_k)]%*%ret), check.names = F)
      ret2[,-1] <- sapply(ret2[,-1], function(x) as.numeric(as.character(x)))
      colnames(ret2)<-c("dig_code",colnames(con_alpha)[-c(1:2)])
    # colnames(tra2)[1]<-"dig_code"; colnames(who2)[1]<-"dig_code"; colnames(ret2)[1]<-"dig_code"
    
    # Combine four items, Equation S7 
    beta <- bind_rows(pro, tra2, who2, ret2 )
    beta <- aggregate(beta[,-c(1:2)], by=list(dig_code=beta$dig_code), FUN=sum )
      colnames(beta)<-c("dig_code",colnames(con_alpha)[-c(1:2)])
    
    #add a column duplicating the final column so sweep functions work on datasets with a single column; later remove it
    beta$extra <-beta[,ncol(beta)] 
    
    # Find column sums, Equation S8
    beta_sum <- colSums(beta[,-1]) #doesn't work for single column; works since Extra is added
    
    #Normalize by column sums, Equation S9. Note: again we take advantage f the Sweep function
    beta[,-1] <- sweep(beta[,-1],2,beta_sum, FUN = "/") #doesn't work for single column; works since Extra is added
      beta$extra<-NULL
    
    #merge with all commodity DIGs
    beta<-left_join(dig_codes_com,beta)
    beta[is.na(beta)]<-0
    
    beta
  }
  
  # test6 <-equations5_s9(con_gov_6,6)
  
#(a)ii. 1	Nonresidential private fixed investment in structures ----
  con_nrp_str_beta <-equations5_s9(con_nrp_str,1) 

#(a)ii. 2	Nonresidential private fixed investment in equipment ----
#this is combined with step (a)i #separate steps due to many-to-many structure of PEQ Bridge
  peq_brg<-read.csv("peq_brg.csv")
  # now peq_brg_07 and peq_brg_12
  
  #replace "pur" with sum of "pro" and margins, to address any minor rounding error
  peq_brg[,9]<-rowSums(peq_brg[,5:8])
  
  #remove Used, and Scrap, and Noncomparable Imports DIGs
  peq_brg<-peq_brg[which(peq_brg$dig_code != "S00401" & peq_brg$dig_code != "S00402" & peq_brg$dig_code!="S00300"),]
  
  #remove residentiual equipment
  peq_brg<-peq_brg[which(peq_brg$peq_desc != "Residential equipment"),]
  
  #create function to cast each item
  peq_brg_cast <-function(item){
    cast<-dcast(peq_brg,dig_code + dig_desc ~ peq_desc, value.var = item, fun.aggregate = sum)
    cast<-left_join(dig_codes_com,cast) #merge with all commodity DIGs
    cast[is.na(cast)]<-0
    rownames(cast)<-cast$dig_code
    cast<-data.matrix(cast[,-c(1:2)]) #create data matrix
  }
  
  #create a function for equations S2,S5-S9 (to prevent creation of many temporary objects, even though only applied once)
  equations_nrp_equ <- function(Y_k){
    # Y_k <-2, for diagnosis
    
    peq_brg_items= c("pro","tra","who","ret","pur")
      for(i in peq_brg_items){
        assign(paste(i,"cast",sep="_"), peq_brg_cast(i)) #effectively the result of Equation S2; don't need Equation S1
        assign(i, matrix(1,1,nrow(dig_codes_com))%*%get(paste(i,"cast",sep="_"))) #Equation S5
      }
    
    #add row of "dig_code" to pro_cast
    pro_cast<-data.frame(pro_cast, check.names = F)
    pro_cast$dig_code <-rownames(pro_cast)
    
    #same as general function, equations5_s9 
        #allocate margin sum to appropriate DIG commodity from P_mar_, Equation S6
        tra2<- data.frame(cbind(P_mar_tra[,"dig_code"],data.matrix(P_mar_tra[,paste(Y_k)])%*%tra), check.names = F)
          tra2[,-1] <- sapply(tra2[,-1], function(x) as.numeric(as.character(x)))
        who2<- data.frame(cbind(P_mar_who[,"dig_code"],P_mar_who[,paste(Y_k)]%*%who), check.names = F)
          who2[,-1] <- sapply(who2[,-1], function(x) as.numeric(as.character(x)))
        ret2<- data.frame(cbind(P_mar_ret[,"dig_code"],P_mar_ret[,paste(Y_k)]%*%ret), check.names = F)
          ret2[,-1] <- sapply(ret2[,-1], function(x) as.numeric(as.character(x)))
        colnames(tra2)[1]<-"dig_code"; colnames(who2)[1]<-"dig_code"; colnames(ret2)[1]<-"dig_code"
        
        # Combine four items, Equation S7 
        beta <- bind_rows(pro_cast, tra2, who2, ret2 )
        beta <- aggregate(beta[,names(beta) != "dig_code"], by=list(dig_code=beta$dig_code), FUN=sum )
        
        # Find column sums, Equation S8
        beta_sum <- colSums(beta[,-1])
        
        #Normalize by column sums, Equation S9. Note: again we take advantage f the Sweep function
        beta[,-1] <- sweep(beta[,-1],2,beta_sum, FUN = "/")
        
        #There is some discrepancy between the FAA and PEQ/NIPA assets. 
          #There are more FAA assets than PEQ/NIPA assets.  Each FAA asset is assigned to one PEQ/NIPA category
          #Assign the concordance for the PEQ/NIPA assets to corresponding FAA assets
        
        peq_faa <-read.csv("peq_faa.csv")
          peq_faa_cast <-dcast(peq_faa, peq_desc ~ asset_nipa, value.var = "count", fun.aggregate = sum)
        
        beta2 <- data.frame(cbind(beta[,"dig_code"],data.matrix(beta[,-1])%*%data.matrix(peq_faa_cast[,-1])), check.names = F)
          colnames(beta2)[1] <- "dig_code"
          beta2[,-1] <- sapply(beta2[,-1], function(x) as.numeric(as.character(x)))
          
        beta2 <-left_join(dig_codes_com,beta2)
            
    beta2
  }
  
  con_nrp_equ_beta <-equations_nrp_equ(2)
  
  
  
#(a)ii. 3	Nonresidential private fixed investment in IPP ----
  con_nrp_ipp_beta <- equations5_s9(con_nrp_ipp,3)
  
#(a)ii. 4	Residential private fixed investment ----
  # separated by owner/tenant
  con_rp_hso_beta <- equations5_s9(con_rp_hso,4)
  con_rp_hst_beta <- equations5_s9(con_rp_hst,4)
  
  
#(a)ii. 5-13	Government fixed investment ----
  
  #there are no margins for government structures, so alpha = beta / purchasers'=producers'. Define simpler function, removing some parts of broader function
    equations5_s9_nomarg <- function(con_alpha){
      
      beta <-con_alpha
      # Find column sums, Equation S8
      beta_sum <- colSums(beta[,-c(1:2)])
      
      #Normalize by column sums, Equation S9. Note: again we take advantage f the Sweep function
      beta[,-c(1:2)] <- sweep(beta[,-c(1:2)],2,beta_sum, FUN = "/")
      
      #merge with all commodity DIGs
      beta<-left_join(dig_codes_com,beta)
      beta[is.na(beta)]<-0
      beta
    }
  
  #create beta concordances for government Structures Yks
  for(i in c(5,8,11)){
      assign(paste("con_gov",i,"beta",sep="_"),equations5_s9_nomarg(get(paste("con_gov_comm",i,sep="_"))))
  }
  
  #create beta concordances for government equipment and IPP Yks
  for(i in c(6,7,9,10,12,13)){
    assign(paste("con_gov",i,"beta",sep="_"),equations5_s9(get(paste("con_gov_comm",i,sep="_")),i))
  }
  
    
  #combine them all together, order by line
  con_gov_beta <-con_gov_5_beta #initialize
  
  for(i in 6:13){
    con_gov_beta <-left_join(con_gov_beta, get(paste("con_gov",i,"beta", sep="_")))
    }

  #reorder columns; a bit inefficient, seemingly easier solutions didn't work
    con_gov_beta2 <-con_gov_beta[,-c(1:2)] 
    con_gov_beta2 <-con_gov_beta2[,order(colnames(con_gov_beta2))]
    con_gov_beta  <-cbind(con_gov_beta[,c(1:2)],con_gov_beta2)
    rm(con_gov_beta2)
  
  
  #check colsums
  # for(i in c(6:8,10:11,13)){
  #     assign(paste("colsums",i) , colSums(get(paste("con_gov",i,"beta", sep="_"))[,-c(1:2)]))
  # }
  # 
 
##############################################################################################################################################################

#(a)iii.	Apply commodity concordance to CFC data ----

#(a)iii. 1-3	Nonresidential private fixed investment ----
  
  # read in asset codes 
  nrp_faa_codes <-read.csv("nrp_faa_codes.csv")
      nrp_faa_codes$asset_code <- as.character(nrp_faa_codes$asset_code)
  
  cfc_nrp <-read.csv("cfc_nrp.csv", check.names = F)
    cfc_nrp <-cfc_nrp[,c("code",year)] #get year of interest
  
  #split out complex code to industry code (sig_code) and asset code
  cfc_nrp <- separate(cfc_nrp,code, c("skip1","sig_code","skip2","asset_code", "skip3"),  sep=c(3,7, 8, 12))
    cfc_nrp$skip1<-NULL;cfc_nrp$skip2<-NULL; cfc_nrp$skip3<-NULL #Not necessary
  
  # combine asset descriptions with CFC data
  cfc_nrp <- left_join(cfc_nrp,nrp_faa_codes, by="asset_code")
    cfc_nrp <- cfc_nrp[!is.na(cfc_nrp$asset_cat),]
    
    #wide-format CFC data 
    cfc_nrp <- dcast(cfc_nrp,asset_cat + asset_desc + asset_code ~ sig_code, value.var = year, fun.aggregate =sum)
    
    # cfc_nrp_check <-data.frame(cbind(cfc_nrp[,c(1:3)],rowSums(cfc_nrp[,-c(1:3)])))
    
  #create function to perform Equation 2
  equation2_nrp <- function(con_beta, cat){
    temp<- cbind(con_beta[,c(1:2)],data.matrix(con_beta[,-c(1:2)])%*% data.matrix(cfc_nrp[which(cfc_nrp$asset_cat==cat),-c(1:3)]))
    
    tempsum <-rowSums(temp[,c("5210","5220")]) #issue with concordance between DIG and SIG; aggregating into "52XX"
      temp[,"5210"]<-tempsum
      temp[,"5220"]<-NULL
      colnames(temp)[43]<-"52XX"
      
    temp
                  
    }
    
    
  #(a)iii. 1	Nonresidential private fixed investment in structures ----
  
    Ukstar_nrp_str <- equation2_nrp(con_nrp_str_beta,"Structures")
    
  #(a)iii. 2	Nonresidential private fixed investment in equipment ----
  
    Ukstar_nrp_equ <- equation2_nrp(con_nrp_equ_beta,"Equipment")  
  
  #(a)iii. 3	Nonresidential private fixed investment in IPP ----
  
    Ukstar_nrp_ipp <- equation2_nrp(con_nrp_ipp_beta,"IPP")  
    
#(a)iii. 4	Residential private fixed investment ----
  cfc_rp <-read.csv("cfc_rp.csv", check.names = F)
    cfc_rp <- cfc_rp[order(cfc_rp$rp_fa_code),]
    # check that rp_fa_code is parsed (should be)
      # cfc_rp_check <-data.frame(cbind(as.character(cfc_rp[,"rp_fa_code"])),colnames(con_rp_hso_beta)[-c(1:2)])
      # identical(cfc_rp_check[,1],cfc_rp_check[,2]) #true
    
    Ukstar_rp_hso <-cbind(con_rp_hso_beta[,1:2], data.matrix(con_rp_hso_beta[,-c(1:2)])%*%cfc_rp[,year])
      colnames(Ukstar_rp_hso)[3]<- "531HSO" 
    Ukstar_rp_hst <-cbind(con_rp_hst_beta[,1:2], data.matrix(con_rp_hst_beta[,-c(1:2)])%*%cfc_rp[,year])
      colnames(Ukstar_rp_hst)[3]<- "531HST"
     
   
#(a)iii. 5-13	Government fixed investment ----
    
    cfc_gov<-read.csv("cfc_gov.csv", check.names = F)
    
    #since assigning CFC to government DIGs is complicated, for this step we will retain the lines, and spread out the CFC per line
      #multiplying the CFC x 1000 to convert from billions to millions, matching NRP and RP
     Ukstar_gov <- cbind(con_gov_beta[,1:2], sweep(con_gov_beta[,-c(1:2)],2,t(cfc_gov[,year]*1000),FUN="*"))
    
    
##############################################################################################################################################################

#(a)iv.	Allocate CFC from aggregate investors to DIGs ----
    
#(a)iv. 1-3	Nonresidential private fixed investment ----
    #Get Intermediate Output and Gross Operating Surplus (GOS), from the value add matrix in the Use table
    gos <- read.csv("gos.csv")
    
    #Get dig-sig industry concordance; fewer than 401 because only NRP industries
    nrp_dig_sig <-read.csv("nrp_dig_sig.csv") #note that the structure has changed for orig
    
    gos <-right_join(nrp_dig_sig,gos)
      gos<-gos[!is.na(gos$sig_code_12),] #removing Custom Duties (deal with that later)
    
    gos$value <- gos$gos
    
    #the "523: Securities, commodity contracts, and investments" SIG has negative GOS replace it with intermediate output 
      gos[which(gos$sig_code_12=="523"),"value"]<-gos[which(gos$sig_code_12=="523"),"intermed"]
    
    #the "Private households" & "Customs duties" industries are odd cases; shouldn't be assigned any capital.
      gos[is.na(gos)]<-0
      
    #cast this sig x dig. use 2012 dig codes, but 2007 sig codes, because FAA data not updated
    gos_cast <-dcast(gos, sig_code_07 + sig_desc_07 ~ dig_code, value.var = "value", fun.aggregate=sum)
    
    #Equation 3
    gos_sum <- data.matrix(gos_cast[,-c(1:2)])%*%matrix(1,ncol(gos_cast)-2,1)
    
    #Equation 4
    gos_norm <-gos_cast
    gos_norm[,-c(1:2)] <-sweep(gos_norm[,-c(1:2)],1,gos_sum, FUN = "/")
    
      #parse with sig_codes in Ukstar_nrp's
      gos_norm_order <-match(colnames(Ukstar_nrp_str)[-c(1:2)], gos_norm$sig_code)
      gos_norm <-gos_norm[gos_norm_order,]
    
    #Equation 5, create a function
    equation5_nrp <- function(Ukstar){
      temp<- cbind(Ukstar[,c(1:2)], data.matrix(Ukstar[,-c(1:2)])%*% data.matrix(gos_norm[,-c(1:2)] ))
    }
    

#(a)iv. 1	Nonresidential private fixed investment in structures ----
    Ukstar_nrp_str_det <- equation5_nrp(Ukstar_nrp_str)     
      
  
#(a)iv. 2	Nonresidential private fixed investment in equipment ---- 
    
    Ukstar_nrp_equ_det <- equation5_nrp(Ukstar_nrp_equ) 

#(a)iv. 3	Nonresidential private fixed investment in IPP ----    
    
    Ukstar_nrp_ipp_det <- equation5_nrp(Ukstar_nrp_ipp) 
    
#(a)iv. 1:3	Nonresidential private fixed investment  ----
    #combine 1-3, then aggregate to resolve duplicate rows
    
    Ukstar_nrp_det <- bind_rows(Ukstar_nrp_str_det,Ukstar_nrp_equ_det,Ukstar_nrp_ipp_det)
    Ukstar_nrp_det <- aggregate(Ukstar_nrp_det[,-c(1:2)],by=list(dig_code=Ukstar_nrp_det$dig_code, dig_desc =Ukstar_nrp_det$dig_desc), FUN = sum)

#(a)iv. 4	Residential private fixed investment ----    

    # No allocation is needed since already created in terms of the two DIGs. Combine them:
    Ukstar_rp_det <- merge(Ukstar_rp_hso, Ukstar_rp_hst)
    
#(a)iv. 5-13	Government fixed investment ----
    
   con_gov_ind <-read.csv("con_gov_ind.csv")
    con_gov_ind$gov_ent <- 1 - con_gov_ind$gov_gen
  
   #both 'general government' and 'government enterprises'; prepare separately then combine
    #general government DIGs:
    
      con_gov_ind_gen <- read.csv("con_gov_ind_gen.csv", check.names = F)
      con_gov_ind_gen[is.na(con_gov_ind_gen)]<-0
      
      #normalize by rowsum to remove rounding error
      con_gov_ind_gen[,7:11] <-sweep(con_gov_ind_gen[,7:11],1,rowSums(con_gov_ind_gen[,7:11]),FUN ="/")
      
      # apply the percentage of CFC that goes to general:
      con_gov_ind_gen <- left_join(con_gov_ind_gen,con_gov_ind[,c("line","gov_gen")])
      con_gov_ind_gen[,7:11] <-sweep(con_gov_ind_gen[,7:11],1,con_gov_ind_gen[,12],FUN ="*")
      
    #government enterprises:
      con_gov_ind_ent<- read.csv("con_gov_ind_ent.csv", check.names = F)
      con_gov_ind_ent[is.na(con_gov_ind_ent)]<-0
      
      #normalize by rowsum to remove rounding error
      con_gov_ind_ent[,7:12] <-sweep(con_gov_ind_ent[,7:12],1,rowSums(con_gov_ind_ent[,7:12]),FUN ="/")
      
      # apply the percentage of CFC that goes to general:
      con_gov_ind_ent <- left_join(con_gov_ind_ent,con_gov_ind[,c("line","gov_ent")])
      con_gov_ind_ent[,7:12] <-sweep(con_gov_ind_ent[,7:12],1,con_gov_ind_ent[,13],FUN ="*")
      
    # combine
      con_gov_ind<- left_join(con_gov_ind[,1:2],con_gov_ind_gen[,c(1,7:11)])
      con_gov_ind<- left_join(con_gov_ind,con_gov_ind_ent[,c(1,7:12)])
        con_gov_ind[is.na(con_gov_ind)]<-0
        # check rowsums (only 1 and 2, okay)
        con_gov_ind$check <-rowSums(con_gov_ind[,3:13])
    
    #Allocate CFC per line to DIG industries
        # ensure "line" is parsed:
        # line_check <-data.frame(cbind(as.numeric(colnames(Ukstar_gov[,-c(1:2)])),con_gov_ind[,1]))
        #   identical(line_check[,1],line_check[,2]) #true
        
        Ukstar_gov_det <- cbind(Ukstar_gov[,c(1:2)], data.matrix(Ukstar_gov[,-c(1:2)]) %*% data.matrix(con_gov_ind[,3:13] ))
      
        # check:
          # sum(cfc_gov[,year]); sum(Ukstar_gov_det[,-c(1:2)]) #checks!
 
##############################################################################################################################################################

#(b)	Create an intermediate detailed CFC combining all Y^K categories ----
      
      #combine the three investor classes 
      # maybe dig_codes_com not needed
      
        Ukstar_det <- left_join(dig_codes_com,left_join(Ukstar_nrp_det,left_join(Ukstar_rp_det, Ukstar_gov_det)))
        
      #reorder columns to match Use table 
      dig_codes_ind <- read.csv("dig_codes_ind.csv", check.names = F) #order in Use table
        dig_codes_ind_match<-c("dig_code","dig_desc",as.character(dig_codes_ind$dig_code)) #make list
        dig_codes_ind_match<-match(dig_codes_ind_match,colnames(Ukstar_det)) #make match
      Ukstar_det <-Ukstar_det[,dig_codes_ind_match]
      
      setwd(file.path(yourwd,"final output files",paste(year,"_files",sep="")))
        write.csv(Ukstar_det, "Ukstar_det.csv",row.names = F)
        
      
##############################################################################################################################################################

#(c)	Create the final detailed CFC by re-allocating Highways  & Streets ----
      
      #convert equipment in purchasers' assets to purchasers' DIGs, take sum over vehicle DIGs
        #Yk: 2,6,9,12
        
        setwd(file.path(yourwd,"final input files",paste(year,"_files",sep="")))
        
  #(c) 2.	Nonresidential private fixed investment in equipment ---- 
        
        
          equations_nrp_equ2 <- function(){ #modified version of equations_nrp_equ
            
            peq_brg_items= c("pur")
            for(i in peq_brg_items){
              assign(paste(i,"cast",sep="_"), peq_brg_cast(i)) #effectively the result of Equation S2; don't need Equation S1
              assign(i, matrix(1,1,nrow(dig_codes_com))%*%get(paste(i,"cast",sep="_"))) #Equation S5
            }
            
            #add column of "dig_code" to pur_cast
            pur_cast<-data.frame(pur_cast, check.names = F)
            pur_cast$dig_code <-rownames(pur_cast)
            pur_cast<-pur_cast[,c(26,1:25)] #changed from ncol=26 to 25 (not sure why)
            
            #
            beta <- pur_cast
            
            # Find column sums
            beta_sum <- colSums(beta[,-1])
            
            #Normalize by column sums
            beta[,-1] <- sweep(beta[,-1],2,beta_sum, FUN = "/")
            
            #There is some discrepancy between the FAA and PEQ/NIPA assets. 
            #There are more FAA assets than PEQ/NIPA assets.  Each FAA asset is assigned to one PEQ/NIPA category
            #Assign the concordance for the PEQ/NIPA assets to corresponding FAA assets
            
            peq_faa <-read.csv("peq_faa.csv")
            peq_faa_cast <-dcast(peq_faa, peq_desc ~ asset_nipa, value.var = "count", fun.aggregate = sum)
            
            beta2 <- data.frame(cbind(beta[,"dig_code"],data.matrix(beta[,-1])%*%data.matrix(peq_faa_cast[,-1])), check.names = F)
            colnames(beta2)[1] <- "dig_code"
            beta2[,-1] <- sapply(beta2[,-1], function(x) as.numeric(as.character(x)))
            
            beta2 <-left_join(dig_codes_com,beta2)
            
            beta2
          }
          
        con_nrp_equ<- equations_nrp_equ2()
        
        #Equation 6: Apply CFC to aggregate investors
        
          Ukstar_nrp_equ2 <- equation2_nrp(con_nrp_equ,"Equipment")
        
        #Equation 7: Distribute to  detailed investors
        
          Ukstar_nrp_equ2_det <- equation5_nrp(Ukstar_nrp_equ2)  
        
        #create binary concordance for vehicle DIGs:
        con_veh <-read.csv("con_veh.csv")
        
        #sum over vehicles
        
          Ukstar_nrp_equ2_veh <- data.matrix(t(con_veh$veh))%*%data.matrix(Ukstar_nrp_equ2_det[,-c(1:2)])     
        
        
  #(c) 6,9, 12 Government fixed investment in equipment----  
        
        #combine and merge each gov equipment Y_k with all DIG codes
        con_gov2<-dig_codes_com #initiate it
        
        for(i in c(6,9,12)){
          con_gov2<-left_join(con_gov2,get(paste("con_gov_comm",i,sep="_")))
        }
        
        con_gov2[is.na(con_gov2)]<-0
        con_gov2<-equations5_s9_nomarg(con_gov2) #normalize by column sums
        
        
        #since assigning CFC to government DIGs is complicated, for this step we will retain the lines, and spread out the CFC per line
          #multiplying the CFC x 1000 to convert from billions to millions, matching NRP and RP
        
        con_gov_lines <-match(colnames(con_gov2)[-c(1:2)], cfc_gov$line)
        cfc_gov2<- cfc_gov[con_gov_lines,]
        
          Ukstar_gov2 <- cbind(con_gov2[,1:2], sweep(con_gov2[,-c(1:2)],2,t(cfc_gov2[,year]*1000),FUN="*"))
           
        
        #Allocate CFC per line to DIG industries
       
          Ukstar_gov2_det <- cbind(Ukstar_gov2[,c(1:2)], data.matrix(Ukstar_gov2[,-c(1:2)]) %*% data.matrix(con_gov_ind[con_gov_lines ,3:13] ) )
          
        #sum over vehicles
          Ukstar_gov2_veh <- data.matrix(t(con_veh$veh))%*%data.matrix(Ukstar_gov2_det[,-c(1:2)])
        
    #combine veh sums from NRP and gov; add rp for consistency
        
        Ukstar_rp_veh <- data.frame("531HSO"=0,   "531HST"=0,  check.names = F)
       
        Ukstar_veh <- cbind(Ukstar_nrp_equ2_veh, Ukstar_gov2_veh, Ukstar_rp_veh )
        
        #order columns in line with Use table
        dig_codes_ind_match2<-match(dig_codes_ind$dig_code,colnames(Ukstar_veh))
        
          Ukstar_veh <-Ukstar_veh[,dig_codes_ind_match2]
        
        
    #bring in household share of vehicle CFC, Equation 9
        #RP #find sum of vehicle depreciation in Consumer Durable Goods
        #note that this is a very significant sum!
        
        cfc_hh_veh <-read.csv("cfc_hh_veh.csv", check.names = F) #From Detailed CDG data
        cfc_hh_veh <-matrix(1,1,2)%*% data.matrix(cfc_hh_veh[,year])
     
    
    #calculate the total vehicle CFC, Equation 10
        cfc_veh <- sum(cfc_hh_veh, sum(Ukstar_veh))
        
    #allocate "State and local government" (S00700) "Transportation structures and highways and streets" (2332C0) CFC based on vehicle CFC, Equation 11
       
        
        equation11 <- function(){
          
          psi<-Ukstar_det[which(Ukstar_det$dig_code=="2332C0"),"GSLGO"]
          
          psi_ratio <- psi/cfc_veh
          
          psi_k_star <- sweep(Ukstar_veh,1,psi_ratio, FUN="*")
          
          psi_k_star_hh <- psi_ratio*cfc_hh_veh #see Equation 12 
          
          #Add the H&S for households to that for State & Local government 
          psi_k_star[1,'GSLGO'] <- sum(psi_k_star[1,'GSLGO'],psi_k_star_hh[,])
          
          #zero out the H&S originally in State & Local government
          Ukstar_det[which(Ukstar_det$dig_code=="2332C0"),"GSLGO"] <- 0 
          
          #add another row with the H&S dig_code and dig_desc
          Ukstar_det[nrow(Ukstar_det)+1,] <-Ukstar_det[which(Ukstar_det$dig_code=="2332C0"),]
            Ukstar_det[nrow(Ukstar_det),-c(1:2)] <- psi_k_star
          
          #aggregate the H&S rows
          Ukstar_det <- aggregate(Ukstar_det[,-c(1:2)],by=list(dig_code=Ukstar_det$dig_code, dig_desc=Ukstar_det$dig_desc), FUN = sum )
          
          #reorder to match dig_codes_com
          Ukstar_det<-left_join(dig_codes_com,Ukstar_det)
          
          Uk_det<-Ukstar_det
          }
        
        # run equation
        Uk_det <- equation11()
        
        
#Create technical coefficient matrix for capital formation----

  setwd(file.path(yourwd,"final input files",paste(year,"_files",sep="")))
          
    #read in x, V, q: 
          x<- read.csv("x.csv", check.names = F)
            x[is.na(x)]<-1 #changed 0 to 1 in order for matrix to invert
          V<- read.csv("V.csv", check.names = F)
            V[is.na(V)]<-0 #sparse matrix, changed to 0
          q<- read.csv("q.csv", check.names = F)
            q[is.na(q)]<-1 #changed 0 to 1 in order for matrix to invert
          
      #equation 13 or 15: 
        equation13 <- function(U){
            # U <- U_det #diagnosis
            
            U_mat<- data.matrix(U[,-c(1:2)])
            B <- U_mat %*% solve(diag(x[,3]))
           
            V <-data.matrix(V[,-c(1:2)])
            D <- V %*% solve(diag(q[,3]))
            
            A <-B %*% D
            A <-data.frame(cbind(U[,c(1:2)],A))
            colnames(A) <- colnames(U)
              
            A
          }
          
         
        #Combine Uk with U 
         U_det <- read.csv("U_det.csv", check.names = F) #read in  U_det
            U_det[is.na(U_det)]<- 0
            
            A <- equation13(U_det)
            
        #U has the 4 rows removed from Uk (Scrap, Used, Rest of World, Non-comp imports)
         Uk_det <- left_join(U_det[,1:2], Uk_det) # add in 4 rows, in correct order
            Uk_det[is.na(Uk_det)]<- 0
            
            Ak <- equation13(Uk_det)
            
        # Equation 15: Create total A matrix
         Ut_det <- cbind(U_det[,1:2], U_det[,-c(1:2)]+Uk_det[,-c(1:2)])
        
            At <- equation13(Ut_det)
            At2 <- cbind(U_det[,1:2],A[,-c(1:2)]+Ak[,-c(1:2)]) #alternative approach
         
         setwd(file.path(yourwd,"final output files",paste(year,"_files",sep="")))  
           
           write.csv(U_det,  "U_det.csv", row.names = F)
           write.csv(Uk_det, "Uk_det.csv", row.names = F)
           write.csv(Ut_det, "Ut_det.csv", row.names = F)
          
           write.csv(A,   "A.csv", row.names = F)
           write.csv(Ak,  "Ak.csv", row.names = F)
           write.csv(At,  "At.csv", row.names = F)
           write.csv(At2,  "At2.csv", row.names = F)
           
#Aggregate product categories for comparison----
         setwd(file.path(yourwd,"final input files",paste(year,"_files",sep="")))
           
            con_agg_dig_comm <- read.csv("con_agg_dig_comm.csv")
              con_agg_dig_comm$count <-1
            con_agg_dig_ind <- read.csv("con_agg_dig_ind.csv")
              con_agg_dig_ind$count <-1
            agg_desc <- read.csv("agg_desc.csv")
            agg_desc <- agg_desc[-c(24:25),] #remove HH accounting not included in this method
          
          #prepare binary concordances  
          
          con_agg_dig_ind <- dcast(con_agg_dig_ind, dig_code +dig_desc ~ agg_code, value.var = "count", fun.aggregate = sum)
            con_agg_dig_ind <-left_join(dig_codes_ind, con_agg_dig_ind)#to parst
            
            rownames(con_agg_dig_ind )<- con_agg_dig_ind$dig_code
            con_agg_dig_ind<-data.matrix(con_agg_dig_ind[,-c(1:2)])
            
          con_agg_dig_comm <- dcast(con_agg_dig_comm, dig_code +dig_desc ~ agg_code, value.var = "count", fun.aggregate = sum)
            con_agg_dig_comm <- left_join(U_det[,c(1:2)],con_agg_dig_comm) #to parse
            rownames(con_agg_dig_comm)<-con_agg_dig_comm$dig_code          
            con_agg_dig_comm<-t(con_agg_dig_comm[,-c(1:2)]) #transpose
                      
          
          #aggregate by industries, commodities, or both  
          equation_digxagg <- function(Uk) {
            Uk_1 <- cbind(Uk[,c(1:2)], data.matrix(Uk[,-c(1:2)]) %*% con_agg_dig_ind)
            } 
          
          equation_aggxdig <- function(Uk) {
            Uk_2 <- data.frame(con_agg_dig_comm %*% data.matrix(Uk[,-c(1:2)]), check.names = F)
            Uk_2$agg_code <-rownames(Uk_2)
            Uk_2 <-left_join(agg_desc,Uk_2)
          } 
            
          equation_aggxagg <- function(Uk) {
            Uk_1 <- data.matrix(Uk[,-c(1:2)]) %*% con_agg_dig_ind
            Uk_2 <- data.frame(con_agg_dig_comm %*% Uk_1, check.names = F)
            Uk_2$agg_code <-rownames(Uk_2)
            Uk_2 <-left_join(agg_desc,Uk_2)
          }  
            
          #use equations to create all 6 aggregated matrices
          setwd(file.path(yourwd,"final output files",paste(year,"_files",sep="")))
          
          
          for (j in c("digxagg","aggxdig","aggxagg"))  {
            for (i in c('Uk','Ut')){
              assign(paste(i,j,sep="_"), do.call(paste0("equation_",j),list(get(paste(i,"det",sep="_")))))
                write.csv(get(paste(i,j,sep="_")),paste(paste(i,j,sep="_"),"csv", sep="."))    
            }
          }
          
          
# Agggregate by commodity type ----
        setwd(file.path(yourwd,"final input files",paste(year,"_files",sep="")))    
   
        #For calculation simplicity, this ignores margins, so roughly in producers' values
              
          con_dig_cat <-read.csv("con_dig_cat.csv")
            con_dig_cat <-left_join(Uk_det[,1:2],con_dig_cat)
            con_dig_cat$count <-1
            con_dig_cat <-dcast(con_dig_cat, dig_code + dig_desc ~ asset_cat, value.var = "count"  , fun.aggregate = sum)
            con_dig_cat$'NA' <- NULL
            rownames(con_dig_cat) <-con_dig_cat$dig_code
            con_dig_cat <-t(con_dig_cat[,-c(1:2)])
              
          equation_catxagg <- function(Uk) {
            
            Uk_1 <- data.matrix(Uk[,-c(1:2)]) %*% con_agg_dig_ind
            Uk_2 <- data.frame(con_dig_cat %*% Uk_1, check.names = F)
            Uk_2 <- data.frame(t(Uk_2))
            Uk_2 <-sweep(Uk_2,1,rowSums(Uk_2),FUN="/")
            Uk_2$agg_code <-rownames(Uk_2)
            Uk_2 <-left_join(agg_desc,Uk_2)
          }  
           
          setwd(file.path(yourwd,"final output files",paste(year,"_files",sep="")))  
            Uk_catxagg <- equation_catxagg(Uk_det)
              write.csv(Uk_catxagg,"Uk_catxagg.csv")
               
            
          
        #Ward's hierarchical clustering
        cfc<-Uk_catxagg  
        rownames(cfc)<-cfc[,2]
          
          #https://www.statmethods.net/advstats/cluster.html
          d <- dist(cfc[,-1], method = "euclidean") # distance matrix
          fit <- hclust(d, method="ward.D2") #updated method requires D2 or D; D2 is preferred
          
          # save image:
          jpeg('Wards Cluster Plot D2.jpg')
          
          plot(fit) # display dendogram
          groups <- cutree(fit, k=6) # cut tree into 5 clusters
          # draw dendogram with red borders around the 5 clusters
          rect.hclust(fit, k=6, border="green") 
          
          dev.off()
          
# Clear all objects before running next year
  rm(list = ls())     
          
}



