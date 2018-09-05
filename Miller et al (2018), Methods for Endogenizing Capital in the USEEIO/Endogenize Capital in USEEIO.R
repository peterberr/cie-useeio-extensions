#Code pertaining to: "Methods for Endogenizing Capital in the USEEIO".
# T. Reed Miller1*, Peter Berrill2, Paul Wolfram2, Ranran Wang2, Yookyung Kim2, Xinzhu Zheng2, Edgar G. Hertwich1,2
# 1 School of Engineering and Applied Sciences, Yale University, New Haven, USA 
# 2 Center for Industrial Ecology, School of Forestry and Environmental Studies, Yale University, New Haven, USA 
# * Corresponding author: reed.miller@yale.edu

#BEFORE YOU BEGIN USING THE CODE, REPLACE MY WORKING FILE DIRECTORY PATH WITH YOURS, BE SURE TO USE / NOT \: ----
yourwd<-"C:/Users/trm43/Box Sync/EGH Retreat Group Files/Fixed Assets/R approach/"

yourwd<-"C:/Users/trm43/Box Sync/EGH Retreat Group Files/Fixed Assets/R approach/Miller et al (2018), Methods for Endogenizing Capital in the USEEIO"

#load necessary packages (install first if needed)
library(readxl); library(dplyr); library(reshape2); library(tidyr)

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
setwd(file.path(yourwd,"final input files"))

#Note that in the Equations in the text, the concordance tables are in y x h, suggesting all 389 DIGs y; these tables actually are the subset of pertinent DIGs.  Later in the process the unused DIG commodities will be merged in.

#(a)i. 1	Nonresidential private fixed investment in structures ----
con_nrp_str <-read.csv("con_nrp_str.csv") #long format
#note that there is no investment in "SB44: Local transit structures" in 2007, so nominally allocated to "Other nonresidential structures"

#convert to wide format
con_nrp_str <- dcast(con_nrp_str, dig_code + dig_desc ~ asset_nipa, value.var = "inv", fun.aggregate = sum )


#(a)i. 2	Nonresidential private fixed investment in equipment ----
  # This is based on the PEQ bridge #Steps (a)i and (a)ii combined due to many-to-many table; see below in Step (a)ii


#(a)i. 3	Nonresidential private fixed investment in IPP ----
con_nrp_ipp <-read.csv("con_nrp_ipp.csv", check.names = F) #wide format


#(a)i. 4	Residential private fixed investment ----
#rp concordance happens in steps:
#the fixed asset (fa) types are assigned one rp category
con_rp_fa <-read.csv("con_rp_fa.csv") #long format

con_rp_fa <-dcast(con_rp_fa,rp_fa_code+ rp_fa_type + rp_fa ~ rp_cat, value.var = "inv", fun.aggregate = sum )
rownames(con_rp_fa)<-con_rp_fa$rp_fa_code  #in the BEA, three digits preceed this code depending on investment / CFC/ net stock

#the rp categories are distributed across the detail industry group (dig) commodities
con_rp_dig <-read.csv("con_rp_dig.csv", check.names = F) #wide format

#combine 
con_rp <-cbind(con_rp_dig[,c(1,2)],data.matrix(con_rp_dig[,-c(1,2)])%*%t(data.matrix(con_rp_fa[,-c(1:3)])))

#(a)i. 5-13	Government fixed investment ----

#since government FA data from single tables, reading it in together
con_gov <- read.csv("con_gov.csv") #associates the lines in Tables 7.3 and 7.5 with final demand categories and DIG commodities
  
  #note that "9999" signals a nominal value.  
  #The goal of the concordance is to assign DIGs to assets; if a DIG is spread across multiple assets within a Yk, that is ok; no need to make proportional
  
for(i in 5:13){
  assign(paste("con_gov",i,sep="_"),dcast(con_gov[which(con_gov$yk==i),],dig_code + dig_desc ~ line, value.var = "inv", fun.aggregate = sum))
 }


##############################################################################################################################################################

#(a)ii.	Convert from purchasers' to producers' value  ----

#read in excerpt of BEA Margins table (P) for Y^K categories
#note that not all DIG commodities are listed, generally only those which pertain to the Y^K
P<-read.csv("margins.csv")

#remove Used, and Scrap, and Noncomparable Imports DIGs
P<-P[which(P$dig_code != "S00401" & P$dig_code != "S00402" & P$dig_code!="S00300"),]

#remove irrelevant entries with both Producers' and Purchasers' zero
P<-P[which((P$pro + P$pur)!=0),]

#to create transformation matrices T, want to remove double-counted margins in the DIGs, which have purchasers' value ==0. Pseudo-margins table in Purchasers' (alpha)
P_alpha <-P[which(P$pur!=0),]

#replace "pur" with sum of "pro" and margins, to address any minor rounding error
P_alpha[,10]<-rowSums(P_alpha[,6:9])

#create transformation matrices Trans by normalizing 4 components of Producers' value by the purchasers' value
#see Equation 2
Trans<-P_alpha #same structure as P_alpha
Trans<-Trans[order(Trans$yk,Trans$dig_code),] 

  # This approach works (with additional steps to recombine dataframes; Sweep is used for convenience) 
    # P_alpha_pur <-solve(diag(P_alpha$pur)) #matrix to normalize other components
    # Trans_o<-P_alpha$pro %*% P_alpha_pur
    # Trans_w<-P_alpha$who %*% P_alpha_pur
    # Trans_r<-P_alpha$ret %*% P_alpha_pur
    # Trans_t<-P_alpha$tra %*% P_alpha_pur

#in R, can achieve same result of Equation 2 with Sweep function, which has the benefit of retained dataframe structure
  Trans[,6:10] <-sweep(Trans[,6:10], 1, Trans[,10], FUN= "/")

#to distribute the margin totals to the margin DIGs, extract them from the P matrix. Note that there are no margins for government structures, and no retail margins for other government investment.
  P_mar <-P[which(P$pur==0),1:6] #margins have pro>0 but pur==0
  
  #cast from long to wide format
  P_mar <-dcast(P_mar,yk + yk_code + yk_desc ~ dig_code, value.var = "pro", fun.aggregate = sum)
  
  #read in binary concordance between DIG and margin type
  con_P_mar <-read.csv("con_P_mar.csv")
  
  #this is effectively performing Equations 4 & 5, but simultaneous for all Y^K. Note: Equations 3, 6, 7 below for each YK
    P_mar_sum <- data.matrix(P_mar[,4:12]) %*% data.matrix(con_P_mar[,3:5]) #effectively Equation 4
    
    P_mar_who <-P_mar
    P_mar_who[,4:12] <-data.matrix(P_mar[,4:12]) %*% diag(con_P_mar[,"who"])
    P_mar_who[,4:12] <-solve(diag(P_mar_sum[,"who"]))%*% data.matrix(P_mar_who[,4:12]) #effectively Equation 5
    rownames(P_mar_who)<-P_mar_who$yk
    P_mar_who<- data.frame(t(P_mar_who[,-c(1:3)]),check.names = F)
    P_mar_who$dig_code <- rownames(P_mar_who)
    
    P_mar_ret <-P_mar
    P_mar_ret[,4:12] <-data.matrix(P_mar[,4:12]) %*% diag(con_P_mar[,"ret"])
    P_mar_ret[2:4,4:12] <-solve(diag(P_mar_sum[2:4,"ret"]))%*% data.matrix(P_mar_ret[2:4,4:12]) #limit to non-zero rows 2:4 for matrix to invert
    rownames(P_mar_ret)<-P_mar_ret$yk
    P_mar_ret<- data.frame(t(P_mar_ret[,-c(1:3)]),check.names = F)
    P_mar_ret$dig_code <- rownames(P_mar_ret)
    
    P_mar_tra <-P_mar
    P_mar_tra[,4:12] <-data.matrix(P_mar[,4:12]) %*% diag(con_P_mar[,"tra"])
    P_mar_tra[,4:12] <-solve(diag(P_mar_sum[,"tra"]))%*% data.matrix(P_mar_tra[,4:12])
    rownames(P_mar_tra)<-P_mar_tra$yk
    P_mar_tra<- data.frame(t(P_mar_tra[,-c(1:3)]),check.names = F)
    P_mar_tra$dig_code <- rownames(P_mar_tra)
    
  #read in full list of 389 DIG commodity codes
  dig_codes_com <-read.csv("dig_codes_com.csv")
  
  #Defined a function for Equations 6-10 
  equation6_10 <- function(con_alpha, Y_k){
    
    # diagnose
    # con_alpha<-con_gov_9  
    # Y_k <-9
    # parse_chk <-  cbind(Trans_yk[,c(4:5)], con_alpha[,c(1:2)]) 
    
    # 
    Trans_yk<-Trans[which(Trans$yk==Y_k),]
    pro <- cbind(con_alpha[,1:2],diag(Trans_yk[,6]) %*% data.matrix(con_alpha[,-(1:2)]))
      colnames(pro)<-colnames(con_alpha)
    
    #take the margin sum across DIGs, Equation 6
    tra <- matrix(1,1,NROW(Trans_yk)) %*% data.matrix((diag(Trans_yk[,7]) %*% data.matrix(con_alpha[,-(1:2)])))
    who <- matrix(1,1,NROW(Trans_yk)) %*% (diag(Trans_yk[,8]) %*% data.matrix(con_alpha[,-(1:2)]))
    ret <- matrix(1,1,NROW(Trans_yk)) %*% (diag(Trans_yk[,9]) %*% data.matrix(con_alpha[,-(1:2)]))
    
    #allocate margin sum to appropriate DIG commodity from P_mar_, Equation 7
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
    
    # Combine four items, Equation 8 
    beta <- bind_rows(pro, tra2, who2, ret2 )
    beta <- aggregate(beta[,-c(1:2)], by=list(dig_code=beta$dig_code), FUN=sum )
      colnames(beta)<-c("dig_code",colnames(con_alpha)[-c(1:2)])
    
    #add a column duplicating the final column so sweep functions work on datasets with a single column; later remove it
    beta$extra <-beta[,ncol(beta)] 
    
    # Find column sums, Equation 9
    beta_sum <- colSums(beta[,-1]) #doesn't work for single column; works since Extra is added
    
    #Normalize by column sums, Equation 10. Note: again we take advantage f the Sweep function
    beta[,-1] <- sweep(beta[,-1],2,beta_sum, FUN = "/") #doesn't work for single column; works since Extra is added
      beta$extra<-NULL
    
    #merge with all commodity DIGs
    beta<-left_join(dig_codes_com,beta)
    beta[is.na(beta)]<-0
    
    beta
  }
  
  # test6 <-equation6_10(con_gov_6,6)
  
#(a)ii. 1	Nonresidential private fixed investment in structures ----
  con_nrp_str_beta <-equation6_10(con_nrp_str,1)
  con_nrp_str_beta$`Mobile structures` <-NULL #zero in CFC data
  
#(a)ii. 2	Nonresidential private fixed investment in equipment ----
#this is combined with step (a)i #separate steps due to many-to-many structure of PEQ Bridge
  peq_brg<-read.csv("peq_brg.csv")
  
  #replace "pur" with sum of "pro" and margins, to address any minor rounding error
  peq_brg[,9]<-rowSums(peq_brg[,5:8])
  
  #remove Used, and Scrap, and Noncomparable Imports DIGs
  peq_brg<-peq_brg[which(peq_brg$dig_code != "S00401" & peq_brg$dig_code != "S00402" & peq_brg$dig_code!="S00300"),]
  
  #remove residentiual equipment
  peq_brg<-peq_brg[which(peq_brg$peq_desc != "Residential equipment"),]
  
  #create function to cast each item
  peq_brg_cast <-function(item){
    cast<-dcast(peq_brg,dig_code + dig_desc ~ peq_desc, value.var = item, fun.aggregate = sum)
    cast<-left_join(dig_codes_com,cast) #merge will all commodity DIGs
    cast[is.na(cast)]<-0
    rownames(cast)<-cast$dig_code
    cast<-data.matrix(cast[,-c(1:2)]) #create data matrix
  }
  
  #create a function for equations 3,6-10 (to prevent creation of many temporary objects, even though only applied once)
  equations_nrp_equ <- function(Y_k){
    
    peq_brg_items= c("pro","tra","who","ret","pur")
      for(i in peq_brg_items){
        assign(paste(i,"cast",sep="_"), peq_brg_cast(i)) #effectively the result of Equation 3; don't need Equation 2
        assign(i, matrix(1,1,389)%*%get(paste(i,"cast",sep="_"))) #Equation 6
      }
    
    #add row of "dig_code" to pro_cast
    pro_cast<-data.frame(pro_cast, check.names = F)
    pro_cast$dig_code <-rownames(pro_cast)
    
    #same as general function, equation6_10
        #allocate margin sum to appropriate DIG commodity from P_mar_, Equation 7
        tra2<- data.frame(cbind(P_mar_tra[,"dig_code"],data.matrix(P_mar_tra[,paste(Y_k)])%*%tra), check.names = F)
          tra2[,-1] <- sapply(tra2[,-1], function(x) as.numeric(as.character(x)))
        who2<- data.frame(cbind(P_mar_who[,"dig_code"],P_mar_who[,paste(Y_k)]%*%who), check.names = F)
          who2[,-1] <- sapply(who2[,-1], function(x) as.numeric(as.character(x)))
        ret2<- data.frame(cbind(P_mar_ret[,"dig_code"],P_mar_ret[,paste(Y_k)]%*%ret), check.names = F)
          ret2[,-1] <- sapply(ret2[,-1], function(x) as.numeric(as.character(x)))
        colnames(tra2)[1]<-"dig_code"; colnames(who2)[1]<-"dig_code"; colnames(ret2)[1]<-"dig_code"
        
        # Combine four items, Equation 8 
        beta <- bind_rows(pro_cast, tra2, who2, ret2 )
        beta <- aggregate(beta[,-27], by=list(dig_code=beta$dig_code), FUN=sum )
        
        # Find column sums, Equation 9
        beta_sum <- colSums(beta[,-1])
        
        #Normalize by column sums, Equation 10. Note: again we take advantage f the Sweep function
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
  con_nrp_ipp_beta <- equation6_10(con_nrp_ipp,3)
  
#(a)ii. 4	Residential private fixed investment ----
  con_rp_beta <- equation6_10(con_rp,4)
  
  
#(a)ii. 5-13	Government fixed investment ----
  
  #there are no margins for government structures, so alpha = beta / purchasers'=producers'. Define simpler function, removing some parts of broader function
    equation6_10_nomarg <- function(con_alpha){
      
      beta <-con_alpha
      # Find column sums, Equation 9
      beta_sum <- colSums(beta[,-c(1:2)])
      
      #Normalize by column sums, Equation 10. Note: again we take advantage f the Sweep function
      beta[,-c(1:2)] <- sweep(beta[,-c(1:2)],2,beta_sum, FUN = "/")
      
      #merge with all commodity DIGs
      beta<-left_join(dig_codes_com,beta)
      beta[is.na(beta)]<-0
      beta
    }
  
  #create beta concordances for government Structures Yks
  for(i in c(5,8,11)){
      assign(paste("con_gov",i,"beta",sep="_"),equation6_10_nomarg(get(paste("con_gov",i,sep="_"))))
  }
  
  #create beta concordances for government equipment and IPP Yks
  for(i in c(6,7,9,10,12,13)){
    assign(paste("con_gov",i,"beta",sep="_"),equation6_10(get(paste("con_gov",i,sep="_")),i))
  }
  
    
  #combine them all together, order by line
  con_gov_beta <-con_gov_5_beta
  
  for(i in 6:13){
    con_gov_beta <-left_join(con_gov_beta, get(paste("con_gov",i,"beta", sep="_")))
    }

  #reorder columns; a bit inefficient, easier solutions didn't work
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
  
  cfc_nrp <-read.csv("cfc_nrp.csv", check.names = F)
    cfc_nrp <-cfc_nrp[,c("code","2007","2013")] #get years of interest
  
  #split out complex code to industry code (sig_code) and asset code
  cfc_nrp <- separate(cfc_nrp,code, c("skip1","sig_code","skip2","asset_code", "skip3"),  sep=c(3,7, 8, 12))
    cfc_nrp$skip1<-NULL;cfc_nrp$skip2<-NULL; cfc_nrp$skip3<-NULL #Not necessary
  
  #get asset descriptions, combine  with CFC data
  nrp_faa_codes <-read.csv("nrp_faa_codes.csv")
  
  cfc_nrp <- left_join(cfc_nrp,nrp_faa_codes, by="asset_code")
    cfc_nrp <- cfc_nrp[!is.na(cfc_nrp$asset_cat),]
    
    #subset wide-format CFC data for each year
    for (i in c('2007','2013')){
    assign(paste("cfc_nrp",i,sep="_"), dcast(cfc_nrp,asset_cat + asset_desc + asset_code ~ sig_code, value.var = paste(i), fun.aggregate =sum))
      
      }
  
  
  #create function to perform Equation 11
  equation11_nrp <- function(con_beta, cat, year){
    temp<- cbind(con_beta[,c(1:2)],data.matrix(con_beta[,-c(1:2)])%*% data.matrix(get(paste("cfc_nrp",year,sep="_"))[which(get(paste("cfc_nrp",year,sep="_"))==cat),-c(1:3)]))
    
    tempsum <-rowSums(temp[,c("5210","5220")]) #issue with concordance between DIG and SIG; aggregating into "52XX"
      temp[,"5210"]<-tempsum
      temp[,"5220"]<-NULL
      colnames(temp)[43]<-"52XX"
      
    temp
                  
    }
    
    
  #(a)iii. 1	Nonresidential private fixed investment in structures ----
  
    for (i in c('2007','2013')){
      assign(paste("Ukstar_nrp_str",i, sep="_"), equation11_nrp(con_nrp_str_beta,"Structures",i))
    }
  
    
  #(a)iii. 2	Nonresidential private fixed investment in equipment ----
  
    for (i in c('2007','2013')){
      assign(paste("Ukstar_nrp_equ",i, sep="_"), equation11_nrp(con_nrp_equ_beta,"Equipment",i))
    }
    
    
  #(a)iii. 3	Nonresidential private fixed investment in IPP ----
  for (i in c('2007','2013')){
    assign(paste("Ukstar_nrp_ipp",i, sep="_"), equation11_nrp(con_nrp_ipp_beta,"IPP",i))
  }
    
    
#(a)iii. 4	Residential private fixed investment ----
  cfc_rp <-read.csv("cfc_rp.csv", check.names = F)
    cfc_rp <- cfc_rp[order(cfc_rp$rp_fa_code),]
    
  for (i in c('2007','2013')){
    assign(paste("Ukstar_rp",i,sep="_"), cbind(con_rp_beta[,1:2], data.matrix(con_rp_beta[,-c(1:2)])%*%cfc_rp[,i]))
     # assign(colnames(get(paste("Ukstar_rp",i,sep="_")))[3],i) #doesn't work
  }
  
#(a)iii. 5-13	Government fixed investment ----
    
    cfc_gov<-read.csv("cfc_gov.csv", check.names = F)
    
    #since assigning CFC to government DIGs is complicated, for this step we will retain the lines, and spread out the CFC per line
      #multiplying the CFC x 1000 to convert from billions to millions, matching NRP and RP
    
    for (i in c('2007','2013')){
      assign(paste("Ukstar_gov",i,sep="_"), cbind(con_gov_beta[,1:2], sweep(con_gov_beta[,-c(1:2)],2,t(cfc_gov[,i]*1000),FUN="*")))
      
    }
    
  
    
    
##############################################################################################################################################################

# # Apply commodity concordance to 2013 investment data ---------------------
#     # This step is to provide guidance for the 2013 RAS of the final demand
#     setwd(file.path(yourwd,"final input files/2013 inv"))
#     
#     #(a)iii.	Apply commodity concordance to inv data ----
#     
#     #(a)iii. 1-3	Nonresidential private fixed investment ----
#     
#     inv_nrp <-read.csv("inv_nrp.csv", check.names = F)
#     inv_nrp <-inv_nrp[,c("code","2007","2013")] #get years of interest
#     
#     #split out complex code to industry code (sig_code) and asset code
#     inv_nrp <- separate(inv_nrp,code, c("skip1","sig_code","skip2","asset_code", "skip3"),  sep=c(3,7, 8, 12))
#     inv_nrp$skip1<-NULL;inv_nrp$skip2<-NULL; inv_nrp$skip3<-NULL #Not necessary
#     
#     #get asset descriptions, combine  with inv data
#     nrp_faa_codes <-read.csv("nrp_faa_codes.csv")
#     
#     inv_nrp <- left_join(inv_nrp,nrp_faa_codes, by="asset_code")
#     inv_nrp <- inv_nrp[!is.na(inv_nrp$asset_cat),]
#     
#     #subset wide-format inv data for each year
#     for (i in c('2007','2013')){
#       assign(paste("inv_nrp",i,sep="_"), dcast(inv_nrp,asset_cat + asset_desc + asset_code ~ sig_code, value.var = paste(i), fun.aggregate =sum))
#       
#     }
#     
#     
#     #create function to perform Equation 11
#     equation11_nrp_inv <- function(con_beta, cat, year){
#       temp<- cbind(con_beta[,c(1:2)],data.matrix(con_beta[,-c(1:2)])%*% data.matrix(get(paste("inv_nrp",year,sep="_"))[which(get(paste("inv_nrp",year,sep="_"))==cat),-c(1:3)]))
#       
#       tempsum <-rowSums(temp[,c("5210","5220")]) #issue with concordance between DIG and SIG; aggregating into "52XX"
#       temp[,"5210"]<-tempsum
#       temp[,"5220"]<-NULL
#       colnames(temp)[43]<-"52XX"
#       
#       temp
#       
#     }
#     
#     
#     #(a)iii. 1	Nonresidential private fixed investment in structures ----
#     
#     for (i in c('2007','2013')){
#       assign(paste("Ukstar_invnrp_str",i, sep="_"), equation11_nrp_inv(con_nrp_str_beta,"Structures",i))
#       assign(paste("sum_invnrp_str",i, sep="_"), cbind(con_nrp_str_beta[,1:2], rowSums(get(paste("Ukstar_invnrp_str",i, sep="_"))[,-c(1:2)])) )
#     }
#     
#       colnames(sum_invnrp_str_2013) <-c("dig_code", "dig_desc", "1	Nonresidential private fixed investment in structures ")
#     
#     #(a)iii. 2	Nonresidential private fixed investment in equipment ----
#     
#     for (i in c('2007','2013')){
#       assign(paste("Ukstar_invnrp_equ",i, sep="_"), equation11_nrp_inv(con_nrp_equ_beta,"Equipment",i))
#       assign(paste("sum_invnrp_equ",i, sep="_"), cbind(con_nrp_equ_beta[,1:2], rowSums(get(paste("Ukstar_invnrp_equ",i, sep="_"))[,-c(1:2)])) )
#       
#     }
#       colnames(sum_invnrp_equ_2013) <-c("dig_code", "dig_desc", "2	Nonresidential private fixed investment in equipment")
#     
#     
#     #(a)iii. 3	Nonresidential private fixed investment in IPP ----
#     for (i in c('2007','2013')){
#       assign(paste("Ukstar_invnrp_ipp",i, sep="_"), equation11_nrp_inv(con_nrp_ipp_beta,"IPP",i))
#       assign(paste("sum_invnrp_ipp",i, sep="_"), cbind(con_nrp_ipp_beta[,1:2], rowSums(get(paste("Ukstar_invnrp_ipp",i, sep="_"))[,-c(1:2)])) )
#     }
#     
#       colnames(sum_invnrp_ipp_2013) <-c("dig_code", "dig_desc", "3	Nonresidential private fixed investment in IPP")
#     
#     
#     
#     #(a)iii. 4	Residential private fixed investment ----
#     inv_rp <-read.csv("inv_rp.csv", check.names = F)
#     inv_rp <- inv_rp[order(inv_rp$rp_fa_code),]
#     
#     for (i in c('2007','2013')){
#       assign(paste("sum_invrp",i,sep="_"), cbind(con_rp_beta[,1:2], data.matrix(con_rp_beta[,-c(1:2)])%*%inv_rp[,i]))
#       
#       # assign(colnames(get(paste("Ukstar_invrp",i,sep="_")))[3],i) #doesn't work
#     }
#     
#     colnames(sum_invrp_2013) <-c("dig_code", "dig_desc", "4	Residential private fixed investment")
#     
#     
#     #(a)iii. 5-13	Government fixed investment ----
#     
#     inv_gov<-read.csv("inv_gov.csv", check.names = F)
#     
#     #since assigning inv to government DIGs is complicated, for this step we will retain the lines, and spread out the inv per line
#     #multiplying the inv x 1000 to convert from billions to millions, matching NRP and RP
#     
#     con_inv_gov <- inv_gov[,c(1,3:4)]
#        con_inv_gov$ones <- 1
#        con_inv_gov$yk_name <- paste(con_inv_gov$yk, con_inv_gov$yk_desc, sep="  " )
#        con_inv_gov <- dcast(con_inv_gov, line ~ yk_name, value.var = "ones", fun.aggregate = sum )
#        con_inv_gov <- con_inv_gov[,c(1,6:10,2:5)]
#        
#     for (i in c('2007','2013')){
#       assign(paste("Ukstar_invgov",i,sep="_"), cbind(con_gov_beta[,1:2], sweep(con_gov_beta[,-c(1:2)],2,t(inv_gov[,i]*1000),FUN="*")))
#       assign(paste("sum_invgov",i,sep="_"), cbind(con_gov_beta[,1:2], data.matrix(get(paste("Ukstar_invgov",i,sep="_"))[,-c(1:2)]) %*% data.matrix(con_inv_gov[,-1] ) ))
#     }
#     
#     
#     #combine all sums
#       require (plyr)
#       sum_inv_2013 <- join_all(list(sum_invnrp_str_2013, sum_invnrp_equ_2013, sum_invnrp_ipp_2013, sum_invrp_2013, sum_invgov_2013 ), by = c("dig_code", "dig_desc"), type = 'full')
#     
#       write.csv(sum_inv_2013, "sum_inv_2013.csv", row.names = F)
#     
##############################################################################################################################################################
    

#(a)iv.	Allocate CFC from aggregate investors to DIGs ----
    
#(a)iv. 1-3	Nonresidential private fixed investment ----
    #Get Intermediate Output and Gross Operating Surplus (GOS), from the value add matrix in the 2007 Use table
    gos <- read.csv("gos.csv")
    
    #Get dig-sig industry concordance; fewer than 389 because only NRP industries
    nrp_dig_sig <-read.csv("nrp_dig_sig.csv")
    
    gos <-right_join(nrp_dig_sig,gos)
      gos<-gos[!is.na(gos$sig_code),] #remove government rows
    
    gos$value <- gos$gos
    
    #the "5230: Securities, commodity contracts, and investments" SIG has negative GOS in 2007; replace it with intermediate output 
      gos[which(gos$sig_code=="5230"),"value"]<-gos[which(gos$sig_code=="5230"),"intermed"]
    
    #the "Private households" industry is an odd case; shouldn't be assigned any capital.
      gos[which(gos$dig_desc=="Private households"),"value"]<-0
      
    #cast this sig x dig
    gos_cast <-dcast(gos, sig_code + sig_desc ~ dig_code, value.var = "value", fun.aggregate=sum)
    
    #Equation 12
    gos_sum <- data.matrix(gos_cast[,-c(1:2)])%*%matrix(1,379,1)
    
    #Equation 13
    gos_norm <-gos_cast
    gos_norm[,-c(1:2)] <-sweep(gos_norm[,-c(1:2)],1,gos_sum, FUN = "/")
    
      #parse with sig_codes in Ukstar_nrp's
      gos_norm_order <-match(colnames(Ukstar_nrp_str_2007)[-c(1:2)], gos_norm$sig_code)
      gos_norm <-gos_norm[gos_norm_order,]
    
    #Equation 14, create a function
    equation14_nrp <- function(Ukstar){
      temp<- cbind(Ukstar[,c(1:2)], data.matrix(Ukstar[,-c(1:2)])%*% data.matrix(gos_norm[,-c(1:2)] ))
    }
    

#(a)iv. 1	Nonresidential private fixed investment in structures ----
    for (i in c('2007','2013')){
      assign(paste("Ukstar_nrp_str_det",i,sep="_"), equation14_nrp(get(paste("Ukstar_nrp_str",i,sep="_"))))     
      }
    
#(a)iv. 2	Nonresidential private fixed investment in equipment ---- 
    for (i in c('2007','2013')){
      assign(paste("Ukstar_nrp_equ_det",i,sep="_"), equation14_nrp(get(paste("Ukstar_nrp_equ",i,sep="_"))))     
    }

#(a)iv. 3	Nonresidential private fixed investment in IPP ----    
    for (i in c('2007','2013')){
      assign(paste("Ukstar_nrp_ipp_det",i,sep="_"), equation14_nrp(get(paste("Ukstar_nrp_ipp",i,sep="_"))))     
    }
    
#(a)iv. 1:3	Nonresidential private fixed investment  ----
    #combine 1-3
    for (i in c('2007','2013')){
      temp <- bind_rows(get(paste("Ukstar_nrp_str_det",i,sep="_")),get(paste("Ukstar_nrp_equ_det",i,sep="_")),get(paste("Ukstar_nrp_ipp_det",i,sep="_")))
      assign(paste("Ukstar_nrp_det",i,sep="_"), aggregate(temp[,-c(1:2)], by=list(dig_code=temp$dig_code, dig_desc =temp$dig_desc), FUN = sum ))
      rm(temp)
    }
    
    #for diagnosis:
    # for (i in c('2007','2013')){
    #   temp <- bind_rows(get(paste("Ukstar_nrp_str",i,sep="_")),get(paste("Ukstar_nrp_equ",i,sep="_")),get(paste("Ukstar_nrp_ipp",i,sep="_")))
    #   assign(paste("Ukstar_nrp",i,sep="_"), aggregate(temp[,-c(1:2)], by=list(dig_code=temp$dig_code, dig_desc =temp$dig_desc), FUN = sum ))
    #   rm(temp)
    # }
    

#(a)iv. 4	Residential private fixed investment ----    
    #No allocation is needed; assign to "Housing" DIG industry, "5310HS"
    colnames(Ukstar_rp_2007)[3]<-"5310HS"
    colnames(Ukstar_rp_2013)[3]<-"5310HS"
    
#(a)iv. 5-13	Government fixed investment ----
    
   con_gov_ind <-read.csv("con_gov_ind.csv")
  
   #both 'general government' and 'government enterprises'; prepare separately then combine
    #general government DIGs:
    for (i in c('2007','2013')){
      assign(paste("con_gov_ind_gen",i,sep="_"), dcast(con_gov_ind, line + item + yk + yk_desc  ~ dig_gen,value.var = paste("gov_gen",i, sep="_"), fun.aggregate = sum))  
    }
    
    #government enterprises:
    con_gov_ind_ent<- read.csv("con_gov_ind_ent.csv", check.names = F)
      con_gov_ind_ent<- left_join(con_gov_ind[,c(1,3:5)],con_gov_ind_ent)
      con_gov_ind_ent[is.na(con_gov_ind_ent)]<-0
      
      #normalize by rowsum to remove rounding error
      con_gov_ind_ent[,5:10] <-sweep(con_gov_ind_ent[,5:10],1,rowSums(con_gov_ind_ent[,5:10]),FUN ="/")
      con_gov_ind_ent[is.na(con_gov_ind_ent)]<-0
    
    #distribute the enterprise portion across relevant DIG industries
    for (i in c('2007','2013')){
      assign(paste("con_gov_ind_ent",i,sep="_"), cbind(con_gov_ind_ent[,1:4],sweep(con_gov_ind_ent[,-c(1:4)],1,con_gov_ind[,paste("gov_ent",i, sep="_")], FUN = "*") ) )  
    }
    
    #combine general and enterprise
      for (i in c('2007','2013')){
        assign(paste("con_gov_ind",i,sep="_"), left_join(get(paste("con_gov_ind_gen",i,sep="_")),get(paste("con_gov_ind_ent",i,sep="_"))) )
      }
      
      #con_gov_ind_2007$sum <-rowSums(con_gov_ind_2007[,5:13]) #checks
      
    #Allocate CFC per line to DIG industries
      for (i in c('2007','2013')){
        assign(paste("Ukstar_gov_det",i,sep="_"), cbind(get(paste("Ukstar_gov",i,sep="_"))[,c(1:2)], data.matrix(get(paste("Ukstar_gov",i,sep="_"))[,-c(1:2)]) %*% data.matrix(get(paste("con_gov_ind",i,sep="_"))[,-c(1:4)] ) ) )
      }
      
    
           
##############################################################################################################################################################

#(b)	Create an intermediate detailed CFC combining all Y^K categories ----
      
      #combine the three investor classes
      for (i in c('2007','2013')){
        assign(paste("Ukstar_det",i,sep="_"), left_join(dig_codes_com,left_join(get(paste("Ukstar_nrp_det",i,sep="_")),left_join(get(paste("Ukstar_rp",i,sep="_")), get(paste("Ukstar_gov_det",i,sep="_")))) ))
      }
      
      #reorder columns to match Use table
      dig_codes_ind <- read.csv("dig_codes_ind.csv", check.names = F) #order in Use table
          dig_codes_ind_match<-match(colnames(dig_codes_ind),colnames(Ukstar_det_2007))
      Ukstar_det_2007 <-Ukstar_det_2007[,dig_codes_ind_match]
      Ukstar_det_2013 <-Ukstar_det_2013[,dig_codes_ind_match]
      
      setwd(file.path(yourwd,"final output files"))
        write.csv(Ukstar_det_2007, "Ukstar_det_2007.csv")
        write.csv(Ukstar_det_2013, "Ukstar_det_2013.csv")
      
##############################################################################################################################################################

#(c)	Create the final detailed CFC by re-allocating Highways  & Streets ----
      
      #convert equipment in purchasers' assets to purchasers' DIGs, take sum over vehicle DIGs
        #Yk: 2,6,9,12
        
        setwd(file.path(yourwd,"final input files"))
        
  #(c) 2.	Nonresidential private fixed investment in equipment ---- 
        
        
          equations_nrp_equ2 <- function(){ #modified version of equations_nrp_equ
            
            peq_brg_items= c("pur")
            for(i in peq_brg_items){
              assign(paste(i,"cast",sep="_"), peq_brg_cast(i)) #effectively the result of Equation 3; don't need Equation 2
              assign(i, matrix(1,1,389)%*%get(paste(i,"cast",sep="_"))) #Equation 6
            }
            
            #add row of "dig_code" to pur_cast
            pur_cast<-data.frame(pur_cast, check.names = F)
            pur_cast$dig_code <-rownames(pur_cast)
            pur_cast<-pur_cast[,c(27,1:26)]
            
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
        
        #Equation 15: Apply CFC to aggregate investors
        for (i in c('2007','2013')){
          assign(paste("Ukstar_nrp_equ2",i, sep="_"), equation11_nrp(con_nrp_equ,"Equipment",i))
        }
        
        #Equation 16: Distribute to  detailed investors
        for (i in c('2007','2013')){
          assign(paste("Ukstar_nrp_equ2_det",i,sep="_"), equation14_nrp(get(paste("Ukstar_nrp_equ2",i,sep="_"))))     
        }
        
        #create binary concordance for vehicle DIGs:
        con_veh <-read.csv("con_veh.csv")
        
        #sum over vehicles
        for (i in c('2007','2013')){
          assign(paste("Ukstar_nrp_equ2_veh",i,sep="_"), data.matrix(t(con_veh$veh))%*%data.matrix(get(paste("Ukstar_nrp_equ2_det",i,sep="_"))[,-c(1:2)]))     
        }
        
  #(c) 6,9, 12 Government fixed investment in equipment----  
        
        #combine and merge each gov equipment Y_k with all DIG codes
        con_gov2<-dig_codes_com #initiate it
        
        for(i in c(6,9,12)){
          con_gov2<-left_join(con_gov2,get(paste("con_gov",i,sep="_")))
        }
        
        con_gov2[is.na(con_gov2)]<-0
        con_gov2<-equation6_10_nomarg(con_gov2) #normalize by column sums
        
        
        #since assigning CFC to government DIGs is complicated, for this step we will retain the lines, and spread out the CFC per line
          #multiplying the CFC x 1000 to convert from billions to millions, matching NRP and RP
        
        con_gov_lines <-match(colnames(con_gov2)[-c(1:2)], cfc_gov$line)
        cfc_gov2<- cfc_gov[con_gov_lines,]
        
          for (i in c('2007','2013')){
            assign(paste("Ukstar_gov2",i,sep="_"), cbind(get("con_gov2")[,1:2], sweep(get("con_gov2")[,-c(1:2)],2,t(cfc_gov2[,i]*1000),FUN="*")))
            }
        
        #Allocate CFC per line to DIG industries
        for (i in c('2007','2013')){
          assign(paste("Ukstar_gov2_det",i,sep="_"), cbind(get(paste("Ukstar_gov2",i,sep="_"))[,c(1:2)], data.matrix(get(paste("Ukstar_gov2",i,sep="_"))[,-c(1:2)]) %*% data.matrix(get(paste("con_gov_ind",i,sep="_"))[con_gov_lines ,-c(1:4)] ) ) )
        }
        
        #sum over vehicles
        for (i in c('2007','2013')){
          assign(paste("Ukstar_gov2_veh",i,sep="_"), data.matrix(t(con_veh$veh))%*%data.matrix(get(paste("Ukstar_gov2_det",i,sep="_"))[,-c(1:2)]))     
        }
        
    #combine veh sums from NRP and gov; add rp for consistency
        
        Ukstar_rp_veh <- data.frame('5310HS'=0, check.names = F)
        
        for (i in c('2007','2013')){
          assign(paste("Ukstar_veh",i,sep="_"), cbind(get(paste("Ukstar_nrp_equ2_veh",i,sep="_")),get(paste("Ukstar_gov2_veh",i,sep="_")),Ukstar_rp_veh ))
          }
        
        #order columns in line with Use table
        dig_codes_ind_match2<-match(colnames(dig_codes_ind)[-c(1:2)],colnames(Ukstar_veh_2007))
        
        Ukstar_veh_2007 <-Ukstar_veh_2007[,dig_codes_ind_match2]
        Ukstar_veh_2013 <-Ukstar_veh_2013[,dig_codes_ind_match2]
        
    #bring in household share of vehicle CFC
        #RP #find sum of vehicle depreciation in Consumer Durable Goods
        #note that this is a very significant sum!
        
        cfc_hh_veh <-read.csv("cfc_hh_veh.csv", check.names = F) #From Detailed CDG data
        cfc_hh_veh_sum <-matrix(1,1,2)%*% data.matrix(cfc_hh_veh[,-1])
        
        for (i in c('2007','2013')){
          assign(paste("cfc_hh_veh",i,sep="_"), data.frame(cfc_hh_veh_sum[,i]) )     
        }
    
    #calculate the total vehicle CFC, Equation 18
        for (i in c('2007','2013')){
          assign(paste("cfc_veh",i,sep="_"), sum(get(paste("cfc_hh_veh",i,sep="_")),get(paste("Ukstar_veh",i,sep="_"))))
        }
        
    #allocate "State and local government" (S00700) "Highways & streets" (233293) CFC based on vehicle CFC, Equation 19
       
        
        #SOMETHING WRONG WITH THIS, oh! NEED TO ADD BACK IN THE PRIVATELY OWNED H&S.
        
        equation19 <- function(year){
          #year='2007'
          
          Ukstar_det <-get(paste("Ukstar_det",year,sep="_"))
          cfc_veh <- get(paste("cfc_veh",i,sep="_"))
          cfc_hh_veh <- get(paste("cfc_hh_veh",i,sep="_"))
          Ukstar_veh <-get(paste("Ukstar_veh",year,sep="_"))
          
          psi<-Ukstar_det[which(Ukstar_det$dig_code=="233293"),'S00700']
          
          psi_ratio <- psi/cfc_veh
          
          psi_k_star <- sweep(Ukstar_veh,1,psi_ratio, FUN="*")
          
          psi_k_star_hh <- psi_ratio*cfc_hh_veh
          
          #Add the H&S for households to that for State & Local government
          psi_k_star[1,'S00700'] <- sum(psi_k_star[1,'S00700'],psi_k_star_hh[,])
          
          #zero out the H&S originally in State & Local government
          Ukstar_det[which(Ukstar_det$dig_code=="233293"),'S00700'] <- 0 
          
          #add another row with the H&S dig_code and dig_desc
          Ukstar_det[390,] <-Ukstar_det[which(Ukstar_det$dig_code=="233293"),]
            Ukstar_det[390,-c(1:2)] <- psi_k_star
          
          #aggregate the H&S rows
          Ukstar_det <- aggregate(Ukstar_det[,-c(1:2)],by=list(dig_code=Ukstar_det$dig_code, dig_desc=Ukstar_det$dig_desc), FUN = sum )
          
          #reorder to match dig_codes_com
          Ukstar_det<-left_join(dig_codes_com,Ukstar_det)
          
          Uk_det<-Ukstar_det
          }
        
        for (i in c('2007','2013')){
          assign(paste("Uk_det",i,sep="_"), equation19(i) )
        
        }
        
        setwd(file.path(yourwd,"final output files"))
          write.csv(Uk_det_2007, "Uk_det_2007.csv")
          write.csv(Uk_det_2013, "Uk_det_2013.csv")

#Create technical coefficient matrix for capital formation----
          
          setwd(file.path(yourwd,"final input files"))
          
          #scratch
          equation20 <- function(year, Uk_det){
            #year<-'2007'
            #Uk_det <-Uk_det_2007
            x<- read.csv(paste("x_",year,".csv", sep=""), check.names = F)
            V<- read.csv(paste("V_",year,".csv", sep=""), check.names = F)
            q<- read.csv(paste("q_",year,".csv", sep=""), check.names = F)
              q[,which(q[1,]==0)]<-1 #changed 0 to 1 in order for matrix to invert
              
            Uk<- data.matrix(Uk_det[,-c(1:2)])
            Bk <- Uk %*% solve(diag(x[1,]))
           
            V <-data.matrix(V[,-c(1:2)])
            V[is.na(V)]<-0
               D <- V %*% solve(diag(q[1,]))
            Ak <-data.matrix(Bk) %*% data.matrix(D)
            Ak <-cbind(Uk_det[,c(1:2)],Ak)
            colnames(Ak) <- colnames(Uk_det)
              
            Ak
          }
          
          Ak_2007 <- equation20('2007',Uk_det_2007)
          
          # for (i in c('2007','2013')){
          #   assign(paste("x",i,sep="_"), read.csv() )
          #   
          # } 
        
        #Combine Uk with U
          setwd(file.path(yourwd,"final input files"))
            U_det_2007 <- read.csv("U_det_2007.csv", check.names = F)
            U_det_2007[is.na(U_det_2007)]<- 0
            
            U_det_2013 <- read.csv("U_det_2013.csv", check.names = F) #from "v7 files / balance_us_sut_07r_basic.xlsx"
            U_det_2013[is.na(U_det_2013)]<- 0
            
            
          Ut_det_2007 <- cbind(U_det_2007[,1:2], U_det_2007[,-c(1:2)]+Uk_det_2007[,-c(1:2)])
          
          # setwd(file.path(yourwd,"final output files"))
          # Uk_det_2013 <- read.csv("Uk_det_2013.csv", check.names = F)
          # Uk_det_2013 <-Uk_det_2013[,2:392]

          Ut_det_2013 <- cbind(U_det_2013[,1:2], U_det_2013[,-c(1:2)]+Uk_det_2013[,-c(1:2)])
          
          
#Aggregate product categories for comparison----
          setwd(file.path(yourwd,"final input files"))
            con_agg_dig_com <- read.csv("con_agg_dig_com.csv")
            con_agg_dig_ind <- read.csv("con_agg_dig_ind.csv")
            agg_desc <- read.csv("agg_desc.csv")
            agg_desc <- agg_desc[-c(24:25),]
          
          #prepare binary concordances  
          
          con_agg_dig_ind <- dcast(con_agg_dig_ind, dig_code +dig_desc ~ agg_code, value.var = "count", fun.aggregate = sum)
            dig_codes_ind_t<- data.frame(rownames(t(dig_codes_ind))[-c(1:2)])
              colnames(dig_codes_ind_t)<-"dig_code"
            con_agg_dig_ind <- left_join(dig_codes_ind_t,con_agg_dig_ind)
            rownames(con_agg_dig_ind )<- con_agg_dig_ind$dig_code
            con_agg_dig_ind<-data.matrix(con_agg_dig_ind[,-c(1:2)])
            
          con_agg_dig_com <- dcast(con_agg_dig_com, dig_code +dig_desc ~ agg_code, value.var = "count", fun.aggregate = sum)
            con_agg_dig_com <- left_join(dig_codes_com,con_agg_dig_com)
            rownames(con_agg_dig_com)<-con_agg_dig_com$dig_code          
            con_agg_dig_com<-t(con_agg_dig_com[,-c(1:2)])
            #transpose          
          
          #aggregate by industries, commodities, or both  
          equation_digxagg <- function(Uk) {
            Uk_1 <- cbind(Uk[,c(1:2)], data.matrix(Uk[,-c(1:2)]) %*% con_agg_dig_ind)
            } 
          
          equation_aggxdig <- function(Uk) {
            Uk_2 <- data.frame(con_agg_dig_com %*% data.matrix(Uk[,-c(1:2)]), check.names = F)
            Uk_2$agg_code <-rownames(Uk_2)
            Uk_2 <-left_join(agg_desc,Uk_2)
          } 
            
          equation_aggxagg <- function(Uk) {
            Uk_1 <- data.matrix(Uk[,-c(1:2)]) %*% con_agg_dig_ind
            Uk_2 <- data.frame(con_agg_dig_com %*% Uk_1, check.names = F)
            Uk_2$agg_code <-rownames(Uk_2)
            Uk_2 <-left_join(agg_desc,Uk_2)
          }  
            
          #use equations for both years
          setwd(file.path(yourwd,"final output files"))
          
          # for (i in c('2007','2013')){
            for (i in c('2013')){
            assign(paste("Uk","digxagg",i,sep="_"), equation_digxagg(get(paste("Uk","det",i,sep="_"))))
              write.csv(get(paste("Uk","digxagg",i,sep="_")),paste(paste("Uk","digxagg",i,sep="_"),"csv", sep="."))    
            
            assign(paste("Uk","aggxdig",i,sep="_"), equation_aggxdig(get(paste("Uk","det",i,sep="_"))))
              write.csv(get(paste("Uk","aggxdig",i,sep="_")),paste(paste("Uk","aggxdig",i,sep="_"),"csv", sep="."))
              
            assign(paste("Uk","aggxagg",i,sep="_"), equation_aggxagg(get(paste("Uk","det",i,sep="_"))))
              write.csv(get(paste("Uk","aggxagg",i,sep="_")),paste(paste("Uk","aggxagg",i,sep="_"),"csv", sep="."))
          }
          
          U_aggxagg_2007 <-equation_aggxagg(U_det_2007)
          
          Ut_aggxagg_2007 <-equation_aggxagg(Ut_det_2007)
              write.csv(Ut_aggxagg_2007, "Ut_aggxagg_2007 .csv")    
              
          Ut_aggxagg_2013 <-equation_aggxagg(Ut_det_2013)
              write.csv(Ut_aggxagg_2013, "Ut_aggxagg_2013 .csv") 
              

# Agggregate by commodity type ----
      setwd(file.path(yourwd,"final input files"))     
        
          #For calculation simplicity, this ignores margins, so roughly in producers' values
              
          con_dig_type <-read.csv("con_dig_type.csv")
            con_dig_type <-dcast(con_dig_type, dig_code + dig_desc ~ type, value.var = "count"  , fun.aggregate = sum)
            con_dig_type$'NA' <- NULL
            con_dig_type <- left_join(dig_codes_com,con_dig_type)
            rownames(con_dig_type) <-con_dig_type$dig_code
            con_dig_type <-t(con_dig_type[,-c(1:2)])
              
          equation_typexagg <- function(Uk) {
            
            Uk_1 <- data.matrix(Uk[,-c(1:2)]) %*% con_agg_dig_ind
            Uk_2 <- data.frame(con_dig_type %*% Uk_1, check.names = F)
            Uk_2 <- data.frame(t(Uk_2))
            Uk_2 <-sweep(Uk_2,1,rowSums(Uk_2),FUN="/")
            Uk_2$agg_code <-rownames(Uk_2)
            Uk_2 <-left_join(agg_desc,Uk_2)
          }  
           
          setwd(file.path(yourwd,"final output files"))  
            for (i in c('2007','2013')){
              assign(paste("Uk","typexagg",i,sep="_"), equation_typexagg(get(paste("Uk","det",i,sep="_"))))
              write.csv(get(paste("Uk","typexagg",i,sep="_")),paste(paste("Uk","typexagg",i,sep="_"),"csv", sep="."))
               
            }
          
        #Ward's hierarchical clustering
        cfc<-Uk_typexagg_2013  
        rownames(cfc)<-cfc[,2]
          
          #https://www.statmethods.net/advstats/cluster.html
          d <- dist(cfc[,-1], method = "euclidean") # distance matrix
          fit <- hclust(d, method="ward")
          plot(fit) # display dendogram
          groups <- cutree(fit, k=6) # cut tree into 5 clusters
          # draw dendogram with red borders around the 5 clusters
          rect.hclust(fit, k=6, border="green") 
          