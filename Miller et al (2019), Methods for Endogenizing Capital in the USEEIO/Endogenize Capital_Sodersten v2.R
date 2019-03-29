# The approach below is intended to compare to the methods used here:
    # Södersten, C.-J.H., R. Wood, and E.G. Hertwich. 2018. Endogenizing Capital in MRIO Models: The Implications for Consumption-Based Accounting. Environmental Science & Technology 52(22): 13250-13259.
  # Described in detail here:
    # Södersten, C.-J., R. Wood, and E.G. Hertwich. 2018. Environmental Impacts of Capital Formation. Journal of Industrial Ecology 22(1): 55-67.
  # Details of the method were discussed via email with Carl-Johan Hugo Södersten, carl-johan.sodersten@ntnu.no

# Notes from email exchange:
# Approach overview, for EXIOBASE:
# .	build a one-to-many concordance to match between the 8 KLEM assets and the 200 EXIOBASE products
# .	find the total GFCF for each EXIOBASE product (by summing across all business & government GFCF columns in Y)
# .	for each of the 8 KLEM assets: 
#   o	sum the GFCF associated with the set of matched products
#   o	divide to determine the proportion p1 of that GFCF sum that goes to each EXIOBASE product
# .	for each of the 32 KLEM industries: 
#   o	allocate the CFC for each of the 8 KLEM assets to the matched products based on those proportions p1
# .	Build a one-to-many concordance to match between the 32 KLEM industries and the  163 industries  
#   o	Use this to assign the 32 p2 vectors across the 200 industries 
#   o This is done similarly as the asset distribution - a one-to-many concordance combined with a proxy (the CFC), which is used to distribute one KLEMS industry across several EXIO industries when needed (to avoid double-counting)
# .	For each of the 163 EXIOBASE industries: 
#   o	Distribute the CFC based on the concordance & proxy
#   o	This creates the K matrix


# Approach overview, for USIO:
# .x	build a one-to-many concordance to match between the 8 KLEM assets and the 405 USIO products
#      -did this in purchaser's prices (con_klems_assets)
# .	find the total GFCF for each USIO product (by summing across all business & government GFCF columns in Y)
#      -P_alpha_tot
# .	for each of the 8 KLEM assets: 
#   o	sum the GFCF associated with the set of matched products
#   o	divide to determine the proportion p1 of that GFCF sum that goes to each USIO product
#      -klems_assets, normalized in producers prices

# .	for each of the 32 KLEM industries: 
#   o	allocate the CFC for each of the 8 KLEM assets to the matched products based on those proportions p1
# .x	Build a one-to-many concordance to match between the 32 KLEM industries and the  405 industries  
#   o	Use this to assign the 32 p2 vectors across the 200 industries 
#   o This is done similarly as the asset distribution - a one-to-many concordance combined with a proxy (the CFC), which is used to distribute one KLEMS industry across several EXIO industries when needed (to avoid double-counting)
# .	For each of the 405 USIO industries: 
#   o	Distribute the CFC based on the concordance & proxy
#   o	This creates the K matrix


#BEFORE YOU BEGIN USING THE CODE, REPLACE MY WORKING FILE DIRECTORY PATH WITH YOURS, BE SURE TO USE / NOT \: ----
yourwd<-"C:/Users/trm43/Box Sync/EGH Retreat Group Files/Fixed Assets/R approach/Miller et al (2019), Methods for Endogenizing Capital in the USEEIO"
year=2007

library(dplyr); library(reshape2)

# read in concordance tables:
setwd(file.path(yourwd,"final input files",paste(year,"_files",sep=""),"klems"))

# (a)i. IO-KLEMS commodity concordance in purchasers' value & (a)ii.	Convert from purchasers' to producers' value----
    #here the steps blend together, because the absolute value of GFCF for (a)i. derives from the same margins table P used for (a)ii.

#list of DIG commodities
dig_codes_com <- read.csv("dig_codes_com.csv") #excludes 4 DIGS (scrap, etc.)

# DIG commidity-KLEMS assets concordance (in purchasers values)
con_klems_assets <-read.csv("con_klems_assets.csv") 
  con_klems_assets$one <- 1
  con_klems_assets <- dcast(con_klems_assets, dig_code + dig_desc ~ klems_asset, value.var = "one", fun.aggregate = sum)
  con_klems_assets <- con_klems_assets[which(con_klems_assets$Var.3==0),-3] #remove digs without assets


#Here, adapt code from main analysis to transform from Purchasers' to Producers' value; only assess the sum of Y^K rather than each on its own


#read in excerpt of BEA Margins table (P) for Y^K categories
#note that not all DIG commodities are listed, generally only those which pertain to the Y^K
P<-read.csv("margins.csv") #doesn't have yk 
con_yk <-  read.csv("con_yk.csv") #is unnecessary here
P <- left_join(P, con_yk)
P <- P[,c(10,1:9)] #reorder

#remove Used, and Scrap, and Noncomparable Imports DIGs
P<-P[which(P$dig_code != "S00401" & P$dig_code != "S00402" & P$dig_code!="S00300"),]

# not relevant in this approach:
  # remove several small Asset-Dig combinations which were not assigned (due to unsolveable mismatch in underlying data)
  # P<-P[which( !(P$dig_code== "3219A0" & P$yk==1) ),]  #did not assign "All other wood product manufacturing" to nrp_str
  # P<-P[which( !(P$dig_code== "5111A0" & P$yk==3) ),]  #did not assign "Directory, mailing list, and other publishers" to nrp_ipp
  # P<-P[which( !(P$dig_code== "33721A" & P$yk==6) ),]  #only present in 2012, very small "Office furniture and custom architectural woodwork and millwork manufacturing"

#remove irrelevant entries with both Producers' and Purchasers' zero
P<-P[which((P$pro + P$pur)!=0),]

#to create transformation matrices T, want to remove double-counted margins in the DIGs, which have purchasers' value ==0. Pseudo-margins table in Purchasers' (alpha)
P_alpha <-P[which(P$pur!=0),]

#replace "pur" with sum of "pro" and margins, to address any minor rounding error
P_alpha[,10]<-rowSums(P_alpha[,6:9])
P_alpha <-P_alpha[which(P_alpha$pur!=0),] #in case some "pur" became 0 as a result of the sum

# new code: sum across all Y^K categories
P_alpha_tot <- aggregate(P_alpha[,6:10], by=list(dig_code = P_alpha$dig_code, dig_desc = P_alpha$dig_desc), FUN=sum)
P_alpha_tot <- P_alpha_tot[order(P_alpha_tot$dig_code),]

#to distribute the margin totals to the margin DIGs, extract them from the P matrix. Note that there are no margins for government structures, and no retail margins for other government investment.
# this approach assumes that the margin DIGs are distributed across all the assets in the same way. (Could instead manually match the retail or wholesale type to the asset type, though it would not be straightforward.)
P_mar <-P[which(P$pur==0),1:6] #margins have pro>0 but pur==0
P_mar <- aggregate(P_mar[,6], by=list(dig_code = P_mar$dig_code, dig_desc = P_mar$dig_desc), FUN=sum)
rownames(P_mar) <- P_mar$dig_code

# sort by dig_code
P_mar <- P_mar[order(P_mar$dig_code),]

#read in binary concordance between DIG and margin type
con_P_mar <-read.csv("con_P_mar.csv")
  con_P_mar[is.na(con_P_mar)]<-0
  con_P_mar <- con_P_mar[order(con_P_mar$dig_code),]

# short-cut code, since not separated by Y^k
P_mar <- left_join(P_mar, con_P_mar)
  P_mar[,4:6] <- sweep(P_mar[,4:6],1, P_mar[,3], FUN ="*")

P_mar_sum <-(t(colSums(P_mar[,4:6])))
  P_mar[,4:6] <- sweep(P_mar[,4:6],2, P_mar_sum, FUN = "/")

P_mar <- P_mar[,-3] #this columns is GFCF; we only want CFC in the end
  rownames(P_mar) <- P_mar$dig_code

P_alpha_tot[,8:(8+23-1)]<-0 #add columns for 23 margins DIGs
  colnames(P_alpha_tot)[8:(8+23-1)] <- rownames(P_mar)

P_alpha_tot[,8:(8+23-1)] <- data.matrix(P_alpha_tot[,4:6]) %*% data.matrix(t(P_mar[,3:5]))
# P_alpha_tot$check <- rowSums(P_alpha_tot[,c(3,8:30)]) - P_alpha_tot[,7]
# P_alpha_tot$check <- NULL #checks

#isolate the producers value
P_alpha_pro <- P_alpha_tot[,1:3]

#sum up the margins per asset
con_klems_assets_t <- con_klems_assets
  rownames(con_klems_assets_t)<- con_klems_assets_t$dig_code
  con_klems_assets_t <- con_klems_assets_t[,-c(1:2)]
  con_klems_assets_t <- data.frame(t(con_klems_assets_t), check.names = F)

  # parse check con_klems and p_alpha
  # check <- data.frame(cbind(colnames(con_klems_assets_t),as.character(P_alpha_tot$dig_code)))
  # identical(check$X1,check$X2) #true

klems_mar <- data.frame(data.matrix(con_klems_assets_t)%*%data.matrix(P_alpha_tot[,-c(1:7)]), check.names = F)
  klems_mar <- data.frame(t(klems_mar), check.names = F)
  klems_mar$dig_code <- rownames(klems_mar)
  klems_mar <- left_join(klems_mar, con_P_mar[,1:2])

# distribute the producers value across assigned klems assets
klems_assets <- con_klems_assets
  klems_assets[,-c(1:2)] <- sweep(klems_assets[,-c(1:2)],1,P_alpha_pro$pro,FUN = "*")

#now append the margins!
klems_assets <-bind_rows(klems_assets,klems_mar)

# check sums:
# sum(P_alpha_tot$pur); sum(klems_assets[,-c(1:2)]) #same!

# add in other DIGS not used
klems_assets <- left_join(dig_codes_com ,klems_assets)

klems_assets[is.na(klems_assets)] <-0

# now normalize by column sum
klems_assets[,-c(1:2)] <- sweep(klems_assets[,-c(1:2)],2,colSums(klems_assets[,-c(1:2)]),FUN="/")




# (a)iii.	Apply commodity concordance to CFC data & (a)iv. Allocate CFC from aggregate investors to DIGs----

# .	for each of the KLEM industries: 
#   o	allocate the CFC for each of the 8 KLEM assets to the matched products based on those proportions p1

#Create one-to-many binary concordance between KLEM industries and DIG industries
  #detailed-summary industry concordances:
  gov_dig_sig <-read.csv("gov_dig_sig.csv")
  
  nrp_dig_sig <-read.csv("nrp_dig_sig.csv")
  nrp_dig_sig<- nrp_dig_sig[,-c(4:5)]
  colnames(nrp_dig_sig) <- colnames(gov_dig_sig)
  
  dig_sig <- bind_rows(nrp_dig_sig, gov_dig_sig) #combine
  dig_sig <- unique(dig_sig) #remove duplicates, missing 2
  
  dig_codes_ind <- read.csv("dig_codes_ind.csv")
  
  dig_sig <- left_join(dig_codes_ind, dig_sig) #housing industries were missing, sig is NA
    # write.csv(dig_sig, "dig_sig.csv", row.names = F), to use SIG names in later concordance
  
  #read in klems- summary industry (SIG) concordance
  con_klems_ind <- read.csv("con_klems_ind.csv")
    con_klems_ind <- data.frame(lapply(con_klems_ind, as.character), stringsAsFactors = F)
  
  #use this to fill in missing SIG before casting
  dig_sig[which(is.na(dig_sig$sig_code)),3:4] <- con_klems_ind[71,3:4] #HS
    dig_sig_precast <-dig_sig #to use later
    dig_sig$one <-1
    dig_sig <- dcast(dig_sig, sig_code + sig_desc ~ dig_code, value.var = "one", fun.aggregate = sum )
  
  # prepare klems- summary industry (SIG) concordance
    con_klems_ind$one <- 1
    con_klems_ind <- dcast(con_klems_ind, klems_desc + klems_code ~ sig_code, value.var = "one", fun.aggregate = sum)
    
  #transform klems-industry concordances in DIG
    #parse check:
    # klems_ind_chk <-data.frame(cbind(colnames(con_klems_ind)[-c(1:2)],dig_sig$sig_code))
    # identical(klems_ind_chk$X1, klems_ind_chk$X2) #true
  
  con_klems_ind_dig <- cbind(con_klems_ind[,1:2], data.matrix(con_klems_ind[,-c(1:2)]) %*% data.matrix(dig_sig[,-c(1:2)]))
    # con_klems_ind_dig <-con_klems_ind_dig[order(con_klems_ind_dig$klems_code),]  


# KLEMS cfc data ----
#read in CFC in $1995 dollars
klems_cfc_95 <- read.csv("klems_cfc.csv", check.names = F) 

# read in 2007 GFCF price indices (note: less accurate than using CFC indices; BEA has both)
ip_klems <- read.csv("ip_klems.csv", check.names = F) # $2007 nominal / $1995 real
  ip_klems <- ip_klems[,-c(11:13)] #remove totals

# convert from 1995 to 2007:
require(matrixcalc) #for hadamard.prod

klems_cfc <- hadamard.prod(data.matrix(klems_cfc_95[,3:10]),data.matrix(ip_klems[,3:10])) #now is in $2007
  klems_cfc <- cbind(klems_cfc_95[,1:2], klems_cfc)
  # exclude klems_codes that aren't mutually exclusive (are sub/totals)
  klems_cfc <- inner_join(con_klems_ind[,1:2],klems_cfc) #was 38x10, now 29x10

  # transpose for assets x industries
  rownames(klems_cfc) <- klems_cfc$klems_code
  klems_cfc <- klems_cfc[,-c(1:2)]
  klems_cfc <- klems_cfc[,order(names(klems_cfc))]
  klems_cfc <- data.frame(t(klems_cfc), check.names = F)


#combine normalized DIG x asset matrix with asset x industry CFC data 
  # parse check:
  # checky <- data.frame(cbind(colnames(klems_assets)[3:10],rownames(klems_cfc)))
  # identical(checky$X1,checky$X2) #true

klems_cfc_digxind <- data.frame(cbind(klems_assets[,1:2],data.matrix(klems_assets[,3:10]) %*% data.matrix(klems_cfc)), check.names = F)

#normalize; create proportions for each column
klems_cfc_digxind[,-c(1:2)] <- sweep(klems_cfc_digxind[,-c(1:2)], 2, colSums(klems_cfc_digxind[,-c(1:2)]), FUN="/")

  # colSums(klems_cfc_digxind[,-c(1:2)]) # checks

# Use this to assign the 32 p2 vectors across the 405 industries 
  # parse check:
    # check2<-data.frame(cbind(colnames(klems_cfc_digxind)[-c(1:2)],con_klems_ind_dig$klems_code))
    # identical(check2$X1,check2$X2) #true

klems_cfc_digxdig <- data.frame(cbind(klems_cfc_digxind[,1:2],data.matrix(klems_cfc_digxind[,-c(1:2)]) %*% data.matrix(con_klems_ind_dig[,-c(1:2)])), check.names = F)

  # check3 <-colSums(klems_cfc_digxdig[,-c(1:2)]) #checks
  


# distribute BEA CFC ------------------------------------------------------
  # .	For each of the 405 USIO industries: 
  #   o	Distribute the CFC based on the concordance & proxy
  #   o	This creates the K matrix 

 #approach: take CFC data at the sIG level, distribute it by GOS (or intermed if negative) from Use table; exclude private hh and custom duties 
  
# CFC data source at SIG level: https://www.bea.gov/products/industry-economic-accounts/underlying-estimates
ind_cfc <- read.csv("cfc_sig_2007.csv") #2007 CFC by "ind", rougly SIG but aggregated in 2007-classifications, so slightly more aggregated
  ind_cfc <- ind_cfc[order(ind_cfc$ind),]
  ind_cfc$ind <- as.character(ind_cfc$ind)
  
# read in concordances between 2007 (used in GOS data) and 2012 SIG ind classifications (used here)
con_sig_ind <-read.csv("con_cfc_sig.csv") #matching those industries to the 2012 SIG classifcations

#read in 2007 GOS, detailed, producers (from Use table)
gos <- read.csv("gos.csv") 
  gos[is.na(gos)]<-0

gos$value <-gos$gos #for most, will be by value

gos <- left_join(gos, dig_sig_precast) #bring in SIG codes
   gos <- gos[,c(6:7,1:5)] #reorder

# SIG codes 523 and GLSE have negatives in their GOS, so use intermed instead
gos[which(gos$sig_code=="523"),"value"]<-gos[which(gos$sig_code=="523"),"intermed"]
gos[which(gos$sig_code=="GSLE"),"value"]<-gos[which(gos$sig_code=="GSLE"),"intermed"]
gos[which(gos$sig_code=="GFE"),"value"]<-gos[which(gos$sig_code=="GFE"),"intermed"] #not an issue for main method, since manually assigned

# bring in industries defined for CFC aggregate data
gos <-left_join(gos, con_sig_ind)
  gos<- gos[,c(8,1:7)]
    
# cast the value by ind
gos_ind <- dcast(gos,dig_code + dig_desc ~ ind, value.var = "value", fun.aggregate = sum )  

# normalize by column sum, per ind
gos_ind[,-c(1:2)] <- sweep(gos_ind[,-c(1:2)],2,colSums(gos_ind[,-c(1:2)]), FUN = "/" )

# convert BEA CFC by industry data from SIG to DIG using GOS allocation
  # parse check:
  # check4 <- data.frame(cbind(colnames(gos_ind)[-c(1:2)], ind_cfc$ind ))
  # identical(check4$X1, check4$X2) #true

dig_cfc <- data.frame(cbind(gos_ind[,c(1:2)], data.matrix(gos_ind[,-c(1:2)]) %*% data.matrix(ind_cfc[,2]) ),check.names = F)
colnames(dig_cfc)[3] <- "cfc"

rownames(dig_cfc) <- dig_cfc$dig_code
dig_cfc[,1:2] <-NULL

dig_cfc <-t(dig_cfc)

# finally, sweep the Klems proportions by the CFC total:

  #parse check:
  # check5 <- data.frame(cbind(dig_cfc$dig_code,colnames(klems_cfc_digxdig)[-c(1:2)]))
  # identical(check5$X1,check5$X2)

klems_cfc_digxdig[,-c(1:2)] <-sweep(klems_cfc_digxdig[,-c(1:2)], 2, dig_cfc[1,], FUN = "*")

# sum check
 # sum(dig_cfc); sum(ind_cfc$cfc); sum(klems_cfc_digxdig[,-c(1:2)]) #checks

setwd(file.path(yourwd,"final output files",paste(year,"_files",sep=""),"klems"))
write.csv(klems_cfc_digxdig, "klems_cfc_digxdig.csv", row.names = F)

# klems_cfc_digxdig1 <- read.csv("klems_cfc_digxdig.csv")
  # klems_cfc_digxdig1$X <-NULL

# compare to main method --------------------------------------------------
setwd(file.path(yourwd,"final output files",paste(year,"_files",sep="")))

Ukstar_det <-read.csv("Ukstar_det.csv", check.names = F) #before highways & streets reallocation; doesn't have 4 unused DIGs
Uk_det <- read.csv("Uk_det.csv", check.names = F)  #after highways & streets reallocation

# compare overall CFC sums:
sum(Ukstar_det[,-c(1:2)]);        # 2251880
sum(Uk_det[,-c(1:2)]);            # 2251880 
sum(klems_cfc_digxdig[,-c(1:2)])  # 2252806

# prepare for comparison:
# ensure that all rows are in the same order; add 4 missing DIGs to Ukstar
Ukstar_det <-left_join(Uk_det[,c(1:2)], Ukstar_det)
  Ukstar_det[is.na(Ukstar_det)] <-0

klems_cfc_digxdig<-left_join(Uk_det[,c(1:2)], klems_cfc_digxdig)
  klems_cfc_digxdig[is.na(klems_cfc_digxdig)] <-0 #new 4 rows are NA

#ensure that all column names are the same
  # check6 <- data.frame(cbind(colnames(Ukstar_det), colnames(Uk_det),colnames(klems_cfc_digxdig)))
  # identical(check6$X1, check6$X2) #true
  # identical(check6$X1, check6$X3) #false #true after reorder
dig_cols <- colnames(Uk_det)

klems_cfc_digxdig<- klems_cfc_digxdig[,(dig_cols)]

setwd(file.path(yourwd,"final output files",paste(year,"_files",sep=""),"klems"))

write.csv(klems_cfc_digxdig, "Uk_det_klems.csv", row.names = F)

  # check6 <- data.frame(cbind(colnames(Ukstar_det), colnames(Uk_det),colnames(klems_cfc_digxdig)))
  # identical(check6$X1, check6$X2) #true
  # identical(check6$X1, check6$X3) #false

# determine  rows that have capital in any of the matrices
k_check <- data.frame(cbind(Uk_det[,c(1:2)],rowSums(Ukstar_det[,-c(1:2)]),rowSums(Uk_det[,-c(1:2)]),rowSums(klems_cfc_digxdig[,-c(1:2)])))
  k_check$sum <- rowSums(k_check[,-c(1:2)])
  k_digs <- k_check[which(k_check$sum>0),1:2]

# restrict to those rows for comparison
Ukstar_k <-inner_join(Ukstar_det,k_digs)
Uk_k <-inner_join(Uk_det,k_digs)
klems_k <- inner_join(klems_cfc_digxdig,k_digs)

  #check rows are the same
  # check_k <- data.frame(cbind(as.character(Uk_k$dig_code), klems_k$dig_code))
  # identical(check_k$X1, check_k$X2) #true
  
# approach 1:
# take the difference of the matrices
ukstar_klems <-  data.frame(cbind(Uk_k[,c(1:2)], Ukstar_k[,-c(1:2)] - klems_k[,-c(1:2)]),check.names = F)
uk_klems <-  data.frame(cbind(Uk_k[,c(1:2)], Uk_k[,-c(1:2)] - klems_k[,-c(1:2)]),check.names = F)

# take the percent difference
ukstar_klems_pct <-  data.frame(cbind(Uk_k[,c(1:2)], ukstar_klems[-c(1:2)]/Ukstar_k[,-c(1:2)] ),check.names = F)
uk_klems_pct <-  data.frame(cbind(Uk_k[,c(1:2)], uk_klems[-c(1:2)]/Uk_k[,-c(1:2)] ),check.names = F)
# note that there are many Inf and NaN to deal with
is.na(ukstar_klems_pct) <- sapply(ukstar_klems_pct,is.infinite)


  write.csv(ukstar_klems, "ukstar_klems.csv" ,row.names = F)
  write.csv(uk_klems, "uk_klems.csv" ,row.names = F)
  write.csv(ukstar_klems_pct, "ukstar_klems_pct.csv" ,row.names = F)
  write.csv(uk_klems_pct, "uk_klems_pct.csv" ,row.names = F)

# melt these in order to make density plots
melted <- left_join(melt(ukstar_klems, id.vars=c("dig_code", "dig_desc"), value.name = "ukstar_klems"), melt(uk_klems, id.vars=c("dig_code", "dig_desc"), value.name = "uk_klems"))
melted <- left_join(melted,  melt(Ukstar_k, id.vars=c("dig_code", "dig_desc"),value.name = "Ukstar_k"))
melted <- left_join(melted,  melt(Uk_k, id.vars=c("dig_code", "dig_desc"), value.name = "Uk_k"))
melted <- left_join(melted,  melt(klems_k, id.vars=c("dig_code", "dig_desc"), value.name = "klems_k"))
melted <- left_join(melted,dig_codes_ind, by=c("variable" = "dig_code")) #should this be: dig_codes_com  not dig_codes_ind
melted <- melted[,c(1:3,9,4:8)]
colnames(melted)[1:4] <- c("dig_code_comm", "dig_desc_comm", "dig_code_ind", "dig_desc_ind")

melted2 <- melt(melted, id.vars = c("dig_code_comm", "dig_desc_comm", "dig_code_ind", "dig_desc_ind"), variable.name ="comparator" )

melted_diff <- melted2[which(melted2$comparator=="ukstar_klems" | melted2$comparator=="uk_klems" ),]
# melted_pct <- melted2[which(melted2$comparator!="ukstar_klems" & melted2$comparator!="uk_klems" ),]

  write.csv(melted, "differences, long form.csv")

# plot results
d_ukstar_klems<- density(melted$ukstar_klems)
plot(d_ukstar_klems)

boxplot(value ~ comparator, data= melted_diff, outline=T, main= "difference")
boxplot(value ~ comparator, data= melted_pct, main = "pct difference")

# approach 2
# melt, then take difference / pct difference of any element with >0
melted3 <- left_join(melt(Ukstar_k, id.vars=c("dig_code", "dig_desc"), value.name = "ukstar_k"), melt(Uk_k, id.vars=c("dig_code", "dig_desc"), value.name = "uk_k"))
melted3 <- left_join(melted3,  melt(klems_k, id.vars=c("dig_code", "dig_desc"),value.name = "klems_k"))
melted3$sum <- rowSums(melted3[,-c(1:3)])
melted3 <- melted3[which(melted3$sum !=0),]
melted3$sum <- NULL

melted3$ukstar_klems <- melted3$ukstar_k - melted3$klems_k
melted3$uk_klems <- melted3$uk_k - melted3$klems_k

d3_ukstar_klems<- density(melted3$ukstar_klems)
plot(d3_ukstar_klems, main = "d3_ukstar_klems")


#Create technical coefficient matrix for capital formation----

setwd(file.path(yourwd,"final input files",paste(year,"_files",sep="")))

#read in x, V, q: 
x<- read.csv("x.csv", check.names = F)
  x[is.na(x)]<-1 #changed 0 to 1 in order for matrix to invert
V<- read.csv("V.csv", check.names = F)
  V[is.na(V)]<-0 #sparse matrix, changed to 0
q<- read.csv("q.csv", check.names = F)
  q[is.na(q)]<-1 #changed 0 to 1 in order for matrix to invert

#equation 13: 
equation13 <- function(U){
  # U <- U_det #diagnosis
  
  U_mat <- data.matrix(U[,-c(1:2)])
  x_inv <-1/x[,3]
  x_inv_diag <- diag(x_inv)
  B <- U_mat %*% x_inv_diag
  
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

#check for identical structure
  # x_check <- data.frame(cbind(as.character(x[,1]), colnames(U_det)[-c(1:2)]))
  # identical(x_check$X1, x_check$X2) #true
  # V_check <- data.frame(cbind(V[,c(1:2)], x[,c(1:2)]))
  # identical(V_check$dig_code, V_check$dig_code.1) #true
  # V_check2 <- data.frame(cbind(colnames(V)[-c(1:2)], as.character(U_det[,1])))
  # identical(V_check2$X1, V_check2$X2) #true
  # q_check <- data.frame(cbind(as.character(q[,1]),as.character(U_det[,1])))
  # identical(q_check$X1, q_check$X2) #true

#check that rows match:
  # check_A <- data.frame(cbind(as.character(U_det$dig_code), klems_cfc_digxdig$dig_code))
  # identical(check_A$X1, check_A$X2) #true

#check that columns match:
  # check_x <- data.frame(cbind(as.character(x$dig_code), colnames(U_det)[-c(1:2)]))
  # identical(check_x$X1, check_x$X2)

A <- equation13(U_det)

Ak_klems <- equation13(klems_cfc_digxdig)

# Equation 15: Create total A matrix  
Ut_det_klems <- cbind(U_det[,1:2], U_det[,-c(1:2)]+klems_cfc_digxdig[,-c(1:2)])

At_klems <- equation13(Ut_det_klems)
# At2 <- cbind(U_det[,1:2],A[,-c(1:2)]+Ak[,-c(1:2)]) #alternative approach

setwd(file.path(yourwd,"final output files",paste(year,"_files",sep=""),"klems"))

# write.csv(U_det,  "U_det.csv", row.names = F)
write.csv(klems_cfc_digxdig, "Uk_det_klems.csv", row.names = F)
write.csv(Ut_det_klems, "Ut_det_klems.csv", row.names = F)

write.csv(A,   "A.csv", row.names = F)
write.csv(Ak_klems,  "Ak_klems.csv", row.names = F)
write.csv(At_klems,  "At_klems.csv", row.names = F)
# write.csv(At2,  "At2.csv", row.names = F)


# compare A with main method ----------------------------------------------

# read in files created in main method
setwd(file.path(yourwd,"final output files",paste(year,"_files",sep="")))

A2<- read.csv("A.csv", check.names = F) #to check A's are same
Ak<- read.csv("Ak.csv",check.names = F)
  Ak$dig_code <- as.character(Ak$dig_code)
At<- read.csv("At.csv", check.names = F)

# compare
# check As
  A_check <- data.frame(cbind(A[,c(1:2)],A[,-c(1:2)]-A2[,-c(1:2)] ), check.names = F)
  min(A_check[,-c(1:2)]) ; max(A_check[,-c(1:2)]) #very smallll differences (5E-16); checks!
  rm(A2)
  
  # confirm identical structure:
  identical(Ak$dig_code, as.character(Ak_klems$dig_code)) #true
  identical(colnames(Ak), colnames(Ak_klems)) #true

#find difference in Ak 
Ak_diff <- data.frame(cbind(Ak[,c(1:2)],Ak[,-c(1:2)]-Ak_klems[,-c(1:2)] ), check.names = F)

# melt to long-form for comparison
Ak_melt <- melt(Ak, id.vars=c("dig_code", "dig_desc"), variable.name = "dig_code_ind", value.name = "Ak")
Ak_klems_melt <- melt(Ak_klems, id.vars=c("dig_code", "dig_desc"), variable.name = "dig_code_ind", value.name = "Ak_klems")
Ak_diff_melt <- melt(Ak_diff, id.vars=c("dig_code", "dig_desc"), variable.name = "dig_code_ind", value.name = "Ak_diff")
Ak_comp <- left_join(Ak_melt, Ak_klems_melt); Ak_comp <- left_join(Ak_comp, Ak_diff_melt)
Ak_comp <- left_join(Ak_comp,dig_codes_ind, by=c("dig_code_ind" = "dig_code")) #should this be dig_codes_com 


# filter to matrix elements where either Ak is nonzero
Ak_comp <- Ak_comp[which(Ak_comp$Ak>0 | Ak_comp$Ak_klems>0),]
Ak_comp <- Ak_comp[,c(1:3,7,4:6)]
colnames(Ak_comp)[1:4] <- c("dig_code_comm", "dig_desc_comm", "dig_code_ind", "dig_desc_ind")

Ak_comp_nomar <- anti_join(Ak_comp, P_mar[,c(1:2)], by=c("dig_code_comm" = "dig_code"))
# Ak_comp_mar <- semi_join(Ak_comp, P_mar[,c(1:2)], by=c("dig_code_comm" = "dig_code"))


setwd(file.path(yourwd,"final output files",paste(year,"_files",sep=""),"klems"))
write.csv(Ak_comp,"Ak_comp.csv",row.names = F)
write.csv(Ak_comp_nomar,"Ak_comp, no margins.csv",row.names = F)




Ak_comp_d <- density(Ak_comp$Ak_diff)
plot(Ak_comp_d, main="Ak_comp_d")

# Ak_rows <-rowSums(Ak_diff)

