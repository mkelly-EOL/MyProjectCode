#RS2Analysis-05/10/2018
require(data.table)
require(gmodels) 
require(anytime)
require(stringdist)        
require(gdata) 
require(car)
require(ryouready)
require(ggplot2)
require(scales)
require(vcd)


#Load base data
AARefHosp <- readRDS(file = "V:\\R_Projects\\RHomeDir\\RS2A\\_data\\Reference\\dtRefHosp.rds")

#All invasive tumours excluding nmsc with doi 1994-2016 that matched HIPE discharges 2015/2016
aT_ds1 <- readRDS(file = "V:\\R_Projects\\RHomeDir\\RS2A\\_data\\T_ds1.rds")
View(aT_ds1)
uniqueN(aT_ds1$DUM_REG)#55993

#All NCRI patients with any cancer doi 1994-2016 that matched HIPE discharges 2015/2016
aP_ds1 <- readRDS(file = "V:\\R_Projects\\RHomeDir\\RS2A\\_data\\P_ds1.rds")
View(aP_ds1)
uniqueN(aP_ds1$DUM_REG)#77182

#All HIPE episode data for 2015/2016  that matched data NCRI DOI data 1994-2016
aH_ds1 <- readRDS(file = "V:\\R_Projects\\RHomeDir\\RS2A\\_data\\H_ds1.rds")
View(aH_ds1)
uniqueN(aH_ds1$DUM_REG)#77183

##Deps data
fm_DEPS <- readRDS(file = "V:\\R_Projects\\RHomeDir\\RS2A\\_data\\DEPSData151617_dst2.rds")

#Tumour data set is the analysis dataset (i.e. with exclusions)
#Get relevant Patients only
a1 <- aT_ds1[,c(1)]
a1 <- unique(a1) #55993
uniqueN(a1$DUM_REG)#55993

#Restrict Patient to get relevant Patient data
bP_ds1 <- merge(aP_ds1, a1 ) #inner join
uniqueN(a1$DUM_REG)#55993
table(bP_ds1$sVS, exclude =  NULL)
##42185 alive at end of 2016
##13808 dead at end of 2016


#Restrict dataset further to patients who were alive at end of 2016 or  who died in 2016
bP_ds1[, yearDOD  := year(sDOD)]
bP_ds1 <- bP_ds1[sVS ==1 | yearDOD ==2016 ,] #49652
uniqueN(bP_ds1$DUM_REG) #49652
table(bP_ds1$yearDOD)
# 2016 2017 
# 7467 3239 
table(bP_ds1$sVS, exclude =  NULL)
# 1     2 
# 42185  7467 
#Apply this Restriction to Tumour ds, that is patients who were alive at end of 2016 or who died in 2016

a1 <- bP_ds1[,c(1)]
a1 <- unique(a1) #49652
bT_ds1 <- merge(aT_ds1, a1 ) #inner join 
uniqueN(bT_ds1$DUM_REG)#49652 patients with 54763 tumours 


a <- subset(aT_ds1, !(DUM_REG %in% bT_ds1$DUM_REG)) #7252 records
uniqueN(a$DUM_REG)#6340  patient with  dod = 2015 and 1 patient with dod =2011




#######################################################################

#HIPE data => multiple epsiodes per patient
#Find patients with  Hipe episode data 
bH_ds1 <- merge(aH_ds1, a1, by.x= 'DUM_REG', by.y='DUM_REG' ) #inner join 
uniqueN(bH_ds1$DUM_REG)#49652 patients with 486561

#Identify patients who had at least one epsiode in the cancer centres in 2016
cH_ds1 <- bH_ds1[yearD==2016 & CC > 0,] #163360

uniqueN(cH_ds1$DUM_REG)#24909 cases with 163360 episodes in cc in 2016
a2 <-cH_ds1[,c(1),]
a2 <- unique(a2) #24909  unique patients had at least one epsiode in the cancer centres in 2016

#Find all cases in HIPE for these patients who had at least one epsiode in the cancer centres in 2016
dH_ds1 <-  merge(bH_ds1, a2 ) #inner join  
uniqueN(dH_ds1$DUM_REG) #24909 patients with 293939 episodes

#Repeat check test
# a <- merge(bH_ds1, a2 ) #inner join  
# uniqueN(a$DUM_REG) #24909 patients with 293939 episodes
#Check
dH_ds1 <-  merge(aH_ds1, a2, by.x= 'DUM_REG', by.y='DUM_REG' )  #inner join  
uniqueN(dH_ds1$DUM_REG) #24909 patients with 293939 episodes
# the same 
#end  check


###########################################################################
#Discharged to, 11= Hospice (not in HIPE Hospital Listing) omits 3 Hipe hospice 
#027 Our Lady's Hospice (St Joseph's unit)
#028  Our Lady's Hospice (St Michael's ward)
#048 Blackrock Hospice
table(dH_ds1$DISC_CODE)
dH_ds1$toHospice <- as.numeric(dH_ds1$DISC_CODE == 11)


#Died in hospital
dH_ds1$HospitalDeath <- as.numeric(dH_ds1$DISC_CODE == 6 | dH_ds1$DISC_CODE == 7 )


############################################################################
#/Further HIPE processing Start
#Which episode to  take  for cases and controls
#sequence SPC episodes by DUMREG using dH_ds1 (data for all  who had an episode in the CC in 2016)
uniqueN(dH_ds1$DUM_REG) #24909 patients with 293939 episodes
setorder(dH_ds1, DUM_REG, h1off, h2off)#order the episodes by REG, admission,discharge
dH_ds1[, caco :=  as.numeric(any(toHospice | SPC)) , by=  'DUM_REG'] #identify cases and controls
dH_ds1[, Episeq := sequence(.N), by = c('DUM_REG')] #sequence the episodes by REG, admission,discharge
dH_ds1[, Episcount := uniqueN(row_id), by = c('DUM_REG')]#count the espiodes by reg
##First approach - try to assign each individual  to just one cancer centre , unit of analysis is patient
#Find the index SPC episode 
#Note not taking account whether the chosen episode is a CC episode, may need to revise this
dH_ds1[,Episcase := min(Episeq), by = c('DUM_REG')] #initalise episode  flag
#Cases
dH_ds1[SPC==1 | toHospice==1, Episcase := min(Episeq), by = c('DUM_REG')] #take first index spc episode for cases, (... is this a CC case?)
dH_ds1[,Episcase := max(Episcase),by = c('DUM_REG')] #set  for all episodes of this reg
#Controls
dH_ds1[caco==0 , Episcase := max(Episeq), by = c('DUM_REG')] #take last episode for controls
#Set flag to indicate whether Episcase is a CC case
dH_ds1[,EpiscaseIsCC := 0]
dH_ds1[Episcase ==Episeq, EpiscaseIsCC := as.numeric(CC>0)]
dH_ds1[,EpiscaseIsCC := max(EpiscaseIsCC),by = c('DUM_REG')] #set  for all episodes of this reg

#Find relevant cancer centre for cases and controls where the index episode is not  a CC
#assign CC from last CC episode for cases and controls   where index episode is NOT a CC episode,
dH_ds1[ ,CCEpis := as.integer(0)] #initalise CC episode  flag
dH_ds1[EpiscaseIsCC > 0 ,CCEpis := Episcase] #set CCEpis where the  index epsiode is a CC episode 
dH_ds1[,CCEpis := max(CCEpis),by = c('DUM_REG')] #set CCEpis for all episodes of this reg
dH_ds1[ CCEpis ==0 & CC > 0, CCEpis := max(Episeq), by = c('DUM_REG')] #otherwise take last  CC  episode irrespective of whether this is the index SPC episode for both cases and controls
dH_ds1[,CCEpis := max(CCEpis),by = c('DUM_REG')] #set CCEpis for all episodes of this reg
dH_ds1[, AssignedCC := 0]  #initalise variable to hold the CC  assignment for this reg based on last CC episode
dH_ds1[CCEpis > 0 , AssignedCC := CC,] #find CC
dH_ds1[,AssignedCC := max(AssignedCC),by = c('DUM_REG')] #assign cc for all episodes of this reg
View(dH_ds1)

dH_ds1[, AnyCCEpis :=  as.numeric(any(CC>0 )), by=  c('DUM_REG')]
#only way this would work reliably
dH_ds1[, CaseType :=  0,]
dH_ds1[, CaseSPC := any(SPC==1), by=  'DUM_REG'] #1 SPC Only
dH_ds1[, CaseHospice := any(toHospice==1), by =  'DUM_REG'] #2 Hospice Only
dH_ds1[, CaseBoth := any((CaseSPC==1 & CaseHospice ==1 )), by=  'DUM_REG'] #3 Both
dH_ds1[CaseSPC & !CaseHospice & !CaseBoth  , CaseType :=  1,]
dH_ds1[!CaseSPC & CaseHospice & !CaseBoth  , CaseType :=  2,]
dH_ds1[CaseSPC & CaseHospice, CaseType :=  3,]
dH_ds1$CaseSPC <-  NULL
dH_ds1$CaseHospice <-  NULL
dH_ds1$CaseBoth <-  NULL
dH_ds1$myCaseType <- ordered( dH_ds1$CaseType,
                     levels = c(0, 1,2,3),
                     labels = c("Control","SPC", "Hospice", "Both")) 
View(dH_ds1)
table(dH_ds1$CaseType)
#Set overnight flag per epsiode
dH_ds1[, overnight :=  (h2off -h1off) > 0,] 
#Count overnight flag per patient
dH_ds1[, overnightCount:= 0,] #initalise
dH_ds1[overnight == TRUE, overnightCount:= as.numeric(uniqueN(Episeq)), by = c('DUM_REG')]
dH_ds1[, overnightCount:= max(overnightCount), by = c('DUM_REG')]#set for all epsiodes for this reg] 
#Count number of  centres per case
dH_ds1[, CentreCount :=  uniqueN(DUMHID), by = c('DUM_REG')]#count the espiodes by reg,] 
#Count number of cancer centres per case
dH_ds1[CC==0, CCCount := 0,]
dH_ds1[CC> 0, CCCount :=  uniqueN(CC), by = c('DUM_REG')]#count the espiodes by reg,] #ignore non CC
dH_ds1[, CCCount :=  max(CCCount), by = c('DUM_REG')]#set for all epsiodes for this reg] 
#Count the number of SPC per case
dH_ds1[ SPC==1, SPCCount :=  uniqueN(Episeq), by = c('DUM_REG')]
dH_ds1[,SPCCount := max(SPCCount),by = c('DUM_REG')] #set  for all episodes of this reg
dH_ds1[is.na(SPCCount), SPCCount := 0,]
#Count the number of toHospice per case
dH_ds1[toHospice==1, toHospiceCount :=  uniqueN(Episeq), by = c('DUM_REG')]
dH_ds1[,toHospiceCount := max(toHospiceCount),by = c('DUM_REG')] #set  for all episodes of this reg
dH_ds1[is.na(toHospiceCount), toHospiceCount := 0,]
#Flag patients with no SPC or toHospice  at a cancer centre
dH_ds1[, IsCCCase:= 0, ]
dH_ds1[SPC==1 & CC > 0, IsCCCase:= 1, ]
dH_ds1[toHospice==1 & CC > 0, IsCCCase:= 1, ]
dH_ds1[, IsCCCase:= max(IsCCCase), by = c('DUM_REG')]
       
#Check number of patients with 0 cancer centres
x <- dH_ds1[CCCount ==1 & CC==0,] #0

#Check /
x <- dH_ds1[Episcase ==Episeq,] #24909 
table(x$caco)
#   0     1 
# 20989  3920 
table(x$CC, x$caco)
rm(x)




# End check /
# 
# # as.Date(17115 - 113, origin = '1970-01-01')
# 

# #Take last SPC  epsiode for cases and last  regular epsiode for each control, alternative code snippet
# dH_ds1[dH_ds1[caco==0, .I[Episeq == max(Episeq)], by=  'DUM_REG']$V1, control :=1]

# Further HIPE processing  End/
############################################################################################




#Find cases (with an SPC / toHospice ) and controls (without an SPC/toHospice) among patients who  had at least one epsiode in the cancer centres in 2016
fm_caco <-dH_ds1[Episcase == Episeq,c(1:46),] #24909
fm_caco[,table(caco,AnyCCEpis), ]

uniqueN(fm_caco$DUM_REG)#3811 SPC  + 109 toHospice=  3920 2016 cancer centre patients have had at least one SPC in 2015/2016 (CASES)
table(fm_caco$caco)
#Review  CASES in detail
fm_cases <-  fm_caco[caco==1,] #3920
fm_controls <-  fm_caco[caco==0,] #20989
table(fm_caco$myCaseType)
# Control     SPC   Hospice    Both 
# 0          3334     109      477 


#Check
#3920 + 20989 = 24909 unique REG
#End check
#rm(fm_cases, fm_controls,eH_ds1)



###################################################################################

#Merge the cases with the Patient data (that is patients who were alive at end of 2016 or  who died in 2016)
cP_ds1 <- merge(bP_ds1,fm_caco,  by.x = 'DUM_REG', by.y = 'DUM_REG') #inner join 24909
## test code df2[df1, on="x"][is.na(y), y := 0]

uniqueN(cP_ds1$DUM_REG) #24909

#Do analysis on Patients
cP_ds1[,table(myCaseType)] #3334
# myCaseType
# Control     SPC  Hospice    Both 
# 20989      3334     109     477 


cP_ds1[,table(sVS, caco)]
#         caco
# sVS     0     1
# 1    19449  1323
# 2     1540  2597

cP_ds1[,table(sVS, myCaseType)]
# sVS Control   SPC Hospice  Both
# 1   19449  1220      38    65
# 2    1540  2114      71   412

cP_ds1[,CrossTable(CC, caco)]
# caco
# CC     0    1
# 0    2174  950
# 1    2423  457
# 2    2323  283
# 3    2245  305
# 4    3184  472
# 5    2815  388
# 6    1258  252
# 7    3356  491
# 8    1211  322




cP_ds1[sVS==1,table(CC, caco)]
# caco
# CC     0    1
# 0     2003  408
# 1     2278  125
# 2     2150   76
# 3     1985  107
# 4     2987  141
# 5     2586  108
# 6     1173   74
# 7     3183  183
# 8     1104  101




cP_ds1[sVS==2,table(CC, caco)]
# caco
# CC    0   1
# 0    171 542
# 1    145 332
# 2    173 207
# 3    260 198
# 4    197 331
# 5    229 280
# 6     85 178
# 7    173 308
# 8    107 221



#Restrict to patients who died in 2016
dP_ds1 <-cP_ds1[yearDOD==2016,]#4137
uniqueN(dP_ds1$DUM_REG)#4137


dP_ds1[,table(AnyCCEpis,caco)]
# caco
# AnyCCEpis    0    1
#      1     1540 2597



dP_ds1[,table(sVS,caco),]
# caco
# sVS    0    1
# 2     1540 2597

dP_ds1[,table(CC, caco)]

# CC    0   1
# 0    171 542
# 1    145 332
# 2    173 207
# 3    260 198
# 4    197 331
# 5    229 280
# 6     85 178
# 7    173 308
# 8    107 221




dP_ds1[,table(myCaseType)] #3334
# myCaseType
# Control     SPC  Hospice    Both 
# 1540       2114      71     412 
x <-  dP_ds1[yearD==2015,c(1),] 

#need to decide what is a true case (CC cases only n=2597) and a true control(No SPC or hospice discharge, n=1540, ~cases n= 542)
#Which episode to take
uniqueN(eH_ds1$DUM_REG) #3920
uniqueN(a4$DUM_REG) #3920
uniqueN(a5$DUM_REG) #3920




# CrossTable(dP_ds1$caco, dP_ds1$CC,digits=0, max.width = 1, expected=FALSE, prop.r=TRUE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, format= "SPSS")
# CrossTable(dP_ds1$SEX, dP_ds1$CC,digits=0, max.width = 1, expected=FALSE, prop.r=TRUE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, format= "SPSS")
# write.table(t1[1:3], file = "C:\\temp\\f1.csv", sep = ",", col.names = NA, qmethod = "double")
dP_ds1[,CrossTable(caco,yearD,digits=0, max.width = 1, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]


cP_ds1[,CrossTable(sVS,CC,digits=0, max.width = 1, expected=FALSE, prop.r = FALSE,prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, format= "SPSS")]
cP_ds1[,CrossTable(sVS,firstCC,digits=0, max.width = 1, expected=FALSE, prop.r = FALSE,prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, format= "SPSS")]

dP_ds1[,CrossTable(myCaseType, CC,digits=0, max.width = 1, expected=FALSE, prop.r = FALSE,prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, format= "SPSS")]
dP_ds1[,CrossTable(myCaseType, firstCC,digits=0, max.width = 1, expected=FALSE, prop.r = FALSE,prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, format= "SPSS")]


dP_ds1[,CrossTable(caco,CC,digits=0, max.width = 1, expected=FALSE, prop.r = FALSE,prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, format= "SPSS")]
dP_ds1[,CrossTable(caco,firstCC,digits=0, max.width = 1, expected=FALSE, prop.r = FALSE,prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, format= "SPSS")]

dP_ds1[caco==1,CrossTable(my_sPOD,CC,digits=0, max.width = 1, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]
dP_ds1[caco==0,CrossTable(my_sPOD,CC,digits=0, max.width = 1, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]


dP_ds1[caco==1,CrossTable(my_sPOD,firstCC,digits=0, max.width = 1, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]
dP_ds1[caco==0,CrossTable(my_sPOD,firstCC,digits=0, max.width = 1, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]




dP_ds1[,CrossTable(my_gender,CC,digits=0, max.width = 1, expected=FALSE, prop.r=TRUE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, format= "SPSS")]
dP_ds1[caco==1,CrossTable(my_marital,CC,digits=0, max.width = 1, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, format= "SPSS")]
dP_ds1[caco==1,CrossTable(my_smoker,CC,digits=0, max.width = 1, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, format= "SPSS")]
dP_ds1[caco==1,CrossTable(my_depr_16,CC,digits=0, max.width = 1, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]
dP_ds1[caco==1,CrossTable(my_depr_11,CC,digits=0, max.width = 1, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]
dP_ds1[caco==1,CrossTable(my_sPOD,CC,digits=0, max.width = 1, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]

#Get Patient date of death from cT_ds1
fmTinfo <- cT_ds1[, c(1,31,34:36)] #dum-reg,numTex, VSAge,min and max Survival
fmTinfo <- unique(fmTinfo) #9163
uniqueN(fmTinfo$DUM_REG)#9159
fmTinfo <- fmTinfo[,numP := .N, by = DUM_REG]# 4 patients mismatch on age by 1 year (rounding issue, take older age)
fmTinfo <- fmTinfo[fmTinfo[,.I[VSAge == max(VSAge) ], by = 'DUM_REG']$V1] #9159
fmTinfo$numP <-  NULL

eP_ds1 <- merge(dP_ds1,fmTinfo,  by.x = 'DUM_REG', by.y = 'DUM_REG' ) #inner join 4137

eP_ds1[caco==1,CrossTable(my_age,CC,digits=0, max.width = 1, expected=FALSE, prop.r=TRUE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]

##Prep Work###########################################################

levels(as.factor(t2Dead_AllCCdP_ds1$SEX.x))
t2Dead_AllCCdP_ds1$my_gender <-  as.factor(t2Dead_AllCCdP_ds1$SEX.x)
t2Dead_AllCCdP_ds1$my_gender<-recode(t2Dead_AllCCdP_ds1$my_gender,  " 'F' = 'Female'; 'M' = 'Male' ")
summary(t2Dead_AllCCdP_ds1$my_gender)
table(t2Dead_AllCCdP_ds1$SEX.x, t2Dead_AllCCdP_ds1$my_gender)
table(t2Dead_AllCCdP_ds1$SEX.y, t2Dead_AllCCdP_ds1$my_gender) # 3 mismatch on gender

###
levels(as.factor(t2Dead_AllCCdP_ds1$MARITAL))
t2Dead_AllCCdP_ds1$my_marital <-  as.factor(t2Dead_AllCCdP_ds1$MARITAL)
levels(t2Dead_AllCCdP_ds1$my_marital)
t2Dead_AllCCdP_ds1$my_marital<-recode(t2Dead_AllCCdP_ds1$my_marital,  " 'D' = 'Other'; 'E' = 'Other'; 'S' = 'Other'; 'W' = 'Other'; 'Z' = 'Other'; 'M'= 'Married' ")
summary(t2Dead_AllCCdP_ds1$my_marital)
table(t2Dead_AllCCdP_ds1$MARITAL, t2Dead_AllCCdP_ds1$my_marital)


###

levels(as.factor(t2Dead_AllCCdP_ds1$SMOKER_ID))
t2Dead_AllCCdP_ds1$my_smoker <-  as.factor(t2Dead_AllCCdP_ds1$SMOKER_ID)
t2Dead_AllCCdP_ds1$my_smoker<-recode(t2Dead_AllCCdP_ds1$my_smoker,  " 'C' = 'Ever'; 'N' = 'Never'; 'X' = 'Ever'; 'Z' = 'Unknown' ")
summary(t2Dead_AllCCdP_ds1$my_smoker)
table(t2Dead_AllCCdP_ds1$SMOKER_ID, t2Dead_AllCCdP_ds1$my_smoker)

###
levels(as.factor(t2Dead_AllCCdP_ds1$depr_16))
t2Dead_AllCCdP_ds1$my_depr_16 <-  as.factor(t2Dead_AllCCdP_ds1$depr_16)
t2Dead_AllCCdP_ds1[ is.na(depr_16),my_depr_16:= 'Unknown']
t2Dead_AllCCdP_ds1$my_depr_16<-recode(t2Dead_AllCCdP_ds1$my_depr_16,  " '1' = '1 Least'; '2' = '2'; '3' = '3'; '4' = '4';'5' = '5 Most';")
table(t2Dead_AllCCdP_ds1$depr_16, CCdP_ds1$my_depr_16)
summary(t2Dead_AllCCdP_ds1$my_depr_16)

levels(as.factor(t2Dead_AllCCdP_ds1$depr_11))
t2Dead_AllCCdP_ds1$my_depr_11 <-  as.factor(t2Dead_AllCCdP_ds1$depr_11)
t2Dead_AllCCdP_ds1[ is.na(depr_11),my_depr_11:= 'Unknown']
t2Dead_AllCCdP_ds1$my_depr_11<-recode(t2Dead_AllCCdP_ds1$my_depr_11,  " '1' = '1 Least'; '2' = '2'; '3' = '3'; '4' = '4';'5' = '5 Most'; ")
table(t2Dead_AllCCdP_ds1$depr_11, CCdP_ds1$my_depr_11)
summary(t2Dead_AllCCdP_ds1$my_depr_11)
t2Dead_AllCCdP_ds1[caco==1,summary(my_depr_11)]


#Check depr 11 against 16
table(t2Dead_AllCCdP_ds1$depr_16, t2Dead_AllCCdP_ds1$my_depr_11)





###
levels(as.factor(t2Dead_AllCCdP_ds1$sPOD))
t2Dead_AllCCdP_ds1$my_sPOD <-  as.factor(t2Dead_AllCCdP_ds1$sPOD)
t2Dead_AllCCdP_ds1$my_sPOD<-recode(t2Dead_AllCCdP_ds1$my_sPOD,  " 'H' = 'Home'; 'N' = 'Nursing Home' ; 'O' = 'Hospital' ;  'S' = 'Hospice';  'Z' = 'Unknown'")
summary(t2Dead_AllCCdP_ds1$my_sPOD)
table(t2Dead_AllCCdP_ds1$sPOD, t2Dead_AllCCdP_ds1$my_sPOD)
t2Dead_AllCCdP_ds1[ is.na(my_sPOD), my_sPOD := 'No data']
t2Dead_AllCCdP_ds1[,summary(my_sPOD)]

###
levels(as.factor(t2Dead_AllCCdP_ds1$ED_HB))
t2Dead_AllCCdP_ds1$my_HB<-  as.factor(t2Dead_AllCCdP_ds1$ED_HB)
t2Dead_AllCCdP_ds1$my_HB<-recode(t2Dead_AllCCdP_ds1$my_HB, " 'NE' = 'North Eastern'; 'SE' = 'South Eastern';'W' = 'Western'; 'M' = 'Midlands';'E' = 'Eastern'; 'NW' = 'North Western';'S' = 'Southern';'MW' = 'Mid Western'")
#order levels to be consisten with barchart below
t2Dead_AllCCdP_ds1$my_HB <- factor(t2Dead_AllCCdP_ds1$my_HB , levels = c('North Eastern','South Eastern', 'Western','Midlands','Eastern','North Western','Southern','Mid Western'))
summary(t2Dead_AllCCdP_ds1$my_HB)
table(t2Dead_AllCCdP_ds1$ED_HB, t2Dead_AllCCdP_ds1$my_HB)




####
#age at death -try different cut-offs
table(eP_ds1$VSAge)
#eP_ds1[, my_age:= cut(as.integer(VSAge),c(-1, 59,64,69,74,79,84,89, 10000), right=TRUE, labels=c('<60 years', '60-64', '65-69','70-74', '75-79', '80-84', '85-89', '>89 years'))]
eP_ds1[, my_age:= cut(as.integer(VSAge),c(-1, 59,64,69,74,79,84, 10000), right=TRUE, labels=c('<60 years', '60-64', '65-69','70-74', '75-79', '80-84', '> 84 years'))]
#eP_ds1[, my_age:= cut(as.integer(VSAge),c(-1, 64,74,84,94, 10000), right=TRUE, labels=c('<65 years', '65-74', '75-84', '85-94', '>94 years'))]
#eP_ds1[, my_age:= cut(as.integer(VSAge),c(-1, 64,74,79,84,89, 10000), right=TRUE, labels=c('<65 years', '65-74', '75-79', '80-84', '85-89', '>89 years'))]

table(eP_ds1$my_age)

##

#labels for display
dP_ds1$mycaco <-recode(dP_ds1$caco,  " 0 = 'No SPC'; 1 = 'received SPC' ")
dP_ds1$myCC <-recode(dP_ds1$CC, ' 1 = "Beaumont"; 2="Mater"; 3="St Vincent\'s"; 4="St James\'s";5="Cork";6="Waterford";7="Galway";8= "Limerick" ')



#Display

d1 <- dP_ds1[caco==1,c('myCC', 'my_sPOD'),]
setnames(d1, "my_sPOD", "POD")
table(d1$myCC,d1$POD)
CrossTable(d1$myCC, d1$POD,digits=0, max.width = 1, expected=FALSE, prop.r= TRUE,prop.c = FALSE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")
CrossTable(d1$POD, d1$myCC,digits=0, max.width = 1, expected=FALSE, prop.r= FALSE,prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")
p <-  ggplot(d1, aes(myCC, fill = POD)) + xlab("Cancer centres") + ylab("Place of death (%)") 
p + geom_bar(aes(y = (..count..)/sum(..count..))) + scale_y_continuous(labels=percent) 
# p + guides(fill=guide_legend(title=NULL))
# p + ggtitle("Place of death for patients who died in 2016 receiving specialist palliative care by cancer centre")


p <-  ggplot(d1,aes(x = myCC,fill = POD)) + 
  geom_bar(position = "fill") 
p + xlab("Cancer centres") + ylab("Place of death (%)") 




#Misc checks


##?????????????????????

x <- a2[numP > 1, c(1)]
x <-  unique(x)
x1 <- merge(cH_ds1, x, all.y= TRUE)
x1 <- x1[SPC==1,]
x1[,newSPC := 0]
x1[x1[,.I[h2off == min(h2off)], by = 'DUM_REG']$V1,newSPC := 1]
x1 <- x1[,c(2,28)]
cH_ds1 <- merge(x1,cH_ds1,  by.x = 'row_id', by.y = 'row_id', all.y= TRUE ) #inner join
cH_ds1[!is.na(newSPC), SPC := newSPC]
cH_ds1$newSPC <- NULL
setcolorder(cH_ds1,c(2,1,3:27))
#Check again
#Find cases with an SPC
a1 <-cH_ds1[,c(1,19,26),] #reg/cc/spc
#with SPC
a2 <- a1[SPC==1]
a2 <-  unique(a2)#3517  cancer centre cases have an SPC (-21)
a2 <- a2[,numP := .N, by = DUM_REG]
table(a2$numP)#21 cases removed
uniqueN(cH_ds1$DUM_REG)#9159 cases with 63374 episodes in cc

#########################################################

#Investigate overlaps of same patient in different cc
x <- subset(a4, !(DUM_REG %in% a5$DUM_REG)) 
uniqueN(x$DUM_REG)
x <- subset(a5, !(DUM_REG %in% a4$DUM_REG)) 
uniqueN(x$DUM_REG)
#not relevant to study<
rm(x,x1,a1,a2,a3)
#####################################################
#Identify patients from the cancer centres
a1 <- cH_ds1[,c(1)]
a1 <- unique(a1) #9159

cH_ds1[yearD==2015,table(SPC, CC)]
# SPC    1    2    3    4    5    6    7    8
# 0     7102 3887 4599 5437 8379 1660 7905 2791
# 1     196  104   86  419  325  268  175  384

cH_ds1[yearD==2016,table(SPC, CC)]
# SPC    1    2    3    4    5    6    7    8
# 0     2647 1506 2076 2247 4019  708 2738 1260
# 1     375  234  252  441  289  228  385  252


#Restrict Patient to get relevant Patient data
cP_ds1 <- merge(bP_ds1, a1 ) #inner join 
uniqueN(cP_ds1$DUM_REG)#9159 patients 

#Restrict Tumour to get relevant Tumour data
cT_ds1 <- merge(bT_ds1, a1 ) #inner join 
uniqueN(cT_ds1$DUM_REG)#9159 patients with 10559 tumours 


#Tidy up
rm(x,y,p)
###################################################################
###################################################################
###################################################################

##Different approach 1
#Lookups
#cancer centres HIPE code
# 1 Beaumont Hospital 037	 		
# 2 Mater University Hospital  005		
# 3 St Vincent’s University Hospital 007	
# 4 St James’s Hospital 021	 		
# 5 Cork University Hospital 		235
# 6 Waterford Regional Hospital 100   	
# 7 Galway University Hospital 404
# 8 University Hospital Limerick 303


#For each cancer centre separately identify all cases and all controls within that  centre
#
##Assign regions
dH_ds1[CC==1 | CC==2 |CC==3 |CC==4, region:= 'D']
dH_ds1[CC==5 , region:= 'C']
dH_ds1[CC==6 , region:= 'W']
dH_ds1[CC==7 , region:= 'G']
dH_ds1[CC==8 , region:= 'L']
dH_ds1[CC==0 , region:=  'Z']
dH_ds1 [,regionCount := 0]
dH_ds1[CC> 0, regionCount :=  uniqueN(region), by = c('DUM_REG')]#count the espiodes by reg,]#ignore non cancer centres in count
View(dH_ds1)

#Do this for each cancer centre

CCH_ds <-  dH_ds1[CC==5,] 
CCH_ds[, CCcaco :=  as.numeric(any(toHospice | SPC)) , by=  'DUM_REG'] #identify cases and controls
CCH_ds[caco==1 & CCcaco==0, CCcaco := -1 ]#identify cases elsewhere but not at this CC

CCH_ds[,CCHEpiscase := Episeq]#initalisation issues on type, so do this first and then set to 0
CCH_ds[,CCHEpiscase := 0]
CCH_ds[toHospice ==1 | SPC==1, CCHEpiscase := Episeq, by = c('DUM_REG')] #take first index spc episode for cases 
CCH_ds[,CCHEpiscase := max(CCHEpiscase),by = c('DUM_REG')] #set  for all episodes of this reg
CCH_ds[CCcaco==0 , CCHEpiscase := max(Episeq), by = c('DUM_REG')] #take last episode for controls 
CCH_ds[CCcaco==-1 , CCHEpiscase := max(Episeq), by = c('DUM_REG')] #take last episode for grey cases
CCH_ds[, lastDischargeE := max(Episeq),by = c('DUM_REG')] #flag final discharge from CC for each patient
CCH_ds[, lastDischargeD := 0]
CCH_ds[lastDischargeE==Episeq, lastDischargeD := as.Date(h2off - 113, origin = '1970-01-01'),by = c('DUM_REG')] #flag final discharge from CC for each patient
CCH_ds[,lastDischargeD := max(lastDischargeD),by = c('DUM_REG')] #set  for all episodes of this reg

View(CCH_ds)

CCcaco <-CCH_ds[lastDischargeE == Episeq ,c(1:46)] 
uniqueN(CCcaco$DUM_REG)
CCcaco[,table(caco)]
CCcaco[,table(CCcaco)]
CCcaco[,table(caco, CCcaco)]
#Determine if this cancer centre episode occurred before or after the  index SPC episode
CCcaco[CCcaco==-1 & Episcase <= CCHEpiscase, CCcaco :=-11] 
CCcaco[CCcaco==-1 & Episcase > CCHEpiscase, CCcaco :=-10]
CCcaco[,CrossTable(caco, CCcaco, format = 'SPSS')]


uniqueN(CCcaco$DUM_REG)
CCcaco[,table(caco)]
CCcaco[,table(CCcaco)]
CCcaco[,table(caco, CCcaco)]



CCcP_ds1 <- merge(bP_ds1,CCcaco,  by.x = 'DUM_REG', by.y = 'DUM_REG') #inner join 3192


CCcP_ds1[,CrossTable(sVS, CCcaco, format = 'SPSS')]
x <- CCcP_ds1[,CrossTable(sVS, CCcaco,digits=0, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SAS")]
View(x)

#Restrict to patients who died in 2016
CCdP_ds1 <-CCcP_ds1[yearDOD==2016,]
uniqueN(CCdP_ds1$DUM_REG)
#Count the number of cancer centres attended 
CCdP_ds1[,table(CCCount, CCcaco)]
CCdP_ds1[CCcaco >-1 ,CrossTable(CCCount, CCcaco)]
CCdP_ds1[CCcaco >-1 ,table(CCcaco)]

CCcP_ds1[,CrossTable(sVS, CCcaco,digits=0, max.width = 1, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]
CCcP_ds1[,CrossTable(CCcaco,yearD,digits=0, max.width = 1, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]


x <- CCdP_ds1[,CrossTable(myCaseType, CCcaco,digits=0, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SAS")]
View(x)

x <- CCdP_ds1[,CrossTable(my_gender, CCcaco,digits=0, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SAS")]
View(x)

x <- CCdP_ds1[,CrossTable(my_marital, CCcaco,digits=0, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SAS")]
View(x)

x <- CCdP_ds1[,CrossTable(my_smoker, CCcaco,digits=0, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SAS")]
View(x)

x <- CCdP_ds1[,CrossTable(my_depr_16, CCcaco,digits=0, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SAS")]
View(x)

x <- CCdP_ds1[,CrossTable(my_sPOD, CCcaco,digits=0, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SAS")]
View(x)

x <- CCdP_ds1[,CrossTable(my_sPOD, CCcaco,digits=0, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SAS")]
View(x)

a2 <-  CCdP_ds1[,c(1, 20, 63,66)] 
a2 <-  unique(a2) 
a2[, discarge_td :=  sVSD -lastDischargeD]
a2[, quantile(discarge_td), by = c("CCcaco")]

#Find the tumours assoicated with these deceased patients
fmT <-  CCdP_ds1[, c(1,63)] 

CCcT_ds1 <- merge(bT_ds1,fmT,  by.x = 'DUM_REG', by.y = 'DUM_REG') #inner join 
uniqueN(CCcT_ds1$DUM_REG)
View(CCcT_ds1)
CCcT_ds1[, TumourCount := .N, by = c('DUM_REG')]#count the espiodes by reg
a2 <-  CCcT_ds1[,c(1,37,38 )] 
a2 <-  unique(a2) 
x <- a2[,CrossTable(TumourCount, CCcaco,digits=0, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SAS")]
View(x)

a2 <-  CCcT_ds1[,c(1,34,37)] #482
a2 <-  unique(a2) #423
table(a2$VSAge)
a2[, my_age:= cut(as.integer(VSAge),c(-1, 59,69,79, 10000), right=TRUE, labels=c('<60 years', '60-69','70-79','>80 years'))]
#a2[, my_age:= cut(as.integer(VSAge),c(-1, 59,64,69,74,79,84, 10000), right=TRUE, labels=c('<60 years', '60-64', '65-69','70-74', '75-79', '80-84', '> 84 years'))]
table(a2$my_age)
x <- a2[,CrossTable(my_age, CCcaco,digits=0, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SAS")]
View(x)



a2 <-  CCcT_ds1[,c(1,32,37)]
a2 <-  unique(a2) 
a2[, quantile(sSurvival), by = c("CCcaco")]



a2 <-  CCcT_ds1[,c(1,16,37)] #482
a2 <- droplevels(a2)
table(a2$ICD10_SITE)
# x <- a2[,CrossTable(ICD10_SITE, CCcaco,digits=0, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SAS")]
# View(x)

# ID	Category	                                                    ICD-10 codes
# 1	All cancer types	                                              C00-C97
# 2	Lower gastrointestinal (excl. liver/gallbladder/biliary tract)	C18-C21
# 3	Upper gastrointestinal	                                        C15-C17
# 4	Pulmonary                   	                                  C34
# 5	Breast & gynaecological	                                        C50-C58
# 6	Prostate	                                                      C61
# 7	Haematological	                                                C81-C91
# 8	Other	All other C-Codes                                         


# CancerGroups <- read.csv("G:\\Docs\\S2\\WIP\\ICD10Groups.csv")
# dtCancerGroups <-  data.table(CancerGroups)
saveRDS(dtCancerGroups, file = "V:\\R_Projects\\RHomeDir\\RS2A\\_data\\dtCancerGroups.rds")

a2 <- merge(a2, CancerGroups,by.x ="ICD10_SITE", by.y ="ICD10", all.x =TRUE)

a2$CancerGroup<-recode(a2$Group,  " 2 = 'Lower gastrointestinal (excl. liver/gallbladder/biliary tract)';
3 = 'Upper gastrointestinal'; 4 = 'Pulmonary'; 5= 'Breast & gynaecological'; 6= 'Prostate'; 
                       7 = 'Haematological'; 8 = 'Other' ")
View(a2)
table(a2$Group, a2$ICD10_SITE)
x <- a2[,CrossTable(CancerGroup, CCcaco,digits=0, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SAS")]
View(x)
# x <- a2[Group==8,, ]
# table(x$ICD10_SITE)
# #Recode C69 - C72 to  its' own group 1 = "Eye, brain & central nervous system' 
# a2[ICD10_SITE=='C69'|ICD10_SITE=='C70'| ICD10_SITE=='C71'| ICD10_SITE=='C72' ,Group := 1,]
# table(a2$Group, a2$ICD10_SITE)

a2$CancerGroup<-recode(a2$Group,  "1= 'Eye, brain & central nervous system'; 2 = 'Lower gastrointestinal (excl. liver/gallbladder/biliary tract)';
3 = 'Upper gastrointestinal'; 4 = 'Pulmonary'; 5= 'Breast & gynaecological'; 6= 'Prostate'; 
                       7 = 'Haematological'; 8 = 'Other' ")


#Check numbers
xNoCC <- dH_ds1[CC==0,c(1)] 
xNoCC <- unique(xNoCC)
dH_ds1[CC==0,c(1)]
xAnyCC <- dH_ds1[caco==1 & CC>0,c(1)] 
xAnyCC <- unique(xAnyCC)

xC1CC <-  dH_ds1[caco==1 & CC==1,c(1)] 
xC1CC <- unique(xC1CC)

xC2CC <-  dH_ds1[caco==1 & CC==2,c(1)] 
xC2CC <- unique(xC2CC)

xC3CC <-  dH_ds1[caco==1 & CC==3,c(1)] 
xC3CC <- unique(xC3CC)

xC4CC <-  dH_ds1[caco==1 & CC==4,c(1)] 
xC4CC <- unique(xC4CC)

xC5CC <-  dH_ds1[caco==1 & CC==5,c(1)] 
xC5CC <- unique(xC5CC)

xC6CC <-  dH_ds1[caco==1 & CC==6,c(1)] 
xC6CC <- unique(xC6CC)

xC7CC <-  dH_ds1[caco==1 & CC==7,c(1)] 
xC7CC <- unique(xC7CC)


xC8CC <-  dH_ds1[caco==1 & CC==8,c(1)] 
xC8CC <- unique(xC8CC)

#Investigate
x <- subset(xAnyCC, !(DUM_REG %in% xNoCC$DUM_REG))
uniqueN(x$DUM_REG) 
View(x)#18072 patients have  epsiodes in  CC only  and all patients have at least one epsiodes in CC only

##Different approach 2
#Lookups
#cancer centres HIPE code
# 1 Beaumont Hospital 037	 		
# 2 Mater University Hospital  005		
# 3 St Vincent’s University Hospital 007	
# 4 St James’s Hospital 021	 		
# 5 Cork University Hospital 		235
# 6 Waterford Regional Hospital 100   	
# 7 Galway University Hospital 404
# 8 University Hospital Limerick 303


View(fm_caco)

t2AllCCcP_ds1 <- merge(bP_ds1,fm_caco,  by.x = 'DUM_REG', by.y = 'DUM_REG') 
#Restrict to patients who died in 2016
t2Dead_AllCCd_ds1 <-t2AllCCcP_ds1[yearDOD==2016,]



x <- t2Dead_AllCCdP_ds1[,CrossTable(myCaseType, caco,digits=0, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SAS")]
View(x)

x <- t2Dead_AllCCdP_ds1[,CrossTable(my_gender, caco,digits=0, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SAS")]
View(x)


x <- t2Dead_AllCCdP_ds1[,CrossTable(my_marital, caco,digits=0, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SAS")]
View(x)

x <- t2Dead_AllCCdP_ds1[,CrossTable(my_smoker, caco,digits=0, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SAS")]
View(x)

x <- t2Dead_AllCCdP_ds1[,CrossTable(my_depr_16, caco,digits=0, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]
View(x)

x <- t2Dead_AllCCdP_ds1[,CrossTable(my_depr_11, caco,digits=0, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]
View(x)

x <- t2Dead_AllCCdP_ds1[,CrossTable(my_sPOD, caco,digits=0, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SAS")]
View(x)

x <- t2Dead_AllCCdP_ds1[,CrossTable(my_HB, caco, digits=0, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]
View(x)

#Rerun for test statistics
t1 <-  table(t2Dead_AllCCdP_ds1$my_depr_16, t2Dead_AllCCdP_ds1$caco)
assocstats(t1)
ord.somers.d(t1)

#Median minimum survival per patient
a2 <-  t2Dead_AllCCcT_ds1[,c(1,35,36, 37)]
a2 <-  unique(a2) 
a2[, quantile(min_sSurvival), by = c("caco")]
a2[, quantile(min_sSurvival)]
a2[, quantile(max_sSurvival), by = c("caco")]
a2[, quantile(max_sSurvival)]




#Find the tumours assoicated with these deceased patients
fmT <-  t2Dead_AllCCdP_ds1[, c(1,50)] 
#Count the tumours associated with these patients 
t2Dead_AllCCcT_ds1 <- merge(bT_ds1,fmT,  by.x = 'DUM_REG', by.y = 'DUM_REG') #inner join 
uniqueN(t2Dead_AllCCcT_ds1$DUM_REG)
View(t2Dead_AllCCcT_ds1)
t2Dead_AllCCcT_ds1[, TumourCount := .N, by = c('DUM_REG')]#count the espiodes by reg
a2 <-  t2Dead_AllCCcT_ds1[,c(1,37,38 )] 
a2 <-  unique(a2) 
x <- a2[,CrossTable(TumourCount, caco,digits=0, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SAS")]
View(x)
#Reset to  1 and > 1 tumour
a2[TumourCount > 1,  TumourCount :=2, ]
View(a2)



a2 <-  t2Dead_AllCCcT_ds1[,c(1,34,37)]
a2 <-  unique(a2) # 1 too many, 1 reg two tumours VSage differs by 1 year
a2[, dupP := .N, by = c('DUM_REG')]
a2[DUM_REG=='2XXXXXXX1' , VSAge :=51,]# Fix
a2 <-  unique(a2)
table(a2$VSAge)
a2[, my_age:= cut(as.integer(VSAge),c(-1, 59,69,79, 10000), right=TRUE, labels=c('<60 years', '60-69','70-79','>80 years'))]
#a2[, my_age:= cut(as.integer(VSAge),c(-1, 59,64,69,74,79,84, 10000), right=TRUE, labels=c('<60 years', '60-64', '65-69','70-74', '75-79', '80-84', '> 84 years'))]
table(a2$my_age)
x <- a2[,CrossTable(my_age, caco,digits=0, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, missing.include = FALSE,format= "SPSS")]
View(x)

t1 <-  table(a2$TumourCount, a2$caco)
View(t1)
assocstats(t1)
ord.somers.d(t1)


#Cancer Groups
a2 <-  t2Dead_AllCCcT_ds1[,c(1,16,37)] 
a2 <- droplevels(a2)
table(a2$ICD10_SITE)
a2 <- merge(a2, CancerGroups,by.x ="ICD10_SITE", by.y ="ICD10", all.x =TRUE)


View(a2)
table(a2$Group, a2$ICD10_SITE)
x <- a2[,CrossTable(Group, caco,digits=0, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SAS")]
View(x)
# x <- a2[Group==8,, ]
# table(x$ICD10_SITE)
# #Recode C69 - C72 to  its' own group 1 = "Eye, brain & central nervous system' 
a2[ICD10_SITE=='C69'|ICD10_SITE=='C70'| ICD10_SITE=='C71'| ICD10_SITE=='C72' ,Group := 1,]
table(a2$Group, a2$ICD10_SITE)

a2$CancerGroup<-recode(a2$Group,  "1= 'Eye, brain & central nervous system'; 2 = 'Lower gastrointestinal (excl. liver/gallbladder/biliary tract)';
                       3 = 'Upper gastrointestinal'; 4 = 'Pulmonary'; 5= 'Breast & gynaecological'; 6= 'Prostate'; 
                       7 = 'Haematological'; 8 = 'Other' ")


x <- a2[,CrossTable(CancerGroup, caco,digits=0, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, missing.include = FALSE,format= "SPSS")]
View(x)

t1 <-  table(a2$CancerGroup, a2$caco)
assocstats(t1)


#Revisit POD
a2 <-  t2Dead_AllCCdP_ds1[,c(1,17,18,20,50)] 

#Review DEPS data for these 4137 patients in the study set
a3 <-  t2Dead_AllCCdP_ds1[,c(1)]
a4 <-  merge(a3,fm_DEPS, all.x  = TRUE )

a4[,my_instit := 'Z']
a4[my_instit =='Z'& is.na(place_of_death1), my_instit:= 'zNo Data']
a4[my_instit =='Z'& place_of_death1 %like% 'NH', my_instit:= 'N']
a4[my_instit =='Z'& place_of_death1 %like% ' NU', my_instit:= 'N']
a4[my_instit =='Z'& place_of_death1 %like% ' DISTRICT', my_instit:= 'N']
a4[my_instit =='Z'& place_of_death1 %like% 'DISTRICT', my_instit:= 'N']
a4[my_instit =='Z'& place_of_death1 %like% 'NURSING HOME', my_instit:= 'N']
a4[my_instit =='Z'& place_of_death1 %like% ' CARE CENTRE', my_instit:= 'N']
a4[my_instit =='Z'& place_of_death1 %like% ' CARE HOME', my_instit:= 'N']
a4[my_instit =='Z'& place_of_death1 %like% ' RESIDENTIAL CARE', my_instit:= 'N']
a4[my_instit =='Z'& place_of_death1 %like% ' RESIDENTIAL', my_instit:= 'N']
a4[my_instit =='Z'& place_of_death1 %like% ' RESIDENCE', my_instit:= 'N']
a4[my_instit =='Z'& place_of_death1 %like% 'TLC ', my_instit:= 'N']
a4[my_instit =='Z'& place_of_death1 %like% 'THE MOYNE', my_instit:= 'N']
a4[my_instit =='Z'& place_of_death1 %like%  'MT CARMEL', my_instit:= 'N']
a4[my_instit =='Z'& place_of_death1 %like% 'TRALEE COMMUNITY NU', my_instit:= 'N']
a4[my_instit =='Z'& place_of_death1 %like% ' HSE', my_instit:= 'N']
a4[my_instit =='Z'& place_of_death1 %like% 'YOUGHAL AND DISTRICT', my_instit:= 'N']
a4[my_instit =='Z'& place_of_death1 %like% 'YOUGHAL & DISTRICT', my_instit:= 'N']
a4[my_instit =='Z'& place_of_death1 %like% 'YOUGHAL COMMUNITY HOSP', my_instit:= 'N']
a4[place_of_death1 %like% 'TARA WINTHROP', my_instit:= 'N'] 
a4[my_instit =='Z'& place_of_death1 %like% 'COMMUNITY', my_instit:= 'N']
a4[my_instit =='Z'& place_of_death1 %like% ' HOSPICE', my_instit:= 'S']
a4[my_instit =='Z'& place_of_death1 %like% 'HOSPICE', my_instit:= 'S']
a4[place_of_death1 %like% 'MILFORD CARE', my_instit:= 'S'] #override institution designation for Milford
a4[place_of_death1 %like% 'MARYMOUNT UNIVERSITY', my_instit:= 'S']
a4[my_instit =='Z'& place_of_death1 %like% 'ST. PATRICK\'S UNIVERSITY HOSPITAL/MARYMOUNT', my_instit:= 'S']
a4[my_instit =='Z'& place_of_death1 %like% 'ST. PATRICK\'S UNIVERSITY HOSPITAL/MARYMOUNT UNI', my_instit:= 'S']
a4[my_instit =='Z'& place_of_death1 %like% 'THE N W HOSPICE', my_instit:= 'S']
a4[my_instit =='Z'& place_of_death1 %like% 'THE GALWAY HOSPICE', my_instit:= 'S']
a4[my_instit =='Z'& place_of_death1 %like% 'THE GALWAY HOSPICE FOUNDATION', my_instit:= 'S']
a4[my_instit =='Z'& place_of_death1 %like% 'DONEGAL HOSPICE', my_instit:= 'S']
a4[my_instit =='Z'& place_of_death1 %like% ' HOSP', my_instit:= 'O']
a4[my_instit =='Z'& place_of_death1 %like% 'HOSP', my_instit:= 'O']
a4[my_instit =='Z'& place_of_death1 %like% 'ACCIDENT AND EMERGENCY DEPT' , my_instit:= 'O']
a4[my_instit =='Z'& place_of_death1 %like% 'EMERGENCY' , my_instit:= 'O']
a4[my_instit =='Z'& place_of_death1 %like% 'WARD', my_instit:= 'O']
a4[my_instit =='Z'& place_of_death1 %like% 'CLINIC', my_instit:= 'O']
a4[my_instit =='Z'& place_of_death1 %like% 'UNIT', my_instit:= 'O']
a4[my_instit =='Z'& place_of_death_type_source == 'HOSPITAL', my_instit:= 'O']
a4[my_instit =='Z' & place_of_death_type_source == 'RESIDENTIAL', my_instit:= 'H']

a3 <- merge(a2, a4)
x <-  a3[sDOD != sVSD,c(1)] #0 data
x <- a3[sDOD != dod,c(1)]  #0 data
a3[, table(sPOD, my_instit,exclude = NULL )]


#sPOD
levels(as.factor(a3$sPOD))
a3$my_sPOD <-  as.factor(a3$sPOD)
a3$my_sPOD<-recode(a3$my_sPOD,  " 'H' = 'Home'; 'N' = 'Nursing Home' ; 'O' = 'Hospital' ;  'S' = 'Hospice';  'Z' = 'Unknown'")
summary(a3$my_sPOD)
table(a3$sPOD, a3$my_sPOD,exclude = NULL)
a3[ is.na(my_sPOD), my_sPOD := 'No data']
a3[,summary(my_sPOD)]

#my_instit
levels(as.factor(a3$my_instit))
a3$my_instit <-  as.factor(a3$my_instit)
a3$S_instit<-recode(a3$my_instit,  " 'H' = 'Home'; 'N' = 'Nursing Home' ; 'O' = 'Hospital' ;  'S' = 'Hospice';  'Z' = 'Unknown'")
summary(a3$S_instit)
table(a3$my_instit, a3$S_instit,exclude = NULL)
a3[ is.na(S_instit), S_instit := 'No data']
a3[,summary(S_instit)]

a3[,table(my_sPOD,S_instit)]

x <- a3[my_sPOD=='Hospice',]#take S_instit
x <- a3[S_instit=='Nursing Home',]#take S_instit

x <- a3[,CrossTable(S_instit, caco,digits=0, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SAS")]
View(x)

t1 <-  table(a3$S_instit, a2$caco)
assocstats(t1)

#Compare
a3[,CrossTable(my_sPOD, caco,digits=0, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]

##Comment# recategorisation makes  no real effect on hospital or hospice proportions
#Use S_instit

#County of death
a3[is.na(place_of_death_county_id), place_of_death_county_id := 'ZZ']
a3[,CrossTable(place_of_death_county_id, caco,digits=0, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]
a3[caco==1,CrossTable( place_of_death_county_id, S_instit,digits=0, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]
a3[caco==1,CrossTable( S_instit,ED_COUNTY ,digits=0, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]


x <- a3[place_of_death_county_id == 'CE' & caco==1,CrossTable(S_instit, place_of_death_county_id,digits=0, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SAS")]
View(x)
#Comment Not useful because POD determines county of death
#Look at other regions
a2 <-  t2Dead_AllCCdP_ds1[,c(1,3,4,5,6,53)] 
a3 <- merge(a2, a3)
a3[,table(ED_COUNTY,ED_HB)]


a3[caco==1,CrossTable(S_instit, ED_HB,  digits=0, max.width = 5, expected=FALSE, prop.r= FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]


x <- a3[caco==1,CrossTable(S_instit, ED_HB,digits=0, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SAS")]
View(x)

#Find time from first SPC to Death for cases only
a2 <- t2Dead_AllCCdP_ds1[caco==1, c(1,20,33, 34 ,53,56)]
a2[, DischargeD := 0]
a2[,DischargeD := as.Date(h2off - 113, origin = '1970-01-01'),] 
a2[,AdmissionD := as.Date(h1off - 113, origin = '1970-01-01'),] 
a2[, discarge_td :=  sVSD -DischargeD]
a2[, admission_td :=  sVSD -AdmissionD]
a2[, quantile(discarge_td),]
a2[, quantile(admission_td,na.rm =TRUE),]

#Find number of centres, SPC count, toHospice count per patient
a2 <-  t2Dead_AllCCdP_ds1[,c(1,50,52,54,60,61, 62, 63,65,66)]
a2[, quantile(Episcount), by = 'caco']
a2[, quantile(overnightCount), by = 'caco']
a2[, quantile(SPCCount), by = 'caco']
#a2[, quantile(toHospiceCount), by = 'caco']                          
a2[,CrossTable(CCCount, caco, digits=0, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]
a2[,CrossTable(CentreCount, caco, digits=0, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")] 
a2[,CrossTable(IsCCCase, caco, digits=0, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]



a2[overnight==TRUE,CrossTable(CCCount, caco, digits=0, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]
a2[,CrossTable(overnight, caco, digits=0, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]
table(a2$caco, a2$overnight, exclude = NULL) # the first SPC
x <-  t2Dead_AllCCdP_ds1[is.na(overnight),] # the first SPC

#Display ordered barchart (Figure in paper)
d <- a3[caco==1,c('ED_HB', 'S_instit'),]
#Order within stack
d$POD<-recode(d$S_instit,  " 'Home' = '3 Home'; 'Nursing Home' = '2 Nursing Home' ; 'Hospital' = '5 Hospital' ;  'Hospice' = '4 Hospice';  'zNo Data' = '1 Unknown'")
#Relabel and order X axis by  HB hospital proportion  
d$ED_HB <- factor(d$ED_HB, levels = c('NE','SE', 'W','M','E','NW','S','MW'))
d$HealthBoard <- recode(d$ED_HB, " 'NE' = 'North Eastern'; 'SE' = 'South Eastern';'W' = 'Western'; 'M' = 'Midlands';'E' = 'Eastern'; 'NW' = 'North Western';'S' = 'Southern';'MW' = 'Mid Western'")
d$HealthBoard <- factor(d$HealthBoard, levels = c('North Eastern','South Eastern', 'Western','Midlands','Eastern','North Western','Southern','Mid Western'))
levels(d$HealthBoard)
# p <-  ggplot(d, aes(ED_HB, fill = S_instit)) + xlab("HB") + ylab("Place of death (%)")
# p + geom_bar(aes(y = (..count..)/sum(..count..))) + scale_y_continuous(labels=percent)


#Original
p <-  ggplot(d,aes(x = HealthBoard,fill = POD)) + geom_bar(position = "fill") 
p + xlab("Former health boards") + ylab("Place of death (%)") + theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.6))

#Get numbers and  frequency in same format as bar plot
#SPSS
d[,CrossTable(S_instit,HealthBoard,digits=0, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]
#SAS
x <- d[,CrossTable(S_instit,HealthBoard,digits=0, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SAS")]
x[, count := t]
x[, HealthBoard := 'North Eastern,(n= 178)']
x[6:10, HealthBoard := 'South Eastern,(n= 283)']
x[11:15, HealthBoard := 'Western,(n= 336)']
x[16:20, HealthBoard := 'Midlands,(n= 69)']
x[21:25, HealthBoard := 'Eastern,(n= 1009)']
x[26:30, HealthBoard := 'North Western,(n= 108)']
x[31:35, HealthBoard := 'Southern,(n= 369)']
x[36:40, HealthBoard := 'Mid Western,(n= 245)']
x[, my_percent:= round(prop.col*100)]
View(x)
x[ , S_instit :=  'Home',]
x[x[, .I == 2 | .I == 7 |.I == 12 |.I == 17 |.I == 22 |.I == 27 |.I == 32 |.I == 37 ,], S_instit :=   'Hospice',]
x[x[, .I == 3 | .I == 8 |.I == 13 |.I == 18 |.I == 23 |.I == 28 |.I == 33 |.I == 38 ,], S_instit :=   'Hospital',]
x[x[, .I == 4 | .I == 9 |.I == 14 |.I == 19 |.I == 24 |.I == 29 |.I == 34 |.I == 39 ,], S_instit :=   'Nursing Home',]
x[x[, .I == 5 | .I == 10|.I == 15 |.I == 20 |.I == 25 |.I == 30 |.I == 35 |.I == 40, ], S_instit :=   'zNo Data',]
x$POD<-recode(x$S_instit,  " 'Home' = '3 Home'; 'Nursing Home' = '2 Nursing Home' ; 'Hospital' = '5 Hospital' ;  'Hospice' = '4 Hospice';  'zNo Data' = '1 Unknown'")
x$HealthBoard <- factor(x$HealthBoard, levels = c('North Eastern,(n= 178)','South Eastern,(n= 283)', 'Western,(n= 336)','Midlands,(n= 69)','Eastern,(n= 1009)','North Western,(n= 108)','Southern,(n= 369)','Mid Western,(n= 245)'))
levels(x$HealthBoard)
x[, my_percent2 := my_percent,] 
x[my_percent < 5, my_percent2 := '',]
View(x)
#With labels 
p <-  ggplot(x, aes(x = HealthBoard, y= my_percent, fill = POD, label= ifelse(as.numeric(my_percent)>5,paste0(my_percent,"%"),''))) + geom_bar(stat = "identity") + geom_text(size =3, position = position_stack(vjust=0.5))
p + labs(x = "Former health board region", y = "Place of death (%)")  + theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))

                                                                      


# colnames(xDF)[1] <- "North Eastern"
# colnames(xDF)[2] <- "South Eastern"
# colnames(xDF)[3] <- "Western"
# colnames(xDF)[4] <- "Midlands"
# colnames(xDF)[5] <- "Eastern"
# colnames(xDF)[6] <- "North Western"
# colnames(xDF)[7] <- "Southern"
# colnames(xDF)[8] <- "Western"




#Work more with ed_county
#Merge TN/TS/TY and drop TN and TS to give 26 counties
d <- a3[, c(1,3,6,14)]
d[ED_COUNTY =='TS' | ED_COUNTY == 'TN', ED_COUNTY := 'TY',]
d[,table(ED_COUNTY),]
d$ED_COUNTY <-  factor(d$ED_COUNTY)
d[,table(ED_COUNTY),]
#Cases only
d <- d[caco==1,,]
View(d)
d[, order(ED_COUNTY)]

# a3[caco==1, CrossTable( ED_COUNTY, S_instit,  digits=0, max.width = 5, expected=FALSE, prop.r= TRUE, prop.c = FALSE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]
# x <- d[, CrossTable( ED_COUNTY, S_instit,  digits=0, max.width = 5, expected=FALSE, prop.r= TRUE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SAS")]
# View(x)


# Counties <- read.csv("V:\\R_Projects\\RHomeDir\\Lookups\\Counties.csv")
# Counties <-  data.table(Counties)
# Counties <- Counties[,c(1,2)]
str(Counties)
d <-  merge(d, Counties, by.x ="ED_COUNTY", by.y ="id", all.x =TRUE)
d  <-  d[, c(-6)]
View(d)
##Google maps
# install.packages("ggmap") 
# library(ggmap)
# require(ggmap)
#Requires Google API
# register_google(key = ???)
# ggmap::register_google(key = ????) 
# ggmap(get_googlemap())
# ggmap(get_googlemap(center = c(lon = -8.243890, lat = 53.412910),zoom = 7, size = c(640, 640), scale = 2, format = "png8", maptype = "hybrid", language = "en-EN"))

#tmaps and tmaptools for maps 
# install.packages("tmap")
# install.packages("tmaptools")
# install.packages("sf")
# install.packages("leaflet")
# install.packages("sp")
# install.packages("shinyjs")
# install.packages("ggrepel")


library("tmap")
library("tmaptools")
library("sf")
library("leaflet")
library(sp)
library(shinyjs)
library(ggrepel)


IrelandGeo <- read_shape(file= "C:\\Users\\mkelly\\Downloads\\IRL_adm\\IRL_adm1.shp" , as.sf = TRUE)
#Fix data in sf for LAOIS factor
levels(IrelandGeo$NAME_1)[levels(IrelandGeo$NAME_1)=="Laoighis"] <- "Laois"

qtm(IrelandGeo)
str(IrelandGeo)

IrelandGeo2 <- read_shape(file= "C:\\Users\\mkelly\\Downloads\\Census2011_Admin_Counties_generalised20m\\Census2011_Admin_Counties_generalised20m.shp" , as.sf = TRUE)
#qtm(IrelandGeo2)
IrelandGeo2 <-  data.table(IrelandGeo2)
IrelandGeo2 <-  IrelandGeo2[, c(8, 21)]
IrelandGeo2[, County:= gsub("([A-Za-z]+).*","\\1", COUNTYNAME), ]
#Tidy up more
IrelandGeo2[COUNTYNAME == 'North Tipperary', County:= 'Tipperary north',]
IrelandGeo2[COUNTYNAME == 'South Tipperary', County:= 'Tipperary south',]
IrelandGeo2[COUNTYNAME == 'South Dublin', County:= 'Dublin',]
IrelandGeo2[COUNTYNAME == 'Dún Laoghaire-Rathdown', County:= 'Dublin',]
IrelandGeo2[COUNTYNAME == 'Fingal', County:= 'Dublin',]
#Make specific label variable and tidy
IrelandGeo2[, CountyL:= County, ]
IrelandGeo2[grepl('City', COUNTYNAME) ==1,  CountyL:= '', ]
IrelandGeo2[CountyL =='Dublin',  CountyL:= '', ]
IrelandGeo2[COUNTYNAME =='Dún Laoghaire-Rathdown',  CountyL:= 'Dublin', ]


#Get county, HB distribution
a <-  t2Dead_AllCCdP_ds1[,c(4, 6)]
a <- unique(a)
View(a)
#Make TY TS based on HB assignment SE
a[ED_COUNTY =='TY', ED_COUNTY  := 'TS',]
a <-  unique(a) #27 --correct
#Merge with county names
CountiesHB <-  merge(a, Counties, by.x ="ED_COUNTY", by.y ="id")
CountiesHB$HealthBoard <- recode(CountiesHB$ED_HB, " 'NE' = 'North Eastern'; 'SE' = 'South Eastern';'W' = 'Western'; 'M' = 'Midlands';'E' = 'Eastern'; 'NW' = 'North Western';'S' = 'Southern';'MW' = 'Mid Western'")
#Reorder levels to match table and figures
CountiesHB$HealthBoard <- factor(CountiesHB$HealthBoard, levels = c('North Eastern','South Eastern', 'Western','Midlands','Eastern','North Western','Southern','Mid Western'))
View(CountiesHB)
setcolorder(CountiesHB,c(2,4,1,3))
#setnames(CountiesHB,"Description.x", "county")


#Prepare base  map file
IrelandGeo3 <-  merge(IrelandGeo2, CountiesHB, by.x ="County", by.y ="Description", all.x = TRUE)
#Cast back to sf structure
IrelandGeo3 <- st_as_sf(IrelandGeo3)
#tmaps
M1 <- tm_shape(IrelandGeo3) +
  tm_fill("HealthBoard", title = "Health boards", palette = 'Spectral') +
  tm_text("CountyL", size=0.7 ) +
  tm_layout(legend.outside = TRUE, frame = FALSE) +
  tm_borders(alpha=0.9) + 
  tm_shape(x) +
  tm_dots(NA, size = 0.5) +
  tm_layout(legend.outside = TRUE, frame = FALSE)
  

#M1

#Show table of proportions of case and controls by HB
a2 <-  t2Dead_AllCCdP_ds1[,c(1,4,6,50)] 
a2$HealthBoard <- recode(a2$ED_HB, " 'NE' = 'North Eastern'; 'SE' = 'South Eastern';'W' = 'Western'; 'M' = 'Midlands';'E' = 'Eastern'; 'NW' = 'North Western';'S' = 'Southern';'MW' = 'Mid Western'")
a2$HealthBoard <- factor(a2$HealthBoard, levels = c('North Eastern','South Eastern', 'Western','Midlands','Eastern','North Western','Southern','Mid Western'))
a2[, CrossTable( caco,HealthBoard,  digits=0, max.width = 5, expected=FALSE, prop.r= FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]
#a2[, CrossTable(HealthBoard, caco, digits=0, max.width = 5, expected=FALSE, prop.r= FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]

x <- a2[, CrossTable(caco, HealthBoard,  digits=0, max.width = 5, expected=FALSE, prop.r= TRUE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, missing.include = FALSE,format= "SAS")]
View(x)

t1 <-  table(a2$caco, a2$HealthBoard)
assocstats(t1)


## try ggplot
# g1 = ggplot() + geom_sf(data = HBMap, aes(fill = HealthBoard)) +
#   geom_sf(data = x,  size = 2.5) +
#   labs(fill = "Former health board regions")
#   
# g1

#Work more with CountyCaseCount
#Merge TN/TS/TY and drop TN and TS to give 26 counties
str(CountyCaseCount)
str(Counties)
CountyCaseCount <-  merge(CountyCaseCount, Counties, by.x= 'ED_COUNTY', by.y = 'id')
View(CountyCaseCount)
#Take to excel and elucidate POD proportions by county
County_names <- Counties[,c(2)]
# PODl_proportions <- read.csv("V:\\R_Projects\\RHomeDir\\RS2A\\_data\\temp\\PODl_proportions.csv")
# PODl_proportions <-  data.table(PODl_proportions)
#Hospice_lonlat
# Hospice_lonlat <- read.csv("V:\\R_Projects\\RHomeDir\\RS2A\\_data\\temp\\Hospice_lonlat.csv")
# Hospice_lonlat <- data.table(Hospice_lonlat)
# x<- st_as_sf(Hospice_lonlat, coords = c("lon", "lat"), crs = 4326, agr = "constant")


PODl_proportions[, HospiceP:= cut(as.numeric(Hospice),c(-1, 0.1,0.2, 0.3,0.4,0.5,0.6), right=FALSE, labels=c('<10%', '10-19%', '20-29%','30-39%','40-49%','50-59%'))]
table(PODl_proportions$HospiceP,PODl_proportions$Hospice)
PODl_proportions[, HospitalP:= cut(as.numeric(Hospital),c(-1, 0.1,0.2, 0.3,0.4,0.5,0.6,0.7,0.8), right=FALSE, labels=c('<10%', '10-19%', '20-29%','30-39%','40-49%','50-59%', '60-69%', '70-79%'))]
table(PODl_proportions$HospitalP,PODl_proportions$Hospital)
#Combine home and nursinghome to residential
PODl_proportions[,Residential := Home + NursingHome,]
PODl_proportions[, ResidentialP:= cut(as.numeric(Residential),c(-1, 0.1,0.2, 0.3,0.4,0.5,0.6,0.7), right=FALSE, labels=c('<10%', '10-19%', '20-29%','30-39%','40-49%','50-59%', '60-69%' ))]
table(PODl_proportions$ResidentialP,PODl_proportions$Residential)



#Merge IrelandGeo2 with PODl data 

#qtm(IrelandGeo4, "HospiceP")
#qtm(IrelandGeo4, "HospitalP")
# d  <-  tm_shape(Mapdata)
# 
# d + tm_fill("HospiceP", title = "Hospice Deaths", palette = 'YlOrBr') 


#See palettes
tmaptools::palette_explorer()

#Clear down
rm(M2,M3, M4)

M2 <- tm_shape(IrelandGeo4) +
  tm_fill("HospiceP", title = "Hospice Deaths", palette = 'Blues') +
  tm_text("NAME_1", size=0.7) +
  tm_style('classic') +
  tm_layout(legend.outside = TRUE, frame = FALSE) +
  tm_borders(alpha=0.9) +
  
  
  #M2
  M3 <- tm_shape(IrelandGeo4) +
  tm_fill("HospitalP", title = "Hospital Deaths", palette = 'Greens') +
  tm_text("NAME_1", size=0.7) +
  tm_style('classic') +
  tm_layout(legend.outside = TRUE, frame = FALSE) +
  tm_borders(alpha=0.9)  

#M3

M4 <- tm_shape(IrelandGeo4) +
  tm_fill("ResidentialP", title = "Residential Deaths", palette = 'Purples') +
  tm_text("NAME_1", size=0.7) +
  tm_style('classic') +
  tm_layout(legend.outside = TRUE, frame = FALSE) +
  tm_borders(alpha=0.9)  

#M4


#Get total number of cases and controls per county and HB for mapping purposes
a <-  t2Dead_AllCCdP_ds1[,c(1,4, 6,50)]
a[caco==1, numCases:= uniqueN(DUM_REG), by = 'ED_COUNTY']
a[caco==0, numControls:= uniqueN(DUM_REG), by = 'ED_COUNTY']
a[, countyTotal:= uniqueN(DUM_REG), by = 'ED_COUNTY']
a <- a[caco==1,c(2,3,4,5,7)]
a <-  unique(a)
#Note TY occurs three times , 2 x TY/SE and  1xTY/MW
#Make TY TS based on HB assignment SE
a[ED_COUNTY =='TY', ED_COUNTY  := 'TS',]
#Sum TY/SE counts
a[ED_COUNTY =='TS' & ED_HB =='SE', numCases :=  56,]
a[ED_COUNTY =='TS' & ED_HB =='SE', countyTotal :=  90,]
CountyCaseCount <- unique(a)
rm(a)
#Note will need to sum TY if using county counts in maps
#Get HB counts and proportions
CountyCaseCount[,HBCaseCount := sum(numCases), by = 'ED_HB'] 
CountyCaseCount[,HBTotal := sum(countyTotal), by = 'ED_HB'] 
View(CountyCaseCount)
HBCasesCount <- CountyCaseCount[, c(2,6,7)]
HBCasesCount <-unique(HBCasesCount)
HBCasesCount$HealthBoard <- recode(HBCasesCount$ED_HB, " 'NE' = 'North Eastern'; 'SE' = 'South Eastern';'W' = 'Western'; 'M' = 'Midlands';'E' = 'Eastern'; 'NW' = 'North Western';'S' = 'Southern';'MW' = 'Mid Western'")
View(HBCasesCount)
setcolorder(HBCasesCount,c(1,4,2,3))
HBCasesCount[, HBProp := HBCaseCount / HBTotal, ]

##Logistic regression 
#Prepare dataset
LR_ds<- t2Dead_AllCCdP_ds1[, .(DUM_REG, my_gender, my_marital, my_smoker, my_depr_11,my_depr_16, my_HB)]
LR_ds <- merge(LR_ds, a2)

#Using deprivation 16
#Make Eastern the reference
LR_ds$my_HB <- factor(LR_ds$my_HB, levels = c( 'East','North East','South East', 'West','Midlands','North West','South', 'Mid West'))
table(LR_ds$my_age)

mylogit1 <- glm(caco ~  my_gender + my_marital + my_smoker + my_depr_16   + my_age + my_HB , data = LR_ds, family = "binomial")
mylogit2 <- glm(caco ~ my_gender + my_marital + my_smoker   + my_age,  data = LR_ds, family = "binomial")
mylogit <- glm(caco ~ my_gender, data = LR_ds, family = "binomial")
mylogit <- glm(caco ~ my_gender + my_marital, data = LR_ds, family = "binomial")
mylogit <- glm(caco ~ my_gender + my_marital + VSAge, data = LR_ds, family = "binomial")
mylogit <- glm(caco ~ my_marital + VSAge, data = LR_ds, family = "binomial")

##Check deprivation and HB's only

mylogit3 <- glm(caco ~  my_depr_16  + my_HB , data = LR_ds, family = "binomial")
mylogit3 <- glm(caco ~  my_depr_11  + my_HB , data = LR_ds, family = "binomial")


# summary(mylogit3)
# confint(mylogit3)
m1 <- round(exp(cbind(OR = coef(mylogit1), confint(mylogit1))),3)
# m1
# m2 <- cbind(OR = coef(mylogit1), confint(mylogit1))
#m2 = m2[-1,]
#View(m2)
#with(mylogit, null.deviance - deviance)
#with(mylogit, df.null - df.residual)
#with(mylogit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
#logLik(mylogit)


# #LRT
# install.packages("lmtest")
# require('lmtest')
# #HO;reduced model is true/the null hypothesis of the test states that the smaller model provides as good a fit for the data as the larger model
# #H1: reduced model is not true
# #m1  everything
# #m2 remove variable to be tested
# #lrtest(mylogit1,mylogit2)

# #Hosmer-Lemeshow GOF
# install.packages("ResourceSelection")
# require('ResourceSelection')
# #H0: data fits the model 
# #H1 data does not fit the model well
# hl <- hoslem.test(mylogit1$y, fitted(mylogit1), g=10)
# hl
# 

#Plot these, box labels should match m1 variables (might change dpending on reference category)
boxLabels = c("genderMale", "maritalOther", "smokerNever", 
              "smokerUnknown", "deprivation2","deprivation3","deprivation4","deprivation5_Most","deprivation6_Unknown",
              "60-69 years", "70-79 years", "80 years and over",
              "-r1North East","-r2South East",  "-r3West","-r4Midlands", "-r5North West","-r6South","-r7Mid West")

# # # Enter OR and CI data. boxOdds are the odds ratios,
# # #boxCILow is the lower bound of the CI, boxCIHigh is the upper bound.
# 
#  LRdf <- data.table(yAxis = length(boxLabels):1,
#                    boxOdds =   c(
#                                  -0.10666614,
#                                  0.18077092,
#                                  -0.02971911,
#                                  -0.24038108,
#                                  0.24197933,
#                                  0.25642384,
#                                  0.37378682,
#                                  0.51341033,
#                                  0.29490587,
#                                  0.20320748,
#                                  -0.13225039,
#                                  0.19472397,
#                                  -0.28564831,
#                                  0.07267140,
#                                  0.43493965,
#                                  -0.09398065,
#                                  -0.20477979,
#                                  -0.44389378,
#                                  -0.59930661
# 
# 
#                              ),
#                   boxCILow =  c(
#                                 -0.2383077658,
#                                 0.0494324659,
#                                 -0.2048983894,
#                                 -0.3891669356,
#                                 -0.0134722813,
#                                 0.0003407186,
#                                 0.1248999372,
#                                 0.2730873940,
#                                 0.0741225245,
#                                 -0.1325075865,
#                                 -0.4142678976,
#                                 -0.0879868552,
#                                 -0.7060401488,
#                                 -0.1657396437,
#                                 0.0148100021,
#                                 -0.3614157808,
#                                 -0.4077004848,
#                                 -0.6344504228,
#                                 -0.8027966519
# 
#                                ),
#                   boxCIHigh = c(
#                                 0.024797658,
#                                 0.312454593,
#                                 0.146277013,
#                                 -0.091760229,
#                                 0.498020688,
#                                 0.512992058,
#                                 0.623249713,
#                                 0.754168208,
#                                 0.515318657,
#                                 0.541967396,
#                                 0.148703863,
#                                 0.477045680,
#                                 0.138240846,
#                                 0.308555552,
#                                 0.868892795,
#                                 0.171888547,
#                                 -0.002905535,
#                                 -0.255128183,
#                                 -0.397369669
# 
# 
# 
#                   ))
#  LRdf[, Variable := as.list(boxLabels)]
# 
#  #Plot these
#  (p <- ggplot(LRdf, aes(x = boxOdds, y = boxLabels)) +
#      coord_trans(x = scales:::exp_trans(10)) +
#      #scale_x_continuous(breaks = log10(seq(0.1, 2.5, 0.1)), labels = seq(0.1, 2.5, 0.1),limits = log10(c(0.09,2.5)))+
#      geom_vline(aes(xintercept = 0), size = .25, linetype = "dashed") +
#      geom_point(size = 3.5, color = "orange") +
#      geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = 0.2, color = "gray50") +
#      theme_bw()+
#      theme(panel.grid.minor = element_blank()) +
#      ylab("") +
#      xlab("Beta coeficients") +
#      annotate(geom = "text", y =1.1, x = log10(1.5),
#               label = "", size = 5.5, hjust = 0)
#      #ggtitle("Sociodemographic factors and specialist palliative care")
#  )
# ##OR and CI's ar not correctly transformed but OK on log scale


#Alternative table
m1 <-  data.table(m1)
m1 = m1[-1,]
names(m1)[1]<-"boxOdds"
names(m1)[2]<-"LowCI"
names(m1)[3]<-"HighCI"
m1[, Variable := as.list(boxLabels)]
m1[, yAxis :=length(boxLabels):1]
m1
#Odds Ratios NEED To Be Graphed On Log Scales
# Plot
p <- ggplot(m1, aes(x = boxOdds, y = boxLabels))
p + geom_vline(aes(xintercept = 1), size = .5, linetype = "dashed") +
  geom_errorbarh(aes(xmax = HighCI, xmin = LowCI), size = .9, height = .3, color = "grey5") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  #scale_y_continuous(breaks = yAxis, labels = boxLabels) +
  scale_x_continuous(breaks = seq(0,7,1) ) +
  coord_trans(x = "log10") +
  ylab("sociodemographic factors and former health board region") +
  xlab("Odds ratio (plotted on log scale)") +
  annotate(geom = "text", y =1.1, x = 3.5, label ="",  size = 7, hjust = 0)  
  #ggtitle("Sociodemographic factors and specialist palliative care")



#######################################################################
#Misc final checks

#Look more at under 60 age group
a2 <-  t2Dead_AllCCcT_ds1[,c(1, 34,37)]
a2 <-  unique(a2) # 1 too many, 1 reg two tumours VSage differs by 1 year
a2[, dupP := .N, by = c('DUM_REG')]
a2[DUM_REG=='2XXXXXXX1' , VSAge :=51,]# Fix
a2 <-  unique(a2)
table(a2$VSAge)
a2[, my_age:= cut(as.integer(VSAge),c(-1, 59,69,79, 10000), right=TRUE, labels=c('<60 years', '60-69','70-79','>80 years'))]
#a2[, my_age:= cut(as.integer(VSAge),c(-1, 59,64,69,74,79,84, 10000), right=TRUE, labels=c('<60 years', '60-64', '65-69','70-74', '75-79', '80-84', '> 84 years'))]
table(a2$my_age)
#Restrict to under 60's
x <-  a2[my_age == '<60 years',,]
View(x)
# get patient details for those under 60
x1 <-  merge (x, t2Dead_AllCCdP_ds1, all.x = TRUE)
# get cancer subgroups for those under 60
x2 <-  merge (x, t2Dead_AllCCcT_ds1, all.x = TRUE)
#Get group
x2 <- merge(x2, CancerGroups,by.x ="ICD10_SITE", by.y ="ICD10", all.x =TRUE)
# #Recode C69 - C72 to  its' own group 1 = "Eye, brain & central nervous system' 
x2[ICD10_SITE=='C69'|ICD10_SITE=='C70'| ICD10_SITE=='C71'| ICD10_SITE=='C72' ,Group := 1,]
x2$CancerGroup<-recode(x2$Group,  "1= 'Eye, brain & central nervous system'; 2 = 'Lower gastrointestinal (excl. liver/gallbladder/biliary tract)';
                       3 = 'Upper gastrointestinal'; 4 = 'Pulmonary'; 5= 'Breast & gynaecological'; 6= 'Prostate'; 
                       7 = 'Haematological'; 8 = 'Other' ")
table(x2$Group, x2$ICD10_SITE)

#Run tests on this subgroup
x <- x2[,CrossTable(Group, caco.x, digits=0, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]
View(x)

#Place of death (latest iteration)
x<- x1[,c(1,3),]

x3 <-  merge(x,fm_DEPS, all.x  = TRUE)
x3[,my_instit := 'Z']
x3[my_instit =='Z'& is.na(place_of_death1), my_instit:= 'zNo Data']
x3[my_instit =='Z'& place_of_death1 %like% 'NH', my_instit:= 'N']
x3[my_instit =='Z'& place_of_death1 %like% ' NU', my_instit:= 'N']
x3[my_instit =='Z'& place_of_death1 %like% ' DISTRICT', my_instit:= 'N']
x3[my_instit =='Z'& place_of_death1 %like% 'DISTRICT', my_instit:= 'N']
x3[my_instit =='Z'& place_of_death1 %like% 'NURSING HOME', my_instit:= 'N']
x3[my_instit =='Z'& place_of_death1 %like% ' CARE CENTRE', my_instit:= 'N']
x3[my_instit =='Z'& place_of_death1 %like% ' CARE HOME', my_instit:= 'N']
x3[my_instit =='Z'& place_of_death1 %like% ' RESIDENTIAL CARE', my_instit:= 'N']
x3[my_instit =='Z'& place_of_death1 %like% ' RESIDENTIAL', my_instit:= 'N']
x3[my_instit =='Z'& place_of_death1 %like% ' RESIDENCE', my_instit:= 'N']
x3[my_instit =='Z'& place_of_death1 %like% 'TLC ', my_instit:= 'N']
x3[my_instit =='Z'& place_of_death1 %like% 'THE MOYNE', my_instit:= 'N']
x3[my_instit =='Z'& place_of_death1 %like%  'MT CARMEL', my_instit:= 'N']
x3[my_instit =='Z'& place_of_death1 %like% 'TRALEE COMMUNITY NU', my_instit:= 'N']
x3[my_instit =='Z'& place_of_death1 %like% ' HSE', my_instit:= 'N']
x3[my_instit =='Z'& place_of_death1 %like% 'YOUGHAL AND DISTRICT', my_instit:= 'N']
x3[my_instit =='Z'& place_of_death1 %like% 'YOUGHAL & DISTRICT', my_instit:= 'N']
x3[my_instit =='Z'& place_of_death1 %like% 'YOUGHAL COMMUNITY HOSP', my_instit:= 'N']
x3[place_of_death1 %like% 'TARA WINTHROP', my_instit:= 'N'] 
x3[my_instit =='Z'& place_of_death1 %like% 'COMMUNITY', my_instit:= 'N']
x3[my_instit =='Z'& place_of_death1 %like% ' HOSPICE', my_instit:= 'S']
x3[my_instit =='Z'& place_of_death1 %like% 'HOSPICE', my_instit:= 'S']
x3[place_of_death1 %like% 'MILFORD CARE', my_instit:= 'S'] #override institution designation for Milford
x3[place_of_death1 %like% 'MARYMOUNT UNIVERSITY', my_instit:= 'S']
x3[my_instit =='Z'& place_of_death1 %like% 'ST. PATRICK\'S UNIVERSITY HOSPITAL/MARYMOUNT', my_instit:= 'S']
x3[my_instit =='Z'& place_of_death1 %like% 'ST. PATRICK\'S UNIVERSITY HOSPITAL/MARYMOUNT UNI', my_instit:= 'S']
x3[my_instit =='Z'& place_of_death1 %like% 'THE N W HOSPICE', my_instit:= 'S']
x3[my_instit =='Z'& place_of_death1 %like% 'THE GALWAY HOSPICE', my_instit:= 'S']
x3[my_instit =='Z'& place_of_death1 %like% 'THE GALWAY HOSPICE FOUNDATION', my_instit:= 'S']
x3[my_instit =='Z'& place_of_death1 %like% 'DONEGAL HOSPICE', my_instit:= 'S']
x3[my_instit =='Z'& place_of_death1 %like% ' HOSP', my_instit:= 'O']
x3[my_instit =='Z'& place_of_death1 %like% 'HOSP', my_instit:= 'O']
x3[my_instit =='Z'& place_of_death1 %like% 'ACCIDENT AND EMERGENCY DEPT' , my_instit:= 'O']
x3[my_instit =='Z'& place_of_death1 %like% 'EMERGENCY' , my_instit:= 'O']
x3[my_instit =='Z'& place_of_death1 %like% 'WARD', my_instit:= 'O']
x3[my_instit =='Z'& place_of_death1 %like% 'CLINIC', my_instit:= 'O']
x3[my_instit =='Z'& place_of_death1 %like% 'UNIT', my_instit:= 'O']
x3[my_instit =='Z'& place_of_death_type_source == 'HOSPITAL', my_instit:= 'O']
x3[my_instit =='Z' & place_of_death_type_source == 'RESIDENTIAL', my_instit:= 'H']

x3$S_instit<-recode(x3$my_instit,  " 'H' = 'Home'; 'N' = 'Nursing Home' ; 'O' = 'Hospital' ;  'S' = 'Hospice';  'Z' = 'Unknown'")
table(x3$S_instit)
table(x3$my_instit, x3$S_instit,exclude = NULL)
x3[ is.na(S_instit), S_instit := 'No data']
x3[,table(S_instit)]
x3[,CrossTable(S_instit, caco.x, digits=0, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]

#Look at the 251 controls who died in hospice in more detail
x <- a3[caco==0 & S_instit == 'Hospice',,]
x[,CrossTable(place_of_death_county_id, caco, digits=0, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]


#############################################################################################################################
##Display scratchpad
# d1$CC <-recode(d1$CC, ' 1 = " CC1-B"; 2="CC2-M"; 3="CC3-V"; 4="CC4-J";5="CC5-C";6="CC6-W";7="CC7-G";8= "CC8-L" ')
# d2 <- d1[CCcaco==1,c('CC', 'my_sPOD'),]
# setnames(d2, "my_sPOD", "POD")
# table(d2$CC,d2$POD)
# CrossTable(d2$POD, d2$CC,digits=0, max.width = 1, expected=FALSE, prop.r= FALSE,prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")
# CrossTable(d2$CC, d2$POD,digits=0, max.width = 1, expected=FALSE, prop.r= TRUE,prop.c = FALSE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")
# p <-  ggplot(d2, aes(CC, fill = POD)) + xlab("Cancer centres") + ylab("Place of death (%)") 
# p + geom_bar(aes(y = (..count..)/sum(..count..))) + scale_y_continuous(labels=percent) 
# # p + guides(fill=guide_legend(title=NULL))
# # p + ggtitle("Place of death for patients who died in 2016 receiving specialist palliative care by cancer centre")
# p <-  ggplot(d2,aes(x = CC,fill = POD)) + geom_bar(position = "fill") 
# p + xlab("Cancer centres") + ylab("Place of death (%)") 
# 
#
# d3 <- d1[,c('CC', 'CCcaco'),]
# d3[, Status := as.factor(CCcaco)]
# View(d3)
# d3$Status <-recode(d3$Status, ' 1 = " Case"; 0="Control" ')
# View(d3)
# table(d3$Status,d3$CC)
# CrossTable(d3$Status, d3$CC,digits=0, max.width = 1, expected=FALSE, prop.r= FALSE,prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")
# x <-CrossTable(d3$Status, d3$CC,digits=0, max.width = 1, expected=FALSE, prop.r= FALSE,prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SAS")
# View(x)  
# x <- CrossTable(d3$CC, d3$Status,digits=0, max.width = 1, expected=FALSE, prop.r= TRUE,prop.c = FALSE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SAS")
# View(x) 
# p <-  ggplot(d3, aes(x = CC, fill= Status)) + xlab("Cancer centres") + ylab("Case/Controls (%)") 
# p + geom_bar(aes(y = (..count..)/sum(..count..))) + scale_y_continuous(labels=percent) 
# 
# 
# p <-  ggplot(d3,aes(x = CC,fill= Status,order=Status)) + geom_bar(position = "fill") 
# p + xlab("Cancer centres") + ylab("Case/Controls (%)") 
# 
# #ggplot(data = d3) + geom_bar(mapping = aes(x = CC, fill = Status), position = "fill")
# 
####################################################################################################
####################################################################################################
####################################################################################################
#
#16/04/2020
#Use 2020 revised dataset dtrP_2016_2 to answer reviewers questions
dtrP_2016_2 <-  readRDS(file = "V:\\R_Projects\\RHomeDir\\RS2A\\_data\\dtrP_2016_2.rds" )
dtrP_2016_2[,CrossTable(HasHipe, StudyCase)]
#Original prepared dataset has 4137 Hipe and StudyCase

#get HIPE data for all the cases
aH_ds1 <- readRDS(file = "V:\\R_Projects\\RHomeDir\\RS2A\\_data\\H_ds1.rds")
View(aH_ds1)
uniqueN(aH_ds1$DUM_REG)#77183 

#restrict to our study dataset
w1 <- dtrP_2016_2[, c(1,52,53)]
#w1 <- T2[, c(1,52,53)]
bH_ds1 <-  merge (w1, aH_ds1,by.x = 'DUM_REG', by.y = 'DUM_REG' ) #inner join to gt only those with HIPE
uniqueN(bH_ds1$DUM_REG) #7421 i.e cases with HIPE

#Hospice
table(bH_ds1$DISC_CODE)
bH_ds1$toHospice <- as.numeric(bH_ds1$DISC_CODE == 11)
#Died in hospital
bH_ds1$HospitalDeath <- as.numeric(bH_ds1$DISC_CODE == 6 | bH_ds1$DISC_CODE == 7 )



############################################################################
#/Further HIPE processing Start
#Which episode to  take  for cases and controls
#sequence SPC episodes by DUMREG using dH_ds1 (data for all  who had an episode in the CC in 2016)
uniqueN(bH_ds1$DUM_REG) #7421 patients with 77239 episodes
setorder(bH_ds1, DUM_REG, h1off, h2off)#order the episodes by REG, admission,discharge
bH_ds1[, spccaco :=  as.numeric(any(toHospice | SPC)) , by=  'DUM_REG'] #identify cases and controls
bH_ds1[, Episeq := sequence(.N), by = c('DUM_REG')] #sequence the episodes by REG, admission,discharge
bH_ds1[, Episcount := uniqueN(row_id), by = c('DUM_REG')]#count the espiodes by reg

##First approach - try to assign each individual  to just one cancer centre , unit of analysis is patient
#Find the index SPC episode 
#Note not taking account whether the chosen episode is a CC episode, may need to revise this
bH_ds1[,spcepi := 0, by = c('DUM_REG')] #initalise episode  flag
#Cases
bH_ds1[SPC==1 | toHospice==1, spcepi := as.numeric(Episeq), by = DUM_REG] #take first index spc episode for cases, (... is this a CC case?)
bH_ds1[spcepi == Episeq ,firstspcepi := min(spcepi),by = c('DUM_REG')] #need to ignore the 0 initalisation
bH_ds1[spcepi == Episeq,lastspcepi := max(spcepi),by = c('DUM_REG')] #set  for all episodes of this reg
#Controls
bH_ds1[spccaco==0 , controlepi := as.numeric(max(Episeq)), by = c('DUM_REG')] #take last episode for controls
#Set all na to 0
bH_ds1[is.na(firstspcepi), firstspcepi := 0,]
bH_ds1[is.na(lastspcepi), lastspcepi := 0,]
bH_ds1[is.na(controlepi), controlepi := 0,]
#Set flag to indicate whether Episcase is a CC case
bH_ds1[,EpiscaseIsCC := 0]
bH_ds1[spcepi ==Episeq, EpiscaseIsCC := as.numeric(CC>0)]
bH_ds1[,EpiscaseIsCC := max(EpiscaseIsCC),by = c('DUM_REG')] #set  for all episodes of this reg



#only way this would work reliably
bH_ds1[, CaseType :=  0,]
bH_ds1[, CaseSPC := any(SPC==1), by=  'DUM_REG'] #1 SPC Only
bH_ds1[, CaseHospice := any(toHospice==1), by =  'DUM_REG'] #2 Hospice Only
bH_ds1[, CaseBoth := any((CaseSPC==1 & CaseHospice ==1 )), by=  'DUM_REG'] #3 Both
bH_ds1[CaseSPC & !CaseHospice & !CaseBoth  , CaseType :=  1,]
bH_ds1[!CaseSPC & CaseHospice & !CaseBoth  , CaseType :=  2,]
bH_ds1[CaseSPC & CaseHospice, CaseType :=  3,]
bH_ds1$CaseSPC <-  NULL
bH_ds1$CaseHospice <-  NULL
bH_ds1$CaseBoth <-  NULL
bH_ds1$myCaseType <- ordered( bH_ds1$CaseType,
                              levels = c(0, 1,2,3),
                              labels = c("Control","SPC", "Hospice", "Both")) 
View(bH_ds1)
table(bH_ds1$CaseType)
#Set overnight flag per epsiode
bH_ds1[, overnight :=  (h2off -h1off) > 0,] 
#Count overnight flag per patient
bH_ds1[, overnightCount:= 0,] #initalise
bH_ds1[overnight == TRUE, overnightCount:= as.numeric(uniqueN(Episeq)), by = c('DUM_REG')]
bH_ds1[, overnightCount:= max(overnightCount), by = c('DUM_REG')]#set for all epsiodes for this reg] 
#Count number of  centres per case
bH_ds1[, CentreCount :=  uniqueN(DUMHID), by = c('DUM_REG')]#count the espiodes by reg,] 
#Count number of cancer centres per case
bH_ds1[, CancerCentreCount := 0,]
bH_ds1[CC > 0, CancerCentreCount :=  as.numeric(uniqueN(CC)), by = c('DUM_REG')]#count the espiodes by reg,] #ignore non CC
bH_ds1[, CancerCentreCount :=  max(CancerCentreCount), by = c('DUM_REG')]#set for all epsiodes for this reg] 
#Count the number of SPC per case
bH_ds1[ SPC==1, SPCCount :=  uniqueN(Episeq), by = c('DUM_REG')]
bH_ds1[,SPCCount := max(SPCCount),by = c('DUM_REG')] #set  for all episodes of this reg
bH_ds1[is.na(SPCCount), SPCCount := 0,]
#Count the number of toHospice per case
bH_ds1[toHospice==1, toHospiceCount :=  uniqueN(Episeq), by = c('DUM_REG')]
bH_ds1[,toHospiceCount := max(toHospiceCount),by = c('DUM_REG')] #set  for all episodes of this reg
bH_ds1[is.na(toHospiceCount), toHospiceCount := 0,]
#Flag patients with no SPC or toHospice  at a cancer centre
bH_ds1[, SPCatCC:= 0, ]
bH_ds1[SPC==1 & CC > 0, SPCatCC:= 1, ]
bH_ds1[toHospice==1 & CC > 0, SPCatCC:= 1, ]
bH_ds1[, SPCatCC:= max(SPCatCC), by = c('DUM_REG')]


#Check /
xcaseslastepisode <- bH_ds1[StudyCase==1 & spcepi > 0 & spcepi ==lastspcepi,] #2581 study cases last spc epsiode
xcasesfirstepisode <- bH_ds1[StudyCase==1 & spcepi > 0 & spcepi ==firstspcepi,] #2581 study cases first spc epsiode
xcontrolslasttepisode <- bH_ds1[StudyCase==1 & Episeq ==controlepi,] #1545 #study cases with no spc ( last epispdde taken)


#Keep a formal copy
dtrP_2016_4 <- xcaseslastepisode 
dtrP_2016_5 <-  xcasesfirstepisode 
dtrP_2016_6 <- xcontrolslasttepisode

#Combine cases and controsl to get study dataset
l = list(xcaseslastepisode,xcontrolslasttepisode )
rbindlist(l)
x0 <-  rbindlist(l)
uniqueN(x0$DUM_REG)
dtrP_2016_3 <- x0
x0[,table(spccaco, myCaseType)]

# myCaseType
# spccaco 
#    Control  SPC Hospice Both
# 0    1545    0       0    0
# 1       0 2106      70  405



#Remove HIPE cases where lookback from last spc admission date is > 365 days, see line 2239
xcaseslastepisode <-  merge(w1, xcaseslastepisode, by.x ="DUM_REG", by.y ="DUM_REG", all.x =TRUE) #2258

#Keep a formal copy
dtrP_2016_4A <- xcaseslastepisode 

#Combine cases and controsl to get study dataset
l = list(xcaseslastepisode,xcontrolslasttepisode )
rbindlist(l)
x0 <-  rbindlist(l)
uniqueN(x0$DUM_REG)
dtrP_2016_3 <- x0
x0[,table(spccaco, myCaseType)]

# myCaseType
# spccaco 
#    Control  SPC Hospice Both
# 0    1545    0       0    0
# 1       0 2084      70  404

#Keep a foraml copy of study dataset
dtrP_2016_3 <-  x0




#2020 review study dataset -these are the  relevant HIPE data for patients in study
saveRDS(dtrP_2016_4, file = "V:\\R_Projects\\RHomeDir\\RS2A\\_data\\dtrP_2016_4.rds")
saveRDS(dtrP_2016_5, file = "V:\\R_Projects\\RHomeDir\\RS2A\\_data\\dtrP_2016_5.rds")
saveRDS(dtrP_2016_6, file = "V:\\R_Projects\\RHomeDir\\RS2A\\_data\\dtrP_2016_6.rds")
saveRDS(dtrP_2016_3, file = "V:\\R_Projects\\RHomeDir\\RS2A\\_data\\dtrP_2016_3.rds")

h4 <-  readRDS(file = "V:\\R_Projects\\RHomeDir\\RS2A\\_data\\dtrP_2016_4.rds") 

#Review source HIPE
x0 <- bH_ds1[HasHipe==1,] 
uniqueN(x0$DUM_REG) #7421
uniqueN(x0$DUM_REG) #7421
x0 <- bH_ds1[HasHipe==1 & StudyCase==0,] 
uniqueN(x0$DUM_REG) #3296
x0 <- bH_ds1[HasHipe==1 & StudyCase==1,] 
uniqueN(x0$DUM_REG) #4126
# End check /
# 
# # as.Date(17115 - 113, origin = '1970-01-01')
# 
#
# #Take first SPC  epsiode for cases and last  regular epsiode for each control, alternative code snippet
# bH_ds1[bH_ds1[spccaco==0, .I[Episeq == max(Episeq)], by=  'DUM_REG']$V1, control :=1]
# bH_ds1[bH_ds1[spccaco==1, .I[Episeq == min(Episeq)], by=  'DUM_REG']$V1, control :=0]
# table(bH_ds1$control, bH_ds1$caco)
# table( bH_ds1$caco)
# bH_ds1$control <-  NULL
# x0 <- bH_ds1[, c(1, 29)]
# x0 <- unique(x0)
#table( x0$spccaco) #SPC cases
# Further HIPE processing  End/
############################################################################################
###########################################################################################
##Begin re-analysis  22/04/2020 - refer this to paper figure 2
T1 <-  readRDS(file = "V:\\R_Projects\\RHomeDir\\RS2A\\_data\\dtrP_2016_1.rds") 
T2 <-  readRDS(file = "V:\\R_Projects\\RHomeDir\\RS2A\\_data\\dtrP_2016_2.rds") 
T3 <-  readRDS( file = "V:\\R_Projects\\RHomeDir\\RS2A\\_data\\dtrP_2016_3.rds") #HIPE info
##County of death data
fm_DEPS <- readRDS(file = "V:\\R_Projects\\RHomeDir\\RS2A\\_data\\DEPSData151617_dst2.rds")

T2[, CrossTable(HasHipe, StudyCase)]
T3[, CrossTable(StudyCase, spccaco)]
T3[spccaco ==1, CrossTable(StudyCase,myCaseType)]

##

levels(as.factor(T2$MARITAL))
T2$my_marital <-  as.factor(T2$MARITAL)
levels(T3$my_marital)
T2$my_marital<-recode(T2$my_marital,  " 'D' = 'Other'; 'E' = 'Other'; 'S' = 'Other'; 'W' = 'Other'; 'Z' = 'Other'; 'M'= 'Married' ")
T2summary(T2$my_marital)
table(T2$MARITAL, T2$my_marital)


###

levels(as.factor(T2$SMOKER_ID))
T2$my_smoker <-  as.factor(T2$SMOKER_ID)
levels(T2$my_smoker)
T2$my_smoker<-recode(T2$my_smoker,  " 'C' = 'Ever'; 'N' = 'Never'; 'X' = 'Ever'; 'Z' = 'Unknown' ")
summary(T2$my_smoker)
table(T2$SMOKER_ID, T2$my_smoker)



##Prep
###T2 All cases in HIPE
levels(as.factor(T2$SEX))
T2$my_gender<-recode(T2$SEX,  " 'F' = 'Female'; 'M' = 'Male' ")
table(T2$SEX, T2$my_gender) # 3 mismatch on gender

levels(as.factor(T2$depr_16))
T2$my_depr_16 <-  as.factor(T2$depr_16)
T2[ is.na(depr_16),my_depr_16:= 'Unknown']
T2$my_depr_16<-recode(T2$my_depr_16,  " '1' = '1 Least'; '2' = '2'; '3' = '3'; '4' = '4';'5' = '5 Most';")
table(T2$depr_16, T2$my_depr_16)
summary(T2$my_depr_16)

levels(as.factor(T2$depr_11))
T2$my_depr_11 <-  as.factor(T2$depr_11)
T2[ is.na(depr_11),my_depr_11:= 'Unknown']
T2$my_depr_11<-recode(T2$my_depr_11,  " '1' = '1 Least'; '2' = '2'; '3' = '3'; '4' = '4';'5' = '5 Most';")
table(T2$depr_11, T2$my_depr_11)
summary(T2$my_depr_11)

#Compare 11 and 16
table(T2$my_depr_11, T2$my_depr_16)


T2[, my_age:= cut(as.integer(VSAge),c(-1, 59,69,79, 10000), right=TRUE, labels=c('<60 years', '60-69','70-79','>80 years'))]
#T2[, my_age:= cut(as.integer(VSAge),c(-1, 59,64,69,74,79,84, 10000), right=TRUE, labels=c('<60 years', '60-64', '65-69','70-74', '75-79', '80-84', '> 84 years'))]
table(T2$VSAge, T2$my_age)



###
levels(as.factor(T2$ED_HB))
T2$my_HB<-  as.factor(T2$ED_HB)
T2$my_HB<-recode(T2$my_HB, " 'NE' = 'North Eastern'; 'SE' = 'South Eastern';'W' = 'Western'; 'M' = 'Midlands';'E' = 'Eastern'; 'NW' = 'North Western';'S' = 'Southern';'MW' = 'Mid Western'")
#order levels to be consisten with barchart below
T2$my_HB <- factor(T2$my_HB , levels = c('North Eastern','South Eastern', 'Western','Midlands','Eastern','North Western','Southern','Mid Western'))
summary(T2$my_HB)
table(T2$ED_HB, T2$my_HB)

########################################################################################
##CancerGroup and number of Tumours from T1

#Cancer Group
#/Get these for all tumours in the HIPE dataset 
############################################################
T1 <-  readRDS(file = "V:\\R_Projects\\RHomeDir\\RS2A\\_data\\dtrP_2016_1.rds")
#T3 <-  readRDS(file = "V:\\R_Projects\\RHomeDir\\RS2A\\_data\\dtrP_2016_3.rds")
View(T1)
uniqueN(T1$DUM_REG)
# 10540
uniqueN(T1$DUM_TUM)
# 11954
View(T2)
uniqueN(T2$DUM_REG)
# 10540
uniqueN(T2$DUM_TUM)
# 10540
a1 <-  T2[HasHipe==1, c(1,52,53)] #7421
a2 <-  merge(a1, T1,  by.x= 'DUM_REG', by.y='DUM_REG' ) #inner join, 8533 tumours
a2 <- a2[,c(1,2, 3, 4, 40,50,53 )]
uniqueN(a2$DUM_REG) #7421
###################################################################
#\Get these for all tumours in the HIPE dataset 
#/Get these for call tumours in the study dataset
###################################################################
a1 <-  T3[StudyCase==1, c(1,3,32)] #4106
a2 <-  merge(a1, T1,  by.x= 'DUM_REG', by.y='DUM_REG' ) #inner join, 4727 tumours
uniqueN(a2$DUM_REG) #4103
uniqueN(a2$DUM_TUM) #4727
#\Get these for call tumours in the study dataset
###################################################################


# CancerGroups <- read.csv("G:\\Docs\\S2\\WIP\\ICD10Groups.csv")
# dtCancerGroups <-  data.table(CancerGroups)
saveRDS(dtCancerGroups, file = "V:\\R_Projects\\RHomeDir\\RS2A\\_data\\dtCancerGroups.rds")
a2 <- merge(a2, dtCancerGroups,by.x ="ICD10_SITE", by.y ="ICD10", all.x =TRUE)
a2$CancerGroup<-recode(a2$Group,  " 2 = 'Lower gastrointestinal (excl. liver/gallbladder/biliary tract)';
3 = 'Upper gastrointestinal'; 4 = 'Pulmonary'; 5= 'Breast & gynaecological'; 6= 'Prostate'; 
                     7 = 'Haematological'; 8 = 'Other' ")
View(a2)
table(a2$Group, a2$ICD10_SITE)
x <- a2[,CrossTable(CancerGroup, spccaco,digits=0, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SAS")]
x <- x[, RoundCP := round(prop.col*100)]
View(x)
#SPSS verification
a2[,CrossTable(CancerGroup, spccaco,digits=0, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]

##Recode new group
# x <- a2[Group==8,, ]
# table(x$ICD10_SITE)
##Recode C69 - C72 to  its' own group 1 = "Eye, brain & central nervous system' 
# a2[ICD10_SITE=='C69'|ICD10_SITE=='C70'| ICD10_SITE=='C71'| ICD10_SITE=='C72' ,Group := 1,]
# table(a2$Group, a2$ICD10_SITE)
# a2$CancerGroup<-recode(a2$Group,  "1= 'Eye, brain & central nervous system'; 2 = 'Lower gastrointestinal (excl. liver/gallbladder/biliary tract)';
# 3 = 'Upper gastrointestinal'; 4 = 'Pulmonary'; 5= 'Breast & gynaecological'; 6= 'Prostate';
#                        7 = 'Haematological'; 8 = 'Other' ")
#Analysis
View(a2)
table(a2$Group, a2$ICD10_SITE)
x <- a2[,CrossTable(CancerGroup, spccaco,digits=0, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SAS")]
x <- x[, RoundCP := round(prop.col*100)]
View(x)
#SPSS verification
a2[,CrossTable(CancerGroup, spccaco,digits=0, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]

#Totals #use SPSS output as check 
rm(x1)
x1 <- margin.table(table(a2$CancerGroup, a2$spccaco),1)
View(x1)
#SPSS verification
a2[,CrossTable(CancerGroup, spccaco,digits=0, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]

#Rerun for test statistics
t1 <-  table(a2$CancerGroup, a2$spccaco)
assocstats(t1)
ord.somers.d(t1)

#####################################################
#Number of tumours
a3 <- a2[,c(2,4,53)]#4753
a3 <-  unique(a3) #4126
View(a3)
a3 <-  a3[,numTcat := numT]
a3 <-  a3[numT> 1, numTcat :=2,]
a3[,table(numT,numTcat)]

#Analysis #use SPSS output as check 
x <- a3[,CrossTable(numTcat, spccaco,digits=0, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SAS")]
x <- x[, RoundCP := round(prop.col*100)]
View(x)
a3[,CrossTable(numTcat, spccaco,digits=0, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]

#Totals #use SPSS output as check 
rm(x1)
x1 <- margin.table(table(a3$numTcat, a3$spccaco),1)
View(x1)
#Rerun for test statistics
t1 <-  table(a3$numTcat, a3$spccaco)
assocstats(t1)
ord.somers.d(t1)

################################################################################


##Analysis, 
T2[HasHipe==1,CrossTable(my_smoker,StudyCase,digits=0, max.width = 1, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, format= "SPSS")]
x <- T2[HasHipe==1,CrossTable(my_smoker, StudyCase,digits=0, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SAS")]
#write.table(x, file = "C:\\MKFolders\\Temp\\test.txt", sep = ",", quote = FALSE, row.names = F)
x <- x[, RoundCP := round(prop.col*100)]
View(x)

T2[HasHipe==1,CrossTable(my_smoker, StudyCase,digits=0, max.width = 5, expected=FALSE, prop.r = FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]

#Totals
rm(x1)
x1 <- T2[HasHipe==1,]
x2 <- margin.table(table(x1$my_smoker, x1$StudyCase),1)
View(x2)
#SPSS verification
T2[HasHipe==1,CrossTable(my_smoker, StudyCase,digits=0, max.width = 5, expected=FALSE, prop.r = FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]

#Rerun for test statistics
t1 <-  table(x1$my_smoker, x1$StudyCase)
assocstats(t1)
ord.somers.d(t1)
#Median survival

#Median minimum survival per patient for all death 2016 
#Median survival
t1 <- T2 [,list(
  number=(.N),
  lower=quantile(sSurvival, .10,  na.rm=TRUE),
  middle=quantile(sSurvival, .50, na.rm=TRUE),
  upper=quantile(sSurvival, .90,  na.rm=TRUE)),
 # by = HasHipe
  ]
View(t1)


#Median minimum survival per patient for all death 2016 for all HIPE cases only

#Median survival per patient for all death 2016 and by study case status
t1 <- T2 [HasHipe==1,]
t2 <- t1 [,list(
  number=(.N),
  lower=quantile(sSurvival, .10,  na.rm=TRUE),
  middle=quantile(sSurvival, .50, na.rm=TRUE),
  upper=quantile(sSurvival, .90,  na.rm=TRUE)),
 # by = StudyCase
  ]
View(t2)
#/DOI################################################################
#DOI by year by HIPE
T2 <-  T2[, YOI := year(ddoi)]
w1 <-  T2[,c(1, 52, 60)]
w1<-  w1[HasHipe==0, HipeNoRxYOI :=uniqueN(DUM_REG), by = YOI]
w1<-  w1[HasHipe==1, HipeYesRxYOI :=uniqueN(DUM_REG), by = YOI]
w1<-  w1[, RxDOI :=uniqueN(DUM_REG), by = YOI]
w1<-  w1[, c(-1)]
w1 <-  unique(w1)
w11 <-  unique(w1[, c(2,5 )])
w12 <-  unique(w1[HasHipe==0, c(2,3 )])
w13 <-  unique(w1[HasHipe==1, c(2,4 )])
w14 <-  merge (w12, w13,by.x ="YOI", by.y ="YOI", all.x =TRUE) 
#DOI by year by cancer  centre attendance
w1 <-  T2[,c(1, 52, 53, 60)]
w1<-  w1[HasHipe==1,] #remove non hipe cases
w1 <-  unique(w1)
w1<-  w1[StudyCase==0, StudyCaseNoRxYOI :=uniqueN(DUM_REG), by = YOI]
w1<-  w1[StudyCase==1, StudyCaseYesRxYOI :=uniqueN(DUM_REG), by = YOI]
w1<-  w1[, RxDOI :=uniqueN(DUM_REG), by = YOI]
w1<-  w1[, c(-1, -2)] #Reg abd hipe flag
w1 <-  unique(w1)
w11 <-  unique(w1[, c(2,5 )])
w12 <-  unique(w1[StudyCase==0, c(2,3 )])
w13 <-  unique(w1[StudyCase==1, c(2,4 )])
w14 <-  merge (w12, w13,by.x ="YOI", by.y ="YOI", all.x =TRUE) 
#\DOI####################################################################
##\STUDY DATASET
t2 <-  T2[StudyCase==1,] #4126, 23 cases not removed (i.e. spc survival > 365 days)
t2 <- t2[, c(1,2,46:61)]
#T3 <-  readRDS( file = "V:\\R_Projects\\RHomeDir\\RS2A\\_data\\dtrP_2016_3.rds")
t3 <- T3[,c(1, 14, 15,16,32, 33,36,37,38,41)]#4103, 23 cases removed
t3 <-  merge(t3, fm_DEPS, by.x ="DUM_REG", by.y ="DUM_REG", all.x =TRUE)
t4 <-  merge(t3, t2, by.x ="DUM_REG", by.y ="DUM_REG", all.x =TRUE) #order of merge is NB to rstrict to study dataset i.e 4126  - 23 cases
table(t4$my_instit)
#t4[,CrossTable(my_instit, spccaco,digits=0, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]
#remove cases where spc lookback is not within 365 days of last spc episode, see line 2239



#Analysis #use SPSS output as check 
x <- t4[,CrossTable(my_depr_16, spccaco,digits=0, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SAS")]
x <- x[, RoundCP := round(prop.col*100)]
View(x)
t4[,CrossTable(my_depr_16, spccaco,digits=0, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]
t4[,CrossTable(my_depr_11, spccaco,digits=0, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]

#Totals 
rm(x1)
#t4[,CrossTable(my_depr_16, spccaco,digits=0, max.width = 5, expected=FALSE, prop.r = FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]
x1 <- margin.table(table(t4$my_depr_16, t4$spccaco),1)
View(x1)
#Rerun for test statistics
t1 <-  table(t4$my_depr_16, t4$spccaco)
assocstats(t1)
ord.somers.d(t1)

##my_institution
t4$my_instit<-recode(t4$my_instit,  " 'H' = 'Home'; 'N' = 'Nursing Home' ; 'O' = 'Hospital' ;  'S' = 'Hospice';  'Z' = 'Unknown'")

x <- t4[,CrossTable(my_instit, spccaco,digits=0, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SAS")]
x <- x[, RoundCP := round(prop.col*100)]
View(x)
t4[,CrossTable(my_instit, spccaco, digits=0, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]

#Totals 
rm(x1)
#t4[,CrossTable(my_depr_16, spccaco,digits=0, max.width = 5, expected=FALSE, prop.r = FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]
x1 <- margin.table(table(t4$my_instit, t4$spccaco),1)
View(x1)
#Rerun for test statistics
t1 <-  table(t4$my_instit, t4$spccaco)
assocstats(t1)
ord.somers.d(t1)

#Review breakdown of Nursing home 
 t5<-  t4[my_instit == 'Nursing Home' , ]
t5[grepl('HSE', place_of_death1) ==1, flag := 'HSE', ]
t5[grepl('NH', place_of_death1) ==1, flag := 'NH', ]
t5[is.na(flag), flag  := 'other',]
table(t5$flag)
rm(t5)


##my_HB

x <- t4[,CrossTable(spccaco,my_HB, digits=0, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SAS")]
x <- x[, RoundCP := round(prop.col*100)]
View(x)
t4[,CrossTable(spccaco, my_HB,  digits=0, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]

#Totals 
rm(x1)
#t4[,CrossTable(my_depr_16, spccaco,digits=0, max.width = 5, expected=FALSE, prop.r = FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]
x1 <- margin.table(table(t4$my_HB, t4$spccaco),1)
View(x1)
#Rerun for test statistics
t1 <-  table(t4$my_HB, t4$spccaco)
assocstats(t1)
ord.somers.d(t1)


#Look at median survival by case and control
#Median survival per patient for all death 2016 and by study case status

x1 <- t4 [,list(
  number=(.N),
  lower=quantile(sSurvival, .10,  na.rm=TRUE),
  middle=quantile(sSurvival, .50, na.rm=TRUE),
  upper=quantile(sSurvival, .90,  na.rm=TRUE)),
  #by = spccaco
  ]
View(x1)















################################################################ see line 1819

#Find time from last SPC to Death for cases only
w1 <-  t2[,c(1,3)]
t5 <- T3[spccaco==1, ]
t6 <-  merge(t5, w1, by.x ="DUM_REG", by.y ="DUM_REG", all.x =TRUE) 
t6[, DischargeD := 0]
t6[,DischargeD := as.Date(h2off - 113, origin = '1970-01-01'),] 
t6[,AdmissionD := as.Date(h1off - 113, origin = '1970-01-01'),] 
t6[, discarge_td :=  dod -DischargeD]
t6[, admission_td :=  dod -AdmissionD]
t6[, quantile(discarge_td,na.rm =TRUE),]
t6[, quantile(admission_td,na.rm =TRUE),]


t6[,CrossTable(SPCatCC)]
##########################################
#/Look at this a different way
t7 <- t6[,list(
  number=(.N),
  d0=quantile(discarge_td,  0,  na.rm=TRUE),
  d1=quantile(discarge_td, .10,  na.rm=TRUE),
  d2=quantile(discarge_td, .20, na.rm=TRUE),
  d3=quantile(discarge_td, .30,  na.rm=TRUE),
  d4=quantile(discarge_td, .40,  na.rm=TRUE),
  d5=quantile(discarge_td, .50, na.rm=TRUE),
  d6=quantile(discarge_td, .60,  na.rm=TRUE),
  d7=quantile(discarge_td, .70,  na.rm=TRUE),
  d8=quantile(discarge_td, .80, na.rm=TRUE),
  d9=quantile(discarge_td, .90,  na.rm=TRUE),
  d1=quantile(discarge_td, 1.0,  na.rm=TRUE)
  ),
  ]
View(t7)
#######################################

##/ Restrict dataset to cases with a 365 day lookback period
t8 <-  t6[admission_td >=0 &  admission_td <366, ]
#Eliminate a further 23 cases where date of last SPC epsiode is > 365 days
t8[, quantile(discarge_td,na.rm =TRUE),]
t8[, quantile(admission_td,na.rm =TRUE),]
# Time differences in days
# 0%  25%  50%  75% 100% 
# 0   13   28   53  365 
w1 <-  t8[,c(1)]


###############################################################










##Logistic regression
################################################


##Logistic regression 
#Prepare dataset
LR_ds<- CSO_compare_23[, .(DUM_REG,my_age, my_cso_age_23_1,my_gender, my_marital, my_smoker,my_depr_16, my_HB, spccaco,my_depr_11)]
saveRDS(LR_ds, file = "V:\\R_Projects\\RHomeDir\\RS2A\\_data\\S2LR_ds.rds")
LR_ds<- readRDS(file = "V:\\R_Projects\\RHomeDir\\RS2A\\_data\\S2LR_ds.rds")
#Using deprivation 16
#Make Eastern the reference
LR_ds$my_HB <- factor(LR_ds$my_HB, levels = c( 'Eastern','North Eastern','South Eastern', 'Western','Midlands','North Western','Southern', 'Mid Western'))
table(LR_ds$my_cso_age_23_1)

mylogit1 <- glm(spccaco ~  my_gender + my_marital + my_cso_age_23_1 + + my_depr_16  +  my_HB , data = LR_ds, family = "binomial")
mylogit2 <- glm(spccaco ~ my_gender + my_marital   + my_cso_age_23_1 + my_depr_16,  data = LR_ds, family = "binomial")
mylogit <- glm(spccaco ~ my_gender, data = LR_ds, family = "binomial")
mylogit <- glm(spccaco ~ my_gender + my_marital, data = LR_ds, family = "binomial")
mylogit <- glm(spccaco ~ my_gender + my_marital + VSAge, data = LR_ds, family = "binomial")
mylogit <- glm(spccaco ~ my_marital + VSAge, data = LR_ds, family = "binomial")

##Check deprivation and HB's only

#mylogit1 <- glm(spccaco ~  my_depr_11  + my_HB , data = LR_ds, family = "binomial")
#mylogit3 <- glm(spccaco ~  my_depr_11  + my_HB , data = LR_ds, family = "binomial")


# summary(mylogit1)
# confint(mylogit1)
m1 <- round(exp(cbind(OR = coef(mylogit1), confint(mylogit1))),2)
m2 <- round(exp(cbind(OR = coef(mylogit2), confint(mylogit2))),3)
# m1
# m2
# m2 <- cbind(OR = coef(mylogit1), confint(mylogit1))
#m2 = m2[-1,] #remove intercept data i.e. first row
#View(m2)
#with(mylogit, null.deviance - deviance)
#with(mylogit, df.null - df.residual)
#with(mylogit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))



##LRT
install.packages("lmtest")
require('lmtest')
#HO;reduced (smaller) model is true
#the null hypothesis of the test states that the smaller model provides as good a fit for the data as the larger model
#large test statistic implies the null hypotheseis is false
#H1: reduced model is not true
#m1  everything
#m2 remove variable to be tested
#lrtest(mylogit1,mylogit2)
##logLik(mylogit1)
##anova(mylogit1)
##waldtest(mylogit1,test = "Chisq")
##anova(mylogit1, mylogit2)
##waldtest(mylogit1, mylogit2, test = "Chisq")


#Hosmer-Lemeshow GOF
install.packages("ResourceSelection")
require('ResourceSelection')
#H0: data fits the model
#Interpetation
# p < 0.05 => not a good fit, p > 0.05 not enough evidence to say it is a poor fit
# test power affects p-values
# hl <- hoslem.test(mylogit1$y, fitted(mylogit1), g=10)
# hl
# 


#Plot these, box labels should match m1 variables (might change dpending on reference category)
# boxLabels = c("genderMale", "maritalOther", "smokerNever", 
#               "smokerUnknown",
#                "65-74 years", "75-84 years", "85 years and over",
#               "deprivation2","deprivation3","deprivation4","deprivation5_Most","deprivation6_Unknown",
#               "-r7North East","-r6South East",  "-r5West","-r4Midlands", "-r3North West","-r2South","-r1Mid West")

boxLabels = c("genderMale", "maritalOther", 
               "65-74 years", "75-84 years", "85 years and over",
              "deprivation2","deprivation3","deprivation4","deprivation5_Most","deprivation6_Unknown",
              "-r7North East","-r6South East",  "-r5West","-r4Midlands", "-r3North West","-r2South","-r1Mid West")



# # # Enter OR and CI data. boxOdds are the odds ratios,
# # #boxCILow is the lower bound of the CI, boxCIHigh is the upper bound.
# 
 LRdf <- data.table(yAxis = length(boxLabels):1,
                   boxOdds =   c(
                     -0.09925376,
                     0.19731326,
                     -0.03583543,
                     -0.2613071,
                     -0.19691971,
                     -0.44484769,
                     -0.58846687,
                     0.08084192,
                     0.25048478,
                     0.37766861,
                     0.45887409,
                     0.22790493,
                     -0.38649867,
                     -0.02556642,
                     -0.55570757,
                     -0.14093626,
                     0.1834384,
                     -0.34056389,
                     -0.28843212
                     
                             ),
                  boxCILow =  c(
                    -0.2312381,
                    0.06568888,
                    -0.21176824,
                    -0.41044691,
                    -0.4003739,
                    -0.63631955,
                    -0.7925507,
                    -0.17208397,
                    0.00241025,
                    0.12380561,
                    0.21805638,
                    0.01103088,
                    -0.71348417,
                    -0.34950283,
                    -1.00437166,
                    -0.4313004,
                    -0.26482745,
                    -0.65629147,
                    -0.62888154
                    
                               ),
                  boxCIHigh = c(
                    0.032555347,
                    0.329285084,
                    0.140881794,
                    -0.112367276,
                    0.005499531,
                    -0.255146356,
                    -0.385920018,
                    0.334124701,
                    0.499049186,
                    0.632488228,
                    0.700246205,
                    0.444404241,
                    -0.063950932,
                    0.294493722,
                    -0.106235869,
                    0.143200176,
                    0.642028601,
                    -0.029789242,
                    0.048592691
          
                  ))
#  LRdf[, Variable := as.list(boxLabels)]
# 
 #Plot these
 (p <- ggplot(LRdf, aes(x = boxOdds, y = boxLabels)) +
     coord_trans(x = scales:::exp_trans(10)) +
     #scale_x_continuous(breaks = log10(seq(0.1, 2.5, 0.1)), labels = seq(0.1, 2.5, 0.1),limits = log10(c(0.09,2.5)))+
     geom_vline(aes(xintercept = 0), size = .25, linetype = "dashed") +
     geom_point(size = 3.5, color = "orange") +
     geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = 0.2, color = "gray50") +
     theme_bw()+
     theme(panel.grid.minor = element_blank()) +
     ylab("") +
     xlab("Beta coeficients") +
     annotate(geom = "text", y =1.1, x = log10(1.5),
              label = "", size = 5.5, hjust = 0)
     #ggtitle("Sociodemographic factors and specialist palliative care")
 )
##OR and CI's ar not correctly transformed but OK on log scale


#Alternative table
m1 <-  data.table(m1) #removes variable names i.e. first column 
m1 = m1[-1,] #remove top row i.e. inercept
names(m1)[1]<-"boxOdds"
names(m1)[2]<-"LowCI"
names(m1)[3]<-"HighCI"
m1[, Variable := as.list(boxLabels)]
m1[, yAxis :=length(boxLabels):1]
m1
#Odds Ratios NEED To Be Graphed On Log Scales
# Plot
p <- ggplot(m1, aes(x = boxOdds, y = boxLabels))
p + geom_vline(aes(xintercept = 1), size = .5, linetype = "dashed") +
  geom_errorbarh(aes(xmax = HighCI, xmin = LowCI), size = .9, height = .3, color = "grey5") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  #scale_y_continuous(breaks = yAxis, labels = boxLabels) +
  scale_x_continuous(breaks = seq(0,7,1) ) +
  coord_trans(x = "log10") +
  ylab("sociodemographic factors and former health board region") +
  xlab("Odds ratio (plotted on log scale)") +
  annotate(geom = "text", y =1.1, x = 3.5, label ="",  size = 12, hjust = 0)  
#ggtitle("Sociodemographic factors and specialist palliative care")

#####################################################
#Have a look at POD for all the decedents in 2015


#Place of death (latest iteration)
x<- x1[,c(1,3),]

x3 <-  merge(x,fm_DEPS, all.x  = TRUE)
x3[,my_instit := 'Z']
x3[my_instit =='Z'& is.na(place_of_death1), my_instit:= 'zNo Data']
x3[my_instit =='Z'& place_of_death1 %like% 'NH', my_instit:= 'N']
x3[my_instit =='Z'& place_of_death1 %like% ' NU', my_instit:= 'N']
x3[my_instit =='Z'& place_of_death1 %like% ' DISTRICT', my_instit:= 'N']
x3[my_instit =='Z'& place_of_death1 %like% 'DISTRICT', my_instit:= 'N']
x3[my_instit =='Z'& place_of_death1 %like% 'NURSING HOME', my_instit:= 'N']
x3[my_instit =='Z'& place_of_death1 %like% ' CARE CENTRE', my_instit:= 'N']
x3[my_instit =='Z'& place_of_death1 %like% ' CARE HOME', my_instit:= 'N']
x3[my_instit =='Z'& place_of_death1 %like% ' RESIDENTIAL CARE', my_instit:= 'N']
x3[my_instit =='Z'& place_of_death1 %like% ' RESIDENTIAL', my_instit:= 'N']
x3[my_instit =='Z'& place_of_death1 %like% ' RESIDENCE', my_instit:= 'N']
x3[my_instit =='Z'& place_of_death1 %like% 'TLC ', my_instit:= 'N']
x3[my_instit =='Z'& place_of_death1 %like% 'THE MOYNE', my_instit:= 'N']
x3[my_instit =='Z'& place_of_death1 %like%  'MT CARMEL', my_instit:= 'N']
x3[my_instit =='Z'& place_of_death1 %like% 'TRALEE COMMUNITY NU', my_instit:= 'N']
x3[my_instit =='Z'& place_of_death1 %like% ' HSE', my_instit:= 'N']
x3[my_instit =='Z'& place_of_death1 %like% 'YOUGHAL AND DISTRICT', my_instit:= 'N']
x3[my_instit =='Z'& place_of_death1 %like% 'YOUGHAL & DISTRICT', my_instit:= 'N']
x3[my_instit =='Z'& place_of_death1 %like% 'YOUGHAL COMMUNITY HOSP', my_instit:= 'N']
x3[place_of_death1 %like% 'TARA WINTHROP', my_instit:= 'N'] 
x3[my_instit =='Z'& place_of_death1 %like% 'COMMUNITY', my_instit:= 'N']
x3[my_instit =='Z'& place_of_death1 %like% ' HOSPICE', my_instit:= 'S']
x3[my_instit =='Z'& place_of_death1 %like% 'HOSPICE', my_instit:= 'S']
x3[place_of_death1 %like% 'MILFORD CARE', my_instit:= 'S'] #override institution designation for Milford
x3[place_of_death1 %like% 'MARYMOUNT UNIVERSITY', my_instit:= 'S']
x3[my_instit =='Z'& place_of_death1 %like% 'ST. PATRICK\'S UNIVERSITY HOSPITAL/MARYMOUNT', my_instit:= 'S']
x3[my_instit =='Z'& place_of_death1 %like% 'ST. PATRICK\'S UNIVERSITY HOSPITAL/MARYMOUNT UNI', my_instit:= 'S']
x3[my_instit =='Z'& place_of_death1 %like% 'THE N W HOSPICE', my_instit:= 'S']
x3[my_instit =='Z'& place_of_death1 %like% 'THE GALWAY HOSPICE', my_instit:= 'S']
x3[my_instit =='Z'& place_of_death1 %like% 'THE GALWAY HOSPICE FOUNDATION', my_instit:= 'S']
x3[my_instit =='Z'& place_of_death1 %like% 'DONEGAL HOSPICE', my_instit:= 'S']
x3[my_instit =='Z'& place_of_death1 %like% ' HOSP', my_instit:= 'O']
x3[my_instit =='Z'& place_of_death1 %like% 'HOSP', my_instit:= 'O']
x3[my_instit =='Z'& place_of_death1 %like% 'ACCIDENT AND EMERGENCY DEPT' , my_instit:= 'O']
x3[my_instit =='Z'& place_of_death1 %like% 'EMERGENCY' , my_instit:= 'O']
x3[my_instit =='Z'& place_of_death1 %like% 'WARD', my_instit:= 'O']
x3[my_instit =='Z'& place_of_death1 %like% 'CLINIC', my_instit:= 'O']
x3[my_instit =='Z'& place_of_death1 %like% 'UNIT', my_instit:= 'O']
x3[my_instit =='Z'& place_of_death_type_source == 'HOSPITAL', my_instit:= 'O']
x3[my_instit =='Z' & place_of_death_type_source == 'RESIDENTIAL', my_instit:= 'H']



################################################
#Save changes
#2020 review study dataset
#saveRDS(dtrP_2016_1, file = "V:\\R_Projects\\RHomeDir\\RS2A\\_data\\dtrP_2016_1.rds")
saveRDS(T2, file = "V:\\R_Projects\\RHomeDir\\RS2A\\_data\\dtrP_2016_2.rds")
#saveRDS(T3, file = "V:\\R_Projects\\RHomeDir\\RS2A\\_data\\dtrP_2016_3.rds")


##Compare our study dataset with CSO data for deaths from malignant cancer in 2016 #start line 1850
T2[, CrossTable(HasHipe, StudyCase)]
CSO_compare <-  T2[StudyCase==1,]
a1 <-  CSO_compare[, CrossTable(my_HB)]

CSO_compare[, my_cso_age:= cut(as.integer(VSAge),c(-1, 14,24,34,44,54,64,74,84, 10000), right=TRUE, labels=c('0-14', '15-24', '25-34','35-44', '45-54', '55-64', '65-74','75-84','85 & over'))]

CSO_compare[,table(my_cso_age, VSAge)]
a1 <-  CSO_compare[, CrossTable(my_cso_age)]

CSOCancerGroups <- read.csv("G:\\Docs\\S2\\WIP\\ICD10Groups.csv")
#saveRDS(CSOCancerGroups, file = "V:\\R_Projects\\RHomeDir\\RS2A\\_data\\CSOCancerGroups.rds")

#Get all tumours for these 4126 cases
fmCases <- CSO_compare[,c(1)]
CSO_compareTumours <-  merge(fmCases, T1, all.x=TRUE)
uniqueN(CSO_compareTumours$DUM_REG)#4126
uniqueN(CSO_compareTumours$DUM_TUM)#4753


#Cancer Groups
a2 <-  CSO_compareTumours[,c(1,2,38)] 
a2 <- unique(a2)
a2 <- droplevels(a2)
table(a2$ICD10_SITE)
table(CSOCancerGroups$ICD10)
a3 <- merge(a2, CSOCancerGroups,by.x ="ICD10_SITE", by.y ="ICD10", all.x =TRUE)
uniqueN(a3$DUM_REG)#4126
uniqueN(a3$DUM_TUM)#4753
a1 <-  a3[, CrossTable(CSO_group)]

#Restrict to 4103 analysis dataset, so need spccacp0 variable also
a4 <-  T3[,c(1,32)]

#Cases
CSO_compare_23 <-  merge(a4,CSO_compare, by.x ="DUM_REG", by.y ="DUM_REG",all.x=TRUE) 
uniqueN(CSO_compare_23$DUM_REG)#4103
uniqueN(CSO_compare_23$DUM_TUM)#4103, cases only

#Analysis
CSO_compare_23[, my_cso_age_23:= cut(as.integer(VSAge),c(-1,54,64,74,84, 10000), right=TRUE, labels=c('< 55', '55-64', '65-74','75-84','85 & over'))]


CSO_compare_23[,table(my_cso_age_23, VSAge)]
a1 <-  CSO_compare_23[, CrossTable(my_cso_age_23)]

x <- CSO_compare_23[,CrossTable(my_cso_age_23, spccaco,digits=0, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SAS")]
View(x)
CSO_compare_23[,CrossTable(my_cso_age_23, spccaco,digits=0, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]


#Totals 
rm(x1)
x1 <- margin.table(table(CSO_compare_23$my_cso_age_23, CSO_compare_23$spccaco),1)
x1 <- x1[, RoundCP := round(prop.col*100)]
View(x1)
CSO_compare_23[,CrossTable(my_cso_age_23, spccaco,digits=0, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]


#Rerun for test statistics
t1 <-  table(CSO_compare_23$my_cso_age_23, CSO_compare_23$spccaco)
assocstats(t1)
ord.somers.d(t1)



#Tumours 
CSO_compareTumours_23 <-  merge(a4,CSO_compareTumours, by.x ="DUM_REG", by.y ="DUM_REG",all.x=TRUE) 
uniqueN(CSO_compareTumours_23$DUM_REG)#4103
uniqueN(CSO_compareTumours_23$DUM_TUM)#4727

#Cancer Groups
a2 <-  CSO_compareTumours_23[,c(1,2,3,39)] 
a2 <- unique(a2)
a2 <- droplevels(a2)
table(a2$ICD10_SITE)
table(CSOCancerGroups$ICD10)
a3 <- merge(a2, CSOCancerGroups,by.x ="ICD10_SITE", by.y ="ICD10", all.x =TRUE)
uniqueN(a3$DUM_REG)#4103
uniqueN(a3$DUM_TUM)#4727

#Analysis
x <- a3[,CrossTable(CSO_group, spccaco,digits=0, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SAS")]
View(x)
a3[,CrossTable(CSO_group, spccaco,digits=0, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]


#Totals 
rm(x1)
x1 <- margin.table(table(a3$CSO_group, a3$spccaco),1)
x1 <- x1[, RoundCP := round(prop.col*100)]
View(x1)
#Rerun for test statistics
t1 <-  table(a3$CSO_group, a3$spccaco)
assocstats(t1)
ord.somers.d(t1)


saveRDS(CSO_compare, file = "V:\\R_Projects\\RHomeDir\\RS2A\\_data\\CSO_compare.rds")
saveRDS(CSO_compare_23, file = "V:\\R_Projects\\RHomeDir\\RS2A\\_data\\CSO_compare_23.rds")
saveRDS(CSO_compareTumours, file = "V:\\R_Projects\\RHomeDir\\RS2A\\_data\\CSO_compareTumours.rds")
saveRDS(CSO_compareTumours_23, file = "V:\\R_Projects\\RHomeDir\\RS2A\\_data\\CSO_compareTumours_23.rds")

CSO_compare_23 <- readRDS(file = "V:\\R_Projects\\RHomeDir\\RS2A\\_data\\CSO_compare_23.rds")
#regroup my_CSO_age again
CSO_compare_23[, my_cso_age_23_1:= cut(as.integer(VSAge),c(-1,64,74,84, 10000), right=TRUE, labels=c('< 65', '65-74','75-84','85 & over'))]

#Analysis
CSO_compare_23[,table(my_cso_age_23_1, VSAge)]
a1 <-  CSO_compare_23[, CrossTable(my_cso_age_23_1)]



x <- CSO_compare_23[,CrossTable(my_cso_age_23_1, spccaco,digits=0, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SAS")]
View(x)
CSO_compare_23[,CrossTable(my_cso_age_23_1, spccaco,digits=0, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]


#Totals 
rm(x1)
x1 <- margin.table(table(CSO_compare_23_1$my_cso_age_23, CSO_compare_23$spccaco),1)
x1 <- x1[, RoundCP := round(prop.col*100)]
View(x1)
CSO_compare_23[,CrossTable(my_cso_age_23_1, spccaco,digits=0, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = FALSE, missing.include = FALSE,format= "SPSS")]


#Rerun for test statistics
t1 <-  table(CSO_compare_23$my_cso_age_23_1, CSO_compare_23$spccaco)
assocstats(t1)
ord.somers.d(t1)
