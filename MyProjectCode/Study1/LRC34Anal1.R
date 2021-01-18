require(RODBC)
require (data.table)
require(car)
require(gmodels)
require(vcd)
require(ryouready)
require(ggplot2)
require(reshape2)
library(Rcpp)

myconn <-odbcConnect('TRIAL1')
RC34_all <- sqlQuery(myconn,"select  * from RC34_all", as.is=T)
RC34_DC <- sqlQuery(myconn,"select  * from RC34_DC", as.is=T)
RC34_Episodes <- sqlQuery(myconn,"select  * from RC34_Episodes", as.is=T)
RC34_Diag <- sqlQuery(myconn,"select  * from RC34_Diag", as.is=T)
RC34_Morph <-  sqlQuery(myconn,"select  * from  C34_iarc_morphology", as.is=T)

#Save as data tables
dtRC34_Diag <- data.table(RC34_Diag)
dtRC34_Episode <- data.table(RC34_Episodes)
dtRC34_all <- data.table(RC34_all)
dtRC34_DC <- data.table(RC34_DC)
dtRC34_Morph <- data.table(RC34_Morph)

#Convert dates
dtRC34_all$my_doi <- as.Date(dtRC34_all$DOI)
dtRC34_all$my_censor <- as.Date(dtRC34_all$CENSOR_DT)
#Categorise survival times
dtRC34_all[, survival_cat2:= cut(SURVIVAL,c(-1, 30,90, 180,360, 10000), right=TRUE, labels=c('0-30d', '31-90d','91-180d','181-360d', '>360d'))]
table (dtRC34_all$SURVIVAL, dtRC34_all$survival_cat2)
#Recategorise age groups
dtRC34_all$my_agecat <-recode(dtRC34_all$AGE_GROUP,  " '00_04':'55_59'= '< 60 years'; '60_64':'65_69'= '60-69 years';  '70_74':'75_79'= '70-79 years'; '80_84':'85+'= '80+ years'")
table (dtRC34_all$AGE, dtRC34_all$my_agecat)
table (dtRC34_all$survival_cat2, dtRC34_all$my_agecat)

CrossTable(dtRC34_all$my_agecat,dtRC34_all$survival_cat2,  digits = 2, expected = TRUE, format="SPSS")
#Review cases with zero survival
dtRC34_all [ SURVIVAL==0, table (he,survival_cat2),]
t1 <- dtRC34_all[ SURVIVAL==0, .(PID, my_doi, my_censor, SURVIVAL, DCO, AUTO_ONLY, he, SEQ_TUM),]
View(t1)

#Convert dates
dtRC34_DC$my_doi <- as.Date(dtRC34_DC$DOI)
dtRC34_DC$my_censor <- as.Date(dtRC34_DC$CENSOR_DT)
dtRC34_Episode$my_doi <- as.Date(dtRC34_Episode$DOI)
dtRC34_Episode$my_censor <- as.Date(dtRC34_Episode$CENSOR_DT)
dtRC34_Episode$my_adm <- as.Date(dtRC34_Episode$admission)
dtRC34_Episode$my_dis <- as.Date(dtRC34_Episode$discharge)
dtRC34_Episode[SURVIVAL==0 & Dia_E==1,.(PID,my_adm, my_dis, my_doi, my_censor, SURVIVAL),by =PID]
#Identify cases with 0 survival who were diagnosed  as inpatients
t1 <- dtRC34_Episode[SURVIVAL==0 & Dia_E==1,.(PID)]
t1[,my_survival := 1,]
View (t1)#69 cases included 
#Create dummy my_survival variable
dtRC34_all[, my_survival:= SURVIVAL,]
dtRC34_DC[, my_survival:= SURVIVAL,]
dtRC34_Episode[, my_survival:= SURVIVAL,]
dtRC34_Diag[, my_survival:= SURVIVAL,]
#Cases with 0 survival but who were diagnosed as inpatients have SURVIVAL set to 1
setkey(dtRC34_all, PID)
setkey(dtRC34_DC, PID)
setkey(dtRC34_Episode, PID)
setkey(dtRC34_Diag, PID)

dtRC34_all[t1, my_survival:= i.my_survival]
dtRC34_DC[t1, my_survival:= i.my_survival]
dtRC34_Episode[t1, my_survival:= i.my_survival]
dtRC34_Diag[t1, my_survival:= i.my_survival]

#Review
dtRC34_all [ SURVIVAL==0, table (he,survival_cat2),]
dtRC34_all [ my_survival==0, table (he,survival_cat2),]
dtRC34_all [ SURVIVAL==0, table (SURVIVAL,my_survival),]
dtRC34_DC [ SURVIVAL==0, table (SURVIVAL,my_survival),]
dtRC34_Episode [ SURVIVAL==0, table (SURVIVAL,my_survival),]
dtRC34_Diag [ SURVIVAL==0, table (SURVIVAL,my_survival),]

#Prepare working datasets
NZS <- dtRC34_all [ my_survival >0, .(PID)]
NZStall <- dtRC34_all [ my_survival >0, ,]
NZStDC <- dtRC34_DC [ my_survival >0, ,]
NZStEP <- dtRC34_Episode [ my_survival >0, ,]
NZStDI <- dtRC34_Diag [ my_survival >0, ,]

NZStall[,length(unique(PID))]#13845
NZStEP[,length(unique(PID))] #12433
NZStDI[,length(unique(PID))] #12433
NZStDC[,length(unique(PID))] #13732

#Recode variables
#NCRI

levels(as.factor(NZStall$MARITAL))
NZStall$my_marital <-  as.factor(NZStall$MARITAL)
levels(NZStall$my_marital)
NZStall$my_marital<-recode(NZStall$my_marital,  " 'D' = 'Other'; 'E' = 'Other'; 'S' = 'Other'; 'W' = 'Other'; 'Z' = 'Other'; 'M'= 'Married' ")
summary(NZStall$my_marital)
table(NZStall$MARITAL, NZStall$my_marital)


levels(as.factor(NZStall$SMOKER_ID))
NZStall$my_smoker <-  as.factor(NZStall$SMOKER_ID)
NZStall$my_smoker<-recode(NZStall$my_smoker,  " 'C' = 'Ever'; 'N' = 'Never'; 'X' = 'Ever'; 'Z' = 'Unknown' ")
summary(NZStall$my_smoker)
table(NZStall$SMOKER_ID, NZStall$my_smoker)


levels(as.factor(NZStall$SEX))
NZStall$my_gender <-  as.factor(NZStall$SEX)
NZStall$my_gender<-recode(NZStall$my_gender,  " 'F' = 'Female'; 'M' = 'Male' ")
summary(NZStall$my_gender)
table(NZStall$SEX, NZStall$my_gender)


levels(as.factor(NZStall$VITAL_CAN))
NZStall$my_vital_can <-  as.factor(NZStall$VITAL_CAN)
NZStall$my_vital_can<-recode(NZStall$my_vital_can,  " '1' = 'Other cause (incl. other cancer)'; '2' = 'Lung cancer' ")
summary(NZStall$my_vital_can)
table(NZStall$VITAL_CAN, NZStall$my_vital_can)


levels(as.factor(NZStall$SUMSTAGE0))
NZStall$my_sumstage0 <-  as.factor(NZStall$SUMSTAGE0)
NZStall$my_sumstage0<-recode(NZStall$my_sumstage0,  " '0' = 'Stage0'; '1A' = 'Stage1'; '1B' = 'Stage1'; '2' = 'Stage2'; '2A' = 'Stage1'; '2B' = 'Stage2'; '3' = 'Stage3'; '3A' = 'Stage3'; '3B' = 'Stage3'; '4' = 'Stage4'; '4A' = 'Stage4'; '4B' = 'Stage4';'4C' = 'Stage4'; '9' = 'Unstaged'; '9' = 'Unstaged'; 'N/A' = 'Unstaged'; 'X' = 'Unstaged'")
summary(NZStall$my_sumstage0)
table(NZStall$SUMSTAGE0, NZStall$my_sumstage0)
#levels(C34_allT1$my_sumstage0) <- c("Stage 0","Stage 0", "Stage I/II","Stage I/II", "Stage I/II", "Stage I/II", "Stage I/II", "Stage I/II", "Stage I/II", "Stage I/II", "Stage I/II", "Stage III+", "Stage III+", "Stage III+", "Stage III+", "Stage III+", "Stage III+", "Stage III+", "Stage III+", "Unstaged", "Unstaged", "Unstaged", "Unstaged", "Unstaged", "Unstaged", "Unstaged", "Unstaged", "Unstaged", "Unstaged", "Unstaged","Unstaged","Unstaged","Unstaged", "Unstaged", "Unstaged", "Unstaged", "Unstaged", "Unstaged", "Unstaged", "Unstaged", "Unstaged", "Unstaged")
#Regroup further
NZStall$my_sumstage0.1<-NZStall$my_sumstage0
summary(NZStall$my_sumstage0.1)
levels(NZStall$my_sumstage0.1)
NZStall[is.na(my_sumstage0.1),.(PID)]
NZStall[is.na(my_sumstage0.1),my_sumstage0.1 :='Missing',]
summary(NZStall$my_sumstage0.1)
levels(NZStall$my_sumstage0.1)
NZStall$my_sumstage0.1<-recode(NZStall$my_sumstage0.1,  " 'Stage0' = 'Stage 0/1/2'; 'Stage1' = 'Stage 0/1/2';  'Stage2' = 'Stage 0/1/2'; 'Missing' = 'Unstaged'")
summary(NZStall$my_sumstage0.1)
levels(NZStall$my_sumstage0.1)

tmorphology  <- NZStall[, .(PID, MORPH3, mstem = strtoi(substr(MORPH3, 3,6))), ]
View(tmorphology)
tmorphology <-  merge(tmorphology, dtRC34_Morph, by.x ='mstem', by.y='morphology_stem', all.x = TRUE)
tmorphology <- tmorphology[,.(PID, id)]
View(tmorphology)
tmorphology$my_morph <- as.factor(tmorphology$id)
levels(tmorphology$my_morph)
tmorphology$my_morph<-recode(tmorphology$my_morph,  " '-999' = 'Unspecified malignant'; '1' = 'Carcinoma';  '2' = 'Squamous cell'; '3' = 'Adenocarcinoma'; '4'= 'Small cell'; 
                             '5' = 'Large cell'; '7' = 'Sarcoma'; '9' = 'Unspecified malignant' ")
View(tmorphology)
summary(tmorphology$my_morph)
tmorphology[,table( mstem, my_morph),]
tmorphology <- tmorphology[,.(PID, my_morph)]
NZStall <-  merge(NZStall,tmorphology, by.x ='PID', by.y='PID', all.x = TRUE)
View(NZStall)



#Look at treatments
NZStall[, any_treat := as.numeric(CHEMO_1Y==1 | STRSU_1Y==1 | RADIO_1Y==1),]
NZStall[, surg_only := as.numeric(CHEMO_1Y==0 & STRSU_1Y==1 & RADIO_1Y==0),]
NZStall[, chemo_only := as.numeric(CHEMO_1Y==1 & STRSU_1Y==0 & RADIO_1Y==0),]
NZStall[, radio_only := as.numeric(CHEMO_1Y==0 & STRSU_1Y==0 & RADIO_1Y==1),]
NZStall[, sc_only := as.numeric(CHEMO_1Y==1 & STRSU_1Y==1 & RADIO_1Y==0),]
NZStall[, sr_only := as.numeric(CHEMO_1Y==0 & STRSU_1Y==1 & RADIO_1Y==1),]
NZStall[, src_only := as.numeric(CHEMO_1Y==1 & STRSU_1Y==1 & RADIO_1Y==1),]
NZStall[, rc_only := as.numeric(CHEMO_1Y==1 & STRSU_1Y==0 & RADIO_1Y==1),]

table(NZStall$any_treat)
table(NZStall$any_treat,NZStall$surg_only)
table(NZStall$any_treat,NZStall$chemo_only)
table(NZStall$any_treat,NZStall$radio_only)
table(NZStall$any_treat,NZStall$sc_only)
table(NZStall$any_treat,NZStall$sr_only)
table(NZStall$any_treat,NZStall$src_only)
table(NZStall$any_treat,NZStall$rc_only)

#Create new treatement variable
#1 radio, 
#2 chemo+radio
#3 chemo
#4 surgery
#5 surgery plus
#6 no treatment


NZStall[radio_only ==1, my_treat := 1,]
NZStall[rc_only == 1, my_treat := 2,]
NZStall[chemo_only == 1,my_treat := 3,]
NZStall[surg_only == 1, my_treat := 4,]
NZStall[sc_only == 1 | sr_only==1 | src_only==1, my_treat := 5,]
NZStall[is.na(my_treat), my_treat:= 0,]
table(NZStall$my_treat)
#Make a factor 
NZStall$my_treat  <-  as.factor(NZStall$my_treat)
levels(NZStall$my_treat)<- c('No treatment', 'Radio', 'Radio/Chemo', 'Chemo', 'Surgery', 'Surgery Plus')
summary(NZStall$my_treat)

#DC

levels (as.factor(NZStDC$marital_status))
NZStDC$my_dcMarital<-  as.factor(NZStDC$marital_status)
levels(NZStDC$my_dcMarital)<- c('Divorced', 'Separated', 'Married', 'Single', 'Widowed')
summary(NZStDC$my_dcMarital)
table(NZStDC$marital_status, NZStDC$my_dcMarital)

levels(as.factor(NZStDC$institution_id))
NZStDC$my_institut <-  as.factor(NZStDC$institution_id)
levels(NZStDC$my_institut)<- c('Home', 'NursingH', 'Hospital', 'Hospice', 'Unknown')
summary(NZStDC$my_institut)
table(NZStDC$institution_id, NZStDC$my_institut)



# SOCIAL_GROUP	DESCRIPTION
# -	Unskilled manual workers
# &	Unknown
# 0	Farmers, relatives assisting, farm managers
# 1	Farm labourers, fishermen
# 2	Higher professional
# 3	Lower professional
# 4	Employers and managers
# 5	Salaried employees
# 6	Non-manual wage earners (white collar)
# 7	Non-manual wage earners (other)
# 8	Skilled manual workers
# 9	Semi-skilled manual workers

levels(as.factor(NZStDC$socio_economic_group))
NZStDC$my_dcSeg <-  as.factor(NZStDC$socio_economic_group)
levels(NZStDC$my_dcSeg)
levels(NZStDC$my_dcSeg)<- c('Unskilled manual','Unknown1', 'Unknown2', 'Unknown3','Unknown4','Unknown5', 'Farmers',
                               'Farm labourer/Fisherman' , 'Higher professional', 'Lower professional', 'Employer/manager',
                               'Salaried', 'White collar', 'Non manual waged', 'Skilled manual', 'Semi skilled manual','Unknown6')
summary(NZStDC$my_dcSeg)
table(NZStDC$my_dcSeg,NZStDC$socio_economic_group)
#Regroup further
NZStDC$my_dcSeg1 <-NZStDC$my_dcSeg
NZStDC[is.na(my_dcSeg1),.(PID)]
NZStDC[is.na(my_dcSeg1),my_dcSeg1 :='Missing',]
levels(NZStDC$my_dcSeg1)
levels(NZStDC$my_dcSeg1)<-c('Unskilled manual','Unknown', 'Unknown', 'Unknown','Unknown','Unknown', 'Farmers',
  'Farm labourer/Fisherman' , 'Higher professional', 'Lower professional', 'Employer/manager',
  'Salaried', 'White collar', 'Non manual waged', 'Skilled manual', 'Semi skilled manual','Unknown', 'Unknown')
summary(NZStDC$my_dcSeg1)
table(NZStDC$my_dcSeg1,NZStDC$socio_economic_group)


#HIPE Episode
NZStall[,length(unique(PID))] #13845
NZStEP[,length(unique(PID))] #12433
table(NZStall$he) #12433 with Hipe, 1412 without Hipe
#Need to look at all episodes initally
dtRC34_Episode[,length(unique(PID))] #12470
dtRC34_Episode[,diag_to_adm := my_adm - my_doi,]
#Get diagnosis episode or episode with -7 days of diagnosis
dtRC34_Ep1 <- dtRC34_Episode[Dia_E==1 | (diag_to_adm < 15 & diag_to_adm > -8), ,] 
View(dtRC34_Ep1)
dtRC34_Ep1[,length(unique(PID))] #10844, 1589 Hipe omitted
#Get earliest episode of these
dtRC34_Ep2 <-  dtRC34_Ep1[,.SD[which.min(episode_seq)] ,by= PID]
View(dtRC34_Ep2)
dtRC34_Ep2[SURVIVAL==0,table(SURVIVAL,my_survival)] #extra two records with 0 SURVIVAL  here not in NZSxx datasets
dtRC34_Ep2[,uniqueN(PID)]  #10844,
#Review omitted HIPE records by PID
#Get all
t1 <-  dtRC34_Episode[,(PID) ,by=PID]
View(t1)
#Get relevant
t2 <- dtRC34_Ep2[,(PID) ,by=PID]
View(t2)
#Get omitted
t3 <- fsetdiff(t1, t2) #PIDs excluded
View(t3)
##Look at these
setkey(t3, PID)
dtRC34_Episode[t3, omitted := i.PID,]
View(dtRC34_Episode)
rm(t1,t2,t3)

levels(as.factor(dtRC34_Ep2$admtype))
dtRC34_Ep2$my_admtype <- as.factor(dtRC34_Ep2$admtype)
levels(dtRC34_Ep2$my_admtype) <- c('Planned','Planned', 'Unplanned', 'Unplanned')
table(dtRC34_Ep2$admtype, dtRC34_Ep2$my_admtype)

dtRC34_Ep2$my_discode <- as.factor(dtRC34_Ep2$discode)
levels(dtRC34_Ep2$my_discode) <- c('Self Discharge','Self Discharge', 'Home', 'Nursing Home',  'Acute hospital',
'Acute hospital', 'Other',  'Died', 'Died',  'Non acute hospital', 'Home','Non acute hospital', 'Hospice',
'Other', 'Other', 'Other',  'Nursing Home',  'Acute hospital', 'Acute hospital', 'Other', 'Died', 'Died',
'Non acute hospital','Non acute hospital')
table(dtRC34_Ep2$discode, dtRC34_Ep2$my_discode)


dtRC34_Ep2$my_disstatus <- as.factor(dtRC34_Ep2$disstatus)
levels(dtRC34_Ep2$my_disstatus) <- c('Public','Private')
table(dtRC34_Ep2$disstatus, dtRC34_Ep2$my_disstatus)

dtRC34_Ep2$my_mc <- as.factor(dtRC34_Ep2$mc)
levels(dtRC34_Ep2$my_mc) <- c('No','Yes', 'Unknown')
table(dtRC34_Ep2$mc, dtRC34_Ep2$my_mc)

#HIPE Diagnosis
#Take relevant first episode only  
#so one episode per patient
hid <- dtRC34_Ep2[,.(PID,RowID)]
View(hid)
dtRC34_Diag1 <-  merge(hid, dtRC34_Diag, by = c('PID', 'RowID'), all.x=TRUE)
View(dtRC34_Diag1)
dtRC34_Diag2 <- dtRC34_Diag1[, c(1:2, 10:31)]
View(dtRC34_Diag2)
#---------------------------
#---------------------------
# Get comorbidities --17 charlson cmbs
#remove text to leave only diagnosis codes
for(i in 1:20){dtRC34_Diag2[ , paste0("d", i) := gsub( " .*$","", get(paste0("d", i)))]}
View(dtRC34_Diag2)
##Add column for each cmb to be checked and initalise to 0
system.time(for (i in 1:17) {dtRC34_Diag2[ , paste0("E_Ch",i) := 0]})
View(dtRC34_Diag2)
#Get data table of Charlson cmbs
load("I:/EOL/RHomeDir/RLungProject/_Data/dtCharlson.Rda")
#Prepare 17 row vectors of diagnosis codes for each Charlson cmb
cnames <- names(dtCharlson)
#Prepare 17 vectors listing diagnosis indicating Charlson cmb
for (i in 1:17){assign(paste0("c",i), paste(dtCharlson[i, cnames[-1], with = FALSE], collapse = "|"))}
#remove lung cancer code (C34) from c14
c14
c14 <- "C00|C01|C02|C03|C04|C05|C06|C07|C08|C09|C10|C11|C12|C13|C14|C15|C16|C17|C18|C19|C20|C21|C22|C23|C24|C25|C26|C30|C31|C32|C33|C37|C38|C39|C40|C41|C43|C45|C46|C47|C48|C49|C50|C51|C52|C53|C54|C55|C56|C57|C58|C60|C61|C62|C63|C64|C65|C66|C67|C68|C69|C70|C71|C72|C73|C74|C75|C76|C81|C82|C83|C84|C85|C88|C90|C91|C92|C93|C94|C95|C96|C97"
#Prepare character string listing names of the 17 row vectors
chDia  <-  paste0("c",1:17) 
#Prepare character string listing corresponding column name,(0 initalised) in Hipe diagnostic data
E_Cmb  <- paste0("E_Ch", 1:17)
#Get cmbs in loop for each row in hipe data
#17 charlson cmbs, 20 diagnosis columns in hipe data
#E_Cmb[i] = E_ch i column in hipe data
#chDia[i] = vector containing diagnositic codes for i Charlson cmb
system.time(for(i in 1:17){dtRC34_Diag2[ , E_Cmb[i] := as.numeric(any(grepl(get(chDia[i]), c(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)))), by=PID]})
View(dtRC34_Diag2)

##Apply Charlson Hierarchies  
# NB do this before assigning weights
dtRC34_Diag2[(E_Ch9==1 & E_Ch15==1), E_Ch9 := 0]
dtRC34_Diag2[(E_Ch10==1 & E_Ch11==1), E_Ch10 := 0]
#ignore c34 lung cancer mets  or just mets with no other cancer listed
dtRC34_Diag2[(E_Ch14==0 & E_Ch16==1), E_Ch16 := 0]
# otherwise proceed as normal (non lung cancer and mets)
dtRC34_Diag2[(E_Ch14==1 & E_Ch16==1), E_Ch14 := 0]
View(dtRC34_Diag2)

#Add empty columns to datatable for Charlson weights and  set to 0
#Epsiode
for(i in 1:17){dtRC34_Diag2[ , paste0("E_wt", i) := 0]}
View(dtRC34_Diag2)


#Assign weights - explicitly
#Updated weights used here
#MBK --lung cancer code excluded so include E_Ch14
dtRC34_Diag2[(E_Ch1==1), E_wt1 := 0]
dtRC34_Diag2[(E_Ch2==1), E_wt2 := 2]
dtRC34_Diag2[(E_Ch3==1), E_wt3 := 0]
dtRC34_Diag2[(E_Ch4==1), E_wt4 := 0]
dtRC34_Diag2[(E_Ch5==1), E_wt5 := 2]
dtRC34_Diag2[(E_Ch6==1), E_wt6 := 1]
dtRC34_Diag2[(E_Ch7==1), E_wt7 := 1]
dtRC34_Diag2[(E_Ch8==1), E_wt8 := 0]
dtRC34_Diag2[(E_Ch9==1), E_wt9 := 2]
dtRC34_Diag2[(E_Ch10==1), E_wt10 := 0]
dtRC34_Diag2[(E_Ch11==1), E_wt11 := 1]
dtRC34_Diag2[(E_Ch12==1), E_wt12 := 2]
dtRC34_Diag2[(E_Ch13==1), E_wt13 := 1]
dtRC34_Diag2[(E_Ch14==1), E_wt14 := 2] 
dtRC34_Diag2[(E_Ch15==1), E_wt15 := 4]
dtRC34_Diag2[(E_Ch16==1), E_wt16 := 6] 
dtRC34_Diag2[(E_Ch17==1), E_wt17 := 4]

#Add weights by row  
#Note this excludes the C34 lung cancer diagnosis as C34 was dropped from c14 vector
#Episode
dtRC34_Diag2[, Ewcharlsum := sum(c(E_wt1,E_wt2,E_wt3,E_wt4,E_wt5,E_wt6,E_wt7,E_wt8,E_wt9,E_wt10,E_wt11,E_wt12,E_wt13,E_wt14,E_wt15,E_wt16,E_wt17)), by = PID]


#Set charlson index 
#episode
dtRC34_Diag2[Ewcharlsum==0, Echarlindex :=0]
dtRC34_Diag2[Ewcharlsum==1, Echarlindex :=1]
dtRC34_Diag2[Ewcharlsum> 1, Echarlindex :=2]
View(dtRC34_Diag2)

#try other grouping 22/12/2017
dtRC34_Diag2[Ewcharlsum==0, cncmb :=0] #7845
dtRC34_Diag2[Ewcharlsum==1, cncmb :=1] #1569
dtRC34_Diag2[Ewcharlsum > 1 & Ewcharlsum <6,cncmb :=2] #1142
dtRC34_Diag2[Ewcharlsum >5,cncmb :=3] #288
table(dtRC34_Diag2$cncmb)

#Check most common cmb
t1 <- table(dtRC34_Diag2$E_Ch6) #COPD 1521
View(t1)

#Take summary file of relevant data
dtRC34_Diag3 <- dtRC34_Diag2[, 1:2, 23:24,30, 60, with=FALSE,]
##dtRC34_Diag3 <- dtRC34_Diag2[,.SD,.SDcols= c(1:2, 23:24,30, 60)]
View(dtRC34_Diag3)
table(dtRC34_Diag3$Echarlindex) ##All cmb 2999
table(dtRC34_Diag3$Echarlindex, dtRC34_Diag3$E_Ch6 )
#Clean up
rm(list=chDia)
rm (cnames, chDia, E_Cmb, i,hid)
#---------------------------
#---------------------------
#Basic Analysis
require(gmodels)
t1 = CrossTable(NZStall$my_agecat,NZStall$survival_cat2, digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, format= "SAS")
write.table(t1[1:2], file = "C:\\temp\\f1.csv", sep = ",", col.names = NA, qmethod = "double")


levels(NZStall$my_gender)
table(NZStall$my_gender)
#Reorder
NZStall$my_gender <- factor(NZStall$my_gender, levels = c("Male", "Female"  ))
t1 = CrossTable(NZStall$my_gender,NZStall$survival_cat2, digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, format= "SAS")
write.table(t1[1:2], file = "C:\\temp\\f1.csv", sep = ",", col.names = NA, qmethod = "double")


levels(NZStall$my_marital)
table(NZStall$my_marital)
t1 = CrossTable(NZStall$my_marital,NZStall$survival_cat2, digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, format= "SAS")
write.table(t1[1:2], file = "C:\\temp\\f1.csv", sep = ",", col.names = NA, qmethod = "double")


levels(NZStall$my_smoker)
table(NZStall$my_smoker)
t1 = CrossTable(NZStall$my_smoker,NZStall$survival_cat2, digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, format= "SAS")
write.table(t1[1:2], file = "C:\\temp\\f1.csv", sep = ",", col.names = NA, qmethod = "double")


levels(NZStall$my_sumstage0.1)
table(NZStall$my_sumstage0.1)
t1 = CrossTable(NZStall$my_sumstage0.1,NZStall$survival_cat2, digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, format= "SAS")
write.table(t1[1:2], file = "C:\\temp\\f1.csv", sep = ",", col.names = NA, qmethod = "double")


levels(NZStall$my_morph)
NZStall$my_morph0.1 <- NZStall$my_morph
levels(NZStall$my_morph0.1)
levels(NZStall$my_morph0.1)<- c('Adenocarcinoma','Carcinoma','Large cell', 'Unspecified malignant',
                                'Small cell','Squamous cell','Unspecified malignant')
table(NZStall$my_morph, NZStall$my_morph0.1)
table(NZStall$my_morph)
t1 = CrossTable(NZStall$my_morph0.1,NZStall$survival_cat2, digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, format= "SAS")
write.table(t1[1:2], file = "C:\\temp\\f1.csv", sep = ",", col.names = NA, qmethod = "double")


table(NZStall$my_vital_can)
t1 = CrossTable(NZStall$my_vital_can,NZStall1$survival_cat2, digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, format= "SAS")
write.table(t1[1:2], file = "C:\\temp\\f1.csv", sep = ",", col.names = NA, qmethod = "double")

table(NZStall$my_treat)
t1 = CrossTable(NZStall$my_treat,NZStall1$survival_cat2, digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, format= "SAS")
write.table(t1[1:2], file = "C:\\temp\\f1.csv", sep = ",", col.names = NA, qmethod = "double")


#DC
NZStDC1 <- NZStDC[,.(PID, my_institut, my_dcSeg1),]
View(NZStDC1)
NZStall1 <-  merge(NZStall, NZStDC1, by = 'PID', all.x = TRUE )


levels(NZStall1$my_dcSeg1)
NZStall1$my_dcSeg1.1 <- NZStall1$my_dcSeg1
NZStall1[is.na(my_dcSeg1),my_dcSeg1.1 :='Missing',]
levels(NZStall1$my_dcSeg1.1)
levels(NZStall1$my_dcSeg1.1)<- c('Manual','Unknown', 'Farmer/Fisher', 'Farmer/Fisher',
                               'Professional', 'Professional', 'Waged','Waged','Waged',
                                'Non-manual', 'Manual', 'Manual','No DC')
table(NZStall1$my_dcSeg1, NZStall1$my_dcSeg1.1) # NA's don't show up
NZStall1$my_dcSeg1.1 <- factor(NZStall1$my_dcSeg1.1, levels = c("Manual", "Non-manual" ,'Waged', 
                                                   'Farmer/Fisher', 'Professional','Unknown','No DC'))
t1 = CrossTable(NZStall1$my_dcSeg1.1,NZStall1$survival_cat2, digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, format= "SAS")
write.table(t1[1:2], file = "C:\\temp\\f1.csv", sep = ",", col.names = NA, qmethod = "double")

levels(NZStall1$my_institut)
NZStall1$my_institut0.1 <- NZStall1$my_institut
NZStall1[is.na(my_institut0.1),my_institut0.1 :='Missing',]
levels(NZStall1$my_institut0.1)
table(NZStall1$my_institut, NZStall1$my_institut0.1)
levels(NZStall1$my_institut0.1) <- c( 'Home' ,'NursingH', 'Hospital', 'Hospice', 'Unknown','No DC')
#Reorder
NZStall1$my_institut0.1 <- factor(NZStall1$my_institut0.1, levels = c('Hospital', 'Home' ,'Hospice','NursingH', 'Unknown','No DC' ))

t1 = CrossTable(NZStall1$my_institut0.1,NZStall1$survival_cat2, digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, format= "SAS")
write.table(t1[1:2], file = "C:\\temp\\f1.csv", sep = ",", col.names = NA, qmethod = "double")

#Hipe
dtRC34_Ep3 <- dtRC34_Ep2[,.(PID, my_admtype,my_discode,my_disstatus,my_mc, los),]
#Episode details
NZStall1 <-  merge(NZStall1, dtRC34_Ep3, by = 'PID', all.x = TRUE )
#Diag details
NZStall1 <-  merge(NZStall1, dtRC34_Diag3, by = 'PID', all.x = TRUE )
View (NZStall1)

levels(NZStall1$my_admtype)
NZStall1$my_admtype0.1 <- NZStall1$my_admtype
NZStall1[he==1 & is.na(my_admtype0.1),my_admtype0.1 :='Not relevant',]
NZStall1[is.na(my_admtype0.1),my_admtype0.1 :='No HIPE',]
View(NZStall1)
levels(NZStall1$my_admtype0.1)
table(NZStall1$my_admtype, NZStall1$my_admtype0.1)
NZStall1$my_admtype0.1 <- factor(NZStall1$my_admtype0.1, levels =
                          c( "Unplanned", "Planned", "Not relevant",  "No HIPE"))

t1 = CrossTable(NZStall1$my_admtype0.1,NZStall1$survival_cat2, digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, format= "SAS")
write.table(t1[1:2], file = "C:\\temp\\f1.csv", sep = ",", col.names = NA, qmethod = "double")


levels(as.factor(NZStall1$Echarlindex))
NZStall1$Echarlindex0.1 <- as.factor(NZStall1$Echarlindex)
table(NZStall1$Echarlindex0.1)
NZStall1[he==1 & is.na(Echarlindex0.1),Echarlindex0.1 :='Not relevant',]
NZStall1[is.na(Echarlindex0.1),Echarlindex0.1 :='No HIPE',]
View(NZStall1)
levels(NZStall1$Echarlindex0.1)
table(as.factor(NZStall1$Echarlindex), NZStall1$Echarlindex0.1)
levels(NZStall1$Echarlindex0.1) <-  c( "None", "Any", "Any", "Not relevant",  "No HIPE")
table(as.factor(NZStall1$Echarlindex), NZStall1$Echarlindex0.1)
t1 = CrossTable(NZStall1$Echarlindex0.1,NZStall1$survival_cat2, digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, format= "SAS")
write.table(t1[1:2], file = "C:\\temp\\f1.csv", sep = ",", col.names = NA, qmethod = "double")
#Quick look
levels(as.factor(NZStall1$my_discode))
NZStall1$my_discode0.1 <- as.factor(NZStall1$my_discode)
NZStall1[he==1 & is.na(my_discode0.1),my_discode0.1 :='Not relevant',]
NZStall1[is.na(my_discode0.1),my_discode0.1 :='No HIPE',]
t1 = CrossTable(NZStall1$my_discode0.1,NZStall1$survival_cat2, digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, format= "SAS")
write.table(t1[1:3], file = "C:\\temp\\f1.csv", sep = ",", col.names = NA, qmethod = "double")

levels(as.factor(NZStall1$my_disstatus))
NZStall1$my_disstatus0.1 <- as.factor(NZStall1$my_disstatus)
NZStall1[he==1 & is.na(my_disstatus0.1),my_disstatus0.1 :='Not relevant',]
NZStall1[is.na(my_disstatus0.1),my_disstatus0.1 :='No HIPE',]
t1 = CrossTable(NZStall1$my_disstatus0.1,NZStall1$survival_cat2, digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, format= "SAS")
write.table(t1[1], file = "C:\\temp\\f1.csv", sep = ",", col.names = NA, qmethod = "double")


levels(as.factor(NZStall1$my_mc))
NZStall1$my_mc0.1 <- as.factor(NZStall1$my_mc)
NZStall1[he==1 & is.na(my_mc0.1),my_mc0.1 :='Not relevant',]
NZStall1[is.na(my_mc0.1),my_mc0.1 :='No HIPE',]
t1 = CrossTable(NZStall1$my_mc0.1,NZStall1$survival_cat2, digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, format= "SAS")
write.table(t1[1], file = "C:\\temp\\f1.csv", sep = ",", col.names = NA, qmethod = "double")
 
t1 = CrossTable(NZStall1$E_Ch6,NZStall1$survival_cat2, digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, format= "SAS")
 
#Median survival
t1 <- NZStall1 [,list(
  number=(.N),
  lower=quantile(my_survival, .25,  na.rm=TRUE),
  middle=quantile(my_survival, .50, na.rm=TRUE),
  upper=quantile(my_survival, .75,  na.rm=TRUE)),
  ]
View(t1)
          
#Median LOS
NZStall1$my_los <- NZStall1$los
#Make day cases los = 1 (diagnosis episode)
NZStall1[, my_los := my_los + 1,]
t1 <- NZStall1 [,list(
  number=(.N),
  lower=quantile(my_los, .25,  na.rm=TRUE),
  middle=quantile(my_los, .50, na.rm=TRUE),
  upper=quantile(my_los, .75,  na.rm=TRUE)),
  by = survival_cat2]
View(t1)


#Check most common procedures by survival category
t1 <- NZStall1 [survival_cat2=='0-30d',table(p1),]
View(t1)
t1 <- NZStall1 [survival_cat2=='31-90d',table(p1),]
View(t1)
t1 <- NZStall1 [survival_cat2=='91-180d',table(p1),]
View(t1)
t1 <- NZStall1 [survival_cat2=='181-360d',table(p1),]
View(t1)





#get Cramer`s V statistic for categorical

t1  <- table(NZStall1$my_gender,NZStall1$survival_cat2)
assocstats(t1)


t1  <- table(NZStall1$my_marital,NZStall1$survival_cat2) 
assocstats(t1)

t1  <- table(NZStall1$my_smoker,NZStall1$survival_cat2) 
assocstats(t1)

t1  <- table(NZStall1$my_dcSeg1.1,NZStall1$survival_cat2) 
assocstats(t1)

t1  <- table(NZStall1$my_admtype0.1,NZStall1$survival_cat2) 
assocstats(t1)

t1  <- table(NZStall1$my_morph0.1,NZStall1$survival_cat2) 
assocstats(t1)

t1  <- table(NZStall1$my_treat,NZStall1$survival_cat2) 
assocstats(t1)

t1  <- table(NZStall1$my_institut0.1,NZStall1$survival_cat2) 
assocstats(t1)

t1  <- table(NZStall1$my_vital_can,NZStall1$survival_cat2) 
assocstats(t1)

t1  <- table(NZStall1$Echarlindex0.1,NZStall1$survival_cat2) 
assocstats(t1)

#Find Somers'' d statistic for ordinal variables agecat2, sumstage1
#rows are IV and columns are survival_cat2, the depenendent variable
t1  <- table(NZStall1$my_agecat,NZStall1$survival_cat2) 
ord.somers.d(t1)
assocstats(t1)
t1  <- table(NZStall1$my_sumstage0.1,NZStall1$survival_cat2) 
ord.somers.d(t1)
assocstats(t1)

#t1  <- table(NZStall1$Echarlindex0.1,NZStall1$survival_cat2) 
#ord.somers.d(t1)
######################################
#Graph 
#wide to long by PID and survial_cat2

NZStall1Graph <- NZStall1[,.(PID,survival_cat2, my_agecat, AGE, my_survival, my_gender, my_marital,
                             my_smoker, my_sumstage0.1, my_morph0.1, my_dcSeg1.1, 
                             Echarlindex0.1, my_admtype0.1, my_treat, 
                             my_institut0.1, my_vital_can,los,E_Ch6 ),]
View(NZStall1Graph)
#Use 30 day intervals 
NZStall1Graph[, survival_cat3:= cut(my_survival,breaks= c(-1,30,60,90,120,150,180,210,240,270,300,330,360, 10000), right=TRUE,
                              labels=c('0-30d', '31-60d','61-90d','91-120d','121-150d','151-180d',
                            '181-210d','211-240d','241-270d','271-300d','301-330d','331-360d', '>360d'))]


table(NZStall1Graph$survival_cat3,NZStall1Graph$survival_cat2)
#Make morphology graph look better - shorten names
levels(NZStall1Graph$my_morph0.1)
levels(NZStall1Graph$my_morph0.1)<- c('Adeno','Carcinoma','Large cell', 'Unspecified',
                                'Small cell','Squamous cell')

#Transform
names(NZStall1Graph)
View(NZStall1Graph)
melted<- melt(NZStall1Graph, id.vars = c( 'PID', 'survival_cat3'))
View(melted)
melted$value <-  as.factor(melted$value)
#names(melted) <- c('PID', 'X', 'dfsff')
#melted$value <-  as.factor(melted$value)
#ggplot(melted, aes(x = survival_cat2, y=value)) + geom_bar() + facet_wrap(~variable, ncol = 3)

#Prepare dataset for graphs
t1= CrossTable(NZStall1Graph$my_agecat, NZStall1Graph$survival_cat3,digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, format= "SAS")
t1[2]
a <-  data.frame(t1[2])
names(a) <- c('Key', 'Survival', 'Proportion')
View(a)
t1 = CrossTable(NZStall1Graph$my_gender, NZStall1Graph$survival_cat3,digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, format= "SAS")
t1[2]
b <- data.frame(t1[3])
View(b)
t1 = CrossTable(NZStall1Graph$my_marital, NZStall1Graph$survival_cat3,digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, format= "SAS")
t1[2]
c <- data.frame(t1[2])
View(c)
t1 = CrossTable(NZStall1Graph$my_smoker, NZStall1Graph$survival_cat3,digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, format= "SAS")
t1[2]
d <- data.frame(t1[2])
View(d)
t1 = CrossTable(NZStall1Graph$my_sumstage0.1, NZStall1Graph$survival_cat3,digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, format= "SAS")
t1[2]
e <- data.frame(t1[2])
names(e) <- c('Key', 'Survival', 'Proportion')
View(e)
t1 = CrossTable(NZStall1Graph$my_morph0.1, NZStall1Graph$survival_cat3,digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, format= "SAS")
t1[2]
f <- data.frame(t1[2])
names(f) <- c('Key', 'Survival', 'Proportion')
View(f)
t1 = CrossTable(NZStall1Graph$my_dcSeg1.1, NZStall1Graph$survival_cat3,digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, format= "SAS")
t1[2]
g <- data.frame(t1[2])
View(g)
t1 = CrossTable(NZStall1Graph$Echarlindex0.1, NZStall1Graph$survival_cat3,digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, format= "SAS")
#t1 = CrossTable(NZStall1Graph$E_Ch6, NZStall1Graph$survival_cat3,digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, format= "SAS")
t1[2]
h <- data.frame(t1[2])
names(h) <- c('Key', 'Survival', 'Proportion')
View(h)
t1 = CrossTable(NZStall1Graph$my_admtype0.1, NZStall1Graph$survival_cat3,digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, format= "SAS")
t1[2]
j <- data.frame(t1[2])
names(j) <- c('Key', 'Survival', 'Proportion')
View(j)
t1 = CrossTable(NZStall1Graph$my_treat, NZStall1Graph$survival_cat3,digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, format= "SAS")
t1[2]
k <- data.frame(t1[2])
names(k) <- c('Key', 'Survival', 'Proportion')
View(k)
t1 = CrossTable(NZStall1Graph$my_institut0.1, NZStall1Graph$survival_cat3,digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, format= "SAS")
t1[2]
m <- data.frame(t1[2])
names(m) <- c('Key', 'Survival', 'Proportion')
View(m)
t1 = CrossTable(NZStall1Graph$my_vital_can, NZStall1Graph$survival_cat3,digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, format= "SAS")
t1[2]
n <- data.frame(t1[2])
View(n)
#aefjkm are df to plot
#Look at individual plots
#age
p1 <- ggplot(subset(a, (Survival != ">360d" )), aes(x = as.numeric(Survival), y= Proportion, color=Key), subset(a, Survival=='>360d')) + geom_line()
p1 <-  p1 + ggtitle("a. Age by survival category, row proportions") + theme(plot.title = element_text(size = 12))
p1 <-  p1 + scale_x_discrete(name ="Survival category(days)", 
                             limits=c('0-30', '60','90','120','150','180',
                                      '210','240','270','300','330','360'))
p1 <-  p1  + ylim(0, 0.3)

#ggplot(b, aes(x = as.numeric(prop.col.y), y= prop.col.Freq, color=prop.col.x)) + geom_line()
#ggplot(c, aes(x = as.numeric(prop.col.y), y= prop.col.Freq, color=prop.col.x)) + geom_line()
#ggplot(d, aes(x = as.numeric(prop.col.y), y= prop.col.Freq, color=prop.col.x)) + geom_line()
#stage
p2 <- ggplot(subset(e, (Survival != ">360d")), aes(x = as.numeric(Survival), y= Proportion, color=Key)) + geom_line() 
p2 <-  p2 + ggtitle("c. Stage by survival category, row proportions") + theme(plot.title = element_text(size = 12))
p2 <-  p2 + scale_x_discrete(name ="Survival category(days)", 
                             limits=c('0-30', '60','90','120','150','180',
                                      '210','240','270','300','330','360'))
p2 <-  p2  + ylim(0, 0.3)
#morph
p3 <- ggplot(subset(f, (Survival != ">360d")), aes(x = as.numeric(Survival), y= Proportion, color=Key)) + geom_line() 
p3 <-  p3 + ggtitle("d. Histology by survival category, row proportions")  + theme(plot.title = element_text(size = 12))
p3 <-  p3 + scale_x_discrete(name ="Survival category(days)", 
                             limits=c('0-30', '60','90','120','150','180',
                                      '210','240','270','300','330','360'))
p3 <-  p3  + ylim(0, 0.4)
#ggplot(g, aes(x = as.numeric(prop.col.y), y= prop.col.Freq, color=prop.col.x)) + geom_line()
#ggplot(h, aes(x = as.numeric(prop.col.y), y= prop.col.Freq, color=prop.col.x)) + geom_line()
#admission
p4 <- ggplot(subset(j, (Key=='Planned' & Survival != '>360d'| Key=='Unplanned' & Survival != '>360d' )), 
             aes(x = as.numeric(Survival), y= Proportion, color=Key)) + geom_line() 
p4 <-  p4 + ggtitle( "b. Diagnosis episode admission by survival category, row proportions")  + theme(plot.title = element_text(size = 12))
p4 <-  p4 + scale_x_discrete(name ="Survival category(days)", 
                             limits=c('0-30', '60','90','120','150','180',
                                      '210','240','270','300','330','360'))
p4 <-  p4  + ylim(0, 0.3)
#treat
p5 <- ggplot(subset(k, (Survival != ">360d")), aes(x = as.numeric(Survival), y= Proportion, color=Key)) + geom_line() 
p5 <-  p5 + ggtitle( "e. Treatment by survival category, row proportions")  + theme(plot.title = element_text(size = 12))
p5 <-  p5 + scale_x_discrete(name ="Survival category(days)", 
                             limits=c('0-30', '60','90','120','150','180',
                                      '210','240','270','300','330','360'))
#pod
p6 <- ggplot(subset(m, (Key !='No DC'  &  Key !='Unknown' & Survival != ">360d" )), aes(x = as.numeric(Survival), y= Proportion, color=Key)) + geom_line() 
p6 <-  p6 + ggtitle( "f. Place of death by survival category, row proportions")  + theme(plot.title = element_text(size = 12))
p6 <-  p6 + scale_x_discrete(name ="Survival category(days)" ,
                            limits=c('0-30', '60','90','120','150','180',
                                     '210','240','270','300','330','360' ))
p6 <-  p6  + ylim(0, 0.3)

#cmb
rm(p0)
p0 <- ggplot(subset(h, (Key=='None' & Survival != '>360d'| Key=='Any' & Survival != '>360d' )),aes(x = as.numeric(Survival), y= Proportion, color=Key)) 
#p0 <- ggplot(subset(h, (Key=='0' & Survival != '>360d'| Key=='1' & Survival != '>360d' )),  aes(x = as.numeric(Survival), y= Proportion, color=Key))

p0 <-  p0  + geom_line() + ggtitle("g. Comorbidities by survival category, row proportions")  + theme(plot.title = element_text(size = 12))
p0 <-  p0 + scale_x_discrete(name ="Survival category(days)" ,
                             limits=c('0-30', '60','90','120','150','180',
                                      '210','240','270','300','330','360' ))
p0 <-  p0  + ylim(0, 0.3)
#leave out cmb, p0
multiplot(p1, p2, p5, p4, p3,p6, cols=2)
#LOS
t1 <-  NZStall1Graph[,.(survival_cat3 ,los)]
names(t1) <- c('Survival', 'los')
# Get summary
t1meds <- t1[,list(
  number=(.N),
  lower=quantile(los, .25,  na.rm=TRUE),
  middle=quantile(los, .50, na.rm=TRUE),
  upper=quantile(los, .75,  na.rm=TRUE)),
  by = Survival]
View(t1meds)  
p7 <- ggplot(t1, aes(x = Survival, y= los)) + geom_boxplot(aes(color=Survival), outlier.colour=NA )
p7 <-  p7 + geom_text(data=t1meds, aes(x=Survival, y=middle, label=middle), col='purple', size=3,  vjust=-0.5)
p7 <-  p7 + geom_text(data=t1meds, aes(x=Survival, y=upper, label= paste('n=',number)), col='black', size=3,  vjust=1.5)
p7 <-  p7 + ggtitle("Diagnosis episode length of stay, by survival category")
p7 <-  p7 + theme(legend.position="none") 
p7 <-  p7 +  scale_x_discrete(name ="Survival(days)")
p7 <-  p7 + scale_y_discrete(name ="Length of stay (days)",limits=c(0,5,10,15,20,25,30,35,40))
p7 <-  p7 + theme(plot.title = element_text(hjust = 0.5))
sts <- boxplot.stats(NZStall1Graph$los)$stats
p7 = p7 + coord_cartesian(ylim = c(sts[2]/2,max(sts)*1.05))
  




######################################
#Logistic Regression analysis
NZStall1LR <-  NZStall1[,.(PID,survival_cat2, my_agecat, AGE, my_survival, my_gender, my_marital,
                           my_smoker, my_sumstage0.1, my_morph0.1, my_dcSeg1.1, 
                           Echarlindex0.1, my_admtype0.1, my_discode0.1,my_treat, 
                           my_institut0.1, my_vital_can
                          ),]

#Logistic Regression
NZStall1LR[ ,caco :=0,]
NZStall1LR[survival_cat2=="0-30d", caco :=1,]
NZStall1LR[, table(caco, survival_cat2),]
NZStall1LR[, table( my_survival, caco),]

View(NZStall1LR)

###############################
#Case control analyis 

#Do basic analysis for caco data
#table1
t1 = CrossTable(NZStall1LR$my_agecat,NZStall1LR$caco, digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, format= "SAS")
write.table(t1[1:3], file = "C:\\temp\\f1.csv", sep = ",", col.names = NA, qmethod = "double")


t1 = CrossTable(NZStall1LR$my_gender,NZStall1LR$caco, digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, format= "SAS")
write.table(t1[1:3], file = "C:\\temp\\f1.csv", sep = ",", col.names = NA, qmethod = "double")


t1 = CrossTable(NZStall1LR$my_marital,NZStall1LR$caco, digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, format= "SAS")
write.table(t1[1:3], file = "C:\\temp\\f1.csv", sep = ",", col.names = NA, qmethod = "double")


t1 = CrossTable(NZStall1LR$my_smoker,NZStall1LR$caco, digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, format= "SAS")
write.table(t1[1:3], file = "C:\\temp\\f1.csv", sep = ",", col.names = NA, qmethod = "double")


levels(NZStall1LR$my_admtype0.1)
table(NZStall1LR$my_admtype, NZStall1LR$my_admtype0.1)
NZStall1LR$my_admtype0.1 <- factor(NZStall1LR$my_admtype0.1, levels =
                                   c(  "Planned", "Unplanned", "Not relevant",  "No HIPE"))

t1 = CrossTable(NZStall1LR$my_admtype0.1,NZStall1LR$caco, digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, format= "SAS")
write.table(t1[1:3], file = "C:\\temp\\f1.csv", sep = ",", col.names = NA, qmethod = "double")



levels(NZStall1LR$my_sumstage0.1)
table(NZStall1LR$my_sumstage0.1)
t1 = CrossTable(NZStall1LR$my_sumstage0.1,NZStall1LR$caco, digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, format= "SAS")
write.table(t1[1:3], file = "C:\\temp\\f1.csv", sep = ",", col.names = NA, qmethod = "double")



levels(NZStall1LR$my_morph)
t1 = CrossTable(NZStall1LR$my_morph0.1,NZStall1LR$caco, digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, format= "SAS")
write.table(t1[1:3], file = "C:\\temp\\f1.csv", sep = ",", col.names = NA, qmethod = "double")


levels(NZStall1$Echarlindex0.1)
t1 = CrossTable(NZStall1LR$Echarlindex0.1,NZStall1LR$caco, digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, format= "SAS")
write.table(t1[1:3], file = "C:\\temp\\f1.csv", sep = ",", col.names = NA, qmethod = "double")

levels(NZStall1LR$my_discode0.1)
table(NZStall1LR$my_discode0.1)
NZStall1LR$my_discode0.2 <-  NZStall1LR$my_discode0.1
levels(NZStall1LR$my_discode0.2)  <-  c( "Other", "Home", "Other", "Other","Other", "Died",  "Other",  "Other", "Not relevant",  "No HIPE")
table(NZStall1LR$my_discode0.1,NZStall1LR$my_discode0.2)
#Reorder
NZStall1LR$my_discode0.2 <- factor(NZStall1LR$my_discode0.2, levels = c("Died", "Home", "Other", "Not relevant",  "No HIPE"))
#Regroup to twe variables
NZStall1LR$my_discode0.3 <-  NZStall1LR$my_discode0.2
levels(NZStall1LR$my_discode0.3) <- c("Died", "Other", "Other", "Not relevant",  "No HIPE")
table(NZStall1LR$my_discode0.2,NZStall1LR$my_discode0.3)

table(NZStall1LR$my_discode0.1, NZStall1LR$my_discode0.2)
t1 = CrossTable(NZStall1LR$my_discode0.3,NZStall1LR$caco, digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, format= "SAS")
write.table(t1[1:3], file = "C:\\temp\\f1.csv", sep = ",", col.names = NA, qmethod = "double")



levels(NZStall1LR$my_treat)
NZStall1LR$my_treat0.1 <-  NZStall1LR$my_treat
levels(NZStall1LR$my_treat0.1)  <-  c( "None", "Any","Any","Any","Any","Any")
table(NZStall1LR$my_treat,NZStall1LR$my_treat0.1)
table(NZStall1LR$my_treat0.1)
t1 = CrossTable(NZStall1LR$my_treat0.1,NZStall1LR$caco, digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, format= "SAS")
write.table(t1[1:3], file = "C:\\temp\\f1.csv", sep = ",", col.names = NA, qmethod = "double")


table(NZStall1LR$my_vital_can)
t1 = CrossTable(NZStall1LR$my_vital_can,NZStall1LR$caco, digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, format= "SAS")
write.table(t1[1:3], file = "C:\\temp\\f1.csv", sep = ",", col.names = NA, qmethod = "double")


table(NZStall1LR$my_institut0.1)
t1 = CrossTable(NZStall1LR$my_institut0.1,NZStall1LR$caco, digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, format= "SAS")
write.table(t1[1:3], file = "C:\\temp\\f1.csv", sep = ",", col.names = NA, qmethod = "double")


#get Cramer`s V statistic for categorical

t1  <- table(NZStall1LR$my_gender,NZStall1LR$caco)
assocstats(t1)


t1  <- table(NZStall1LR$my_marital,NZStall1LR$caco) 
assocstats(t1)

t1  <- table(NZStall1LR$my_smoker,NZStall1LR$caco) 
assocstats(t1)

t1  <- table(NZStall1LR$my_dcSeg1.1,NZStall1LR$caco) 
assocstats(t1)

t1  <- table(NZStall1LR$my_admtype0.1,NZStall1LR$caco) 
assocstats(t1)

t1  <- table(NZStall1LR$my_morph0.1,NZStall1LR$caco) 
assocstats(t1)

t1  <- table(NZStall1LR$my_discode0.3,NZStall1LR$caco) 
assocstats(t1)


t1  <- table(NZStall1LR$my_treat0.1,NZStall1LR$caco) 
assocstats(t1)

t1  <- table(NZStall1LR$my_institut0.1,NZStall1LR$caco) 
assocstats(t1)

t1  <- table(NZStall1LR$my_vital_can,NZStall1LR$caco) 
assocstats(t1)

t1  <- table(NZStall1LR$Echarlindex0.1,NZStall1LR$caco) 
assocstats(t1)

#Find Somers'' d statistic for ordinal variables agecat2, sumstage1
#rows are IV and columns are caco, the depenendent variable
t1  <- table(NZStall1LR$my_agecat,NZStall1LR$caco) 
ord.somers.d(t1)
assocstats(t1)

t1  <- table(NZStall1LR$my_sumstage0.1,NZStall1LR$caco) 
ord.somers.d(t1)
assocstats(t1)

#t1  <- table(NZStall1LR$Echarlindex0.1,NZStall1LR$caco) 
#ord.somers.d(t1)




################################################


#Do column graph for treatmetn and place of death
levels(NZStall1Graph$my_treat)
NZStall1Graph$my_treat0.1 <-  NZStall1Graph$my_treat
levels(NZStall1Graph$my_treat0.1)  <-  c( "None", "Any","Any","Any","Any","Any")
table(NZStall1Graph$my_treat,NZStall1Graph$my_treat0.1)

#treat graph
t1 = CrossTable(NZStall1Graph$my_treat0.1, NZStall1Graph$survival_cat3,digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, format= "SAS")
t1[2]
k <- data.frame(t1[3])
names(k) <- c('Key', 'Survival', 'Proportion')
View(k)

#treat
p5 <- ggplot(subset(k, (Survival != ">360d")), aes(x = as.numeric(Survival), y= Proportion, color=Key)) + geom_line() 
p5 <-  p5 + ggtitle( "e. Treatment by survival category, column proportions")  + theme(plot.title = element_text(size = 12))
p5 <-  p5 + scale_x_discrete(name ="Survival category(days)", 
                             limits=c('0-30', '60','90','120','150','180',
                                      '210','240','270','300','330','360'))

#pod graph
t1 = CrossTable(NZStall1Graph$my_institut0.1, NZStall1Graph$survival_cat3,digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, format= "SAS")
t1[2]
m <- data.frame(t1[3])
names(m) <- c('Key', 'Survival', 'Proportion')
View(m)

#pod
p6 <- ggplot(subset(m, (Key !='No DC'  &  Key !='Unknown' & Survival != ">360d" )), aes(x = as.numeric(Survival), y= Proportion, color=Key)) + geom_line() 
p6 <-  p6 + ggtitle( "f. Place of death by survival category, column proportions")  + theme(plot.title = element_text(size = 12))
p6 <-  p6 + scale_x_discrete(name ="Survival category(days)" ,
                             limits=c('0-30', '60','90','120','150','180',
                                      '210','240','270','300','330','360' ))


#age graph
t1= CrossTable(NZStall1Graph$my_agecat, NZStall1Graph$survival_cat3,digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, format= "SAS")
t1[2]
a <-  data.frame(t1[3])
names(a) <- c('Key', 'Survival', 'Proportion')
View(a)

#age
p1 <- ggplot(subset(a, (Survival != ">360d" )), aes(x = as.numeric(Survival), y= Proportion, color=Key), subset(a, Survival=='>360d')) + geom_line()
p1 <-  p1 + ggtitle("a. Age by survival category, column proportions") + theme(plot.title = element_text(size = 12))
p1 <-  p1 + scale_x_discrete(name ="Survival category(days)", 
                             limits=c('0-30', '60','90','120','150','180',
                                      '210','240','270','300','330','360'))




#Logistic regression

View(NZStall1LR)
View(NZStall1)
#Reset NO DC and No Hipe to NA
#Revert to character/numeric
NZStall1LR$my_dcSeg1.1 <- as.character(NZStall1LR$my_dcSeg1.1)
NZStall1LR$my_institut0.1 <- as.character(NZStall1LR$my_institut0.1)
NZStall1LR$my_admtype0.1 <- as.character(NZStall1LR$my_admtype0.1)
NZStall1LR$Echarlindex0.1 <- as.character(NZStall1LR$Echarlindex0.1)
View(NZStall1LR)

#Change mising values back to NA
NZStall1LR[my_dcSeg1.1 == 'No DC', my_dcSeg1.1 := NA,]
NZStall1LR[my_institut0.1 == 'No DC', my_institut0.1 := NA,]
NZStall1LR[my_admtype0.1 == 'No HIPE'|my_admtype0.1 == 'Not relevant', my_admtype0.1 := NA,]
NZStall1LR[Echarlindex0.1 == 'No HIPE' | Echarlindex0.1 == 'Not relevant', Echarlindex0.1 := NA,]
#Debug/Check
NZStall1LR[my_dcSeg1.1 == 'NA', my_dcSeg1.1 := NA,]
NZStall1LR[my_institut0.1 == 'NA', my_institut0.1 := NA,]
NZStall1LR[my_admtype0.1 == 'NA', my_admtype0.1 := NA,]
NZStall1LR[Echarlindex0.1 == 'NA', Echarlindex0.1 := NA,]


#Revert back to factors
NZStall1LR$my_dcSeg1.1 <- as.factor(NZStall1LR$my_dcSeg1.1)
NZStall1LR$my_institut0.1 <- as.factor(NZStall1LR$my_institut0.1)
NZStall1LR$my_admtype0.1 <- as.factor(NZStall1LR$my_admtype0.1)
NZStall1LR$Echarlindex0.1 <- as.factor(NZStall1LR$Echarlindex0.1)
#Reorder levels here 
NZStall1LR$my_admtype0.1 <- factor(NZStall1LR$my_admtype0.1, levels = c(  "Planned", "Unplanned"))
NZStall1LR$Echarlindex0.1 <- factor(NZStall1LR$Echarlindex0.1, levels = c(  "None", "Any"))


#Check
table(NZStall1LR$my_dcSeg1.1)
table(NZStall1LR$my_institut0.1)
table(NZStall1LR$my_admtype0.1)
table(NZStall1LR$Echarlindex0.1)


#Everything
mylogit  <- glm(caco~ my_agecat + my_marital + my_gender +  my_smoker + my_dcSeg1.1
                + my_sumstage0.1 + my_morph0.1 + my_admtype0.1 + Echarlindex0.1  + my_treat 
                + my_institut0.1 + my_vital_can
                , data=NZStall1LR, family="binomial"   )

#Build model
#1 available at admission
mylogit  <- glm(caco~ my_agecat + my_marital + my_gender  +  my_smoker + my_admtype0.1 + Echarlindex0.1
                , data=NZStall1LR, family="binomial")


summary(mylogit)
##exp(coef(mylogit))
m1 <- round(exp(cbind(OR = coef(mylogit), confint(mylogit))),2)

#2 available after diagnosis admission investigation
mylogit  <- glm(caco~ my_agecat + my_marital + my_gender  +  my_smoker  + 
                  + my_sumstage0.1 + my_morph0.1 + my_admtype0.1 + Echarlindex0.1  
                , data=NZStall1LR, family="binomial"   )
summary(mylogit)
##exp(coef(mylogit))
m2 <-  round(exp(cbind(OR = coef(mylogit), confint(mylogit))),2)

#3 based on treatment decisions
mylogit  <- glm(caco~ my_agecat + my_marital + my_gender + 
                  + my_sumstage0.1  + my_morph0.1 + my_admtype0.1 + Echarlindex0.1 + my_treat 
                , data=NZStall1LR, family="binomial"   )
summary(mylogit)
##exp(coef(mylogit))
m3 <-  round( exp(cbind(OR = coef(mylogit), confint(mylogit))),2)







#LRT
install.packages("lrtest")
require('lrtest')
#HO;reduced model is true
#H1: reduced model is not true
#m1  everything
#m2 remove variable to be tested
#lrtest(m1)
#Hosmer-Lemeshow GOF
install.packages("ResourceSelection")
require('ResourceSelection')
#H0: data fits the model 
#H1 data does not fit the model well
hl <- hoslem.test(mylogit$y, fitted(mylogit), g=10)
hl

#Misc
NZStall1[ ,caco :=0,]
NZStall1[survival_cat2=="0-30d", caco :=1,]
NZStall1[, table(caco, survival_cat2),]
NZStall1[, table( my_survival, caco),]

CrossTable(NZStall1$my_admtype, NZStall1$caco, digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, format= "SAS")
t1  <- table( NZStall1$my_admtype,NZStall1$caco) 
assocstats(t1)

NZStall1$Echarlindex0.2 <- factor(NZStall1$Echarlindex)
table(NZStall1$Echarlindex, NZStall1$Echarlindex0.2)
levels(NZStall1$Echarlindex0.2)
levels(NZStall1$Echarlindex0.2)  <- c( "None", "Any","Any")

CrossTable(NZStall1$Echarlindex0.2,NZStall1$caco, digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, format= "SAS")
t1  <- table( NZStall1$Echarlindex0.2,NZStall1$caco) 
assocstats(t1)



NZStall1$my_discode0.2 <- factor(NZStall1$my_discode)
table(NZStall1$my_discode0.2, NZStall1$my_discode)
levels(NZStall1$my_discode0.2)
levels(NZStall1$my_discode0.2)  <- c( "Other","Other", "Other", "Other", "Other", "Death", "Other", "Other")
CrossTable(NZStall1$my_discode0.2,NZStall1$caco, digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c = TRUE, prop.t=FALSE,prop.chisq=FALSE,chisq = TRUE, format= "SAS")
t1  <- table(NZStall1$my_discode0.2,NZStall1$caco) 
assocstats(t1)

#No HIPE
table(NZStall1$he,NZStall1$my_institut )

