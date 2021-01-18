#library(RODBC)
require(RODBC)
require(data.table)
require(gmodels)
require(stringdist)
require(anytime)
#require(icd)
#require(gdata)

##Get HIPE data
channel <- odbcConnectAccess2007("T:\\myHIPE.accdb")
Hipe1517 <- sqlFetch(channel, "Hipe")
close(channel)
dtHipe1517 <- data.table(Hipe1517)
dtHipe1517[, c("NAME","Initial", "DOB"):=NULL]  # remove three columns
rm(Hipe1517)
dtHipe1517 <- unique(dtHipe1517)

# Save data to a file
saveRDS(dtHipe1517, file = "V:\\R_Projects\\RHomeDir\\RS2\\_data\\dtHipe1517.rds")
# Restore the data.table
rm(t1)
t1 <- readRDS(file = "V:\\R_Projects\\RHomeDir\\RS2\\_data\\dtHipe1517_1.rds")
View(t1)

######################################################################################
#Get a list of unique hospital_ids and generate dumhid for hospital and cancer centres
dtRefHosp <- unique(t1[,c('HOSPITAL_NAME', 'HOSPITAL')])
dtRefHosp[, DUMHID := .I]
dtRefHosp[, CC := 0]
dtRefHosp[HOSPITAL==37,CC:=1]
dtRefHosp[HOSPITAL==5, CC:=2]
dtRefHosp[HOSPITAL==7, CC:=3]
dtRefHosp[HOSPITAL==21, CC:=4]
dtRefHosp[HOSPITAL==235, CC:=5]
dtRefHosp[HOSPITAL==100, CC:=6]
dtRefHosp[HOSPITAL==404, CC:=7]
dtRefHosp[HOSPITAL==303, CC:=8]
#cancer centres HIPE code
# Beaumont Hospital 037	 		
# Mater University Hospital  005		
# St Vincent’s University Hospital 007	
# St James’s Hospital 021	 		
# Cork University Hospital 		235
# Waterford Regional Hospital 100   	
# Galway University Hospital 404
# University Hospital Limerick 303	
View(dtRefHosp)
saveRDS(dtRefHosp, file = "V:\\R_Projects\\RHomeDir\\RS2\\_data\\Reference\\dtRefHosp.rds")
dtRefHosp <- data.table(readRDS(file = "V:\\R_Projects\\RHomeDir\\RS2\\_data\\Reference\\dtRefHosp.rds"))
write.table(dtRefHosp, file = "C:\\temp\\f1.csv", sep = ",", col.names = NA, qmethod = "double")

#ForMergeAnonHosp
fmAH <- dtRefHosp[, .(HOSPITAL, DUMHID, CC)] 
View (fmAH)
###########################################################################################

##Separate out by year of discharge
dtHipe15 <- dtHipe1517[year(DISCHARGE)==2015,]
dtHipe16 <- dtHipe1517[year(DISCHARGE)==2016,]
dtHipe17 <- dtHipe1517[year(DISCHARGE)==2017,]

#2015 

setorder(dtHipe15, CLEAN_MRN, ADMISSION)
dtHipe15[, RID := .I]
dtHipe15[, MID := .GRP, by = .(CLEAN_MRN,HOSPITAL)] #coomon id by mrn and hospital
setcolorder(dtHipe15, c(87,88, 1:86))
View(dtHipe15)
#as.numeric(any(grepl('Z515 Palliative care', dtHipe15$C3DIA_LONG)))
#cnames <- names(dtHipe15[,35:54])
dtHipe15$SPC <- as.numeric(rowSums(dtHipe15[,35:52] == 'Z515 Palliative care',na.rm=T) > 0)
#Discharged to, 11= Hospice (not in HIPE Hospital Listing) omits 3 Hipe hospice 
table(dtHipe15$DISC_CODE)
dtHipe15$toHospice <- as.numeric(dtHipe15$DISC_CODE == 11) 
View(dtHipe15)

#Anonymise
dtHipe15 <-  merge(fmAH, dtHipe15, by.x ='HOSPITAL', by.y='HOSPITAL', all.y = TRUE) 
View(dtHipe15)
dtHipe15[, c("MRN","CLEAN_MRN", "HOSPITAL", "NCR_HOS", "HOSPITAL_NAME","NCR_HOSP", "migrated_regno"):=NULL]  # remove these columns
dtHipe15[, c("Day","MOB", "YOB", "COMMENT", "ID","HIDDEN", "admward", "disward", "Match MRN/Hosp", "UPDATED ON", "matched yet"):=NULL]  # remove these columns
View(dtHipe15)



#Counts
#episodes by hospital with spc 
table(dtHipe15$HOSPITAL_NAME,dtHipe15$SPC)
#patients by hospital with spc
dtHipe15[SPC==1, .(count = uniqueN(MID)), by = HOSPITAL]
#Patients discharged to hospice with no SPC
table(dtHipe15$toHospice,dtHipe15$SPC)
t1 <- dtHipe15[SPC==0 & toHospice==1,,]
t2 <- t1[, .(count = uniqueN(MID)), by = HOSPITAL_NAME]
View(t2)
t1 <- dtHipe15[SPC==1, .(DIA)]
#count frequencies as character not factor levels
t2 <- table(as.character(t1$DIA))
#t2 is  a matrix so convert to data table
t2 <- data.table(as.data.frame( as.table(t2)))
names(t2)[1]<-"Principal Diagnosis"
t2[, percen := sum(Freq),]
t2[, percen := round((Freq/percen)*100,digits =2), ]
View(t2)  
write.table(t2, file = "f1.csv", sep = ":", quote = FALSE, row.names = F)
# people discharged to hospice by hospital
t1 <- dtHipe15[toHospice==1, .(count = uniqueN(MID)), by = HOSPITAL_NAME]
View(t1)

#2016

setorder(dtHipe16, CLEAN_MRN, ADMISSION)
dtHipe16[, RID := .I]
dtHipe16[, MID := .GRP, by = .(CLEAN_MRN,HOSPITAL)]
setcolorder(dtHipe16, c(87,88, 1:86))
View(dtHipe16)
#as.numeric(any(grepl('Z515 Palliative care', dtHipe16$C3DIA_LONG)))
#cnames <- names(dtHipe16[,35:54])
dtHipe16$SPC <- as.numeric(rowSums(dtHipe16[,35:54] == 'Z515 Palliative care',na.rm=T) > 0)
#Discharged to, 11= Hospice (not in HIPE Hospital Listing) omits 3 Hipe hospice - 027 Blackrock/027 OLH/028 OLH/
table(dtHipe16$DISC_CODE)
dtHipe16$toHospice <- as.numeric(dtHipe16$DISC_CODE == 11 ) 
View(dtHipe16)
#Anonymise
dtHipe16 <-  merge(fmAH, dtHipe16, by.x ='HOSPITAL', by.y='HOSPITAL', all.y = TRUE) 
View(dtHipe16)
dtHipe16[, c("MRN","CLEAN_MRN", "HOSPITAL", "NCR_HOS", "HOSPITAL_NAME","NCR_HOSP", "migrated_regno"):=NULL]  # remove these columns
dtHipe16[, c("Day","MOB", "YOB", "COMMENT", "ID","HIDDEN", "admward", "disward", "Match MRN/Hosp", "UPDATED ON", "matched yet"):=NULL]  # remove these columns
View(dtHipe16)





#Counts
#episodes by hospital with spc 
table(dtHipe16$HOSPITAL_NAME,dtHipe16$SPC)
#patients by hospital with spc
dtHipe16[SPC==1, .(count = uniqueN(MID)), by = HOSPITAL_NAME]
#Patients discharged to hospice with no SPC
table(dtHipe16$toHospice,dtHipe16$SPC)
t1 <- dtHipe16[SPC==0 & toHospice==1,,]
t2 <- t1[, .(count = uniqueN(MID)), by = HOSPITAL_NAME]
View(t2)
t1 <- dtHipe16[SPC==1, .(DIA)]
#count frequencies as character not factor levels
t2 <- table(as.character(t1$DIA))
#t2 is  a matrix so convert to data table
t2 <- data.table(as.data.frame( as.table(t2)))
names(t2)[1]<-"Principal Diagnosis"
t2[, percen := sum(Freq),]
t2[, percen := round((Freq/percen)*100,digits =2), ]
View(t2)  
write.table(t2, file = "f1.csv", sep = ":", quote = FALSE, row.names = F)
# people discharged to hospice by hospital
t1 <- dtHipe16[toHospice==1, .(count = uniqueN(MID)), by = HOSPITAL_NAME]
View(t1)
rm(t1,t2)
#2016 

#cancer centres HIPE code
# Beaumont Hospital 037	 		
# Mater University Hospital  005		
# St Vincent’s University Hospital 007	
# St James’s Hospital 021	 		
# Cork University Hospital 		235
# Waterford Regional Hospital 100   	
# Galway University Hospital 404
# University Hospital Limerick 303	
dtHipe16x8 <- dtHipe16[HOSPITAL == 37|HOSPITAL == 5|HOSPITAL == 7|HOSPITAL == 21|HOSPITAL == 235|HOSPITAL == 100|HOSPITAL == 404|HOSPITAL == 303,, ]
#drop.levels(dtHipe16x8$HOSPITAL_NAME)
dtHipe16x8$HOSPITAL_NAME <- factor(dtHipe16x8$HOSPITAL_NAME)
levels(dtHipe16x8$HOSPITAL_NAME)
View(dtHipe16x8) #n=188008
dtHipe16x8C <-  dtHipe16x8[SPC==1 | toHospice==1,,]
View(dtHipe16x8C) #n=4250
dtHipe16x8C[,HIPE := matched == 'Y' | matched == 'y',]
table(dtHipe16x8C$HIPE)
table(dtHipe16x8C$HOSPITAL_NAME, dtHipe16x8C$HIPE)
S2C <-  dtHipe16x8C[Patient_id != 'NA',3]
View(S2C) #n=3410
S2C <- unique(S2C)

###################################################################################
###################################################################################

#Get DC and DEPS data for 2015, 2016, 2017

#open ODBC connection
myconn <-odbcConnect('toPPVMLiverestore')
#sqlSave(myconn, S2C)
DEPSData151617 <- sqlQuery(myconn, "select distinct b.id pid, place_of_death_county_id, address_type, place_of_death1, place_of_death_type_source, convert(date, a.date_of_death) dod
from ed_deps a, patient b where a.ppsn = b.pps_number and a.date_of_death = b.date_of_death
                           and datepart(year, a.date_of_death) in (2015,2016,2017)")
View(DEPSData151617)

#Anonymise
DEPSData151617_ds <- merge(DEPSData151617, t3, by.x = 'pid', by.y ='PAT_ID', all.x = TRUE) #all DEPS
setcolorder(DEPSData151617_ds, c(1,8,2:7))
DEPSData151617_ds <-  DEPSData151617_ds[, c(-1)]
rm(DEPSData151617)




################################################
#19/10/2017 - major update to DEPS matching so take it out again

DEPSData151617_t2 <- sqlQuery(myconn, "select distinct b.id pid, place_of_death_county_id, address_type, place_of_death1, place_of_death_type_source, convert(date, a.date_of_death) dod
from CRS2DB.dbo.ed_deps a, CRS2DB.dbo.patient b where a.ppsn = b.pps_number and a.date_of_death = b.date_of_death
                           and datepart(year, a.date_of_death) in (2015,2016,2017)")
View(DEPSData151617_t2)

#Anonymise
DEPSData151617_dst2 <- merge(DEPSData151617_t2, t3, by.x = 'pid', by.y ='PAT_ID', all.x = TRUE) #all DEPS
setcolorder(DEPSData151617_dst2, c(1,7,2:6))
DEPSData151617_dst2 <-  DEPSData151617_dst2[, c(-1)]
rm(DEPSData151617_t2)
DEPSData151617_dst2 <-  data.table(DEPSData151617_dst2)


#################################################


DCData151617 <- sqlQuery(myconn, "select b.id, a.institution_id,   convert(date, a.date_of_death) dc_dod, occupation, cancer
from ed_death_cert a, patient b where a.matched_patient_id = b.id
                             and datepart(year, b.date_of_death) in (2015,2016,2017) order by b.date_of_death")
View(DCData151617)

#Anonymise
DCData151617_ds <- merge(DCData151617, t3, by.x = 'id', by.y ='PAT_ID', all.x = TRUE) #all DC's
setcolorder(DCData151617_ds, c(1,6,2:5))
DCData151617_ds <-  DCData151617_ds[, c(-1)]
rm(DCData151617)




## Review, tidy and save DEPS data to a file
##Redo for take2

saveRDS(DEPSData151617_dst2, file = "V:\\R_Projects\\RHomeDir\\RS2\\_data\\DEPSData151617_dst2.rds")
DEPSData151617_dst2 <- data.table(readRDS(file = "V:\\R_Projects\\RHomeDir\\RS2\\_data\\DEPSData151617_dst2.rds"))
View(DEPSData151617_dst2)

#Find duplicate entries in DEPS data
DEPSData151617_dst2[, dupP := as.numeric(.N >1), by = DUM_REG] 
table(DEPSData151617_dst2$dupP) #819 duplicates 
DEPSData151617_dst2[is.na(DUM_REG), table(dupP)] #627 duplicates have DUM_REG NA
DEPSData151617_dst2[!is.na(DUM_REG), table(dupP)] #192 DUM_REG are duplicates, 38771 are not
uniqueN(DEPSData151617_dst2$DUM_REG) #38868 unique DUM_REGS + 1 NA
###########################################################################################################
#Review and tidy 192 DUM_REGS with duplicates, that is keep 96 and remove 96
DEPSData151617_dst2[!is.na(DUM_REG), table(dupP)] #192 DUM_REG are duplicates, 38771 are not
DEPSData151617_dst2[, tempRID := .I]
#Take first instance of duplicate and remove NAs
DEPSData151617_dst2 <- DEPSData151617_dst2[DEPSData151617_dst2[,.I[tempRID == min(tempRID)], by = 'DUM_REG']$V1] #38868 ( -96 duplicates and 626 NA's')
DEPSData151617_dst2$tempRID <- NULL
#now DEPSData151617_dst2 has 38686 rows


###Categorise my institution##################################################

DEPSData151617_dst2[,my_instit := 'Z']
DEPSData151617_dst2[my_instit =='Z'& place_of_death1 %like% 'NH', my_instit:= 'N']
DEPSData151617_dst2[my_instit =='Z'& place_of_death1 %like% ' NU', my_instit:= 'N']
DEPSData151617_dst2[my_instit =='Z'& place_of_death1 %like% 'NURSING HOME', my_instit:= 'N']
DEPSData151617_dst2[my_instit =='Z'& place_of_death1 %like% ' CARE CENTRE', my_instit:= 'N']
DEPSData151617_dst2[my_instit =='Z'& place_of_death1 %like% ' CARE HOME', my_instit:= 'N']
DEPSData151617_dst2[my_instit =='Z'& place_of_death1 %like% ' RESIDENTIAL CARE', my_instit:= 'N']
DEPSData151617_dst2[my_instit =='Z'& place_of_death1 %like% ' RESIDENTIAL', my_instit:= 'N']
DEPSData151617_dst2[my_instit =='Z'& place_of_death1 %like% ' RESIDENCE', my_instit:= 'N']
DEPSData151617_dst2[my_instit =='Z'& place_of_death1 %like% 'TLC ', my_instit:= 'N']
DEPSData151617_dst2[my_instit =='Z'& place_of_death1 %like% 'THE MOYNE', my_instit:= 'N']
DEPSData151617_dst2[my_instit =='Z'& place_of_death1 %like%  'MT CARMEL', my_instit:= 'N']
DEPSData151617_dst2[my_instit =='Z'& place_of_death1 %like% 'TRALEE COMMUNITY NU', my_instit:= 'N']
DEPSData151617_dst2[my_instit =='Z'& place_of_death1 %like% ' HSE', my_instit:= 'N']
DEPSData151617_dst2[my_instit =='Z'& place_of_death1 %like% 'YOUGHAL AND DISTRICT', my_instit:= 'N']
DEPSData151617_dst2[my_instit =='Z'& place_of_death1 %like% 'YOUGHAL & DISTRICT', my_instit:= 'N']
DEPSData151617_dst2[my_instit =='Z'& place_of_death1 %like% ' HOSPICE', my_instit:= 'S']
DEPSData151617_dst2[my_instit =='Z'& place_of_death1 %like% 'HOSPICE', my_instit:= 'S']
DEPSData151617_dst2[place_of_death1 %like% 'MILFORD CARE', my_instit:= 'S'] #override institution designation for Milford
DEPSData151617_dst2[my_instit =='Z'& place_of_death1 %like% 'ST. PATRICK\'S UNIVERSITY HOSPITAL/MARYMOUNT', my_instit:= 'S']
DEPSData151617_dst2[my_instit =='Z'& place_of_death1 %like% 'ST. PATRICK\'S UNIVERSITY HOSPITAL/MARYMOUNT UNI', my_instit:= 'S']
DEPSData151617_dst2[my_instit =='Z'& place_of_death1 %like% 'THE N W HOSPICE', my_instit:= 'S']
DEPSData151617_dst2[my_instit =='Z'& place_of_death1 %like% 'THE GALWAY HOSPICE', my_instit:= 'S']
DEPSData151617_dst2[my_instit =='Z'& place_of_death1 %like% 'THE GALWAY HOSPICE FOUNDATION', my_instit:= 'S']
DEPSData151617_dst2[my_instit =='Z'& place_of_death1 %like% 'DONEGAL HOSPICE', my_instit:= 'S']
DEPSData151617_dst2[my_instit =='Z'& place_of_death1 %like% ' HOSP', my_instit:= 'O']
DEPSData151617_dst2[my_instit =='Z'& place_of_death1 %like% 'HOSP', my_instit:= 'O']
DEPSData151617_dst2[my_instit =='Z'& place_of_death1 %like% 'ACCIDENT AND EMERGENCY DEPT' , my_instit:= 'O']
DEPSData151617_dst2[my_instit =='Z'& place_of_death1 %like% 'EMERGENCY' , my_instit:= 'O']
DEPSData151617_dst2[my_instit =='Z'& place_of_death1 %like% 'WARD', my_instit:= 'O']
DEPSData151617_dst2[my_instit =='Z'& place_of_death1 %like% 'CLINIC', my_instit:= 'O']
DEPSData151617_dst2[my_instit =='Z'& place_of_death1 %like% 'UNIT', my_instit:= 'O']
DEPSData151617_dst2[my_instit =='Z'& place_of_death_type_source == 'HOSPITAL', my_instit:= 'O']
DEPSData151617_dst2[my_instit =='Z' & place_of_death_type_source == 'RESIDENTIAL', my_instit:= 'H']
View(DEPSData151617_dst2)

############################################################################################

## Review, tidy and save DC data to a file
saveRDS(DCData151617_ds, file = "V:\\R_Projects\\RHomeDir\\RS2\\_data\\DCData151617_ds.rds")
DCData151617_ds <- data.table(readRDS(file = "V:\\R_Projects\\RHomeDir\\RS2\\_data\\DCData151617_ds.rds"))
View(DCData151617_ds)
#Find duplicate entries in DC data
DCData151617_ds[, dupP := as.numeric(.N >1), by = DUM_REG]
table(DCData151617_ds$dupP) #349 duplicates i.e. all cases with no DUM_REG (NA)
uniqueN(DCData151617_ds$DUM_REG) #19930 unique DUM_REGS + 1 NA


#######################################################################################
##REDONE on 02/10/2018 so rowid 's do not map to any other rowid columns in this project
channel <- odbcConnectAccess2007("T:\\myHIPE.accdb")
Hipe1517 <- sqlFetch(channel, "Hipe")
close(channel)
View(Hipe1517)
# dtHipe1517_1 <- data.table(Hipe1517) #938631
# dtHipe1517_1 <- unique(dtHipe1517_1) #938630
# View(dtHipe1517_1)
# setorder(dtHipe1517_1, CLEAN_MRN, ADMISSION)
# dtHipe1517_1[, row_id := .I]
# dtHipe1517_1[, row_id2 := .GRP, by = .(CLEAN_MRN,HOSPITAL)]
# setcolorder(dtHipe1517_1, c(90,91, 1:89))
# dtHipe1517_1[, yearD := year(DISCHARGE)]
# dtHipe1517_1 <- dtHipe1517_1[,c(1,2,15,17,38:58,92)]
# # Save data to a file
# saveRDS(dtHipe1517_1, file = "V:\\R_Projects\\RHomeDir\\RS2\\_data\\dtHipe1517_1.rds")
# View(dtHipe1517_1)
##REDONE on 02/10/2018 so rowid 's do not map to any other rowid columns in this project
#########################################################################################


#########################################################################################################################
#Anonymise
#########################################################################################################################
dtAnHipe <- dtHipe1517_2[,c(1,2,5,6,15,16,18,24,25,27,29,31,34,36,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,81)]
View(dtAnHipe)
#Anonymise dates
dtAnHipe[,yearD:= year(DISCHARGE)]
dtAnHipe[,h1off:= as.numeric(as.Date(as.character(ADMISSION))) + 113 ] #NB for POSIXct to get correct date
dtAnHipe[,h2off:= as.numeric(as.Date(as.character(DISCHARGE))) + 113 ] #NB for POSIXct to get correct date
dtAnHipe[,h3off:= as.numeric(as.Date(as.character(Pd1))) + 113 ] #NB for POSIXct to get correct date
# as.Date(16910, origin = '1970-01-01')
# as.Date(17023, origin = '1970-01-01')

#Truncate DIAG/PROC
cnames <- names(dtAnHipe[,c(15:35)])
cnames
for (i in 1:21){dtAnHipe[, paste0("D", i) := gsub( " .*$","", get(cnames[i])), by = row_id] }
View(dtAnHipe)
#Anonymise hospital
dtAnHipe <-  merge(dtAnHipe, dtRefHosp, by.x = 'HOSPITAL', by.y ='HOSPITAL')
View(dtAnHipe)
#Take anonmyised data only
dtAnHipe <- dtAnHipe[,c(2:5, 8:14, 37:61, 63:64)]
View(dtAnHipe)
#Remove mypid
dtAnHipe <-  merge(dtAnHipe, t2, by.x = 'mypid', by.y ='PAT_ID', all.x = TRUE)
View(dtAnHipe) #673068
setcolorder(dtAnHipe, c(1,39,2:38))
View(dtAnHipe) #673068
dtAnHipe <- dtAnHipe[, c(-1)] #anonymise 
saveRDS(dtAnHipe, file = "V:\\R_Projects\\RHomeDir\\RS2\\_data\\dtAnHipe.rds")


#####################################################################################################
#Charlson score
###################################################################################################
dtAnHipecmb <- dtAnHipe[,c(1,2,3,16:35)]

##Add column for each cmb to be checked and initalise to 0
system.time(for (i in 1:17) {dtAnHipecmb[ , paste0("E_Ch",i) := 0]})
View(dtAnHipecmb)
#Get data table of Charlson cmbs
load("V:/R_Projects/RHomeDir/RLungProject/_Data/dtCharlson.Rda")
saveRDS(dtCharlson, file = "V:\\R_Projects\\RHomeDir\\RS2\\_data\\Reference\\dtCHarslosn.rds")
#Prepare 17 row vectors of diagnosis codes for each Charlson cmb
cnames <- names(dtCharlson)
#Prepare 17 vectors listing diagnosis indicating Charlson cmb
for (i in 1:17){assign(paste0("c",i), paste(dtCharlson[i, cnames[-1], with = FALSE], collapse = "|"))}
#remove ?? cancer code (C34) from c14
##not relevant here 
#Prepare character string listing names of the 17 row vectors
chDia  <-  paste0("c",1:17) 
#Prepare character string listing corresponding column name,(0 initalised) in Hipe diagnostic data
E_Cmb  <- paste0("E_Ch", 1:17)
#Get cmbs in loop for each row in hipe data
#17 charlson cmbs, 20 diagnosis columns in hipe data
#E_Cmb[i] = E_ch i column in hipe data
#chDia[i] = vector containing diagnositic codes for i Charlson cmb
system.time(for(i in 1:17){dtAnHipecmb[ , E_Cmb[i] := as.numeric(any(grepl(get(chDia[i]), c(D1,D2,D3,D4,D5,D6,D7,D8,D9,D10,D11,D12,D13,D14,D15,D16,D17,D18,D19,D20)))), by=row_id]})
View(dtAnHipecmb)

#Flag cancer and mets before changing values to account for hierarchies
dtAnHipecmb[,CCan := E_Ch14]
dtAnHipecmb[,CMets := E_Ch16]

##Apply Charlson Hierarchies  
# NB do this before assigning weights
dtAnHipecmb[(E_Ch9==1 & E_Ch15==1), E_Ch9 := 0]   #Liver disease
dtAnHipecmb[(E_Ch10==1 & E_Ch11==1), E_Ch10 := 0] #Diabetes
dtAnHipecmb[(E_Ch14==1 & E_Ch16==1), E_Ch14 := 0] #Cancer
View(dtAnHipecmb)


#Add empty columns to datatable for Charlson weights and  set to 0
#Epsiode
for(i in 1:17){dtAnHipecmb[ , paste0("E_wt", i) := 0]}
View(dtAnHipecmb)


#Assign weights - explicitly
#Updated weights used here - Quan 2011
dtAnHipecmb[(E_Ch1==1), E_wt1 := 0]
dtAnHipecmb[(E_Ch2==1), E_wt2 := 2]
dtAnHipecmb[(E_Ch3==1), E_wt3 := 0]
dtAnHipecmb[(E_Ch4==1), E_wt4 := 0]
dtAnHipecmb[(E_Ch5==1), E_wt5 := 2]
dtAnHipecmb[(E_Ch6==1), E_wt6 := 1]
dtAnHipecmb[(E_Ch7==1), E_wt7 := 1]
dtAnHipecmb[(E_Ch8==1), E_wt8 := 0]
dtAnHipecmb[(E_Ch9==1), E_wt9 := 2]
dtAnHipecmb[(E_Ch10==1), E_wt10 := 0]
dtAnHipecmb[(E_Ch11==1), E_wt11 := 1]
dtAnHipecmb[(E_Ch12==1), E_wt12 := 2]
dtAnHipecmb[(E_Ch13==1), E_wt13 := 1]
dtAnHipecmb[(E_Ch14==1), E_wt14 := 2] 
dtAnHipecmb[(E_Ch15==1), E_wt15 := 4]
dtAnHipecmb[(E_Ch16==1), E_wt16 := 6] 
dtAnHipecmb[(E_Ch17==1), E_wt17 := 4]

#Add weights by row  
#Episode
dtAnHipecmb[, EwcharlsumW_inC := sum(c(E_wt1,E_wt2,E_wt3,E_wt4,E_wt5,E_wt6,E_wt7,E_wt8,E_wt9,E_wt10,E_wt11,E_wt12,E_wt13,E_wt14,E_wt15,E_wt16,E_wt17)), by = row_id] # 5 duplicate ncri record 
dtAnHipecmb[, EwcharlsumW_exC := sum(c(E_wt1,E_wt2,E_wt3,E_wt4,E_wt5,E_wt6,E_wt7,E_wt8,E_wt9,E_wt10,E_wt11,E_wt12,E_wt13,E_wt15,E_wt17)), by = row_id] #5 duplicate ncri record 


#Set charlson index 
#episode including cancer 
dtAnHipecmb[EwcharlsumW_inC==0, Echarlindex_inC :=0]
dtAnHipecmb[EwcharlsumW_inC==1, Echarlindex_inC :=1]
dtAnHipecmb[EwcharlsumW_inC> 1, Echarlindex_inC :=2]
#episode excluding cancer 
dtAnHipecmb[EwcharlsumW_exC==0, Echarlindex_exC :=0]
dtAnHipecmb[EwcharlsumW_exC==1, Echarlindex_exC :=1]
dtAnHipecmb[EwcharlsumW_exC> 1, Echarlindex_exC :=2]

View(dtAnHipecmb)



table(dtAnHipecmb$Echarlindex_inC)
table(dtAnHipecmb$Echarlindex_exC)

#try other grouping 
dtAnHipecmb[Ewcharlsum==0, cncmb :=0] #7845
dtAnHipecmb[Ewcharlsum==1, cncmb :=1] #1569
dtAnHipecmb[Ewcharlsum==2, cncmb :=2] #793
#dtAnHipecmb[Ewcharlsum > 1 & Ewcharlsum <6,cncmb :=2] #1142
dtAnHipecmb[Ewcharlsum >2,cncmb :=3] #637

#Flag palliative care episodes
dtAnHipecmb$SPC <- as.numeric(rowSums(dtAnHipecmb[,3:22] == 'Z515',na.rm=T) > 0)
View(dtAnHipecmb)
#Check most common cmb
t1 <- table(dtAnHipecmb$E_Ch6) #COPD 1521
View(t1)
#Remove unwanted processing columns
for(i in 1:20){dtAnHipecmb[ , paste0("D", i) := NULL]}     #Diags
for(i in 1:17){dtAnHipecmb[ , paste0("E_Ch", i) := NULL]}  #Cmb score
for(i in 1:17){dtAnHipecmb[ , paste0("E_wt", i) := NULL]}  #Weights score

View(dtAnHipecmb)
saveRDS(dtAnHipecmb, file = "V:\\R_Projects\\RHomeDir\\RS2\\_data\\dtAnHipecmb.rds")
dtAnHipecmb <- readRDS(file = "V:\\R_Projects\\RHomeDir\\RS2\\_data\\dtAnHipecmb.rds")

################################################################################################
#Merge these two and take relevant columns only
View(dtAnHipe)
uniqueN(dtAnHipe$row_id)#673063
dtAnHipe[, dupP := as.numeric(.N >1), by = row_id]
View(dtAnHipecmb)
uniqueN(dtAnHipecmb$row_id)#673063
dtAnHipecmb[, dupP := as.numeric(.N >1), by = row_id]

dtAnHipe1 <- unique(dtAnHipe[,c(1:16, 36:38)])
View(dtAnHipe1)
dtAnHipe2 <-  merge(dtAnHipe1, dtAnHipecmb, by.x = 'row_id', by.y ='row_id', all.x =TRUE)
  
dtAnHipe2 <-  dtAnHipe2[,c(1:19, 22:28)]
setnames(dtAnHipe2, old = c('row_id2.x','mypid.x'), new = c('row_id2','mypid'))
View(dtAnHipe2)
dtAnHipe_ds <- dtAnHipe2 #673078 (duplicates x 5 cause increase of 10 rows)
uniqueN(dtAnHipe_ds$row_id)#673063
dtAnHipe_ds[, dupP := as.numeric(.N >1), by = row_id]
rm(dtAnHipe1,dtAnHipe2)
saveRDS(dtAnHipe_ds, file = "V:\\R_Projects\\RHomeDir\\RS2\\_data\\dtAnHipe_ds.rds")
#dtAnHipe_ds <- readRDS(file = "V:\\R_Projects\\RHomeDir\\RS2\\_data\\dtAnHipe_ds.rds")
View(dtAnHipe_ds)
uniqueN(dtAnHipe_ds$DUM_REG)#87288

###################################################################################################
#Get patient details for these HIPE data
t1 <- dtAnHipe_ds[,c('mypid')]
View(t1)
t1 <- unique(t1)
View(t1) #88367
#Write to database
#open ODBC connection
myconn <-odbcConnect('toVM1ncrdb')unique
sqlSave(myconn, t1)
AugFFData <- sqlQuery(myconn, "select * from wtMK1") #take only case with HIPE matches
Management_PC <- sqlQuery(myconn, "select * from wtMK4") #take all managements that have PC as purpose_of_treatment #97481
close(myconn)
View(AugFFData) ##116454 #116559
dtAugFFData <- data.table(AugFFData)
dtManagement_PC <- data.table(Management_PC)

#saveRDS(dtAugFFData, file = "V:\\R_Projects\\RHomeDir\\RS2\\_data\\dtAugFFData")



t2 <- dtAugFFData[,c(1,2)]
View(t2)
t2 <-  unique(t2)#87278

###################################################################################################
#Anonymise all

#flat file #116559

dtAugFFData_ds <-  merge(dtAugFFData, t2, by.x = 'PAT_ID', by.y ='PAT_ID', all.x = TRUE)
View(dtAugFFData_ds) #116559
dtAugFFData_ds <- dtAugFFData_ds[, c(-1)]
setnames(dtAugFFData_ds, old = c('DUM_REG.x'), new = c('DUM_REG'))
saveRDS(dtAugFFData_ds, file = "V:\\R_Projects\\RHomeDir\\RS2\\_data\\dtAugFFData_ds.rds")
dtAugFFData <- dtAugFFData[,c(-1)]
saveRDS(dtAugFFData, file = "V:\\R_Projects\\RHomeDir\\RS2\\_data\\dtAugFFData")

#Hipe #673078
dtAnHipe_ds <-  merge(dtAnHipe_ds, t2, by.x = 'mypid', by.y ='PAT_ID', all.x = TRUE)
setcolorder(dtAnHipe_ds, c(1, 27,2:26))
dtAnHipe_ds <-  dtAnHipe_ds[, c(-1)]
saveRDS(dtAnHipe_ds, file = "V:\\R_Projects\\RHomeDir\\RS2\\_data\\dtAnHipe_ds.rds")

#DEPS #35272 --> 18284
Hipe151617DEPSData_ds <- merge(DEPSData151617_ds, t2, by.x = 'DUM_REG', by.y ='DUM_REG') #only those that are in Hipe
setcolorder(Hipe151617DEPSData_ds, c(1,9,2:8))
Hipe151617DEPSData_ds <-  Hipe151617DEPSData_ds[, c(-2)]
saveRDS(Hipe151617DEPSData_ds, file = "V:\\R_Projects\\RHomeDir\\RS2\\_data\\Hipe151617DEPSData_ds.rds")


#DC #20279 --> 10009
Hipe151617DCData_ds <- merge(DCData151617, t2, by.x = 'id', by.y ='PAT_ID') #only those that are in Hipe
setcolorder(Hipe151617DCData_ds, c(1,6,2:5))
Hipe151617DCData_ds <-  Hipe151617DCData_ds[, c(-1)]
saveRDS(Hipe151617DCData_ds, file = "V:\\R_Projects\\RHomeDir\\RS2\\_data\\Hipe151617DCData_ds.rds")


#Provide Sample
dtAnHipe_ds10 <-  dtAnHipe_ds[1000:1020,]

###########################################################################################################

save.image()

#VITAL_STAT 1=alive at CENSOR_DT, 2=dead on or before CENSOR_DT (see DOD).
#VITAL_CAN  1=alive at Censor date or died of another cause on or before the Censor date, 2=died of this cancer on or before the Censor date (see DOD).
#VITAL_STAT_DATE = DOD or CENSOR_DATE
##########################################################################################################
# xa <-  dtAnHipe_ds[,c('DUM_REG')]
# xa <-  unique(xa)                   #87288
# xb <- dtAnHipe_ds[,c('DUM_REG')]
# xb <-  unique(xb)
# xb <- dtAugFFData_ds[,c('DUM_REG')] 
# xb <-  unique(xb)                   #87287
# subset(xa, !(DUM_REG %in% xb$DUM_REG))
# rm(xa,xb)                           #ANSWER = 'NA'
#########################################################################################################

#Compile Patient data
#restrict to Hipe matched patients
# dtHP <- t2[,c(2)]
# View(dtHP)
rm(dtHP)
Patient_1 <- dtAugFFData_ds[,c(1,6,9:24)] #including vital stats data causes duplicates
Patient_1[, myvsd := as.Date(VITAL_STAT_DATE, format = "%d/%m/%Y")]
#CENSOR_DT is 15/06/2015
uniqueN(Patient_1$DUM_REG) #87287
Patient_1 <- unique(Patient_1) #96527
uniqueN(Patient_1$DUM_REG)#87287
uniqueN(Hipe151617DCData_ds$DUM_REG) #10009
uniqueN(Hipe151617DEPSData_ds$DUM_REG) #18284

Patient_1 <- merge(Patient_1, Hipe151617DCData_ds , by.x = 'DUM_REG', by.y ='DUM_REG', all.x = TRUE)   #96527
Patient_1 <- merge(Patient_1, Hipe151617DEPSData_ds , by.x = 'DUM_REG', by.y ='DUM_REG', all.x = TRUE) #96527
#Identify the duplicate patients
Patient_1[, dupP := as.numeric(.N >1), by = DUM_REG] #18562 due to VITAL_STATS values 
table(Patient_1$dupP)
#Remove obvious duplicates
Patient_2 <-  Patient_1[ dupP==0 | (!is.na(VITAL_STAT) & dupP==1),] #87287
#Find duplicates again
Patient_2[, dupP := as.numeric(.N >1), by = DUM_REG]
table(Patient_2$dupP) # 52 from DEPS data
Patient <- Patient_2
Patient$dupP <-  NULL
rm(Patient_1, Patient_2)
uniqueN(Patient) #87287
#Review DEPS classification of POD institution
Patient[is.na(place_of_death1), my_instit := NA]
Patient[, v1 := as.numeric(grepl('HOSPICE', Patient$place_of_death1))]
Patient[ v1==1 , my_instit := 'S']
Patient[, v1 := as.numeric(grepl('HOSP ', Patient$place_of_death1))]
Patient[, v1 := as.numeric(grepl('HOSP', Patient$place_of_death1))]
Patient[ v1==1 & my_instit != 'S', my_instit := 'O']
Patient[, v1 := as.numeric(grepl(' NH ', Patient$place_of_death1))]
Patient[, v1 := as.numeric(grepl(' NH', Patient$place_of_death1))]
Patient[ v1==1 & my_instit != 'S' &  my_instit != 'O', my_instit := 'N']
Patient[ my_instit == 'Z' & place_of_death_type_source == 'FROM HOSPITAL LOOKUP' , my_instit := 'N']
Patient[ my_instit == 'Z' & address_type == 'H', my_instit := 'H']
Patient[, v1 := as.numeric(grepl(' COMMUNITY', Patient$place_of_death1))]
Patient[ v1==1 & my_instit == 'O', my_instit := 'N']
Patient$v1 <-  NULL 
table(Patient$my_instit, exclude = NULL)
table(Patient$institution_id, exclude = NULL)
table(Patient$my_instit, Patient$institution_id , exclude = NULL)
table(Patient$my_instit, Patient$institution_id)
w1 <- Patient[my_instit == 'S' & institution_id == 'O',]

Patient[, mDOD :=as.numeric(dc_dod == dod)]
Patient[, mPOD := as.numeric(institution_id == my_instit)]
table(Patient$mDOD)
table(Patient$mPOD)
rm(w1)
#277 Milford Care Centre deaths are DC classified as O (hospital deaths) 
w1 <- Patient[institution_id != my_instit & place_of_death1 != 'MILFORD CARE CENTRE', c(1, 20,21, 26,28:31 )] #474 remaining don't match on POD
#2records don't match on dod
#For study DOD and POD take DC value excepting the 277 Milford Care Centre recorded deaths
Patient[,sDOD := dc_dod] #take dc data, only two mismatches
Patient[is.na(dc_dod),sDOD := dod]
Patient[mPOD==1,sPOD := institution_id] # take dc institution
Patient[is.na(institution_id),sPOD := my_instit] #take DEPS pod where DC is NA
Patient[mPOD==0,sPOD := institution_id] # take dc institution
Patient[institution_id != my_instit & place_of_death1 == 'MILFORD CARE CENTRE', sPOD := my_instit] #take 277 Milford care POD as 'S' hospice

#Check
Patient[,table (institution_id, my_instit)]
Patient[,table (institution_id, sPOD)]
Patient[,table (my_instit, sPOD)]
#Patient[,table (sPOD)]
#Create study Vital Stat and Vital Stat date
Patient[,table(VITAL_STAT)]
Patient[,sVS := 1, ]
Patient[,sVSD := as.Date('31/12/2016', format = "%d/%m/%Y") ]
# Patient[VITAL_STAT==2,sVS := 2, ]
# Patient[VITAL_STAT==2,sVSD := myvsd,]
#Note myvsd is date for of fflatfile VITAL_STAT_DATE
#Note x number of cases with dead flag 'D' have a VSD but I can't find a DEPS or DC records to match
#Note 22 deaths occuring in 2015 have no DEPS/DC records
#Note pwff removed VITAL_STAT_DATE, so I will ignore these 22 an only work mith matched DEPS/DC data
#Note a <-  Patient[year(sVSD)> 2014  & sVS==2 & !is.na(sVSD) & is.na(sDOD),]
Patient[sDOD < '2017/01/01',sVS := 2, ]
Patient[sDOD < '2017/01/01',sVSD := sDOD,]
Patient[,table (sVS)]
#Tidy
Patient <-Patient [, c(1:16,33:36 )]  
saveRDS(Patient, file = "V:\\R_Projects\\RHomeDir\\RS2\\_data\\Patient.rds")
Patient  <- readRDS(file = "V:\\R_Projects\\RHomeDir\\RS2\\_data\\Patient.rds")
fm_sDOD <- Patient[,c('DUM_REG','sVS', 'sVSD', 'sDOD')]
uniqueN(Patient$DUM_REG)#87287




#########################################################################################################

#Compile Tumour data
Tumour_1 <- dtAugFFData_ds[,c(1:3,26:30, 33,34,37,39,40,43:48, 54,55,57, 58,64,67:72)] #including vital stats data causes duplicates
Tumour_1 <-  unique(Tumour_1) #116559
uniqueN(Tumour_1$DUM_REG) #87287 with 116559 tumours
#sort by DUM_REG, DOI
Tumour_1[, my_doi := as.Date(DOI, format = "%d/%m/%Y")]
x1 <- Tumour_1[,c('DOI','my_doi')]
setorder(Tumour_1, DUM_REG, my_doi)
Tumour_1[, Tseq := sequence(.N), by = c('DUM_REG')]
setcolorder(Tumour_1, c(1,3,31:32,2,4:30))
#count number of tumours per patient, includes C44 for now
Tumour_1[,numT := .N, by = DUM_REG] 
#remove C44s and all D codes
Tumour_2 <-  Tumour_1[ICD10_SITE != 'C44',] #88557
uniqueN(Tumour_2$DUM_REG)#74423                     
Tumour_2 <-  Tumour_2[!grepl('D', ICD10_SITE)]#71320
uniqueN(Tumour_2$DUM_REG) #62901 with 71320 tumours 
Tumour_2[,numTex := .N, by = DUM_REG]
#Merge to get min and max survival
#FF has no exact dob so use age-exact to calulate VSAge
Tumour_2 <-  merge(fm_sDOD, Tumour_2, by.x ='DUM_REG', by.y='DUM_REG', all.y = TRUE)
Tumour_2[,sSurvival := difftime(as.POSIXct(sVSD), as.POSIXct(my_doi, tz="UTC"), units="days")]
Tumour_2[sSurvival == 0,table(sSurvival)]
Tumour_2[,sSurvivalY :=  sSurvival/365.25]
Tumour_2[,VSAge := floor(AGE_EXACT + sSurvivalY)]
Tumour_2[,min_sSurvival := min(sSurvival), by = DUM_REG]
Tumour_2[,max_sSurvival := max(sSurvival), by = DUM_REG]

#check multiples
table(Tumour_2$MULTIPLE,exclude = NULL)# 0-63838,1-1662,<NA>-5820
#remove 2017 YOI's
Tumour_2 <-  Tumour_2[YOI !=2017, ]#65500
uniqueN(Tumour_2$DUM_REG)#58025
#check multiples
table(Tumour_2$MULTIPLE,exclude = NULL)# 0-63838,1-1662
#Have a look at the cases with a multiple tumour
Tumour_3 <-  Tumour_2[!is.na(MULTIPLE), if(any(MULTIPLE== 1)) .SD, DUM_REG]
uniqueN(Tumour_3$DUM_REG)    #1601 cases have tumours that  are multiples
#how many cases are we losing by excluding multiples
Tumour_3[, countP := as.numeric(.N), by = DUM_REG]
table(Tumour_3$countP)#330 cases have one tumour which is a multiple, prior non multiple is a 'D' code, 2817 are 'true' multiple
#look at keeping these 330 cases
x1 <- Tumour_3[countP ==1, c(1,37)]
uniqueN(x1$DUM_REG) #330
#Set multiple to 0 for these
x1[,mymultiple := as.integer(0)]
x1 <- x1[, c(1,3)] #remove the NumTex column
Tumour_2 <-  merge(x1, Tumour_2, by.x ='DUM_REG', by.y='DUM_REG', all.y = TRUE)
View(Tumour_2)
table(Tumour_2$MULTIPLE,Tumour_2$mymultiple, exclude= NULL )
Tumour_2[is.na(mymultiple), mymultiple := MULTIPLE]
uniqueN(Tumour_2$DUM_REG) #58025 cases with 65500 tumours
Tumour_3 <-  Tumour_2[mymultiple != 1,]
uniqueN(Tumour_3$DUM_REG) #58005 cases  with 64168 tumours, 20 cases lost
############################################################################
##Investigate
a <- subset(Tumour_2, !(DUM_REG %in% Tumour_3$DUM_REG)) #41 records
uniqueN(a$DUM_REG)#20

#Go through manually and set multiple to correct value
table(a$YOI)
a[a[,.I[Tseq == min(Tseq)], by = 'DUM_REG']$V1,newmultiple := 0]
x1 <-  a[newmultiple==0,c(6,44)]
#Set mumultiple value correct for these 20
Tumour_3 <-  merge(x1, Tumour_2, by.x ='DUM_TUM', by.y='DUM_TUM', all.y = TRUE)
Tumour_3[newmultiple==0, mymultiple := as.integer(newmultiple)]
Tumour_3$newmultiple <- NULL
###Rerun this and shouldn't lose the 20 cases now##################
Tumour_3 <-  Tumour_3[mymultiple != 1,] #64188
uniqueN(Tumour_3$DUM_REG) #58025 cases  with 64188 tumours ==> no lost cases
###################################################################
Tumour <-  Tumour_3[, c(2:8, 15:43)]
uniqueN(Tumour$DUM_REG) #58025 with 64188 tumours
saveRDS(Tumour, file = "V:\\R_Projects\\RHomeDir\\RS2\\_data\\Tumour.rds")
# Tumour data has less cases than patient data due to exlsuion of C44's, insitu and multiples
rm(a, x1,x2,Tumour_1,Tumour_2,Tumour_3)
     
#############################################################################################################################
#14092018 New ff imported from csv
pwff <- read.csv("Z:\\Coding Group\\Flatfile download\\Flatfile 2018 testing\\Flatfile CSV latest\\30082018_flatfileFINAL_Paultest.csv")
View(pwff)
dtpwff <-  data.table(pwff)
rm (pwff)
uniqueN(dtpwff$dum_reg) ##59005

#Checked numbers and they are the same as dtAugFFData_ds
############################################################################################################################

#yearD == year of discharge
#Restrict dataset to discharges from 2015 and 2016 
H_ds1 <- dtAnHipe_ds[yearD==2015 | yearD==2016, ] #568956
uniqueN(H_ds1$DUM_REG)##77183
#Get relevant patients only
w1 <- H_ds1[, c(1)] #568956
w1 <-  unique(w1) #77183 DUMREGS
uniqueN(w1$DUM_REG)
#Order of merge is NB because source files may have duplicate DUM_REG i.e. 1P:nT 
P_ds1 <- merge(Patient,w1,  by.x = 'DUM_REG', by.y = 'DUM_REG' ) #77182
#Tumour ds have extra cases removed due to C44, non-invasive and multiples
#Inner join to restrict to direct matches between dataset
T_ds1 <- merge(Tumour, w1 ) #62015
uniqueN(T_ds1$DUM_REG)#55993


saveRDS(H_ds1, file = "V:\\R_Projects\\RHomeDir\\RS2A\\_data\\H_ds1.rds")
saveRDS(H_ds1, file = "V:\\R_Projects\\RHomeDir\\RS2\\_data\\H_ds1.rds")
saveRDS(P_ds1, file = "V:\\R_Projects\\RHomeDir\\RS2A\\_data\\P_ds1.rds")
saveRDS(P_ds1, file = "V:\\R_Projects\\RHomeDir\\RS2\\_data\\P_ds1.rds")
saveRDS(T_ds1, file = "V:\\R_Projects\\RHomeDir\\RS2A\\_data\\T_ds1.rds")
saveRDS(T_ds1, file = "V:\\R_Projects\\RHomeDir\\RS2\\_data\\T_ds1.rds")


#########################################################################
#Narrative

#Compile Master Patient data
#restrict to Hipe matched patients
#dtmkff <-  dtmkff[,c(-2)]
mPatient_1 <- dtmkff[,c(1,5,8:23)] #including vital stats data causes duplicates 715303
mPatient_1[, myvsd := as.Date(VITAL_STAT_DATE, format = "%d/%m/%Y")]
#CENSOR_DT is 15/06/2015
uniqueN(mPatient_1$DUM_REG) #590095
mPatient_1 <- unique(mPatient_1)  #605955
uniqueN(mPatient_1$DUM_REG)#59005
uniqueN(DCData151617_ds$DUM_REG) #19931
uniqueN(DEPSData151617_dst2$DUM_REG) #38868

mPatient_1 <- merge(mPatient_1, DCData151617_ds , by.x = 'DUM_REG', by.y ='DUM_REG', all.x = TRUE)   #605955
mPatient_1 <- merge(mPatient_1, DEPSData151617_dst2 , by.x = 'DUM_REG', by.y ='DUM_REG', all.x = TRUE) #605955
uniqueN(mPatient_1$DUM_REG)#59005
#Identify the duplicate patients
mPatient_1[, dupP := 0]
mPatient_1[, dupP := as.numeric(.N >1), by = DUM_REG]  
table(mPatient_1$dupP) #1
setorder(mPatient_1, dupP, DUM_REG)

#Remove obvious duplicates
mPatient_2 <-  mPatient_1[ dupP==0 | (!is.na(VITAL_STAT) & dupP==1),] #590094
#Find duplicates again
mPatient_2[, dupP := as.numeric(.N >1), by = DUM_REG]
table(mPatient_2$dupP)
mPatient_1 <- mPatient_2
rm(mPatient_2)
uniqueN(mPatient_1) #590094

mPatient_1[, mDOD :=as.numeric(dc_dod == dod)]
table(mPatient_1$mDOD, exclude= NULL)
table(mPatient_1$VITAL_STAT, exclude= NULL)
#2records don't match on dod
#For study DOD take DC value 
mPatient_1[VITAL_STAT==2, sDOD := myvsd]
mPatient_1[VITAL_STAT!=2,sDOD := dc_dod] #take dc data, only two mismatches
mPatient_1[VITAL_STAT!=2 & is.na(dc_dod),sDOD := dod]


#Create study Vital Stat and Vital Stat date
mPatient_1[,table(VITAL_STAT)]
mPatient_1[,sVS := 1, ]
mPatient_1[,sVSD := as.Date('31/12/2016', format = "%d/%m/%Y") ]

mPatient_1[sDOD < '2017/01/01',sVS := 2, ]
mPatient_1[sDOD < '2017/01/01',sVSD := sDOD,]
mPatient_1[,table (sVS)]
mPatient_1[, yearDOD  := year(sDOD)]
table(mPatient_1$sVS,exclude = NULL)
table(mPatient_1$VITAL_STAT, mPatient_1$sVS, exclude = NULL)
##351138 alive at end of 2016
##238957 dead at end of 2016
table(mPatient_1$yearDOD)
x1 <- table(mPatient_1$yearDOD)
View(x1)
rm(x1)
##Get Patients who are alive at 31/12/2016 or died in 2015/2016
sPatient_1 <- mPatient_1[sVS==1 | yearDOD ==2015 | yearDOD ==2016 ,] #374593
uniqueN(sPatient_1$DUM_REG)#374593



##########Tumour Narrative###########################################################
#########More cases will be eliminated as we exclude non invasive,C44's and multiple
#####################################################################################

mTumour_1 <- dtmkff[, c(1,2,33,36,38:47, 53:56, 66:71)] #715303
mTumour_1 <-  unique(mTumour_1)
uniqueN(mTumour_1$DUM_REG) #590095  with 715303 tumours
mTumour_1[, my_doi := as.Date(DOI, format = "%d/%m/%Y")]
x2 <- mTumour_1[,c('DOI','my_doi')]
setorder(mTumour_1, DUM_REG, my_doi)
mTumour_1[, Tseq := sequence(.N), by = c('DUM_REG')]
setcolorder(mTumour_1, c(1,26,25,2:24))
#Restrict to sPatient_1 we are interested in 
#alive at end 2016 or dod in 2015/2016
w1 <- sPatient_1[,c(1)]
sTumour_1 <-  merge(mTumour_1, w1,  by.x= 'DUM_REG', by.y= 'DUM_REG' ) #451994 inner join
uniqueN(sTumour_1$DUM_REG)
#Check number of 2017 YOI
x1 <- table(sTumour_1$YOI)# 2017 -35508
View(x1)
sTumour_1[YOI==2017, uniqueN(DUM_REG)] # 34311  cases have tumours with 2017 doi
x1 <-  sTumour_1[, uniqueN(DUM_REG), by = YOI] # 34311  cases have tumours with 2017 doi
View(x1)
#Remove tumours with doi 2017
sTumour_1 <- sTumour_1[YOI != 2017,] #416486
uniqueN(sTumour_1$DUM_REG) #346111
sTumour_1[,numT := .N, by = DUM_REG] 
#remove D codes
x1D <- sTumour_1[grepl('D', ICD10_SITE), ] #100066 tumours
uniqueN(x1D$DUM_REG)#97163
# x1D$CANCTYPE <- droplevels(x1D$CANCTYPE)
# x1D$ICD10_SITE <- droplevels(x1D$ICD10_SITE)
# table(x1D$ICD10_SITE)
sTumour_2 <-  sTumour_1[!grepl('D', ICD10_SITE), ] #316420 tumours
uniqueN(sTumour_2$DUM_REG)#270698
#Check number of 2017 YOI
table(sTumour_2$YOI)# 2017 -0
#Remove C44's
x1C44 <- sTumour_2[ICD10_SITE == 'C44', ] #122664 tumours
uniqueN(x1C44$DUM_REG)#109487
sTumour_3 <-  sTumour_2[ICD10_SITE != 'C44', ] #193756 tumours
uniqueN(sTumour_3$DUM_REG)#178910
sTumour_3[,numTex := .N, by = DUM_REG]
#remove multiples
table(sTumour_3$MULTIPLE,exclude = NULL)# 0-189743 1-4013 

#Have a look at the cases with at least one tumour that is a multiple
x1 <-  sTumour_3[!is.na(MULTIPLE), if(any(MULTIPLE== 1)) .SD, DUM_REG] #7401 records
uniqueN(x1$DUM_REG) #3899 cases

#how many cases are we losing by excluding multiples
x1[, countP := as.numeric(.N), by = DUM_REG]
table(x1$countP)#901 cases have one tumour which is a multiple, prior non multiple is a 'D' code, 2817 are 'true' multiple
#look at keeping these 901 cases
x2 <- x1[countP ==1, c(1,28)]
uniqueN(x2$DUM_REG) #901
x2[,mymultiple := as.integer(0)]
x2 <- x2[, c(1,3)] #remove the NumTex column
sTumour_3 <-  merge(x2, sTumour_3, by.x ='DUM_REG', by.y='DUM_REG', all.y = TRUE)
View(sTumour_3) 
table(sTumour_3$MULTIPLE,sTumour_3$mymultiple )
#Set mymultiple equal to MULTIPLE from ff where mymultiple is na
sTumour_3[is.na(mymultiple), mymultiple := MULTIPLE]
table(sTumour_3$MULTIPLE,sTumour_3$mymultiple, exclude= NULL) 
###################################################################
sTumour_4 <-  sTumour_3[mymultiple != 1,] #190644
uniqueN(sTumour_4$DUM_REG) #178876 cases  with 190644 tumours ==> lost 34 cases, 69 tumours
###################################################################
#Investigate
a <- subset(sTumour_3, !(DUM_REG %in% sTumour_4$DUM_REG))
uniqueN(a$DUM_REG)#34
#Go through manually and set multiple to correct value
table(a$YOI)
a[a[,.I[Tseq == min(Tseq)], by = 'DUM_REG']$V1,newmultiple := 0]
x1 <-  a[newmultiple==0,c(5,30)]
#Set mumultiple value correct for these 34
sTumour_4 <-  merge(x1, sTumour_3, by.x ='DUM_TUM', by.y='DUM_TUM', all.y = TRUE)
sTumour_4[newmultiple==0, mymultiple := as.integer(newmultiple)]
sTumour_4$newmultiple <- NULL
###Rerun this and shouldn't lose the 34 cases now##################################
sTumour_4 <-  sTumour_4[mymultiple != 1,] #190682
uniqueN(sTumour_4$DUM_REG) #178910 cases  with 190682 tumours ==> no lost cases
###################################################################
sTumour_1 <-  sTumour_4[, c(2:6, 8:29)]
rm(x1,sTumour_2, sTumour_3,sTumour_4)
uniqueN(sTumour_1$DUM_REG)#178910
table(sTumour_1$ICD10_SITE)

# sTumour_1$CANCTYPE <- droplevels(sTumour_1$CANCTYPE)
# sTumour_1$ICD10_SITE <- droplevels(sTumour_1$ICD10_SITE)
# table(sTumour_1$ICD10_SITE)


# Inner join with  HIPE data discharged in 2015/2016 for these cases
x <- merge(w1, sTumour_1) #61917 tumours cases
uniqueN(x$DUM_REG)#55907

#Compare with Tumour dataset prepared 
uniqueN(Tumour$DUM_REG) #58025, any HIPE
uniqueN(T_ds1$DUM_REG) #55993 Hipe discharges in 2015/2016
uniqueN(sTumour_1$DUM_REG) #178910 - patient alive or died in 2015 and 2016


#Investigate
a <- subset(T_ds1, !(DUM_REG %in% sTumour_1$DUM_REG))
uniqueN(a$DUM_REG) #86 cases with 101 tumours
View(a)# 98 alive and censored at 31/12/2016, 3 died in 2011



#Tumour restricted to Hipe matches
#T_ds1 derived from Tumour and further restricted to Hipe matches with year discharge 2015/2016
#sTumour_1 restricted to patient alive or died in 2015 and 2016

##############################################################
#First method (restricted to HIPE initally)
y <- T_ds1[sVS==1|year(sVSD)==2015|year(sVSD)==2016,] #see note re vital_stat above
uniqueN(y$DUM_REG)#55992 with 62012 tumours
y <- T_ds1[sVS==1|year(sDOD)==2015|year(sDOD)==2016,] #see note re vital_stat above
uniqueN(y$DUM_REG)#55992 with 62012 tumours

#Second method (all tumours)
# Inner join with  HIPE data discharged in 2015/2016 for these cases
x <- merge(w1, sTumour_1) #61917 tumours cases
uniqueN(x$DUM_REG)#55907 with 61917 tumours

a <- subset(x, !(DUM_REG %in% y$DUM_REG))
a <- subset(y, !(DUM_REG %in% x$DUM_REG)) # 98 alive in 2016
#####SAME###########


rm(a,x,y,z,x1,x2)
#End narrative
save.image()
########################################################################################




