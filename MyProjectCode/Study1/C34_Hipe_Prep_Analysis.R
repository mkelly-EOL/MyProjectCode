#HIPE matches to NCRI dataset= 12470
#Note HIPE data was split vertically to diagnosis data (DIA) and general data (Episodes)
#Prepare C34 HIPE Diagnosis
View(C34_DIA)
dtC34_DIA  <-  data.table(C34_DIA)
View(dtC34_DIA)

#remove text to leave only diagnosis
for(i in 1:17){dtC34_DIA[ , paste0("d", i) := gsub( " .*$","", get(paste0("d", i)))]}
#take copy
dtC34_DIA_copy  <- dtC34_DIA
dtC34_DIA[,uniqueN(PID)] #12470


#Identify relevant episodes
dtC34_Episodes_copy  <-  dtC34_Episodes
dtC34_Episodes[,uniqueN(PID)] #12470
#Find last episodes per patient
dtC34_Episodes[, last_episode := max(episode_seq), by= PID]
dtC34_Episodes[, max_episode := .N, by= PID]
View(dtC34_Episodes)

#Identify episodes post diagnosis and pre_censor dates i.e Is episode...?
dtC34_Episodes[, post_doi := as.integer(my_dis + 1 >= my_doi), by= PID]  #count episodes within a day of diagnosis
dtC34_Episodes[, pre_censor := as.integer(my_dis - 1  <= my_censor), by= PID]  #discharge recorded day after  death occasionally
View(dtC34_Episodes)


#sub out these episodes to separate dataset
dtC34_Episodes_A  <-  dtC34_Episodes[(post_doi == 1 & pre_censor ==1),]
setcolorder(dtC34_Episodes_A, c(setdiff(names(dtC34_Episodes_A), "episode_seq"), "episode_seq"))
View(dtC34_Episodes_A)
dtC34_Episodes_A[, uniqueN(PID)]  #  #PID = 12282 with relevant episodes

#Check the cases omitted
#Get unique ids
#all
t1 <- dtC34_Episodes_A[, unique(PID)]
length(t1) #vector
#or
t1 <- dtC34_Episodes[, .(unique(PID))] #list all PIDs 
#subsetted above
t2 <- dtC34_Episodes_A[, .(unique(PID))] #list subsetted PIDs
#Find omitted PIDs ie cases in t1 not in t2
setkey(t1, V1)
setkey(t2, V1)
t3 <- t1[!t2] #188
Omitted  <- dtC34_Episodes[PID %in% t3[,c(V1)]]
View(Omitted)
Omitted[,uniqueN(PID)] #188
t4 <- Omitted[, .(PID, post_doi, pre_censor)]
#or
t4 <- Omitted[, .(post_doi, pre_censor), by= PID]
setkey(t4, PID)
t4 <-  unique(t4)
t4[CrossTable(post_doi, pre_censor)]
#Check survial category for patients that had no epsiodes post_diagnosis
t5 <- dtC34_Episodes[PID %in% t4[post_doi==0,c(PID)]]
#Find out survival category for these patients
t6 <- t5[, unique(.(survival_cat)), by= PID]
View(t6)
setkey(t6, PID)
t6 <-  unique(t6)
t6[,table(V1)]



#NCRI cases = 14228
#Prepare C34 NCRI data for merge with HIPE  data
C34_allT1_fm  <- C34_allT1[, .(PID, DOI, VITAL_STAT,VITAL_CAN,CENSOR_DT,SURVIVAL,he,dc)]
C34_allT1_fm[,uniqueN(PID)] # 14228
# Prepare HIPE general data for merge (relevant epsiodes only)
dtC34_Episodes_A_fm <- dtC34_Episodes_A[, .(PID, episode_seq)]
dtC34_Episodes_A_fm[,uniqueN(PID)]  #12282


#Merge NCRI data with HIPE diagnosis data (all episodes)
#Expect 12470 cases to match
dtC34DIA_A <-  merge(dtC34_DIA, C34_allT1_fm, by.x=c("PID"), by.y=c("PID") )
dtC34DIA_A[, uniqueN(PID)] #12470

#Merge this file Hipe  with HIPE general data (relevant episodes only)
#Expect 12282 cases to match
dtC34DIA_B <-  merge(dtC34DIA_A, dtC34_Episodes_A_fm, by.x=c("PID", "episode_seq"), by.y=c("PID", "episode_seq") )
dtC34DIA_B[, uniqueN(PID)]  #12282
View(dtC34DIA_B)
dtC34DIA_B_copy  <- dtC34DIA_B

#Do summary stats on both HIPE files
#dtC34DIA_B ---- see CharlsonScript


#dtC34_Episodes_A 
#Sequence relevant  episodes by patient
#dtC34_Episodes[(my_dis + 1 >= my_doi), RE_seq := sequence(.N), by = PID]
#Determine interval between episodes by patient
#Default is  lag -1 i.e. takes value for -1 row previous
#dtC34_Episodes_A[,  EInterval := my_adm - shift(my_dis), by = PID]

#Determine interval between doi to first epsiode admission
dtC34_Episodes_A[episode_seq==minE,  doi_to_adm := my_doi - my_adm, by = PID]


#Prepare dataset to look in details at first episode post diagnosis
dtC34_Episodes_A_fe_analysis_ds <-  dtC34_Episodes_A[episode_seq==minE, .(PID, episode_seq,Dia_E, my_admtype, doi_to_adm, my_adm,my_dis,my_doi)]
View(dtC34_Episodes_A_fe_analysis_ds)
require(gmodels)
CrossTable(dtC34_Episodes_A_fe_analysis_ds$Dia_E,dtC34_Episodes_A_fe_analysis_ds$my_admtype)

#Determine interval between final_discharge and censor
dtC34_Episodes_A[episode_seq==last_episode,  discharge_to_censor := my_censor - my_dis, by = PID]
#Set discharge_to_censor value for earlier episodes to 0 and sum by PID
dtC34_Episodes_A[is.na(discharge_to_censor),  discharge_to_censor := 0]
dtC34_Episodes_A[,discharge_to_censor := sum(discharge_to_censor), by= PID]
#number of day_case admissions by patient
dtC34_Episodes_A[,num_day := sum(as.integer(los==0)), by=PID]
##Count total LOS
dtC34_Episodes_A[,  total_los := sum(los), by = PID]
#Flag overnight episodes
dtC34_Episodes_A[,  num_overnight := sum(as.integer(los>0)), by=PID]
#Total_admissions
dtC34_Episodes_A[,num_adm := (.N), by=PID]
#Emergency
dtC34_Episodes_A[,no_emergency_adm := sum(as.integer(my_admtype=="Emergency")), by =PID]
#Prepare patient level summary file
dtC34_Episodes_A_summ <- unique(dtC34_Episodes_A[,.(PID,total_los,num_day, num_overnight, num_adm, no_emergency_adm,discharge_to_censor)])
View(dtC34_Episodes_A_summ)
#Save to file
save(dtC34_Episodes_A_summ, file = "D:/HSR/RHomeDir/RLungProject/_Data/dtC34_Episodes_A_summ.rda")
View(dtC34_Episodes_A)

#dtC34_EpisodesExtra
#Check datasets are the same and in the same order
dtC34_Episodes[PID ==11695, c(1:5), , with = FALSE]
dtC34_EpisodesExtra[PID ==11695,.SD, .SDcols = c(1:5)]

dtC34_EpisodesExtra[,uniqueN(PID) ]  #12470
#Restrict to relevant episodes and check
dtC34_EpisodesExtra_A <-  merge(dtC34_EpisodesExtra, dtC34_Episodes_A_fm, by.x=c("PID", "episode_seq"), by.y=c("PID", "episode_seq") )
dtC34_EpisodesExtra_A[,uniqueN(PID) ] #12282

#Prepare summary stats

#Count number of hospitals by PID
dtC34_EpisodesExtra_A[,  num_hospitals := uniqueN(NCR_HOSP), by = PID]
#Count number of specialities by PID
dtC34_EpisodesExtra_A[,  num_special := uniqueN(special), by = PID]
#Look for palliatve care consultant
dtC34_EpisodesExtra_A[,  any_palliative_cons := as.numeric(any(grepl("7300", special))), by= PID]
#Look for  ITU sum
dtC34_EpisodesExtra_A[,  total_itu_los := sum(LOS_ITU), by= PID]
View(dtC34_EpisodesExtra_A)
#Prepare summ data
dtC34_EpisodesExtra_A_summ <- unique(dtC34_EpisodesExtra_A[,.(PID, num_hospitals,num_special,any_palliative_cons, total_itu_los)])


#Get PID, survival category and survival and merge with each summary dataset and then do analysis
m1_N <- C34_allT1[, unique(.(PID, survival_cat,SURVIVAL ))] #loses column names so do below instead
m1_N <- C34_allT1[, .(PID, survival_cat2,SURVIVAL)]
m1_N <- unique(m1_N)

dtC34_EpisodesExtra_A_summ <-  merge(dtC34_EpisodesExtra_A_summ, m1_N, by.x=c("PID"), by.y=c("PID") )
dtC34_EpisodesExtra_A_summ[,uniqueN(PID) ] #12282

#Number of hospitals
#Try these
dtC34_EpisodesExtra_A_summ[, summary(num_hospitals), by = survival_cat] #not pretty
dtC34_EpisodesExtra_A_summ[, CrossTable(survival_cat, num_hospitals, digits=3, max.width = 5, expected=FALSE, prop.r=FALSE,  prop.t=FALSE,prop.chisq=FALSE, format= "SAS")] #OK but proportions by col
dtC34_EpisodesExtra_A_summ[survival_cat=="0-30d", summary(num_hospitals)] #OK but individual categories
#Use this instead
t101 <- dtC34_EpisodesExtra_A_summ[,list(
                                  mean=round(mean(num_hospitals),3),
                                  min=min(num_hospitals),
                                  lower=quantile(num_hospitals, .25, na.rm=TRUE),
                                  middle=quantile(num_hospitals, .50, na.rm=TRUE),
                                  upper=quantile(num_hospitals, .75, na.rm=TRUE),
                                  max=max(num_hospitals)),
                                  by='survival_cat']


write.table(t101, file ="C:/Temp/data.csv",row.names=FALSE,sep=",")

#See below,  all do similar things find best and use 
t102 <- dtC34_EpisodesExtra_A_summ[, CrossTable(survival_cat, num_hospitals, digits=3, max.width = 5, expected=FALSE, prop.r=FALSE,  prop.t=FALSE,prop.chisq=FALSE, format= "SAS")] 
write.table(t102, file ="C:/Temp/data.csv",row.names=FALSE,sep=",") # no good
#same as above
#dtC34_EpisodesExtra_A_summ[,write.table( CrossTable(survival_cat, num_hospitals,digits=3, max.width = 5, expected=FALSE, prop.r=FALSE,  prop.t=FALSE,prop.chisq=FALSE, format= "SAS"), file = "C:/Temp/data.csv", row.names=FALSE,sep=",")]


  #More info output 
capture.output(dtC34_EpisodesExtra_A_summ[, CrossTable(survival_cat, num_hospitals,digits=3, max.width = 5, expected=FALSE, prop.r=FALSE,  prop.t=FALSE,prop.chisq=FALSE, format= "SAS")], file = "C:/Temp/data.csv")
  #less info output  -best
dtC34_EpisodesExtra_A_summ[,capture.output( CrossTable(survival_cat, num_hospitals,digits=3, max.width = 5, expected=FALSE, prop.r=FALSE,  prop.t=FALSE,prop.chisq=FALSE, format= "SAS"), file = "C:/Temp/data.csv")]
 

#num_special
t101 <- dtC34_EpisodesExtra_A_summ[,list(
  mean=round(mean(num_special),3),
  min=min(num_special),
  lower=quantile(num_special, .25, na.rm=TRUE),
  middle=quantile(num_special, .50, na.rm=TRUE),
  upper=quantile(num_special, .75, na.rm=TRUE),
  max=max(num_special)),
  by='survival_cat']

setkey(t101, survival_cat)
write.table(t101, file ="C:/Temp/data.csv",row.names=FALSE,sep=",")
#Recode num specials to 1-6 and > 6, check and process
dtC34_EpisodesExtra_A_summ[, num_special_cat := cut(num_special,c(0,1,2,3,4,5,100), labels=c('One', 'Two','Three','Four','Five', '>Five'))]
dtC34_EpisodesExtra_A_summ[, table(num_special_cat,num_special)]
dtC34_EpisodesExtra_A_summ[,capture.output( CrossTable(survival_cat, num_special_cat,digits=3, max.width = 5, expected=FALSE, prop.r=FALSE,  prop.t=FALSE,prop.chisq=FALSE, format= "SAS"), file = "C:/Temp/data.csv")]


#any_palliative_cons
dtC34_EpisodesExtra_A_summ[, table(any_palliative_cons)]
dtC34_EpisodesExtra_A_summ[,capture.output( CrossTable(survival_cat, any_palliative_cons,digits=3, max.width = 5, expected=FALSE, prop.r=FALSE,  prop.t=FALSE,prop.chisq=FALSE, format= "SAS"), file = "C:/Temp/data.csv")]


#total_itu_los
dtC34_EpisodesExtra_A_summ[, table(total_itu_los)]
#Replace NA with 0
dtC34_EpisodesExtra_A_summ[is.na(total_itu_los), total_itu_los := 0, ]
#Most cases did not have ITU stay so just count number in each category that had any ITU stay
t101 <- dtC34_EpisodesExtra_A_summ[total_itu_los> 0,list(
  number=(.N),
  mean=round(mean(total_itu_los),3),
  min=min(total_itu_los),
  lower=quantile(total_itu_los, .25, na.rm=TRUE),
  middle=quantile(total_itu_los, .50, na.rm=TRUE),
  upper=quantile(total_itu_los, .75, na.rm=TRUE),
  max=max(total_itu_los),
  total=sum(total_itu_los)),
  by='survival_cat']
View(t101)

#SURVIVAL
t101 <- dtC34_EpisodesExtra_A_summ[,list(
 
  mean=round(mean(SURVIVAL),3),
  'lower quart'=quantile(SURVIVAL, .25, na.rm=TRUE),
  'median' =quantile(SURVIVAL, .50, na.rm=TRUE),
  'upper quart'=quantile(SURVIVAL, .75, na.rm=TRUE),
  number=(.N)),
  by='survival_cat2']
View(t101)

# Prepare dtC34_Episodes_A_summ
dtC34_Episodes_A_summ <-  merge(dtC34_Episodes_A_summ, m1_N, by.x=c("PID"), by.y=c("PID") )
dtC34_Episodes_A_summ[,uniqueN(PID) ] #12282
#total los
dtC34_Episodes_A_summ[, table(total_los)]
#Replace NA with 0
#not needed dtC34_Episodes_A_summ[is.na(total_los), total_los := 0, ]

t101 <- dtC34_Episodes_A_summ[,list(
  number=(.N),
  mean=round(mean(total_los),digits=3),
  min=min(total_los),
  lower=quantile(total_los, .25, na.rm=TRUE),
  middle=quantile(total_los, .50, na.rm=TRUE),
  upper=quantile(total_los, .75, na.rm=TRUE),
  max=max(total_los),
  total=sum(total_los)),
  by='survival_cat2']
View(t101)



#discharge_to_censor
t101 <- dtC34_Episodes_A_summ[,list(
  number=(.N),
  mean=mean(discharge_to_censor, na.rm=TRUE), #round did not work here ????
  min=min(discharge_to_censor, na.rm=TRUE),
  lower=quantile(discharge_to_censor, .25, na.rm=TRUE),
  middle=quantile(discharge_to_censor, .50, na.rm=TRUE),
  upper=quantile(discharge_to_censor, .75, na.rm=TRUE),
  max=max(discharge_to_censor, na.rm=TRUE),
  total=sum(discharge_to_censor, na.rm=TRUE)),
  by='survival_cat']
View(t101)

# total admissions
t101 <- dtC34_Episodes_A_summ[,list(
  number=(.N),
  mean=round(mean(num_adm, na.rm=TRUE),3),
  min=min(num_adm, na.rm=TRUE),
  lower=quantile(num_adm, .25, na.rm=TRUE),
  middle=quantile(num_adm, .50, na.rm=TRUE),
  upper=quantile(num_adm, .75, na.rm=TRUE),
  max=max(num_adm, na.rm=TRUE),
  total=sum(num_adm, na.rm=TRUE)),
  by='survival_cat']
View(t101)

# day admissions
t101 <- dtC34_Episodes_A_summ[,list(
  number=(.N),
  mean=round(mean(num_day),3),
  min=min(num_day),
  lower=quantile(num_day, .25, na.rm=TRUE),
  middle=quantile(num_day, .50, na.rm=TRUE),
  upper=quantile(num_day, .75, na.rm=TRUE),
  max=max(num_day),
  total=sum(num_day)),
  by='survival_cat']
View(t101)


# overnight admissions
t101 <- dtC34_Episodes_A_summ[,list(
  number=(.N),
  mean=round(mean(num_overnight),3),
  min=min(num_overnight),
  lower=quantile(num_overnight, .25, na.rm=TRUE),
  middle=quantile(num_overnight, .50, na.rm=TRUE),
  upper=quantile(num_overnight, .75, na.rm=TRUE),
  max=max(num_overnight),
  total=sum(num_overnight)),
  by='survival_cat2']
View(t101)


# Emergency admissions
t101 <- dtC34_Episodes_A_summ[,list(
  number=(.N),
  mean=round(mean(no_emergency_adm, na.rm=TRUE),3),
  min=min(no_emergency_adm, na.rm=TRUE),
  lower=quantile(no_emergency_adm, .25, na.rm=TRUE),
  middle=quantile(no_emergency_adm, .50, na.rm=TRUE),
  upper=quantile(no_emergency_adm, .75, na.rm=TRUE),
  max=max(no_emergency_adm, na.rm=TRUE),
  total=sum(no_emergency_adm, na.rm=TRUE)),
  by='survival_cat2']
View(t101)

#Diagnosis data
# Prepare dtC34DIA_B_summ
dtC34DIA_B_summ<-  merge(dtC34DIA_B_summ, m1_N, by.x=c("PID"), by.y=c("PID") )
dtC34DIA_B_summ[,uniqueN(PID) ] #12282
View(dtC34DIA_B_summ)


# Patient Charlson Index 
#not appropriate here
# t101 <- dtC34DIA_B_summ[,list(
#   number=(.N),
#   mean=round(mean(Pcharlindex, na.rm=TRUE),3),
#   min=min(Pcharlindex, na.rm=TRUE),
#   lower=quantile(Pcharlindex, .25, na.rm=TRUE),
#   middle=quantile(Pcharlindex, .50, na.rm=TRUE),
#   upper=quantile(Pcharlindex, .75, na.rm=TRUE),
#   max=max(Pcharlindex, na.rm=TRUE),
#   total=sum(Pcharlindex, na.rm=TRUE)),
#   by='survival_cat']
# View(t101)


#CharlIndex
dtC34DIA_B_summ[,capture.output( CrossTable(survival_cat2, Pcharlindex,digits=3, max.width = 5, expected=FALSE, prop.r=FALSE,  prop.t=FALSE,prop.chisq=FALSE, format= "SAS"), file = "C:/Temp/data.csv")]
#Comorbidity count
dtC34DIA_B_summ[,capture.output( CrossTable(survival_cat2, Pcmb_count,digits=3, max.width = 5, expected=FALSE, prop.r=FALSE,  prop.t=FALSE,prop.chisq=FALSE, format= "SAS"), file = "C:/Temp/data.csv")]

#Prepare first and last DIA episode summary files
dtC34DIA_B_summ_first_episode <-  merge(dtC34DIA_B_summ_first_episode, m1_N, by.x=c("PID"), by.y=c("PID") )
dtC34DIA_B_summ_first_episode[,uniqueN(PID) ] #12282
View(dtC34DIA_B_summ_first_episode)

dtC34DIA_B_summ_last_episode <-  merge(dtC34DIA_B_summ_last_episode, m1_N, by.x=c("PID"), by.y=c("PID") )
dtC34DIA_B_summ_last_episode[,uniqueN(PID) ] #12282
View(dtC34DIA_B_summ_last_episode)

#First Episode
# Charlson Index
dtC34DIA_B_summ_first_episode[,capture.output( CrossTable(survival_cat2, Echarlindex,digits=3, max.width = 5, expected=FALSE,  prop.t=FALSE,prop.chisq=FALSE, format= "SAS"), file = "C:/Temp/data.csv")]
#Comorbidity count
dtC34DIA_B_summ_first_episode[,capture.output( CrossTable(survival_cat2, Ecmb_count,digits=3, max.width = 5, expected=FALSE,  prop.t=FALSE,prop.chisq=FALSE, format= "SAS"), file = "C:/Temp/data.csv")]


#Last Episode
# Charlson Index
dtC34DIA_B_summ_last_episode[,capture.output( CrossTable(survival_cat2, Echarlindex,digits=3, max.width = 5, expected=FALSE, prop.r=FALSE,  prop.t=FALSE,prop.chisq=FALSE, format= "SAS"), file = "C:/Temp/data.csv")]
#Comorbidity count
dtC34DIA_B_summ_last_episode[,capture.output( CrossTable(survival_cat2, Ecmb_count,digits=3, max.width = 5, expected=FALSE, prop.r=FALSE,  prop.t=FALSE,prop.chisq=FALSE, format= "SAS"), file = "C:/Temp/data.csv")]


#Look at total_los as a proportion of survival
dtC34_Episodes_A_summ[,tlosPS := round(total_los/SURVIVAL, 3)] 

t101 <- dtC34_Episodes_A_summ[,list(
  number=(.N),
  mean=round(mean(tlosPS, na.rm=TRUE),3),
  min=min(tlosPS, na.rm=TRUE),
  lower=quantile(tlosPS, .25, na.rm=TRUE),
  middle=quantile(tlosPS, .50, na.rm=TRUE),
  upper=quantile(tlosPS, .75, na.rm=TRUE),
  max=max(tlosPS, na.rm=TRUE),
  total=sum(tlosPS, na.rm=TRUE)),
  by='survival_cat2']
View(t101)


#Useful code
# 
# #capture.output(CrossTable(C34_allT1$survival_cat, C34_allT1$my_hsearea,digits=3, max.width = 5, expected=FALSE, prop.r=FALSE,  prop.t=FALSE,prop.chisq=FALSE, format= "SAS"), file = "C:\\Temp\\data.csv")
# C34_allT1[,CrossTable(survival_cat,my_grade2)]
# t101 <- C34_allT1[,CrossTable(survival_cat,my_grade2)]
# t101 <- C34_allT1[,CrossTable(survival_cat,my_grade2)]
# t101 <- C34_allT1[,CrossTable(survival_cat,my_grade2,digits=3, max.width = 5, expected=FALSE, prop.r=FALSE,  prop.t=FALSE,prop.chisq=FALSE, format= "SAS")]
# capture.output(t101, file = "C:/Temp/data.csv")
# View(t101)
# C34_allT1[,capture.output(CrossTable(survival_cat,my_grade2,digits=3, max.width = 5, expected=FALSE, prop.r=FALSE,  prop.t=FALSE,prop.chisq=FALSE, format= "SAS"), file = "C:/Temp/data.csv")]
# # example - output graph to jpeg file
# jpeg("c:/mygraphs/myplot.jpg")
# plot(x)
# dev.off()
