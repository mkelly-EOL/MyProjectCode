#MBK 01/10/2020
#sample code and data to derive Charson comorbidities from ICD10 diagnosis data



##Charson cmbs
#ch1 "AMI (Acute Myocardial)" 
#ch2 "CHF (Congestive Heart)" 
#ch3 "PVD (Peripheral Vascular)"
#ch4 "CEVD (Cerebrovascular"
#ch5 "Dementia"
#ch6 "COPD (Chronic Obstructive Pulmonary)"
#ch7 "Rheumatoid Disease"
#ch8 "PUD (Peptic Ulcer)"
#ch9 "Mild LD (Liver)"
#ch10 "Diabetes"
#ch11 "Diabetes + Complications"
#ch12 "HP/PAPL (Hemiplegia or Paraplegia)"
#ch13 "RD (Renal)"
#ch14 "Cancer"
#ch15 "Moderate/Severe LD (Liver)"
#ch16 "Metastic Cancer"
#ch17 "AIDS"


require(data.table)


#Set working directory
getwd()
setwd( "D:\\HSR/GitHub\\EOL_SampleCode\\Sample1")
getwd()


#Get data table of Charlson cmbs
load("dtCharlson.Rda")
View(dtCharlson)
save(dtCharlson, file="dtCharlson.Rda")

#Get sample diagmostic codes
load("dtTestDiagnosisData.Rda")
save(dtTestDiagnosisData, file="dtTestDiagnosisData.Rda")
View(dtTestDiagnosisData)



##need this for next line of code
cnames <- names(dtCharlson) 
#prepare individual cmb diagnosis vector 
for (i in 1:17){assign(paste0("c",i), paste(dtCharlson[i, cnames[-1], with = FALSE], 
                                            collapse = "|"))}
#Prepare a list of names of  for the Charlson cmb diagnosis vectors created above
#This is needed for loop at line 59
chDia  <-  paste0("c",1:17)


##PER EPISODE
#Determine comorbidites per episode 
#Look for one Charlson cmb across all diagnosis
#system.time(dtTestDiagnosisData[ , Ch5 := as.numeric(any(grepl(c5, c(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17)))), by=rn])
#View(dtTestDiagnosisData)
#dtTestDiagnosisData$Ch5 <-  NULL


#Lists for col names to be added or in datatable
E_Cmb  <- paste0("E_Ch", 1:17)


#Get cmbs in loop 
#Find all Charslon cmbs across all diagosis for every episode
system.time(for(i in 1:17){dtTestDiagnosisData[ , E_Cmb[i] := as.numeric(any(grepl(get(chDia[i]), c(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17)))), by=rn]})
View(dtTestDiagnosisData)



##Apply Charlson Hierarchies  
# NB do this before assigning weights
dtTestDiagnosisData[(E_Ch9==1 & E_Ch15==1), E_Ch9 := 0]
dtTestDiagnosisData[(E_Ch10==1 & E_Ch11==1), E_Ch10 := 0]
dtTestDiagnosisData[(E_Ch14==1 & E_Ch16==1), E_Ch14 := 0]
View(dtTestDiagnosisData)




#Add empty columns to datatable for Charlson weights and  set to 0
#Epsiode
for(i in 1:17){dtTestDiagnosisData[ , paste0("E_wt", i) := 0]}
View(dtTestDiagnosisData)


##Debug
dtTestDiagnosisData[,sum(E_wt2, E_Ch2)] #columns sum
dtTestDiagnosisData[,sum(E_wt2)] #column sum
dtTestDiagnosisData[,sum(E_Ch2)] #column sum


#Assign weights - explicitly
#Updated weights used here
#MBK --ignore cancer Y
dtTestDiagnosisData[(E_Ch1==1), E_wt1 := 0]
dtTestDiagnosisData[(E_Ch2==1), E_wt2 := 2]
dtTestDiagnosisData[(E_Ch3==1), E_wt3 := 0]
dtTestDiagnosisData[(E_Ch4==1), E_wt4 := 0]
dtTestDiagnosisData[(E_Ch5==1), E_wt5 := 2]
dtTestDiagnosisData[(E_Ch6==1), E_wt6 := 1]
dtTestDiagnosisData[(E_Ch7==1), E_wt7 := 1]
dtTestDiagnosisData[(E_Ch8==1), E_wt8 := 0]
dtTestDiagnosisData[(E_Ch9==1), E_wt9 := 2]
dtTestDiagnosisData[(E_Ch10==1), E_wt10 := 0]
dtTestDiagnosisData[(E_Ch11==1), E_wt11 := 1]
dtTestDiagnosisData[(E_Ch12==1), E_wt12 := 2]
dtTestDiagnosisData[(E_Ch13==1), E_wt13 := 1]
#dtTestDiagnosisData[(E_Ch14==1), E_wt14 := 2] #ignore cancer
dtTestDiagnosisData[(E_Ch15==1), E_wt15 := 4]
#dtTestDiagnosisData[(E_Ch16==1), E_wt16 := 6] #ignore cancer
dtTestDiagnosisData[(E_Ch17==1), E_wt17 := 4]

#Debug
dtTestDiagnosisData[,sum(E_wt2, E_Ch2)] #columns sum
dtTestDiagnosisData[,sum(E_wt2)] #column sum
dtTestDiagnosisData[,sum(E_Ch2)] #column sum
View(dtTestDiagnosisData)


##PER PATIENT
#Determine commorbidities per patient 
#NB This is based on episode comorbidities(derived above),  with correct hierarchies applied
for(i in 1:17){dtTestDiagnosisData[ , paste0("P_Ch", i) := max(get(paste0("E_Ch", i))), by=rpid]}
View(dtTestDiagnosisData)

#Add empty columns to datatable for Charlson weights per Patient and  set to 0
#Patient
for(i in 1:17){dtTestDiagnosisData[ , paste0("P_wt", i) := 0]}

#Assign weights - explicitly
#Updated weights used here
#MBK --ignore cancer Y
dtTestDiagnosisData[(P_Ch1==1), P_wt1 := 0]
dtTestDiagnosisData[(P_Ch2==1), P_wt2 := 2]
dtTestDiagnosisData[(P_Ch3==1), P_wt3 := 0]
dtTestDiagnosisData[(P_Ch4==1), P_wt4 := 0]
dtTestDiagnosisData[(P_Ch5==1), P_wt5 := 2]
dtTestDiagnosisData[(P_Ch6==1), P_wt6 := 1]
dtTestDiagnosisData[(P_Ch7==1), P_wt7 := 1]
dtTestDiagnosisData[(P_Ch8==1), P_wt8 := 0]
dtTestDiagnosisData[(P_Ch9==1), P_wt9 := 2]
dtTestDiagnosisData[(P_Ch10==1), P_wt10 := 0]
dtTestDiagnosisData[(P_Ch11==1), P_wt11 := 1]
dtTestDiagnosisData[(P_Ch12==1), P_wt12 := 2]
dtTestDiagnosisData[(P_Ch13==1), P_wt13 := 1]
#dtTestDiagnosisData[(P_Ch14==1), P_wt14 := 2] #ignore cancer
dtTestDiagnosisData[(P_Ch15==1), P_wt15 := 4]
#dtTestDiagnosisData[(P_Ch16==1), P_wt16 := 6] #ignore cancer
dtTestDiagnosisData[(P_Ch17==1), P_wt17 := 4]

#Debug
dtTestDiagnosisData[,sum(P_wt2,P_Ch2)] #columns sum
dtTestDiagnosisData[,sum(P_wt2)] #column sum
dtTestDiagnosisData[,sum(P_Ch2)] #column sum
View(dtTestDiagnosisData)

#charlson index calculated from sum of weighted comorbidities

#Add weights by row  #Note this excludes the cancer diagnosis  (exclude E_wt14 and E_wt16 are 0)
#Episode
dtTestDiagnosisData[, Ewcharlsum := sum(c(E_wt1,E_wt2,E_wt3,E_wt4,E_wt5,E_wt6,E_wt7,E_wt8,E_wt9,E_wt10,E_wt11,E_wt12,E_wt13,E_wt14,E_wt15,E_wt16,E_wt17)), by = rn]
#Patient #Note this excludes the cancer diagnosis  (exclude P_wt14 and P_wt16 are 0)
dtTestDiagnosisData[, Pwcharlsum := sum(c(P_wt1,P_wt2,P_wt3,P_wt4,P_wt5,P_wt6,P_wt7,P_wt8,P_wt9,P_wt10,P_wt11,P_wt12,P_wt13,P_wt14,P_wt15,P_wt16,P_wt17)), by = rn]


#Set charlson index 
#episode
dtTestDiagnosisData[Ewcharlsum==0, Echarlindex :=0]
dtTestDiagnosisData[Ewcharlsum==1, Echarlindex :=1]
dtTestDiagnosisData[Ewcharlsum> 1, Echarlindex :=2]

#Set charlson index 
#patient
dtTestDiagnosisData[Pwcharlsum==0, Pcharlindex :=0]
dtTestDiagnosisData[Pwcharlsum==1, Pcharlindex :=1]
dtTestDiagnosisData[Pwcharlsum> 1, Pcharlindex :=2]

View(dtTestDiagnosisData)



#Count comorbidities by row(episode) #Note this excludes the cancer diagnosis  (exclude E_Ch14 and E_Ch16 )
dtTestDiagnosisData[, Ecmb_count := sum(c(E_Ch1,E_Ch2,E_Ch3,E_Ch4,E_Ch5,E_Ch6,E_Ch7,E_Ch8,E_Ch9,E_Ch10,E_Ch11,E_Ch12,E_Ch13,E_Ch15,E_Ch17)), by = rn]
#Determine comorbidites per patient #Note this excludes the cancer diagnosis  (exclude P_Ch14 and P_Ch16 )
dtTestDiagnosisData[, Pcmb_count := sum(c(P_Ch1,P_Ch2,P_Ch3,P_Ch4,P_Ch5,P_Ch6,P_Ch7,P_Ch8,P_Ch9,P_Ch10,P_Ch11,P_Ch12,P_Ch13,P_Ch15,P_Ch17)), by = rn]


###########################################################

#Look for a palliative care episode  Z515
dtTestDiagnosisData[ , E_palliative := 0]
system.time(dtTestDiagnosisData[ , E_palliative := as.numeric(any(grepl("Z515", c(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17)))), by=rn])
dtTestDiagnosisData[ , P_palliative := max(E_palliative), by = rpid]
View(dtTestDiagnosisData)

# Order the columns
dtTestDiagnosisData[order(rpid, episode_seq)]
View(dtTestDiagnosisData)

#Output summary data
dtTestDiagnosisData <- unique(dtTestDiagnosisData[,.(rpid,Pcharlindex, Pcmb_count, P_palliative)])
#Uncommnent next line to save to file
#save(dtTestDiagnosisData, file = "dtTestDiagnosisData.rda")

#Output summary data from first episode
dtTestDiagnosisData <- dtTestDiagnosisData[,.SD[which.min(episode_seq),Echarlindex, Ecmb_count], by = rpid]
dtTestDiagnosisData[,uniqueN(rpid)]
View (dtTestDiagnosisData)

#Output summary data from last episode
dtTestDiagnosisData <- dtTestDiagnosisData[, .SD[which.max(episode_seq),Echarlindex, Ecmb_count], by = rpid]
dtTestDiagnosisData[,uniqueN(rpid)]
View (dtTestDiagnosisData)


#Useful scripts


#merge datasets
#dtTestDiagnosisData  <- merge(dtfm, dtTestDiagnosisData, by.x=c("PID", "episode_seq"), by.y=c("PID", "episode_seq"))
#View(dtTestDiagnosisData)

#reorder columns
#setcolorder(dtTestDiagnosisData, c("rn", setdiff(names(dtTestDiagnosisData), "rn")))

#remove series of colums
#dtTestDiagnosisData[, 38:56 := NULL]

#reset
#dtTestDiagnosisData[, c("P_w11","P_wt12","P_wt13","P_wt14","P_wt15", "P_wt16", "P_wt16") := 0]


#For info

#ORIGINAL WEIGHTS
# #MBK --ignore cancer ?
# 
#  weightch11=2 if ch11>0   
#  weightch12=2 if ch12>0
#  weightch13=2 if ch13>0
#  # weightch14=2 if ch14>0 #cancer
#  weightch15=3 if ch15>0
#  # weightch16=6 if ch16>0 #cancer
#  weightch17=6 if ch17>0

#UPDATED WEIGHTS
# #MBK --ignore cancer ?
#
#  weightch1 =0 if  ch1> 0
#  weightch2 =2 if  ch2> 0  
#  weightch3 =0 if  ch3> 0
#  weightch4 =0 if  ch4> 0
#  weightch5 =2 if  ch5> 0
#  weightch6 =1 if  ch6> 0
#  weightch7 =1 if  ch7> 0
#  weightch8 =0 if  ch8> 0
#  weightch9 =2 if  ch9> 0
#  weightch10 =0 if ch10> 0
#  weightch11 =1 if ch11> 0
#  weightch12 =2 if ch12> 0
#  weightch13 =1 if ch13> 0
#  # weightch14 =2 if ch14> 0
#  weightch15 =4 if ch15> 0
#  # weightch16 =6 if ch16> 0
#  weightch17 =4 if ch17> 0



# Code to apply original weights
# #Consider not assigning any weights to cancer patients
# #Assign weights - explicitly
# #Original
# #MBK --ignore cancer Y
# dtTestDiagnosisData[(E_Ch1==1), E_wt1 := 1]
# dtTestDiagnosisData[(E_Ch2==1), E_wt2 := 1]
# dtTestDiagnosisData[(E_Ch3==1), E_wt3 := 1]
# dtTestDiagnosisData[(E_Ch4==1), E_wt4 := 1]
# dtTestDiagnosisData[(E_Ch5==1), E_wt5 := 1]
# dtTestDiagnosisData[(E_Ch6==1), E_wt6 := 1]
# dtTestDiagnosisData[(E_Ch7==1), E_wt7 := 1]
# dtTestDiagnosisData[(E_Ch8==1), E_wt8 := 1]
# dtTestDiagnosisData[(E_Ch9==1), E_wt9 := 1]
# dtTestDiagnosisData[(E_Ch10==1), E_wt10 := 1]
# dtTestDiagnosisData[(E_Ch11==1), E_wt11 := 2]
# dtTestDiagnosisData[(E_Ch12==1), E_wt12 := 2]
# dtTestDiagnosisData[(E_Ch13==1), E_wt13 := 2]
# #dtTestDiagnosisData[(E_Ch14==1), E_wt14 := 2] #ignore cancer
# dtTestDiagnosisData[(E_Ch15==1), E_wt15 := 3]
# #dtTestDiagnosisData[(E_Ch16==1), E_wt16 := 6] #ignore cancer
# dtTestDiagnosisData[(E_Ch17==1), E_wt17 := 6]

