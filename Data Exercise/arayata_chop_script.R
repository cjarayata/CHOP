# Charles J. Arayata: CHOP Healthcare Data Analyst Exercise
# Set up directory; import data ####
pacman::p_load(tidyverse, lubridate, plyr, dplyr)

# Fun 'anti-join' function
'%not%' <- Negate('%in%')

setwd("C:/Users/carayata/Desktop/CHOP")
# setwd("~/Desktop/Jobs 2018/CHOP/Exercise")

# List all files and add to environment
temp <- list.files(pattern="*.csv")

list2env(lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
                read.csv), envir = .GlobalEnv)


# Part 1. Cohort Assembly ####
## 1.1 Hospital Encounter
encounters <- visit[which(visit$DICT_ENC_TYPE_KEY == 83), ]

# 1.2. After Aug 1, 2014
encounters$HOSP_ADMIT_DT <- ymd_hms(encounters$HOSP_ADMIT_DT)
encounters$HOSP_DISCHRG_DT <- ymd_hms(encounters$HOSP_DISCHRG_DT)

# Greater than or equal to August 2nd
encounters <- encounters[which(encounters$HOSP_ADMIT_DT >= "2014-08-02"), ]

# 1.3. Age between 1 and (up to) 18
children <- encounters[which(encounters$AGE > 1 & encounters$AGE < 19), ]

# 1.4. Emergency Department Diagnosis
# ICD9 codes, not the best way to create list but *a* way
allergy <- c("995.0", "995.3", "995.6", "995.60", "995.61", "995.62", "995.63",
             "995.64", "995.65", "995.66", "995.67", "995.68", "995.69",
             "995.7", "999.4", "999.41", "999.42", "999.49")

allergy.diag <- diagnosis[which(diagnosis$ICD9_CD %in% allergy), ]

# Cut down visits_diagnosis to just emergency, and then again to just allergy
emergency <- visit_diagnosis[which(visit_diagnosis$DICT_DX_STS_KEY == 313 | visit_diagnosis$DICT_DX_STS_KEY == 314), ]
emergency.allergy <- emergency[which(emergency$DX_KEY %in% allergy.diag$DX_KEY), ]

# Emergency allergy visits w. diagnosis
er.allergy.diag <- join(emergency.allergy, allergy.diag)

# Child Emergency Allergy Visits: 502 records
child.allergy <- children[which(children$VISIT_KEY %in% er.allergy.diag$VISIT_KEY), ]

# Make sure NOT at Urgent Care; first make list of Departments
urgent <- department[apply(department[,colnames(department)],1,function(row) length(grep("URGENT CARE",row))>0), ]

# Then EXCLUDE with %not% function
child.allergy <- child.allergy[child.allergy$DEPT_KEY %not% urgent$DEPT_KEY, ]

# None removed; let's cross-check to be sure
child.dept.check <- department[department$DEPT_KEY %in% child.allergy$DEPT_KEY, ]

# None of these department keys correspond to Urgent Care, so let's proceed

# Join the children visits with ER Allergy Diagnosis
## This creates 554 records, so we know there are duplicates/multiple diagnoses for some patients
child.allergy.diag <- join(child.allergy, er.allergy.diag, type = "left", by = c("VISIT_KEY", "PAT_KEY"))

length(unique(child.allergy.diag$PAT_KEY)) # 487 unique children
length(unique(child.allergy.diag$VISIT_KEY)) # 502 unique visits

## Part 2: Additional Fields ####

## Anaphylaxis ####
anaphl <- child.allergy.diag[, c(1, 2, 32)]

# Create binary. *A more thorough method could be to partial string match, eg. "anaphyla_" for "Anaphylactic".
anaphl$ana_bin <- ifelse(grepl("anaphylaxis", anaphl$DX_NM, ignore.case = T), 1, 0)
sum(anaphl$ana_bin) # 93 diagnoses of anaphylasis

# Count number of anaphylaxis diagnoses per patient/visit combination
anaphl <- ddply(anaphl, .(VISIT_KEY, PAT_KEY), summarise,
                ana = sum(ana_bin))

# Create target variable ANAPH_DX_IND; if it occured at all for a patient, 1 = yes
anaphl$ANAPH_DX_IND <- ifelse(anaphl$ana >= 1, 1, 0)

# Mini data-set
ana <- anaphl[, c(1, 2, 4)]

## Epinephrine ####
# Medication Order, trim down
med_order_trim <- medication_order[, c("VISIT_KEY", "PAT_KEY", "MED_KEY", "MED_ORD_NM", "MED_ORD_DESC")]

# Cut to relevant visit records
med_order_trim <- med_order_trim[which(med_order_trim$VISIT_KEY %in% child.allergy.diag$VISIT_KEY), ]

# *NOTE: Instructions state to use Name instead of Description. However, in lieu of hand-cleaning the inconsistent blanks/NAs, I decided to go with Description as the more complete variable. Judgement call!
sum(is.na(med_order_trim$MED_ORD_NM)) # 359 blanks
sum(is.na(med_order_trim$MED_ORD_DESC)) # 6 blanks!

# Create binary
med_order_trim$epi_bin <- ifelse(grepl("epinephrine", med_order_trim$MED_ORD_DESC, ignore.case = T), 1, 0)
sum(med_order_trim$epi_bin) # 368 medication orders for epinephrine

# Count number of medication orders for each visit/patient combination
med_order_trim <- ddply(med_order_trim, .(VISIT_KEY, PAT_KEY), summarise,
                        epi = sum(epi_bin))

# Recode into target variable; if it occured at all, yes
med_order_trim$EPI_ORDER_IND <- ifelse(med_order_trim$epi >= 1, 1, 0)

# Mini data-set again
epi <- med_order_trim[, c(1, 2, 4)]

# Put mini-sets together
ana.epi <- join(ana, epi, type = "full")

# If epi column is NA, means no medicine was ordered for that visit! Fill in with 0
sum(is.na(ana.epi)) # 13
ana.epi$EPI_ORDER_IND[is.na(ana.epi$EPI_ORDER_IND)] <- 0
sum(is.na(ana.epi)) # 0

## Follow-up Visits ####
# first, get the allergy patient IDs of interest
allergy.patients <- unique(child.allergy.diag$PAT_KEY)

# Trim visits down to allergy patients and Office Visits
follow.ups <- visit[which(visit$PAT_KEY %in% allergy.patients), ]
follow.ups <- follow.ups[which(follow.ups$DICT_ENC_TYPE_KEY == 108), ]

# Trim to relevant variables
follow.ups <- follow.ups[, c(1, 2, 6, 7, 9)]

# Trim initial hospital encounters to relevant variables
initial <- child.allergy.diag[, c(1, 2, 6, 7, 11, 12)]

# This has *every* combination of hospital visit and follow up
two.visits <- join(initial, follow.ups, by = "PAT_KEY")

length(unique(two.visits$PAT_KEY)) # 487 patients, still good here

# Convert Appointment check-in to lubridate format
two.visits$APPT_CHECKIN_DT <- ymd_hms(two.visits$APPT_CHECKIN_DT)

# Calculate time interval (in days) between Discharge and Appointment
## With every permutation, some will be extra long and some will be negative
two.visits$interv.days <- round(
  interval(two.visits$HOSP_DISCHRG_DT, two.visits$APPT_CHECKIN_DT) /
    days(1), 1)

# We only care about the (positive) range between 0 and 8 days
two.visits <- two.visits[which(two.visits$interv.days > 0 & two.visits$interv.days <8), ]

# Sort in order of intervals
two.visits <- arrange(two.visits, PAT_KEY, interv.days)

length(unique(two.visits$PAT_KEY)) # 52 unique patients came for follow up

# Because dataframe is arranged, removing duplicates keeps most recent (shortest duration) follow-up, if multiple
two.visits <- two.visits[!duplicated(two.visits$PAT_KEY), ]

# Sort descending and check, looks like 2 records need to be cut because they actually span 8 calendar days
two.visits <- arrange(two.visits, desc(interv.days))
two.visits <- two.visits[which(two.visits$interv.days < 7.6), ]

length(unique(two.visits$PAT_KEY)) # now, 50 patients with follow up

# Cut to relevant columns
follow.up.vars <- two.visits[, c(1, 2, 10, 11)]
colnames(follow.up.vars) <- c("VISIT_KEY", "PAT_KEY", "FOLLOW_UP_DATE", "DAYS_TO_FOLLOW_UP")

# Create (positive) binary
follow.up.vars$FOLLOW_UP_IND <- 1

# Visit, Patient, Age, Hosp Admit Dates for Child Emergency Allergy Visits: 502 records
visit.vars <- child.allergy[, c(1, 2, 7, 11)]

# Join with Anaphylaxis/Epinephrine tables, then Follow Up
dataset <- join(visit.vars, ana.epi)
dataset <- join(dataset, follow.up.vars)

# Create negative binary for follow-up; if missing, 0
dataset$FOLLOW_UP_IND[is.na(dataset$FOLLOW_UP_IND)] <- 0

# Checking numbers
length(unique(dataset$PAT_KEY)) # 487 unique patients
length(unique(dataset$VISIT_KEY)) # 502 visits

# Rearrange for final write
dataset <- dataset[, c(2, 1, 4, 3, 5, 6, 9, 7, 8)]

# Write out to submit
write.csv(dataset, "arayata_exercise_output.csv", row.names = F, na = "")