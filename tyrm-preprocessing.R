library(haven)
library(yingtools2)
library(tidyverse)
library(readxl)
library(lubridate)

# NHAMCS --------------------------

ed2021_raw = read_dta('/Users/Tyler/Documents/Stanford Classes/CS229/CS229Final/Source Files/NHAMCS/ed2021-stata.dta') 
ed2021 = ed2021_raw %>%
  mutate(across(everything(), as_factor)) %>% # Converts coded numbers to label factors
  mutate(across(everything(), as.character)) %>%
  select(RACEUN, RACERETH, SEX, AGE,
         TEMPF, PULSE, RESPR, BPSYS, BPDIAS, POPCT, PAINSCALE, ADMIT,
         IMMEDR,
         RFV1) 

ed2020_raw = read_dta('/Users/Tyler/Documents/Stanford Classes/CS229/CS229Final/Source Files/NHAMCS/ed2020-stata.dta') 
ed2020 = ed2020_raw %>%
  mutate(across(everything(), as_factor)) %>% # Converts coded numbers to label factors
  mutate(across(everything(), as.character)) %>%
  select(RACEUN, RACERETH, SEX, AGE,
         TEMPF, PULSE, RESPR, BPSYS, BPDIAS, POPCT, PAINSCALE, ADMIT,
         IMMEDR,
         RFV1) 

ed2022_raw = read_dta('/Users/Tyler/Documents/Stanford Classes/CS229/CS229Final/Source Files/NHAMCS/ed2022-stata.dta') 
ed2022 = ed2022_raw %>%
  mutate(across(everything(), as_factor)) %>% # Converts coded numbers to label factors
  mutate(across(everything(), as.character)) %>%
  select(RACEUN, RACERETH, SEX, AGE,
         TEMPF, PULSE, RESPR, BPSYS, BPDIAS, POPCT, PAINSCALE, ADMIT,
         IMMEDR,
         RFV1) 

NHAMCS_data = rbind(ed2020, ed2021, ed2022)
cleaned_NHAMCS = NHAMCS_data %>%
  select(-c(RACERETH, ADMIT)) %>%
  rename(race = RACEUN,
         gender = SEX,
         age_at_visit = AGE, 
         temperature = TEMPF,
         heartrate = PULSE,
         resprate = RESPR,
         o2sat = POPCT,
         sbp = BPSYS,
         dbp = BPDIAS,
         pain = PAINSCALE,
         chiefcomplaint = RFV1,
         acuity = IMMEDR) %>%
  select(race:pain, chiefcomplaint, acuity) %>% 
  mutate(acuity = case_match(acuity,
                             "Immediate"   ~ 1,
                             "Emergent"    ~ 2,
                             "Urgent"      ~ 3,
                             "Semi-urgent" ~ 4,
                             "Nonurgent"   ~ 5,
                             .default      = NA),
         age_at_visit = as.numeric(case_match(age_at_visit,
                                   "Under one year" ~ "0",
                                   "93 years and over" ~ "93",
                                   "94 years and over" ~ "94",
                                   .default = age_at_visit))) %>%
  mutate(across(all_of(c('temperature', 'heartrate', 'resprate', 'sbp', 'dbp', 'o2sat')), 
                ~ as.numeric(case_when(
                  .x == "Blank" ~ NA,
                  .x == "DOPP or DOPPLER" ~ NA, 
                  .x == "P, Palp, DOP or DOPPLER" ~ NA,
                  TRUE ~ as.character(.x)
                )))) %>%          
  mutate(resprate = case_when(
    resprate > 70 ~ NA,  # anything higher than 70 is noise
    resprate < 4  ~ NA,  # rates below 4 are usually incompatible with life without a vent
    TRUE          ~ resprate)) %>%
  mutate(heartrate = case_when(
    heartrate > 250 ~ NA,  # values this high are almost always monitor artifact
    heartrate < 20  ~ NA,  # extremely unlikely without cardiac arrest / bad sensor
    TRUE            ~ heartrate
  )) %>%
  mutate(sbp = case_when(
    sbp > 250 ~ NA, 
    sbp < 40  ~ NA,  
    TRUE      ~ sbp)) %>%
  mutate(dbp = case_when(
    dbp > 150 ~ NA, 
    dbp < 30  ~ NA,  
    TRUE      ~ dbp)) %>%
  filter(!is.na(acuity)) %>%
  mutate(unable = if_else(pain %in% c("Unknown", "Blank"), 1, 0),
         pain = as.numeric(case_when(unable == 1  ~ NA,
                          TRUE         ~ pain)),
         gender = as.numeric(case_match(gender,
                             "Male"   ~ 0,
                             "Female" ~ 1))) %>%
  filter(chiefcomplaint != "Inadequate data base") %>% 
  select(race:pain, unable, chiefcomplaint, acuity)

peds_NHAMCS = cleaned_NHAMCS %>%
  filter(age_at_visit < 18)
adult_NHAMCS = cleaned_NHAMCS %>%
  filter(age_at_visit >= 18)

write_csv(cleaned_NHAMCS, '/Users/Tyler/Documents/Stanford Classes/CS229/CS229Final/Dataframes/tyrm-allNHAMCS.csv')
write_csv(peds_NHAMCS, '/Users/Tyler/Documents/Stanford Classes/CS229/CS229Final/Dataframes/tyrm-pedsNHAMCS.csv')
write_csv(adult_NHAMCS, '/Users/Tyler/Documents/Stanford Classes/CS229/CS229Final/Dataframes/tyrm-adultsNHAMCS.csv')

# MIMIC IV --------------------------

patients = read.csv('/Users/Tyler/Documents/Stanford Classes/CS229/CS229Final/Source Files/MIMIC-IV/patients.csv')
triage = read.csv('/Users/Tyler/Documents/Stanford Classes/CS229/CS229Final/Source Files/MIMIC-IV/triage.csv')
edstays = read.csv('/Users/Tyler/Documents/Stanford Classes/CS229/CS229Final/Source Files/MIMIC-IV/edstays.csv')

MIMIC = triage %>%
  inner_join(edstays, by = c("subject_id", "stay_id")) %>%
  inner_join(patients, by = "subject_id") %>%
  mutate(admit_year = year(as.POSIXct(intime, format="%Y-%m-%d %H:%M:%S")),
         age_at_visit = anchor_age + (admit_year - anchor_year)) %>%
  select(subject_id, stay_id, race, gender.x, age_at_visit, temperature:chiefcomplaint, disposition) %>%
  rename(gender = gender.x) %>%
  select(race:pain, chiefcomplaint, acuity) 

one_MIMIC = MIMIC %>%
  filter(!is.na(acuity))

unable_versions = c("", "unable", "Critical",
                    "uta", "uta ", "UTA", "ua", "UA", "u/a",
                    "c", "Unable", "unable ", "crit",
                    "critical", "?", "u", "Non-verbal", "n/a")

pain_char = one_MIMIC %>%
  filter(is.na(as.numeric(as.character(pain)))) %>%
  mutate(pain = case_when(
    pain %in% unable_versions ~ "UNABLE",
    pain == "1-2"  ~ "1.5",
    pain == "2-3"  ~ "2.5",
    pain == "3-4"  ~ "3.5",
    pain == "4-5"  ~ "4.5",
    pain == "5-6"  ~ "5.5",
    pain == "6-7"  ~ "6.5",
    pain == "7-8"  ~ "7.5",
    pain == "8-9"  ~ "8.5",
    pain == "9-10" ~ "9.5",
    pain == ">10"  ~ "10",
    TRUE           ~ "TO BE REMOVED")) %>%
  filter(pain != "TO BE REMOVED")

cleaned_MIMIC = one_MIMIC %>%
  filter(!is.na(as.numeric(as.character(pain)))) %>%
  mutate(pain = as.numeric(pain)) %>%
  rbind(pain_char) %>%
  mutate(unable = if_else(pain == "UNABLE", 1, 0),
         pain = as.numeric(case_when(
           pain == "UNABLE" ~ NA,
           TRUE             ~ pain)),
         gender = as.numeric(case_match(gender,
                                        "M"   ~ 0,
                                        "F" ~ 1))) %>%
  filter(chiefcomplaint != "") %>%
  mutate(resprate = case_when(
    resprate > 70 ~ NA,  # anything higher than 70 is noise
    resprate < 4  ~ NA,  # rates below 4 are usually incompatible with life without a vent
    TRUE          ~ resprate)) %>%
  mutate(heartrate = case_when(
    heartrate > 250 ~ NA,  # values this high are almost always monitor artifact
    heartrate < 20  ~ NA,  # extremely unlikely without cardiac arrest / bad sensor
    TRUE            ~ heartrate
  )) %>%
  mutate(sbp = case_when(
    sbp > 250 ~ NA, 
    sbp < 40  ~ NA,  
    TRUE      ~ sbp)) %>%
  mutate(dbp = case_when(
    dbp > 150 ~ NA, 
    dbp < 30  ~ NA,  
    TRUE      ~ dbp)) %>%
  select(race:pain, unable, chiefcomplaint, acuity)

write_csv(cleaned_MIMIC, '/Users/Tyler/Documents/Stanford Classes/CS229/CS229Final/Dataframes/tyrm-MIMIC.csv')

# Both ---------------------

adults_NHAMCS_MIMIC = rbind(cleaned_NHAMCS, cleaned_MIMIC)
write_csv(adults_NHAMCS_MIMIC, '/Users/Tyler/Documents/Stanford Classes/CS229/CS229Final/Dataframes/tyrm-alladults.csv')

# Some Details --------------
cat("Pre-PreProcessing NHAMCS:", nrow(NHAMCS_data))
cat("Post-PreProcessing NHAMCS:", nrow(cleaned_NHAMCS))
cat("With", nrow(adult_NHAMCS), "Adult Patients")
cat("And", nrow(peds_NHAMCS), "Pediatric Patients")

cat("Pre-PreProcessing MIMIC:", nrow(MIMIC))
cat("Post-PreProcessing MIMIC:", nrow(cleaned_MIMIC))

cat("Total number of adults:", nrow(adults_NHAMCS_MIMIC))

print(max(lengths(strsplit(adults_NHAMCS_MIMIC$chiefcomplaint, " ")))) 



