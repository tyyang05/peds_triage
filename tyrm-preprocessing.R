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
  select(race:pain, chiefcomplaint, acuity)

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

cleaned_MIMIC = triage %>%
  inner_join(edstays, by = c("subject_id", "stay_id")) %>%
  inner_join(patients, by = "subject_id") %>%
  mutate(admit_year = year(as.POSIXct(intime, format="%Y-%m-%d %H:%M:%S")),
         age_at_visit = anchor_age + (admit_year - anchor_year)) %>%
  select(subject_id, stay_id, race, gender.x, age_at_visit, temperature:chiefcomplaint, disposition) %>%
  rename(gender = gender.x) %>%
  select(race:pain, chiefcomplaint, acuity)

write_csv(cleaned_MIMIC, '/Users/Tyler/Documents/Stanford Classes/CS229/CS229Final/Dataframes/tyrm-MIMIC.csv')

# Both ---------------------

adults_NHAMCS_MIMIC = rbind(cleaned_NHAMCS, cleaned_MIMIC)
write_csv(adults_NHAMCS_MIMIC, '/Users/Tyler/Documents/Stanford Classes/CS229/CS229Final/Dataframes/tyrm-alladults.csv')



