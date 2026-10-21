install.packages(c("tidyverse","stringr"))
library(tidyverse)
clinic_raw = read_csv("data/clinic_raw.csv", skip = 2)
head(clinic_raw)

str(clinic_raw)
summary(clinic_raw$weight_kg)

clinic_clean =
  clinic_raw %>%
  arrange(age)

#remove white spaces from site and convert all to lower case
clinic_clean =
  clinic_clean %>%
  mutate(site = tolower(trimws(site)))

#replace _ with blank
clinic_clean = 
  clinic_clean %>%
  mutate(site = gsub("_"," ",site))

#tidy notes to lower case and remove white spaces
clinic_clean =
  clinic_clean %>%
  mutate(notes = tolower(trimws(notes)))
table(clinic_clean$notes, useNA = "always")

#convert text to date
clinic_clean = 
  clinic_clean %>%
  mutate(visit_date = as.Date(visit_date, format = "%d/%m/%y"))

#tidy sex to all upper case M and F
clinic_clean =
  clinic_clean %>%
  mutate(sex = toupper(trimws(sex)))

#convert weight to numeric and convert blanks to NA
clinic_clean =
  clinic_clean %>%
  mutate(weight_kg = na_if(weight_kg,"-")
         , weight_kg = as.numeric(weight_kg))

#arrange by patient id
clinic_clean =
  clinic_clean %>%
  arrange(patient_id)

library(stringr)

#convert blanks in BP to NA
clinic_clean =
  clinic_clean %>%
  mutate(blood_pressure = na_if(blood_pressure, "N/A"))

#divide BP into systolic and diastolic
clinic_clean =
  clinic_clean %>%
  mutate(blood_pressure = trimws(blood_pressure),
         systolic_bp = as.numeric(str_extract(blood_pressure,"^[0-9]+")),
         diastolic_bp = as.numeric(str_extract(blood_pressure,"(?<=/)[0-9]+")))

#check mean BP by site
clinic_clean %>%
  group_by(site) %>%
  summarise(
    mean_sbp = mean(systolic_bp, na.rm = TRUE),
    mean_dbp = mean(diastolic_bp, na.rm = TRUE),
    n = n())

#check number of cases for follow up by site
clinic_clean %>%
  group_by(site) %>%
  summarise(
    follow_up = sum(notes == "follow up", na.rm = TRUE)
  )

#select only necessary columns and save to file
clinic_clean =
  clinic_clean %>%
  select(patient_id, site, sex, age, visit_date, weight_kg, notes, systolic_bp, diastolic_bp)

write.csv(clinic_clean, "data/clinic_clean_practice.csv")

#for own learning, how to extract numbers after a -
#clinic_clean = 
# clinic_clean %>%
#mutate(
# id = str_extract(patient_id,"(?<=-).*")
#)
#head(clinic_clean$id)
