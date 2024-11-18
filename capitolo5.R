library(tidyverse)
library(here)
library(rio)
library(lubridate)
stroke <- read_csv(here("data", "stroke_data.csv"))
#using rio
pep <- import(here("data", "peptic_ulcer.xlsx"))

glimpse(stroke)
glimpse(pep)

pep %>% slice_head(n = 5)
head(pep)

pep2 <- pep %>% 
  dplyr::select(age, systolic, diastolic, perforation, twc,
                gender, vomiting, malena, ASA, outcome)
glimpse(pep2)
#nuove varibaili
pep2 <- pep2 %>% 
  mutate(pulse_pressure = systolic - diastolic)
pep2 %>% 
  dplyr::select(systolic, diastolic, pulse_pressure ) %>% 
  slice_head(n = 5)

pep2
#conver string to date
stroke <- stroke %>% 
  mutate(doa = dmy(doa), dod = dmy(dod))
stroke

pep2 <- pep2 %>% 
  rename(sex = gender,
         asa = ASA)
#SORT 
stroke %>% 
  arrange(doa)

stroke %>% 
  arrange(desc(doa))

#We use the filter() function to select observations based on certain criteria. Here, in this example, we will create a new dataset (which we will name as stroke_m_40) that contains patients that have sex as male and Glasgow Coma Scale (gcs) at 7 or higher:

#gender is male
#gcs at 7 or higher


stroke_m_7 <- stroke %>% 
  filter(sex == 'male', gcs >= 7)
stroke_m_7

stroke_high_BP <- stroke %>% 
  filter(sbp > 130 | dbp > 90)
stroke_high_BP

stroke_sex <- stroke %>% 
  group_by(sex)

stroke %>% 
  group_by(sex)

stroke_sex %>% 
  summarise(meansbp = mean(sbp, na.rm = TRUE), 
            meandbp  = mean(dbp, na.rm = TRUE),
            meangcs = mean(gcs, na.rm = TRUE))



pep2 %>% 
  group_by(sex) %>%
  count(outcome, sort = TRUE)

pep2 %>% 
  count(sex, outcome, sort = TRUE)

pep2 %>% 
  filter(sex == "male", diastolic >= 60, systolic >= 80) %>% 
  dplyr::select(age, systolic, diastolic, perforation, outcome) %>%
  group_by(outcome) %>%
  summarize(mean_sbp = mean(systolic, na.rm = TRUE), 
            mean_dbp = mean(diastolic, na.rm = TRUE),
            mean_perf = mean(perforation, na.rm = TRUE),
            freq = n())


stroke <- stroke %>% 
  mutate(high_bp = if_else(sbp >= 130 | dbp >= 90, 
                           "High", "Not High"))
stroke %>% count(high_bp)

stroke <- stroke %>% 
  mutate(cat_sbp = cut(sbp, breaks = c(-Inf, 120, 130, Inf),
                       labels = c('<120', '121-130', '>130')))
stroke %>% count(cat_sbp)

stroke %>% 
  group_by(cat_sbp) %>% 
  summarize(minsbp = min(sbp),
            maxsbp = max(sbp))

stroke <- stroke %>%
  mutate(cat_sbp2 = recode(cat_sbp, "<120" = "120 or less",
                           "121-130" = "121 to 130",
                           ">130" = "131 or higher"))
stroke %>% count(cat_sbp2)

levels(stroke$cat_sbp)
stroke %>% count(cat_sbp)
stroke <- stroke %>%
  mutate(relevel_cat_sbp = fct_relevel(cat_sbp, ">130", "121-130", "<120"))
levels(stroke$relevel_cat_sbp)

stroke %>% count(relevel_cat_sbp)