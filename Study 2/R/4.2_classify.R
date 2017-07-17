# Facial Expression Analysis with AFFDEX and FACET: A Validation Study
# Authors:
# Michael Schulte-Mecklenbeck, Sabrina Stöckli, Stefan Borer

# clean slate
rm(list = ls())
source('iMotionsHelperFunctions.R')
source('R_Packages+OwnFunctions.R')

baselined = F
# load (non-)baselined data
if (baselined) {
  print("Load baselined")
  load(file = '../data/respdata_max_1.Rdata')
  load(file = '../data/respdata_max_2.Rdata')
  load(file = '../data/respdata_max_3.Rdata')
  load(file = '../data/respdata_max_4.Rdata')
} else {
  print("Load non-baselined")
  load(file = '../data/respdata_max_1_nb.Rdata')
  load(file = '../data/respdata_max_2_nb.Rdata')
  load(file = '../data/respdata_max_3_nb.Rdata')
  load(file = '../data/respdata_max_4_nb.Rdata')
}

# use MeasureType Max for valence calculation
respdata_max_2 <- respdata_max_2 %>% filter(MeasureType == "Max")
respdata_max_3 <- respdata_max_3 %>% filter(MeasureType == "Max")


###--------------------------------------------------###
# Method 1:
# ---------
# 
# Match emotion measures -> highest rating
# Highest emotion measure -> highest rated emotion
# (or among highest rated emotions)
###--------------------------------------------------###
### Aggrate stimuliwise (over all respondents)
stimuliwise_classified_1 <- respdata_max_1 %>%
  group_by(StimulusName, Algorithm) %>%
  summarise(# number of stimuli correctly classified
            Correct = sum(Match, na.rm = TRUE),
            # number of stimuli
            Occurence = n(),
            # number of stimuli where face was detected by algorithm
            Detected = sum(!is.na(EmotionValue)),
            # Matching Score
            MS = Correct/Occurence,
            # Distinctness Index: distance to 2nd hightest measure (where highest is correct)
            DI = mean(DI[Match == 1], na.rm = TRUE))

### Aggrate databasewise (over all respondents-stimuli)
valencewise_classified_1 <- respdata_max_1 %>%
  group_by(StimulusValence, Algorithm) %>%
  summarise(Correct = sum(Match, na.rm = TRUE),
            Occurence = n(),
            Detected = sum(!is.na(EmotionValue)),
            MS = Correct/Occurence,
            DI = mean(DI[Match == 1], na.rm = TRUE))

### Aggrate over complete df (over all database-respondents-stimuli)
globally_classified_1 <- respdata_max_1 %>%
  group_by(Algorithm) %>%
  summarise(Correct = sum(Match, na.rm = TRUE),
            Occurence = n(),
            Detected = sum(!is.na(EmotionValue)),
            MS = Correct/Occurence,
            DI = mean(DI[Match == 1], na.rm = TRUE))


###--------------------------------------------------###
# Method 2:
# ---------
# 
# Match Emotion Measures pos/neg <-> Valence Rating:
# Gpos/Gneg <-> pos/neg
# IAPS valence high /IAPS valence low <-> pos/neg
###--------------------------------------------------###
### Aggrate stimuliwise (over all respondents)
stimuliwise_classified_2 <- respdata_max_2 %>%
  group_by(StimulusName, Algorithm) %>%
  summarise(Correct = sum(Match, na.rm = TRUE),
            Occurence = n(),
            Detected = sum(!is.na(EmotionValue)),
            MS = Correct/Occurence)

### Aggrate databasewise (over all respondents-stimuli)
valencewise_classified_2 <- respdata_max_2 %>%
  group_by(StimulusValence, Algorithm) %>%
  summarise(Correct = sum(Match, na.rm = TRUE),
            Occurence = n(),
            Detected = sum(!is.na(EmotionValue)),
            MS = Correct/Occurence)

### Aggrate over complete df (over all database-respondents-stimuli)
globally_classified_2 <- respdata_max_2 %>%
  group_by(Algorithm) %>%
  summarise(Correct = sum(Match, na.rm = TRUE),
            Occurence = n(),
            Detected = sum(!is.na(EmotionValue)),
            MS = Correct/Occurence)


###--------------------------------------------------###
# Method 3:
# ---------
# 
# Match dataset validationNeg/Pos with emotion measures 
# Gpos/Gneg <-> pos/neg
# IAPS valence high /IAPS valence low <-> pos/neg
###--------------------------------------------------###
### Aggrate stimuliwise (over all respondents)
stimuliwise_classified_3 <- respdata_max_3 %>%
  group_by(StimulusName, Algorithm) %>%
  summarise(Correct = sum(Match, na.rm = TRUE),
            Occurence = n(),
            Detected = sum(!is.na(EmotionValue)),
            MS = Correct/Occurence)

### Aggrate databasewise (over all respondents-stimuli)
valencewise_classified_3 <- respdata_max_3 %>%
  group_by(StimulusValence, Algorithm) %>%
  summarise(Correct = sum(Match, na.rm = TRUE),
            Occurence = n(),
            Detected = sum(!is.na(EmotionValue)),
            MS = Correct/Occurence)

### Aggrate over complete df (over all database-respondents-stimuli)
globally_classified_3 <- respdata_max_3 %>%
  group_by(Algorithm) %>%
  summarise(Correct = sum(Match, na.rm = TRUE),
            Occurence = n(),
            Detected = sum(!is.na(EmotionValue)),
            MS = Correct/Occurence)


###--------------------------------------------------###
# Method 4:
# ---------
# 
# Match dataset validated emotions with emotion measures
# RafD only
###--------------------------------------------------###
### Aggrate stimuliwise (over all respondents)
stimuliwise_classified_4 <- respdata_max_4 %>%
  group_by(StimulusName, Algorithm) %>%
  summarise(Correct = sum(Match, na.rm = TRUE),
            Occurence = n(),
            Detected = sum(!is.na(EmotionValue)),
            MS = Correct/Occurence,
            DI = mean(DI[Match == 1], na.rm = TRUE),
            DI_norm = mean(DI_norm[Match == 1], na.rm = TRUE)) %>%
  arrange(StimulusName, Algorithm)


### Aggrate over complete df (over all respondents-stimuli) ###
globally_classified_4 <- respdata_max_4 %>%
  group_by(Algorithm) %>%
  summarise(Correct = sum(Match, na.rm = TRUE),
            Occurence = n(),
            Detected = sum(!is.na(EmotionValue)),
            MS = Correct/Occurence,
            DI = mean(DI[Match == 1], na.rm = TRUE),
            DI_norm = mean(DI_norm[Match == 1], na.rm = TRUE))

### Aggrate over complete df (over all respondents-stimuli) ###
algorithmwise_classified_4 <- respdata_max_4 %>%
  group_by(Algorithm, StimulusName) %>%
  summarise(Correct = sum(Match, na.rm = TRUE),
            Occurence = n(),
            Detected = sum(!is.na(EmotionValue)),
            MS = Correct/Occurence,
            DI = mean(DI[Match == 1], na.rm = TRUE),
            DI_norm = mean(DI_norm[Match == 1], na.rm = TRUE))



### Save with matching suffix (none or _nb)
write.table(stimuliwise_classified_1, file = paste0('../data/output/stimuliwise_classified_1', ifelse(baselined, '', '_nb'), '.csv'),
            sep = ';', dec = '.', row.names = FALSE, col.names = TRUE, quote = FALSE)
write.table(valencewise_classified_1, file = paste0('../data/output/valencewise_classified_1', ifelse(baselined, '', '_nb'), '.csv'),
            sep = ';', dec = '.', row.names = FALSE, col.names = TRUE, quote = FALSE)
write.table(globally_classified_1, file = paste0('../data/output/globally_classified_1', ifelse(baselined, '', '_nb'), '.csv'),
            sep = ';', dec = '.', row.names = FALSE, col.names = TRUE, quote = FALSE)

write.table(stimuliwise_classified_2, file = paste0('../data/output/stimuliwise_classified_2', ifelse(baselined, '', '_nb'), '.csv'),
            sep = ';', dec = '.', row.names = FALSE, col.names = TRUE, quote = FALSE)
write.table(valencewise_classified_2, file = paste0('../data/output/valencewise_classified_2', ifelse(baselined, '', '_nb'), '.csv'),
            sep = ';', dec = '.', row.names = FALSE, col.names = TRUE, quote = FALSE)
write.table(globally_classified_2, file = paste0('../data/output/globally_classified_2', ifelse(baselined, '', '_nb'), '.csv'),
            sep = ';', dec = '.', row.names = FALSE, col.names = TRUE, quote = FALSE)

write.table(stimuliwise_classified_3, file = paste0('../data/output/stimuliwise_classified_3', ifelse(baselined, '', '_nb'), '.csv'),
            sep = ';', dec = '.', row.names = FALSE, col.names = TRUE, quote = FALSE)
write.table(valencewise_classified_3, file = paste0('../data/output/valencewise_classified_3', ifelse(baselined, '', '_nb'), '.csv'),
            sep = ';', dec = '.', row.names = FALSE, col.names = TRUE, quote = FALSE)
write.table(globally_classified_3, file = paste0('../data/output/globally_classified_3', ifelse(baselined, '', '_nb'), '.csv'),
            sep = ';', dec = '.', row.names = FALSE, col.names = TRUE, quote = FALSE)

write.table(stimuliwise_classified_4, file = paste0('../data/output/stimuliwise_classified_4', ifelse(baselined, '', '_nb'), '.csv'),
            sep = ';', dec = '.', row.names = FALSE, col.names = TRUE, quote = FALSE)
write.table(algorithmwise_classified_4, file = paste0('../data/output/algorithmwise_classified_4', ifelse(baselined, '', '_nb'), '.csv'),
            sep = ';', dec = '.', row.names = FALSE, col.names = TRUE, quote = FALSE)
write.table(globally_classified_4, file = paste0('../data/output/globally_classified_4', ifelse(baselined, '', '_nb'), '.csv'),
            sep = ';', dec = '.', row.names = FALSE, col.names = TRUE, quote = FALSE)


save(stimuliwise_classified_1, file = paste0('../data/stimuliwise_classified_1', ifelse(baselined, '', '_nb'),'.Rdata'))
save(valencewise_classified_1, file = paste0('../data/valencewise_classified_1', ifelse(baselined, '', '_nb'),'.Rdata'))
save(globally_classified_1, file = paste0('../data/globally_classified_1', ifelse(baselined, '', '_nb'),'.Rdata'))

save(stimuliwise_classified_2, file = paste0('../data/stimuliwise_classified_2', ifelse(baselined, '', '_nb'),'.Rdata'))
save(valencewise_classified_2, file = paste0('../data/valencewise_classified_2', ifelse(baselined, '', '_nb'),'.Rdata'))
save(globally_classified_2, file = paste0('../data/globally_classified_2', ifelse(baselined, '', '_nb'),'.Rdata'))

save(stimuliwise_classified_3, file = paste0('../data/stimuliwise_classified_3', ifelse(baselined, '', '_nb'),'.Rdata'))
save(valencewise_classified_3, file = paste0('../data/valencewise_classified_3', ifelse(baselined, '', '_nb'),'.Rdata'))
save(globally_classified_3, file = paste0('../data/globally_classified_3', ifelse(baselined, '', '_nb'),'.Rdata'))

save(stimuliwise_classified_4, file = paste0('../data/stimuliwise_classified_4', ifelse(baselined, '', '_nb'),'.Rdata'))
save(algorithmwise_classified_4, file = paste0('../data/algorithmwise_classified_4', ifelse(baselined, '', '_nb'),'.Rdata'))
save(globally_classified_4, file = paste0('../data/globally_classified_4', ifelse(baselined, '', '_nb'),'.Rdata'))
