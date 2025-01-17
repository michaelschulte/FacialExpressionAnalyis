# Facial Expression Analysis with AFFDEX and FACET: A Validation Study
# Authors:
# Michael Schulte-Mecklenbeck, Sabrina Stöckli, Stefan Borer

# clean slate
rm(list = ls())
source('iMotionsHelperFunctions.R')
source('R_Packages+OwnFunctions.R')

baselined = F
# load (non-)baselined data
if(baselined) {
  print("Load baselined")
  load(file = '../data/respdata_max_baselined.Rdata')
} else {
  print("Load non-baselined")
  load(file = '../data/respdata_max_nb.Rdata')
}

### Aggrate stimuliwise (over all respondents)
stimuliwise_classified <- respdata_max %>%
  filter(Max == 1) %>%
  group_by(Dataset, StimulusName, Algorithm) %>%
  summarise(
    # number of stimuli correctly classified
    Correct = sum(Match, na.rm = TRUE),
    # number of stimuli
    Occurence = n(),
    # number of stimuli where face was detected by algorithm
    Detected = sum(!is.na(EmotionValue)),
    # Matching Score
    MS = Correct/Occurence,
    # Distinctness Index: distance to 2nd hightest measure (where highest is correct)
    DI = mean(DI[Match == 1], na.rm = TRUE),
    DI_norm = mean(DI_norm[Match == 1], na.rm = TRUE)) %>%
  
  # sort conveniently
  arrange(StimulusName, Dataset, Algorithm)


### Aggrate databasewise (over all respondents-stimuli) ###
datasetwise_classified <- respdata_max %>%
  filter(Max == 1) %>%
  group_by(Dataset, Algorithm) %>%
  summarise(Correct = sum(Match, na.rm = TRUE),
            Occurence = n(),
            Detected = sum(!is.na(EmotionValue)),
            MS = Correct/Occurence,
            DI = mean(DI[Match == 1], na.rm = TRUE),
            DI_norm = mean(DI_norm[Match == 1], na.rm = TRUE))


### Aggrate over complete df (over all database-respondents-stimuli) ###
globally_classified <- respdata_max %>%
  filter(Max == 1) %>%
  group_by(Algorithm) %>%
  summarise(Correct = sum(Match, na.rm = TRUE),
            Occurence = n(),
            Detected = sum(!is.na(EmotionValue)),
            MS = Correct/Occurence,
            DI = mean(DI[Match == 1], na.rm = TRUE),
            DI_norm = mean(DI_norm[Match == 1], na.rm = TRUE))


### Aggrate over complete df (over all database-respondents) ###
algorithmwise_classified <- respdata_max %>%
  filter(Max == 1) %>%
  group_by(Algorithm, StimulusName) %>%
  summarise(Correct = sum(Match, na.rm = TRUE),
            Occurence = n(),
            Detected = sum(!is.na(EmotionValue)),
            MS = Correct/Occurence,
            DI = mean(DI[Match == 1], na.rm = TRUE),
            DI_norm = mean(DI_norm[Match == 1], na.rm = TRUE))


### Save
write.table(stimuliwise_classified, file = paste0('../data/output/stimuliwise_classified', ifelse(baselined, '_baselined', '_nb'), '.csv'),
            sep = ';', dec = '.', row.names = FALSE, col.names = TRUE, quote = FALSE)
write.table(datasetwise_classified, file = paste0('../data/output/datasetwise_classified', ifelse(baselined, '_baselined', '_nb'), '.csv'),
            sep = ';', dec = '.', row.names = FALSE, col.names = TRUE, quote = FALSE)
write.table(globally_classified, file = paste0('../data/output/globally_classified', ifelse(baselined, '_baselined', '_nb'), '.csv'),
            sep = ';', dec = '.', row.names = FALSE, col.names = TRUE, quote = FALSE)
write.table(algorithmwise_classified, file = paste0('../data/output/algorithmwise_classified', ifelse(baselined, '_baselined', '_nb'), '.csv'),
            sep = ';', dec = '.', row.names = FALSE, col.names = TRUE, quote = FALSE)

save(stimuliwise_classified, file = paste0('../data/stimuliwise_classified', ifelse(baselined, '_baselined', '_nb'), '.Rdata'))
save(datasetwise_classified, file = paste0('../data/datasetwise_classified', ifelse(baselined, '_baselined', '_nb'), '.Rdata'))
save(globally_classified, file = paste0('../data/globally_classified', ifelse(baselined, '_baselined', '_nb'), '.Rdata'))
save(algorithmwise_classified, file = paste0('../data/algorithmwise_classified', ifelse(baselined, '_baselined', '_nb'), '.Rdata'))
