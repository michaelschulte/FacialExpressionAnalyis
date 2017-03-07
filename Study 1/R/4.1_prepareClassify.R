# Facial Expression Analysis with AFFDEX and FACET: A Validation Study
# Authors:
# Michael Schulte-Mecklenbeck, Sabrina Stöckli, Stefan Borer

# clean slate
rm(list = ls())
source('iMotionsHelperFunctions.R')
source('R_Packages+OwnFunctions.R')

load(file = '../data/adfes_baselined.Rdata')
load(file = '../data/rafd_baselined.Rdata')
load(file = '../data/wsefep_baselined.Rdata')

# standardize StimulusNames
adfes_baselined[adfes_baselined$StimulusName == "Sad","StimulusName"] <- "Sadness"

rafd_baselined$StimulusName <- ifelse(rafd_baselined$StimulusName == "angry", "Anger",
                               ifelse(rafd_baselined$StimulusName == "contemptuous", "Contempt",
                               ifelse(rafd_baselined$StimulusName == "disgusted", "Disgust",
                               ifelse(rafd_baselined$StimulusName == "fearful", "Fear",
                               ifelse(rafd_baselined$StimulusName == "happy", "Joy",
                               ifelse(rafd_baselined$StimulusName == "sad", "Sadness",
                               ifelse(rafd_baselined$StimulusName == "surprised", "Surprise",
                               ifelse(rafd_baselined$StimulusName == "neutral", "Neutral", NA))))))))

wsefep_baselined$StimulusName <- ifelse(wsefep_baselined$StimulusName == "anger", "Anger",
                                 ifelse(wsefep_baselined$StimulusName == "disgust", "Disgust",
                                 ifelse(wsefep_baselined$StimulusName == "fear", "Fear",
                                 ifelse(wsefep_baselined$StimulusName == "joy", "Joy",
                                 ifelse(wsefep_baselined$StimulusName == "sadness", "Sadness",
                                 ifelse(wsefep_baselined$StimulusName == "surprise", "Surprise",
                                 ifelse(wsefep_baselined$StimulusName == "neutral", "Neutral", NA)))))))

# remove unwanted stimuli
adfes_baselined <- adfes_baselined %>% filter(!(StimulusName %in% c("Embarrass", "Neutral", "Pride")))
rafd_baselined <- rafd_baselined %>% filter(StimulusName != "Neutral")
wsefep_baselined <- wsefep_baselined %>% filter(StimulusName != "Neutral")

# combine datasets
all_baselined <- rbind(adfes_baselined, rbind(rafd_baselined, wsefep_baselined))


### get Max Values
respdata_max <- all_baselined %>%
  group_by(Name, StimulusName, Dataset) %>%
  
  # get max for each Name-StimulusName-Dataset combination
  summarise(Anger.Affectiva=max(Anger.Affectiva, na.rm = TRUE), 
            Sadness.Affectiva=max(Sadness.Affectiva, na.rm = TRUE),
            Disgust.Affectiva=max(Disgust.Affectiva, na.rm = TRUE),
            Joy.Affectiva=max(Joy.Affectiva, na.rm = TRUE),
            Surprise.Affectiva=max(Surprise.Affectiva, na.rm = TRUE),
            Fear.Affectiva=max(Fear.Affectiva, na.rm = TRUE),
            Contempt.Affectiva=max(Contempt.Affectiva, na.rm = TRUE),
            Joy.Facet=max(Joy.Facet, na.rm = TRUE),
            Anger.Facet=max(Anger.Facet, na.rm = TRUE),
            Surprise.Facet=max(Surprise.Facet, na.rm = TRUE),
            Fear.Facet=max(Fear.Facet, na.rm = TRUE),
            Contempt.Facet=max(Contempt.Facet, na.rm = TRUE),
            Disgust.Facet=max(Disgust.Facet, na.rm = TRUE),
            Sadness.Facet=max(Sadness.Facet, na.rm = TRUE)) %>%
  
  # transform into long format
  gather(EmotionMeasure, EmotionValue, -Name, -StimulusName, -Dataset) %>%
  
  # separate algorithms
  separate(EmotionMeasure, c("EmotionMeasure", "Algorithm")) %>%
  
  # change column order; sort differtently
  select(Dataset, Algorithm, Name, StimulusName, EmotionMeasure, EmotionValue) %>%
  arrange(Dataset, Algorithm, Name, StimulusName, -EmotionValue) %>%
  
  # for each distinct stimulus, take two max
  group_by(Name, StimulusName, Dataset, Algorithm) %>%
  slice(c(1,2)) %>%
  mutate(Max = c(1,2),
         Match = ifelse(StimulusName == EmotionMeasure, 1, 0),
         DI = EmotionValue - lead(EmotionValue)) %>%
  
  # normalized DI
  group_by(Algorithm) %>%
  mutate(DI_norm = (DI - mean(DI, na.rm = T))/sd(DI, na.rm = T))

# save
save(respdata_max, file = '../data/respdata_max.Rdata')