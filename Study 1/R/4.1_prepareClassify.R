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
  load(file = '../data/adfes_baselined.Rdata')
  load(file = '../data/rafd_baselined.Rdata')
  load(file = '../data/wsefep_baselined.Rdata')
  adfes <- adfes_baselined
  rafd <- rafd_baselined
  wsefep <- wsefep_baselined
  rm(adfes_baselined, rafd_baselined, wsefep_baselined)
} else {
  print("Load non-baselined")
  load(file = '../data/adfes_nb.Rdata')
  load(file = '../data/rafd_nb.Rdata')
  load(file = '../data/wsefep_nb.Rdata')
}

# standardize StimulusNames
adfes[adfes$StimulusName == "Sad","StimulusName"] <- "Sadness"

rafd$StimulusName <- ifelse(rafd$StimulusName == "angry", "Anger",
                            ifelse(rafd$StimulusName == "contemptuous", "Contempt",
                            ifelse(rafd$StimulusName == "disgusted", "Disgust",
                            ifelse(rafd$StimulusName == "fearful", "Fear",
                            ifelse(rafd$StimulusName == "happy", "Joy",
                            ifelse(rafd$StimulusName == "sad", "Sadness",
                            ifelse(rafd$StimulusName == "surprised", "Surprise",
                            ifelse(rafd$StimulusName == "neutral", "Neutral", NA))))))))

wsefep$StimulusName <- ifelse(wsefep$StimulusName == "anger", "Anger",
                              ifelse(wsefep$StimulusName == "disgust", "Disgust",
                              ifelse(wsefep$StimulusName == "fear", "Fear",
                              ifelse(wsefep$StimulusName == "joy", "Joy",
                              ifelse(wsefep$StimulusName == "sadness", "Sadness",
                              ifelse(wsefep$StimulusName == "surprise", "Surprise",
                              ifelse(wsefep$StimulusName == "neutral", "Neutral", NA)))))))

# remove unwanted stimuli
adfes <- adfes %>% filter(!(StimulusName %in% c("Embarrass", "Neutral", "Pride")))
rafd <- rafd %>% filter(StimulusName != "Neutral")
wsefep <- wsefep %>% filter(StimulusName != "Neutral")

# combine datasets
all <- rbind(adfes, rbind(rafd, wsefep))


### get Max Values
respdata_max <- all %>%
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
  
  # recode to NA if a whole stimulus had only NAs, resulting in summarise(max()) values -INF
  mutate(EmotionValue = ifelse(EmotionValue == -Inf, NA, EmotionValue)) %>%
  
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
save(respdata_max, file = paste0('../data/respdata_max', ifelse(baselined, '_baselined', '_nb'), '.Rdata'))
