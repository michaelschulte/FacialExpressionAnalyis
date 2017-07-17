# Facial Expression Analysis with AFFDEX and FACET: A Validation Study
# Authors:
# Michael Schulte-Mecklenbeck, Sabrina Stöckli, Stefan Borer

# clean slate
rm(list = ls())
source('iMotionsHelperFunctions.R')
source('R_Packages+OwnFunctions.R')

# load (non-)baselined data and ratings
baselined = F
if (baselined) {
  print("Load baselined")
  load(file = '../data/idata_baselined_median.Rdata')
  idata <- idata_baselined_median
  rm(idata_baselined_median)
} else {
  print("Load non-baselined")
  load(file = '../data/idata_clean.Rdata')  
}
load(file = '../data/ratings.Rdata')

# remove Mimicry data
idata <- idata %>%
  filter(!grepl("^M", StimulusName))

# add Max.Rating column
ratings_emotions <- ratings %>%
  # exclude Valence
  filter(Scale != "Valence") %>%
  group_by(Name, Image) %>%
  # mark scale with highest rating score
  mutate(Max.Rating = ifelse(Score == max(Score), 1, 0)) %>%
  arrange(Name, Image, -Score)

# reduce to valence ratings
ratings_valence <- ratings %>%
  filter(Scale == "Valence") %>%
  arrange(Name, Image, -Score) %>%
  
  # map ratings: (1,2) -> negative, (6,7) -> positive
  mutate(Score = ifelse(Score %in% c(1,2),"Negative",
                 ifelse(Score %in% c(6,7), "Positive", "None")))


### Max for each Name-Stimulus in long format
idata_long_max <- idata %>%
  # remove irrelevant stimuli:
  # - neutral for GAPED
  # - arousal high/low for IAPS
  # - neutral/surprise for RafD
  filter(!grepl("Gneu|IAPSa..|SMneu", StimulusName)) %>%
  group_by(Name, StimulusName) %>%
  
  # get max for each Name-StimulusName combination
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
  gather(EmotionMeasure, EmotionValue, -Name, -StimulusName) %>%
  arrange(Name, StimulusName) %>%
  
  # recode to NA if a whole stimulus had only NAs, resulting in summarise(max()) values -INF
  mutate(EmotionValue = ifelse(EmotionValue == -Inf, NA, EmotionValue)) %>%
  
  # separate algorithms
  separate(EmotionMeasure, c("EmotionMeasure", "Algorithm")) %>%
  
  # change column order; sort differtently
  select(Name, StimulusName, Algorithm, EmotionMeasure, EmotionValue) %>%
  arrange(Name, StimulusName, Algorithm, -EmotionValue)

  
### split rafd data, recode StimulusName
rafd_long_max <- idata_long_max %>%
  filter(grepl("^SM", StimulusName)) %>%
  mutate(StimulusName = ifelse(StimulusName == "SMang", "Anger",
                        ifelse(StimulusName == "SMcon", "Contempt",
                        ifelse(StimulusName == "SMdis", "Disgust",
                        ifelse(StimulusName == "SMfea", "Fear",
                        ifelse(StimulusName == "SMhap", "Joy",
                        ifelse(StimulusName == "SMsad", "Sadness",
                        ifelse(StimulusName == "SMsur", "Surprise", NA))))))))
                        
idata_long_max <- idata_long_max %>%
  filter(!grepl("^SM", StimulusName))


### Valence: collapse into Positive/Negative Measure
idata_long_valence <- idata_long_max %>%
  
  # ignore Surprise for valence measures
  filter(EmotionMeasure != "Surprise") %>%
  
  # use Joy as Positive, others as Negative
  mutate(EmotionMeasure = ifelse(EmotionMeasure == "Joy", "Positive", "Negative")) %>%
  group_by(Name, StimulusName, Algorithm, EmotionMeasure) %>%
  
  # summarise and longify again
  summarise(Median = median(EmotionValue, na.rm = TRUE),
            Mean = mean(EmotionValue, na.rm = TRUE),
            Max = max(EmotionValue, na.rm = TRUE)) %>%
  gather(MeasureType, EmotionValue, Median, Mean, Max) %>%

  # recode to NA if a whole stimulus had only NAs, resulting in summarise(max()) values -INF
  mutate(EmotionValue = ifelse(EmotionValue == -Inf, NA, EmotionValue)) %>%
  
  arrange(Name, StimulusName)


###--------------------------------------------------###
# Method 1:
# ---------
# 
# Match emotion measures -> highest rating
# Highest emotion measure -> highest rated emotion
# (or among highest rated emotions)
###--------------------------------------------------###
respdata_max_1 <- idata_long_max %>%
  
  # merge emotion ratings
  inner_join(ratings_emotions, by = c("Name" = "Name",
                                      "StimulusName" = "Image",
                                      "EmotionMeasure" = "Scale")) %>%
  
  # take max EmotionValues, decide if emotion and ratings match
  arrange(Name, StimulusName, Algorithm, -EmotionValue) %>%
  rename(Match = Max.Rating) %>%
  
  group_by(Name, StimulusName, Algorithm) %>%
  
  # DI on ratings, not EmotionValue
  mutate(DI = Score - lead(Score)) %>%
  filter(EmotionValue == max(EmotionValue)) %>%
  arrange(Name, StimulusName, Algorithm, -EmotionValue)


###--------------------------------------------------###
# Method 2:
# ---------
# 
# Match Emotion Measures pos/neg <-> Valence Rating:
# Gpos/Gneg <-> pos/neg
# IAPS valence high /IAPS valence low <-> pos/neg
###--------------------------------------------------###
respdata_max_2 <- idata_long_valence %>%
    
  # merge valence ratings
  inner_join(ratings_valence, by = c("Name" = "Name",
                                     "StimulusName" = "Image")) %>%
  
  # take two max EmotionValues, decide if emotion and ratings match
  group_by(Name, StimulusName, Algorithm, MeasureType) %>%
  mutate(Match = ifelse(EmotionMeasure == Score, 1, 0)) %>%
  filter(EmotionValue == max(EmotionValue)) %>%
  
  # remove Scale column, sort columns
  select(-Scale) %>%
  arrange(Name, StimulusName, Algorithm, MeasureType, -EmotionValue)


###--------------------------------------------------###
# Method 3:
# ---------
# 
# Match dataset validation Neg/Pos with emotion measures 
# Gpos/Gneg <-> pos/neg
# IAPS valence high /IAPS valence low <-> pos/neg
###--------------------------------------------------###
respdata_max_3 <- idata_long_valence %>%
    
  # keep only max Values
  group_by(Name, StimulusName, Algorithm, MeasureType) %>%
  filter(EmotionValue == max(EmotionValue)) %>%
  
  # label: Gneg -> Negative, Gpos -> Positive
  mutate(
    Match = ifelse(StimulusName == "Gneg" & EmotionMeasure == "Negative" |
                   StimulusName == "Gpos" & EmotionMeasure == "Positive" |
                   StimulusName %in% c("IAPSvh1", "IAPSvh2") & EmotionMeasure == "Positive" |
                   StimulusName %in% c("IAPSvl1", "IAPSvl2") & EmotionMeasure == "Negative", 1, 0)) %>%
                   
  # sort
  arrange(Name, StimulusName, Algorithm, -EmotionValue)


###--------------------------------------------------###
# Method 4:
# ---------
# 
# Match dataset validated emotions with emotion measures
# RafD only
###--------------------------------------------------###
respdata_max_4 <- rafd_long_max %>%

  # for each distinct stimulus, take two max
  group_by(Name, StimulusName, Algorithm) %>%
  slice(c(1,2)) %>%
  mutate(Max = c(1,2),
         Match = ifelse(StimulusName == EmotionMeasure, 1, 0),
         DI = EmotionValue - lead(EmotionValue)) %>%
  slice(1) %>%
  
  # normalized DI
  group_by(Algorithm) %>%
  mutate(DI_norm = (DI - mean(DI, na.rm = T))/sd(DI, na.rm = T))


### insert Dataset and StimulusValence columns
respdata_max_1 <- respdata_max_1 %>%
  mutate(Dataset = ifelse(grepl("G.*", StimulusName), "GAPED", "IAPS"),
         StimulusValence = ifelse(StimulusName %in% c("Gneg", "IAPSvl1", "IAPSvl2"),
                                  "Negative", "Positive"))
respdata_max_2 <- respdata_max_2 %>%
  mutate(Dataset = ifelse(grepl("G.*", StimulusName), "GAPED", "IAPS"),
         StimulusValence = ifelse(StimulusName %in% c("Gneg", "IAPSvl1", "IAPSvl2"),
                                  "Negative", "Positive"))
respdata_max_3 <- respdata_max_3 %>%
  mutate(Dataset = ifelse(grepl("G.*", StimulusName), "GAPED", "IAPS"),
         StimulusValence = ifelse(StimulusName %in% c("Gneg", "IAPSvl1", "IAPSvl2"),
                                  "Negative", "Positive"))
respdata_max_4 <- respdata_max_4 %>%
  mutate(Dataset = "RAFD",
         StimulusValence = ifelse(StimulusName == "Joy", "Positive",
                           ifelse(StimulusName == "Surprise", NA,
                           "Negative")))

# save
if(baselined) {
  save(respdata_max_1, file = '../data/respdata_max_1.Rdata')
  save(respdata_max_2, file = '../data/respdata_max_2.Rdata')
  save(respdata_max_3, file = '../data/respdata_max_3.Rdata')
  save(respdata_max_4, file = '../data/respdata_max_4.Rdata')
  
  save(idata_long_valence, file = '../data/idata_long_valence.Rdata')
  save(idata_long_max, file = '../data/idata_long_max.Rdata')
  save(rafd_long_max, file = '../data/rafd_long_max.Rdata')
} else {
  save(respdata_max_1, file = '../data/respdata_max_1_nb.Rdata')
  save(respdata_max_2, file = '../data/respdata_max_2_nb.Rdata')
  save(respdata_max_3, file = '../data/respdata_max_3_nb.Rdata')
  save(respdata_max_4, file = '../data/respdata_max_4_nb.Rdata')
  
  save(idata_long_valence, file = '../data/idata_long_valence_nb.Rdata')
  save(idata_long_max, file = '../data/idata_long_max_nb.Rdata')
  save(rafd_long_max, file = '../data/rafd_long_max_nb.Rdata')
}
save(ratings_emotions, file = '../data/ratings_emotions.Rdata')
save(ratings_valence, file = '../data/ratings_valence.Rdata')
