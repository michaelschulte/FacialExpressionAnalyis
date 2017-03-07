# Facial Expression Analysis with AFFDEX and FACET: A Validation Study
# Authors:
# Michael Schulte-Mecklenbeck, Sabrina Stöckli, Stefan Borer

# clean slate
rm(list = ls())
gc()
source('iMotionsHelperFunctions.R')
source('R_Packages+OwnFunctions.R')

load('../data/idata_clean.Rdata')
load('../data/idata_fixation.Rdata')
load('../data/osdata.Rdata')
load('../data/osdata_fixation.Rdata')
load('../data/osdata_words.Rdata')

### Calculate Baselines
# get separate mean and median of baseline stimuli
osbaseline_mean <- osdata_fixation %>%
  group_by(Name, Marker, Gamble) %>%
  summarise(Anger.Affectiva=mean(Anger.Affectiva, na.rm = TRUE), 
            Sadness.Affectiva=mean(Sadness.Affectiva, na.rm = TRUE),
            Disgust.Affectiva=mean(Disgust.Affectiva, na.rm = TRUE),
            Joy.Affectiva=mean(Joy.Affectiva, na.rm = TRUE),
            Surprise.Affectiva=mean(Surprise.Affectiva, na.rm = TRUE),
            Fear.Affectiva=mean(Fear.Affectiva, na.rm = TRUE),
            Contempt.Affectiva=mean(Contempt.Affectiva, na.rm = TRUE),
            Joy.Facet=mean(Joy.Facet, na.rm = TRUE),
            Anger.Facet=mean(Anger.Facet, na.rm = TRUE),
            Surprise.Facet=mean(Surprise.Facet, na.rm = TRUE),
            Fear.Facet=mean(Fear.Facet, na.rm = TRUE),
            Contempt.Facet=mean(Contempt.Facet, na.rm = TRUE),
            Disgust.Facet=mean(Disgust.Facet, na.rm = TRUE),
            Sadness.Facet=mean(Sadness.Facet, na.rm = TRUE),
            Neutral.Facet=mean(Neutral.Facet, na.rm = TRUE),
            Positive.Facet=mean(Positive.Facet, na.rm = TRUE),
            Negative.Facet=mean(Negative.Facet, na.rm = TRUE))

osbaseline_median <- osdata_fixation %>%
  group_by(Name, Marker, Gamble) %>%
  summarise(Anger.Affectiva=median(Anger.Affectiva, na.rm = TRUE), 
            Sadness.Affectiva=median(Sadness.Affectiva, na.rm = TRUE),
            Disgust.Affectiva=median(Disgust.Affectiva, na.rm = TRUE),
            Joy.Affectiva=median(Joy.Affectiva, na.rm = TRUE),
            Surprise.Affectiva=median(Surprise.Affectiva, na.rm = TRUE),
            Fear.Affectiva=median(Fear.Affectiva, na.rm = TRUE),
            Contempt.Affectiva=median(Contempt.Affectiva, na.rm = TRUE),
            Joy.Facet=median(Joy.Facet, na.rm = TRUE),
            Anger.Facet=median(Anger.Facet, na.rm = TRUE),
            Surprise.Facet=median(Surprise.Facet, na.rm = TRUE),
            Fear.Facet=median(Fear.Facet, na.rm = TRUE),
            Contempt.Facet=median(Contempt.Facet, na.rm = TRUE),
            Disgust.Facet=median(Disgust.Facet, na.rm = TRUE),
            Sadness.Facet=median(Sadness.Facet, na.rm = TRUE),
            Neutral.Facet=median(Neutral.Facet, na.rm = TRUE),
            Positive.Facet=median(Positive.Facet, na.rm = TRUE),
            Negative.Facet=median(Negative.Facet, na.rm = TRUE))

# same with idata
ibaseline_mean <- idata_fixation %>%
  group_by(Name, StimulusName) %>%
  summarise(Anger.Affectiva=mean(Anger.Affectiva, na.rm = TRUE), 
            Sadness.Affectiva=mean(Sadness.Affectiva, na.rm = TRUE),
            Disgust.Affectiva=mean(Disgust.Affectiva, na.rm = TRUE),
            Joy.Affectiva=mean(Joy.Affectiva, na.rm = TRUE),
            Surprise.Affectiva=mean(Surprise.Affectiva, na.rm = TRUE),
            Fear.Affectiva=mean(Fear.Affectiva, na.rm = TRUE),
            Contempt.Affectiva=mean(Contempt.Affectiva, na.rm = TRUE),
            Joy.Facet=mean(Joy.Facet, na.rm = TRUE),
            Anger.Facet=mean(Anger.Facet, na.rm = TRUE),
            Surprise.Facet=mean(Surprise.Facet, na.rm = TRUE),
            Fear.Facet=mean(Fear.Facet, na.rm = TRUE),
            Contempt.Facet=mean(Contempt.Facet, na.rm = TRUE),
            Disgust.Facet=mean(Disgust.Facet, na.rm = TRUE),
            Sadness.Facet=mean(Sadness.Facet, na.rm = TRUE),
            Neutral.Facet=mean(Neutral.Facet, na.rm = TRUE),
            Positive.Facet=mean(Positive.Facet, na.rm = TRUE),
            Negative.Facet=mean(Negative.Facet, na.rm = TRUE))

ibaseline_median <- idata_fixation %>%
  group_by(Name, StimulusName) %>%
  summarise(Anger.Affectiva=median(Anger.Affectiva, na.rm = TRUE), 
            Sadness.Affectiva=median(Sadness.Affectiva, na.rm = TRUE),
            Disgust.Affectiva=median(Disgust.Affectiva, na.rm = TRUE),
            Joy.Affectiva=median(Joy.Affectiva, na.rm = TRUE),
            Surprise.Affectiva=median(Surprise.Affectiva, na.rm = TRUE),
            Fear.Affectiva=median(Fear.Affectiva, na.rm = TRUE),
            Contempt.Affectiva=median(Contempt.Affectiva, na.rm = TRUE),
            Joy.Facet=median(Joy.Facet, na.rm = TRUE),
            Anger.Facet=median(Anger.Facet, na.rm = TRUE),
            Surprise.Facet=median(Surprise.Facet, na.rm = TRUE),
            Fear.Facet=median(Fear.Facet, na.rm = TRUE),
            Contempt.Facet=median(Contempt.Facet, na.rm = TRUE),
            Disgust.Facet=median(Disgust.Facet, na.rm = TRUE),
            Sadness.Facet=median(Sadness.Facet, na.rm = TRUE),
            Neutral.Facet=median(Neutral.Facet, na.rm = TRUE),
            Positive.Facet=median(Positive.Facet, na.rm = TRUE),
            Negative.Facet=median(Negative.Facet, na.rm = TRUE))

# strip leading "Fix" in baseline data
ibaseline_mean["StimulusName"] <- sub("^Fix(.*)", "\\1", ibaseline_mean[["StimulusName"]])
ibaseline_median["StimulusName"] <- sub("^Fix(.*)", "\\1", ibaseline_median[["StimulusName"]])

osbaseline_mean["Marker"] <- sub("^Fix(.*)", "\\1", osbaseline_mean[["Marker"]])
osbaseline_median["Marker"] <- sub("^Fix(.*)", "\\1", osbaseline_median[["Marker"]])

idata_baselined_mean <- merge(idata, ibaseline_mean, 
                              by = c("Name", "StimulusName"), 
                              all = TRUE, 
                              suffix = c("", "_baseline"))
idata_baselined_median <- merge(idata, ibaseline_median, 
                                by = c("Name", "StimulusName"), 
                                all = TRUE, 
                                suffix = c("", "_baseline"))
osdata_baselined_mean <- merge(osdata, osbaseline_mean, 
                               by = c("Name", "Marker", "Gamble"), 
                               all = TRUE, 
                               suffix = c("", "_baseline"))
osdata_baselined_median <- merge(osdata, osbaseline_median, 
                                 by = c("Name", "Marker", "Gamble"), 
                                 all = TRUE, 
                                 suffix = c("", "_baseline"))
                                 

### apply baseline
test <- idata_baselined_mean
idata_baselined_mean[c(emotionColsAffectiva(),emotionColsFacet())] <-
  idata_baselined_mean[c(emotionColsAffectiva(),emotionColsFacet())] -
  idata_baselined_mean[paste0(c(emotionColsAffectiva(),emotionColsFacet()),"_baseline")]
idata_baselined_median[c(emotionColsAffectiva(),emotionColsFacet())] <-
  idata_baselined_median[c(emotionColsAffectiva(),emotionColsFacet())] -
  idata_baselined_median[paste0(c(emotionColsAffectiva(),emotionColsFacet()),"_baseline")]
osdata_baselined_mean[c(emotionColsAffectiva(),emotionColsFacet())] <-
  osdata_baselined_mean[c(emotionColsAffectiva(),emotionColsFacet())] -
  osdata_baselined_mean[paste0(c(emotionColsAffectiva(),emotionColsFacet()),"_baseline")]
osdata_baselined_median[c(emotionColsAffectiva(),emotionColsFacet())] <-
  osdata_baselined_median[c(emotionColsAffectiva(),emotionColsFacet())] -
  osdata_baselined_median[paste0(c(emotionColsAffectiva(),emotionColsFacet()),"_baseline")]


### remove '_baseline' cols
idata_baselined_mean <- idata_baselined_mean[!(names(idata_baselined_mean) %in%
                                             paste0(c(emotionColsAffectiva(),emotionColsFacet()),"_baseline"))]
idata_baselined_median <- idata_baselined_median[!(names(idata_baselined_median) %in%
                                                 paste0(c(emotionColsAffectiva(),emotionColsFacet()),"_baseline"))]
osdata_baselined_mean <- osdata_baselined_mean[!(names(osdata_baselined_mean) %in%
                                               paste0(c(emotionColsAffectiva(),emotionColsFacet()),"_baseline"))]
osdata_baselined_median <- osdata_baselined_median[!(names(osdata_baselined_median) %in%
                                                   paste0(c(emotionColsAffectiva(),emotionColsFacet()),"_baseline"))]

# save
save(idata_baselined_mean, file = '../data/idata_baselined_mean.Rdata')
save(idata_baselined_median, file = '../data/idata_baselined_median.Rdata')
save(osdata_baselined_mean, file = '../data/osdata_baselined_mean.Rdata')
save(osdata_baselined_median, file = '../data/osdata_baselined_median.Rdata')