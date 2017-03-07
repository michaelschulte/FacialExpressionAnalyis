# Facial Expression Analysis with AFFDEX and FACET: A Validation Study
# Authors:
# Michael Schulte-Mecklenbeck, Sabrina Stöckli, Stefan Borer

# clean slate
rm(list = ls())
gc()
source('iMotionsHelperFunctions.R')
source('R_Packages+OwnFunctions.R')

load(file = '../data/adfes.Rdata')
load(file = '../data/rafd.Rdata')
load(file = '../data/wsefep.Rdata')
load(file = '../data/adfes_neutral.Rdata')
load(file = '../data/rafd_neutral.Rdata')
load(file = '../data/wsefep_neutral.Rdata')

### Calculate Baselines
adfes_baseline <- adfes_neutral %>%
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

rafd_baseline <- rafd_neutral %>%
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

wsefep_baseline <- wsefep_neutral %>%
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

# merge
adfes_baselined <- merge(adfes, adfes_baseline,
                         by = c("Name"),
                         all = TRUE,
                         suffix = c("", "_baseline"))
rafd_baselined <- merge(rafd, rafd_baseline,
                        by = c("Name"),
                        all = TRUE,
                        suffix = c("", "_baseline"))
wsefep_baselined <- merge(wsefep, wsefep_baseline,
                          by = c("Name"),
                          all = TRUE,
                          suffix = c("", "_baseline"))
rm(adfes_baseline, rafd_baseline, wsefep_baseline)


### subtract baseline
adfes_baselined[c(emotionColsAffectiva(),emotionColsFacet())] <-
    adfes_baselined[c(emotionColsAffectiva(),emotionColsFacet())] -
    adfes_baselined[paste0(c(emotionColsAffectiva(),emotionColsFacet()),"_baseline")]
rafd_baselined[c(emotionColsAffectiva(),emotionColsFacet())] <-
    rafd_baselined[c(emotionColsAffectiva(),emotionColsFacet())] -
    rafd_baselined[paste0(c(emotionColsAffectiva(),emotionColsFacet()),"_baseline")]
wsefep_baselined[c(emotionColsAffectiva(),emotionColsFacet())] <-
    wsefep_baselined[c(emotionColsAffectiva(),emotionColsFacet())] -
    wsefep_baselined[paste0(c(emotionColsAffectiva(),emotionColsFacet()),"_baseline")]

# remove '_baseline' cols
adfes_baselined <- adfes_baselined[!(names(adfes_baselined) %in%
                                     paste0(c(emotionColsAffectiva(),emotionColsFacet()),"_baseline"))]
rafd_baselined <- rafd_baselined[!(names(rafd_baselined) %in%
                                   paste0(c(emotionColsAffectiva(),emotionColsFacet()),"_baseline"))]
wsefep_baselined <- wsefep_baselined[!(names(wsefep_baselined) %in%
                                       paste0(c(emotionColsAffectiva(),emotionColsFacet()),"_baseline"))]

# save
save(adfes_baselined, file = '../data/adfes_baselined.Rdata')
save(rafd_baselined, file = '../data/rafd_baselined.Rdata')
save(wsefep_baselined, file = '../data/wsefep_baselined.Rdata')