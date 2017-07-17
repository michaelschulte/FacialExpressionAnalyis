# Facial Expression Analysis with AFFDEX and FACET: A Validation Study
# Authors:
# Michael Schulte-Mecklenbeck, Sabrina Stöckli, Stefan Borer

# clean slate
rm(list = ls())
source('iMotionsHelperFunctions.R')
source('R_Packages+OwnFunctions.R')
library(caret)

baselined = F
# load (non-)baselined data
if(baselined) {
  load(file = '../data/respdata_max_baselined.Rdata')
} else {
  load(file = '../data/respdata_max_nb.Rdata')
}

lvl_emotions <- c("Anger", "Contempt", "Disgust", "Fear", "Joy", "Sadness", "Surprise")

# make sure all levels appear in EmotionMeasure (predictor)
respdata_max$EmotionMeasure <- factor(respdata_max$EmotionMeasure, levels = lvl_emotions)

# ... and in the reference (ground truth)
respdata_max$StimulusName <- factor(respdata_max$StimulusName, levels = lvl_emotions)

# only use first match (as everywhere)
respdata_max <- respdata_max %>% filter(Max == 1) %>% ungroup() %>% select(-Max)


### Aggrate over complete df (over all database-respondents-stimuli)
respdata_max_aff <- respdata_max %>% filter(Algorithm == "Affectiva")
respdata_max_facet <- respdata_max %>% filter(Algorithm == "Facet")

confmat_affectiva <- confusionMatrix(data = respdata_max_aff$EmotionMeasure,
                                     reference = respdata_max_aff$StimulusName,
                                     mode = "prec_recall")
confmat_facet <- confusionMatrix(data = respdata_max_facet$EmotionMeasure,
                                 reference = respdata_max_facet$StimulusName,
                                 mode = "prec_recall")


### Save
# table first
write.table(t(confmat_affectiva$table), file = paste0('../data/output/confmat_affectiva', ifelse(baselined, '_baselined', '_nb'), '.csv'),
            sep = ';', dec = '.', row.names = TRUE, col.names = TRUE, quote = TRUE)
write.table(t(confmat_facet$table), file = paste0('../data/output/confmat_facet', ifelse(baselined, '_baselined', '_nb'), '.csv'),
            sep = ';', dec = '.', row.names = TRUE, col.names = TRUE, quote = FALSE)

# overall
write.table(confmat_affectiva$overall, file = paste0('../data/output/confmat_affectiva_overall', ifelse(baselined, '_baselined', '_nb'), '.csv'),
            sep = ';', dec = '.', row.names = TRUE, col.names = TRUE, quote = TRUE)
write.table(confmat_facet$overall, file = paste0('../data/output/confmat_facet_overall', ifelse(baselined, '_baselined', '_nb'), '.csv'),
            sep = ';', dec = '.', row.names = TRUE, col.names = TRUE, quote = FALSE)

# by class
write.table(as.table(confmat_affectiva$byClass), file = paste0('../data/output/confmat_affectiva_byClass', ifelse(baselined, '_baselined', '_nb'), '.csv'),
            sep = ';', dec = '.', row.names = TRUE, col.names = TRUE, quote = TRUE)
write.table(as.table(confmat_facet$byClass), file = paste0('../data/output/confmat_facet_byClass', ifelse(baselined, '_baselined', '_nb'), '.csv'),
            sep = ';', dec = '.', row.names = TRUE, col.names = TRUE, quote = TRUE)

save(confmat_affectiva, file = paste0(file = '../data/confmat_affectiva', ifelse(baselined, '_baselined', '_nb'),'.Rdata'))
save(confmat_facet, file = paste0(file = '../data/confmat_facet', ifelse(baselined, '_baselined', '_nb'),'.Rdata'))
