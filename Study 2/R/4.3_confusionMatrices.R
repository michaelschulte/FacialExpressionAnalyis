# Facial Expression Analysis with AFFDEX and FACET: A Validation Study
# Authors:
# Michael Schulte-Mecklenbeck, Sabrina Stöckli, Stefan Borer

# clean slate
rm(list = ls())
source('iMotionsHelperFunctions.R')
source('R_Packages+OwnFunctions.R')
library(caret)

# load (non-)baselined data and ratings
baselined = F
if (baselined) {
  load(file = '../data/idata_long_valence.Rdata')
  load(file = '../data/idata_long_max.Rdata')
  load(file = '../data/rafd_long_max.Rdata')
  load(file = '../data/respdata_max_1.Rdata')
  load(file = '../data/respdata_max_2.Rdata')
  load(file = '../data/respdata_max_3.Rdata')
  load(file = '../data/respdata_max_4.Rdata')
} else {
  load(file = '../data/idata_long_valence_nb.Rdata')
  load(file = '../data/idata_long_max_nb.Rdata')
  load(file = '../data/rafd_long_max_nb.Rdata')
  load(file = '../data/respdata_max_1_nb.Rdata')
  load(file = '../data/respdata_max_2_nb.Rdata')
  load(file = '../data/respdata_max_3_nb.Rdata')
  load(file = '../data/respdata_max_4_nb.Rdata')
}
load(file = '../data/ratings_emotions.Rdata')
load(file = '../data/ratings_valence.Rdata')


lvl_emotions <- c("Anger", "Contempt", "Disgust", "Fear", "Joy", "Sadness", "Surprise")
lvl_valence <- c("Positive", "Negative")

# make sure all levels appear in EmotionMeasure (predictor)
respdata_max_1$EmotionMeasure <- factor(respdata_max_1$EmotionMeasure, levels = lvl_emotions)
respdata_max_2$EmotionMeasure <- factor(respdata_max_2$EmotionMeasure, levels = lvl_valence)
respdata_max_3$EmotionMeasure <- factor(respdata_max_3$EmotionMeasure, levels = lvl_valence)
respdata_max_4$EmotionMeasure <- factor(respdata_max_4$EmotionMeasure, levels = lvl_emotions)

# ... and in the reference (ground truth)
respdata_max_2$Score <- factor(respdata_max_2$Score, levels = lvl_valence)
respdata_max_3$StimulusValence <- factor(respdata_max_3$StimulusValence, levels = lvl_valence)
respdata_max_4$StimulusName <- factor(respdata_max_4$StimulusName, levels = lvl_emotions)

# take max for valence measures
respdata_max_2 <- respdata_max_2 %>% filter(MeasureType == "Max") %>% ungroup() %>% dplyr::select(-MeasureType)
respdata_max_3 <- respdata_max_3 %>% filter(MeasureType == "Max") %>% ungroup() %>% dplyr::select(-MeasureType)


###--------------------------------------------------###
# Method 1:
# ---------
# 
# Match emotion measures -> highest rating
# Highest emotion measure -> highest rated emotion
# (or among highest rated emotions)
###--------------------------------------------------###

### Doesn't make sense for confusion matrix


###--------------------------------------------------###
# Method 2:
# ---------
# 
# Match Emotion Measures pos/neg <-> Valence Rating:
# Gpos/Gneg <-> pos/neg
# IAPS valence high /IAPS valence low <-> pos/neg
###--------------------------------------------------###

### Aggrate over complete df (over all database-respondents-stimuli)
respdata_max_2_aff <- respdata_max_2 %>% filter(Algorithm == "Affectiva", Score %in% lvl_valence)
respdata_max_2_facet <- respdata_max_2 %>% filter(Algorithm == "Facet", Score %in% lvl_valence)

confmat_affectiva_2 <- confusionMatrix(data = respdata_max_2_aff$EmotionMeasure,
                                       reference = respdata_max_2_aff$Score,
                                       mode = "prec_recall")
confmat_facet_2 <- confusionMatrix(data = respdata_max_2_facet$EmotionMeasure,
                                   reference = respdata_max_2_facet$Score,
                                   mode = "prec_recall")


###--------------------------------------------------###
# Method 3:
# ---------
# 
# Match dataset validation Neg/Pos with emotion measures 
# Gpos/Gneg <-> pos/neg
# IAPS valence high /IAPS valence low <-> pos/neg
###--------------------------------------------------###
### Aggrate over complete df (over all database-respondents-stimuli)
respdata_max_3_aff <- respdata_max_3 %>% filter(Algorithm == "Affectiva")
respdata_max_3_facet <- respdata_max_3 %>% filter(Algorithm == "Facet")

confmat_affectiva_3 <- confusionMatrix(data = respdata_max_3_aff$EmotionMeasure,
                                       reference = respdata_max_3_aff$StimulusValence,
                                       mode = "prec_recall")
confmat_facet_3 <- confusionMatrix(data = respdata_max_3_facet$EmotionMeasure,
                                   reference = respdata_max_3_facet$StimulusValence,
                                   mode = "prec_recall")


###--------------------------------------------------###
# Method 4:
# ---------
# 
# Match dataset validated emotions with emotion measures
# RafD only
###--------------------------------------------------###
respdata_max_4_aff <- respdata_max_4 %>% filter(Algorithm == "Affectiva")
respdata_max_4_facet <- respdata_max_4 %>% filter(Algorithm == "Facet")

confmat_affectiva_4 <- confusionMatrix(data = respdata_max_4_aff$EmotionMeasure,
                                       reference = respdata_max_4_aff$StimulusName,
                                       mode = "prec_recall")
confmat_facet_4 <- confusionMatrix(data = respdata_max_4_facet$EmotionMeasure,
                                   reference = respdata_max_4_facet$StimulusName,
                                   mode = "prec_recall")



### Save with matching suffix (none or _nb)
# table first
write.table(t(confmat_affectiva_2$table), file = paste0('../data/output/confmat_affectiva_2', ifelse(baselined, '', '_nb'), '.csv'),
            sep = ';', dec = '.', row.names = TRUE, col.names = TRUE, quote = TRUE)
write.table(t(confmat_affectiva_3$table), file = paste0('../data/output/confmat_affectiva_3', ifelse(baselined, '', '_nb'), '.csv'),
            sep = ';', dec = '.', row.names = TRUE, col.names = TRUE, quote = FALSE)
write.table(t(confmat_affectiva_4$table), file = paste0('../data/output/confmat_affectiva_4', ifelse(baselined, '', '_nb'), '.csv'),
            sep = ';', dec = '.', row.names = TRUE, col.names = TRUE, quote = FALSE)

write.table(t(confmat_facet_2$table), file = paste0('../data/output/confmat_facet_2', ifelse(baselined, '', '_nb'), '.csv'),
            sep = ';', dec = '.', row.names = TRUE, col.names = TRUE, quote = FALSE)
write.table(t(confmat_facet_3$table), file = paste0('../data/output/confmat_facet_3', ifelse(baselined, '', '_nb'), '.csv'),
            sep = ';', dec = '.', row.names = TRUE, col.names = TRUE, quote = FALSE)
write.table(t(confmat_facet_4$table), file = paste0('../data/output/confmat_facet_4', ifelse(baselined, '', '_nb'), '.csv'),
            sep = ';', dec = '.', row.names = TRUE, col.names = TRUE, quote = FALSE)

# overall second
write.table(confmat_affectiva_2$overall, file = paste0('../data/output/confmat_affectiva_2_overall', ifelse(baselined, '', '_nb'), '.csv'),
            sep = ';', dec = '.', row.names = TRUE, col.names = TRUE, quote = TRUE)
write.table(confmat_affectiva_3$overall, file = paste0('../data/output/confmat_affectiva_3_overall', ifelse(baselined, '', '_nb'), '.csv'),
            sep = ';', dec = '.', row.names = TRUE, col.names = TRUE, quote = FALSE)
write.table(confmat_affectiva_4$overall, file = paste0('../data/output/confmat_affectiva_4_overall', ifelse(baselined, '', '_nb'), '.csv'),
            sep = ';', dec = '.', row.names = TRUE, col.names = TRUE, quote = FALSE)

write.table(confmat_facet_2$overall, file = paste0('../data/output/confmat_facet_2_overall', ifelse(baselined, '', '_nb'), '.csv'),
            sep = ';', dec = '.', row.names = TRUE, col.names = TRUE, quote = FALSE)
write.table(confmat_facet_3$overall, file = paste0('../data/output/confmat_facet_3_overall', ifelse(baselined, '', '_nb'), '.csv'),
            sep = ';', dec = '.', row.names = TRUE, col.names = TRUE, quote = FALSE)
write.table(confmat_facet_4$overall, file = paste0('../data/output/confmat_facet_4_overall', ifelse(baselined, '', '_nb'), '.csv'),
            sep = ';', dec = '.', row.names = TRUE, col.names = TRUE, quote = FALSE)

# by class third
write.table(as.table(confmat_affectiva_2$byClass), file = paste0('../data/output/confmat_affectiva_2_byClass', ifelse(baselined, '', '_nb'), '.csv'),
            sep = ';', dec = '.', row.names = TRUE, col.names = TRUE, quote = TRUE)
write.table(as.table(confmat_affectiva_3$byClass), file = paste0('../data/output/confmat_affectiva_3_byClass', ifelse(baselined, '', '_nb'), '.csv'),
            sep = ';', dec = '.', row.names = TRUE, col.names = TRUE, quote = TRUE)
write.table(as.table(confmat_affectiva_4$byClass), file = paste0('../data/output/confmat_affectiva_4_byClass', ifelse(baselined, '', '_nb'), '.csv'),
            sep = ';', dec = '.', row.names = TRUE, col.names = TRUE, quote = TRUE)

write.table(as.table(confmat_facet_2$byClass), file = paste0('../data/output/confmat_facet_2_byClass', ifelse(baselined, '', '_nb'), '.csv'),
            sep = ';', dec = '.', row.names = TRUE, col.names = TRUE, quote = FALSE)
write.table(as.table(confmat_facet_3$byClass), file = paste0('../data/output/confmat_facet_3_byClass', ifelse(baselined, '', '_nb'), '.csv'),
            sep = ';', dec = '.', row.names = TRUE, col.names = TRUE, quote = FALSE)
write.table(as.table(confmat_facet_4$byClass), file = paste0('../data/output/confmat_facet_4_byClass', ifelse(baselined, '', '_nb'), '.csv'),
            sep = ';', dec = '.', row.names = TRUE, col.names = TRUE, quote = FALSE)


save(confmat_affectiva_2, file = paste0(file = '../data/confmat_affectiva_2', ifelse(baselined, '', '_nb'),'.Rdata'))
save(confmat_affectiva_3, file = paste0(file = '../data/confmat_affectiva_3', ifelse(baselined, '', '_nb'),'.Rdata'))
save(confmat_affectiva_4, file = paste0(file = '../data/confmat_affectiva_4', ifelse(baselined, '', '_nb'),'.Rdata'))

save(confmat_facet_2, file = paste0(file = '../data/confmat_facet_2', ifelse(baselined, '', '_nb'),'.Rdata'))
save(confmat_facet_3, file = paste0(file = '../data/confmat_facet_3', ifelse(baselined, '', '_nb'),'.Rdata'))
save(confmat_facet_4, file = paste0(file = '../data/confmat_facet_4', ifelse(baselined, '', '_nb'),'.Rdata'))
