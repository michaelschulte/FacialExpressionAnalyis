# Facial Expression Analysis with AFFDEX and FACET: A Validation Study
# Authors:
# Michael Schulte-Mecklenbeck, Sabrina Stöckli, Stefan Borer

# clean slate
rm(list = ls())
source('R_Packages+OwnFunctions.R')
source('iMotionsHelperFunctions.R')

load(file = '../data/surveydata.Rdata')

### Ratings
# grab wanted columns, aggregate
iaps_cols <- grep("VALUE\\.SIAPS.*", colnames(surveydata))
iaps <- colnames(surveydata)[iaps_cols]
gaped_cols <- grep("VALUE\\.SG.*", colnames(surveydata))
gaped <- colnames(surveydata)[gaped_cols]

ratings <- surveydata[,c("RESPONDENT", iaps, gaped)]
colnames(ratings) <- sub("VALUE\\.(.*)","\\1",colnames(ratings))

# long format
ratings <- ratings %>% 
  gather(Measure, Value, -RESPONDENT) %>%
  separate(col = Measure, into = c("Image", "Throwaway", "Scale")) %>%
  select(-Throwaway) %>%
  rename("Name" = RESPONDENT,
         "Score" = Value)

# recode according to iMotions survey
ratings$Scale <- recode(ratings$Scale,
                        `01` = "Joy",     # Freude
                        `02` = "Anger",   # Wut
                        `03` = "Disgust", # Ekel
                        `04` = "Fear",    # Angst
                        `05` = "Contempt",# Verachtung
                        `06` = "Sadness", # Traurigkeit
                        `07` = "Surprise",# Überraschung
                        `08` = "Valence") # Emotionale Wertigkeit

# strip leading 'S' in Image col
ratings$Image <- sub("S(.*)", "\\1", ratings$Image)


### Respondent info
info_cols <- c("RESPONDENT", "GENDER", "AGE", "TEXT.SUSP_SUSP", "LABELVALUE_EDU_EDU.Question1.EDU")
resp_info <- surveydata[, info_cols]

# save
save(ratings, file = '../data/ratings.Rdata')
save(resp_info, file = '../data/resp_info.Rdata')