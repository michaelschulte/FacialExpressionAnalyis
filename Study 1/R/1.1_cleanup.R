# Facial Expression Analysis with AFFDEX and FACET: A Validation Study
# Authors:
# Michael Schulte-Mecklenbeck, Sabrina Stöckli, Stefan Borer

# clean slate
rm(list = ls())

# load libraries
source('R_Packages+OwnFunctions.R')
source('iMotionsHelperFunctions.R')

load(file = '../data/rawdata.Rdata')
load(file = '../data/respdata.Rdata')


rawdata <- arrange(rawdata, Name, Timestamp)

# Frame ranges for Stimuli 1-10,
# Duration: 5s at 30 fps -> 150 frames
# each range 90 frames long: 150 - 2x30 (start and end)
ranges_c <- c(30:120, 180:270, 330:420, 480:570, 630:720,
              780:870, 930:1020, 1080:1170, 1230:1320, 1380:1470)
ranges <- list(30:120, 180:270, 330:420, 480:570, 630:720,
               780:870, 930:1020, 1080:1170, 1230:1320, 1380:1470)

# check frame numbers
framesPerResp <- rawdata %>%
  group_by(Name) %>%
  summarise(maxFrame = max(FrameIndex))

# time col -> indicates stimulus number
osdata <- rawdata %>%
  group_by(Name) %>%
  filter(FrameIndex %in% ranges_c) %>%
  mutate(time = ifelse(FrameIndex %in% ranges[[1]], 1,
                ifelse(FrameIndex %in% ranges[[2]], 2,
                ifelse(FrameIndex %in% ranges[[3]], 3,
                ifelse(FrameIndex %in% ranges[[4]], 4,
                ifelse(FrameIndex %in% ranges[[5]], 5,
                ifelse(FrameIndex %in% ranges[[6]], 6,
                ifelse(FrameIndex %in% ranges[[7]], 7,
                ifelse(FrameIndex %in% ranges[[8]], 8,
                ifelse(FrameIndex %in% ranges[[9]], 9,
                ifelse(FrameIndex %in% ranges[[10]], 10, 999))))))))))) %>%
  select(-StimulusName) # remove column

# Merge the names of the emotions to the osdata file
osdata <- merge(osdata, respdata, by = c("Name","time"), all.x = TRUE)


### arrange and rename columns ###
keepCols <- c("StudyName", "Name", "StimulusName", "Dataset", "time", "Timestamp",
              "Number.of.faces", emotionColsOriginalAffectiva(),
              "NoOfFaces", emotionColsOriginalFacet())
osdata <- osdata[keepCols]

# short emotion names
colnames(osdata)[which(colnames(osdata) %in% c(emotionColsOriginalAffectiva(),emotionColsOriginalFacet()))] <-
  c(emotionColsAffectiva(),emotionColsFacet())
colnames(osdata)[which(colnames(osdata) %in% c("Number.of.faces", "NoOfFaces"))] <-
  c("NoFaces.Affectiva", "NoFaces.Facet")


### Split into dataset files
adfes <- osdata %>% filter(Dataset == "ADFES")
rafd <- osdata %>% filter(Dataset == "RAFD")
wsefep <- osdata %>% filter(Dataset == "WSEFEP")

# filter neutral stimuli
adfes_neutral <- adfes %>% filter(StimulusName == "Neutral")
rafd_neutral <- rafd %>% filter(StimulusName == "neutral")
wsefep_neutral <- wsefep %>% filter(StimulusName == "neutral")

# save
save(adfes, file = '../data/adfes.Rdata')
save(rafd, file = '../data/rafd.Rdata')
save(wsefep, file = '../data/wsefep.Rdata')
save(adfes_neutral, file = '../data/adfes_neutral.Rdata')
save(rafd_neutral, file = '../data/rafd_neutral.Rdata')
save(wsefep_neutral, file = '../data/wsefep_neutral.Rdata')