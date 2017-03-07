# Facial Expression Analysis with AFFDEX and FACET: A Validation Study
# Authors:
# Michael Schulte-Mecklenbeck, Sabrina Stöckli, Stefan Borer

# clean slate
rm(list = ls())
source('R_Packages+OwnFunctions.R')
source('iMotionsHelperFunctions.R')

load(file = '../data/idata.Rdata')

# keep only interesting stimuli:
# Mimicry, IAPS, GAPED, Instructed (RafD)
keepStim <- grep("^FixM|^FixIAPS|^FixG|^FixSM|^M|^IAPS|^G|^SM", unique(idata$StimulusName), value=TRUE)
idata <- idata %>% filter(StimulusName %in% keepStim)

# split fixation data
idata_fixation <- idata %>% filter(grepl("Fix",StimulusName))
idata <- idata %>% filter(!grepl("Fix", StimulusName))


# grab only rows where FrameNo in between `startFrame` and `endFrame`
startFrame <- 0
endFrame <- 176
idata <- idata[which(idata$FrameNo.Facet >= startFrame & idata$FrameNo.Facet <= endFrame &
                     idata$FrameNo.Affectiva >= startFrame & idata$FrameNo.Affectiva <= endFrame),]


###### TROUBLESHOOTING ######
### search for errors in FrameNo / FrameIndex
names <- unique(idata$Name)
rownames(idata) <- NULL

count <- 0
for (resp in names) {
  temp <- filter(idata, Name == resp)
  print(paste("### Respondent:", resp))
  for (stim in unique(temp$StimulusName)) {
    index <- filter(temp, StimulusName == stim)[["FrameNo.Facet"]]
    if (length(index) != endFrame + 1 || sum(index != startFrame:endFrame) != 0) {
      print(stim)
      cat(paste("Start:", index[1], "\t|| End:", index[length(index)], "\t|| Length:", length(index)), "\n")
      count <- count + 1
    }
  }
}
print(paste0("FACET: Frames in ", startFrame, ":", endFrame, " Total Errors: ", count))

### check for missing faces
idata_missing_faces <- idata %>%
  bind_rows(idata_fixation) %>%
  group_by(Name, StimulusName) %>%
  summarise(NoFaces.Affectiva = sum(NoFaces.Affectiva),
            NoFaces.Facet = sum(NoFaces.Facet),
            Missing.Affectiva = n() - NoFaces.Affectiva,
            Missing.Facet = n() - NoFaces.Facet) %>%
  
  # set as missing if above 50%
  mutate(Missing = ifelse(Missing.Affectiva/NoFaces.Affectiva > 1 |
                          Missing.Facet/NoFaces.Facet > 1, 1, 0)) %>%
  
  # get Ratio of missing stimuli
  group_by(Name) %>%
  mutate(Ratio = sum(Missing)/n())

# remove respondents with missing stimuli ratio > 0.1:
# 11, 16, 20, 35, 50, 74, 82, 104, 106
exclude <- idata_missing_faces %>%
  filter(Ratio > 0.1) %>%
  distinct(Name, .keep_all=T) %>%
  select(Name, Ratio)

idata <- filter(idata, !(Name %in% exclude$Name))
idata_fixation <- filter(idata_fixation, !(Name %in% exclude$Name))

# reset rownames
rownames(idata) <- NULL
rownames(idata_fixation) <- NULL

# save
save(idata, file = '../data/idata_clean.Rdata')
save(idata_fixation, file = '../data/idata_fixation.Rdata')