# Facial Expression Analysis with AFFDEX and FACET: A Validation Study
# Authors:
# Michael Schulte-Mecklenbeck, Sabrina Stöckli, Stefan Borer

# clean slate
rm(list = ls())
source('R_Packages+OwnFunctions.R')
source('iMotionsHelperFunctions.R')

load(file = '../data/rawdata.Rdata')

# delete unusable bits (3 rows)
rawdata <- rawdata %>% filter(!(Name %in% c(76, 90, 113) & Timestamp %in% c(363414,387218,286345)))

# separte df for OpenSesame and iMotions
osdata <- rawdata[grepl("OpenSesame.*", rawdata$StimulusName),]
rawdata <- rawdata[!grepl("OpenSesame.*", rawdata$StimulusName),]

# go through files, split into osdata and postmarkers
respondents <- unique(osdata$Name)
for (resp in respondents) {
  print(resp)
  start <- which(osdata$Name == resp & osdata$LiveMarker == "Next-Slide") + 1
  end <- tail(which(osdata$Name == resp),1)
  if(is.null(start) | is.null(end)) {
    paste(resp, "- start:", start, "- end:", end)
  }
  if(resp == respondents[1]) {
    postmarkers <- osdata[(start:end),]
  } else {
    postmarkers <- rbind(postmarkers, osdata[(start:end),])
  }
  osdata <- osdata[-((start-1):end),]
  rm(start, end)
}

# sort
postmarkers <- arrange(postmarkers, Name, Timestamp)
osdata <- arrange(osdata, Name, Timestamp)


### TESTING ###
# check for duplicated Timestamps
respondents <- as.character(unique(osdata$Name))
duplicates <- replicate(length(respondents), list())
names(duplicates) <- respondents

dups_df <- NULL
dups_rows <- NULL
count <- 0
for(resp in respondents) {
  temp <- filter(postmarkers, Name == resp)
  timestamps <- unique(temp$Timestamp)
  print(resp)
  for(time in timestamps) {
    indizes <- which(temp$Timestamp == time)
    # print info if there are duplicates
    if(length(indizes) != 1){
      count <- count + 1
      print(paste("Timestamp:", time))
      print(paste("Wrong indizes:", indizes))
      print(temp$PostMarker[indizes])
      duplicates[[resp]] <- append(duplicates[[resp]], time)
      
      # collect rows with empty PostMarker-column
      if(is.null(dups_rows)){
        dups_df <- filter(postmarkers, Name == resp &
                                       Timestamp == time)
        dups_rows <- which(postmarkers$Name == resp &
                           postmarkers$Timestamp == time &
                           postmarkers$PostMarker == '')
    } else {
        dups_df <- rbind(dups_df, filter(postmarkers, Name == resp &
                                                      Timestamp == time))
        dups_rows <- append(dups_rows, which(postmarkers$Name == resp &
                                             postmarkers$Timestamp == time &
                                             postmarkers$PostMarker == ''))
      }
    }
  }
}
print(paste("Total duplicated timestamps:", count))

# remove duplicates
postmarkers <- postmarkers[-dups_rows,]
rm(temp, timestamps, time, indizes, count)

### separate face data from other events
# uniq_events <- unique(osdata$EventSource)
# uniq_stim <- unique(postmarkers$StimulusName)

print(uniq_events)
osdata_misc <- osdata %>%
  filter(grepl("OrientationInput|MouseStimulus|ResultInput|Valence|UI", EventSource)) %>%
  select(Name, StimulusName, PostMarker, Timestamp, EventSource, StimId, Result, Valence.1, Orientation)
postmarkers_misc <- postmarkers %>%
  filter(grepl("OrientationInput|MouseStimulus|ResultInput|Valence|UI", EventSource)) %>%
  select(Name, StimulusName, PostMarker, Timestamp, EventSource, StimId, Result, Valence.1, Orientation)
osdata <- osdata %>%
  filter(grepl("AffRaw|Emotient FACET", EventSource))
postmarkers <- postmarkers %>%
  filter(grepl("AffRaw|Emotient FACET", EventSource))

# # same for osdata
# respondents <- as.character(unique(osdata$Name))
# duplicates <- replicate(length(respondents), list())
# names(duplicates) <- respondents
# 
# for(resp in respondents) {
#     temp <- filter(osdata, Name == resp)
#     timestamps <- unique(temp$Timestamp)
#     print(resp)
#     
#     for(time in timestamps) {
#         indizes <- which(temp$Timestamp == time)
#         # print info if there are duplicates
#         if(length(indizes) != 1){
#             print(paste("Timestamp:", time))
#             print(paste("Wrong indizes:", indizes))
#             print(temp$PostMarker[indizes])
#             duplicates[[resp]] <- append(duplicates[[resp]], time)
#         }
#     }
# }


### check if (osdata-postmarkers) pairs have the same data
# use only Name-Timestamp pairs in postmarkers
osdata <- merge(osdata, postmarkers[c("Name", "Timestamp")])
osdata <- arrange(osdata, Name, Timestamp)
postmarkers <- arrange(postmarkers, Name, Timestamp)

for(resp in respondents) {
  osdata_cpy <- osdata %>% filter(Name == resp) %>% select(Name,Timestamp, Anger:Contempt)
  postmarkers_cpy <- postmarkers %>% filter(Name == resp) %>% select(Name,Timestamp, Anger:Contempt)
  print(resp)
  
  # try naive approach
  if(sum(osdata_cpy != postmarkers_cpy, na.rm = TRUE) > 0) {
    cat("Something is wrong...")
  }
}


### Merge PostMarkers from second half of file
osdata$PostMarker <- NULL
postmarkers <- postmarkers[c("Name", "Timestamp", "PostMarker")]
osdata <- merge(osdata, postmarkers, by = c("Name", "Timestamp"), all.x = TRUE)
osdata <- arrange(osdata, Name, Timestamp)
# check if all went well...
cat("Differences in Postmarker column: ", sum(osdata$PostMarker != postmarkers$PostMarker, na.rm = TRUE))

# separate MouseStimulus, ResultInput, Valence
choice <- postmarkers_misc %>% 
  filter(grepl("MouseStimulus",EventSource)) %>%
  select(Name, PostMarker, Timestamp, EventSource, StimId)

result <- postmarkers_misc %>% 
  filter(grepl("ResultInput",EventSource)) %>%
  select(Name, PostMarker, Timestamp, EventSource, Result)

valence <- postmarkers_misc %>% 
  filter(grepl("Valence",EventSource)) %>%
  select(Name, PostMarker, Timestamp, EventSource, Valence.1)
colnames(valence)[5] <- "Valence"

orientation <- osdata_misc %>% 
  filter(grepl("OrientationInput",EventSource)) %>%
  select(Name, PostMarker, Timestamp, EventSource, Orientation)

rm(postmarkers, postmarkers_misc, osdata_misc, postmarkers_cpy, osdata_cpy,
 duplicates, resp, respondents)

# reduce to only one (i.e. the second) entry in Postmarker column
osdata[which(grepl(",", osdata$PostMarker)), "PostMarker"] <-
  sub(".*,(.*)","\\1",filter(osdata, grepl(",", PostMarker))$PostMarker)

### Further cleaning
keepCols <- c("StudyName", "Name", "EventSource", "Timestamp",
              "MediaTime", "PostMarker",
              "Number.of.faces", "FrameIndex", emotionColsOriginalAffectiva(), # AffRaw
              "NoOfFaces", "FrameNo", "FrameTime", emotionColsOriginalFacet()) # FACET

rawdata <- rawdata[append("StimulusName", keepCols)]
osdata <- osdata[keepCols]


### rename columns
# short emotion names
colnames(rawdata)[which(colnames(rawdata) %in% 
                        c(emotionColsOriginalAffectiva(),emotionColsOriginalFacet()))] <-
  c(emotionColsAffectiva(),emotionColsFacet())
colnames(osdata)[which(colnames(osdata) %in% 
                       c(emotionColsOriginalAffectiva(),emotionColsOriginalFacet()))] <-
  c(emotionColsAffectiva(),emotionColsFacet())

colnames(rawdata)[colnames(rawdata) %in% c("Number.of.faces", "FrameIndex","NoOfFaces", "FrameNo")] <- 
  c("NoFaces.Affectiva", "FrameNo.Affectiva", "NoFaces.Facet", "FrameNo.Facet")
colnames(osdata)[colnames(osdata) %in% c("Number.of.faces", "FrameIndex","NoOfFaces", "FrameNo")] <- 
  c("NoFaces.Affectiva", "FrameNo.Affectiva", "NoFaces.Facet", "FrameNo.Facet")


### separate fixations and stimuli, split into columns "Marker", "Gamble" ###
osdata_words <- osdata %>%
  filter(PostMarker %in% c("Angst", "Wut", "Überraschung", "Freude", "Ekel", "Wut", "Angst", "Traurigkeit"))
osdata <- osdata %>%
  filter(!PostMarker %in% c("Angst", "Wut", "Überraschung", "Freude", "Ekel", "Wut", "Angst", "Traurigkeit"))

osdata <- osdata %>%
  tidyr::separate(PostMarker, into = c("Marker", "Gamble"), sep = "_")

osdata_fixation <- osdata %>%
  filter(grepl("Fix", Marker))
osdata <- osdata %>%
  filter(!grepl("Fix", Marker))

# reset rownames
rownames(rawdata) <- NULL
rownames(osdata) <- NULL

idata <- rawdata 

# save
save(idata, file = '../data/idata.Rdata')
save(osdata, file = '../data/osdata.Rdata')
save(orientation, file = '../data/os-orientation.Rdata')
save(choice, file = '../data/os-choice.Rdata')
save(result, file = '../data/os-result.Rdata')
save(valence, file = '../data/os-valence.Rdata')
save(osdata_words, file = '../data/osdata_words.Rdata')
save(osdata_fixation, file = '../data/osdata_fixation.Rdata')