# Facial Expression Analysis with AFFDEX and FACET: A Validation Study
# Authors:
# Michael Schulte-Mecklenbeck, Sabrina Stöckli, Stefan Borer

# clean slate
rm(list = ls())
source('R_Packages+OwnFunctions.R')
source('iMotionsHelperFunctions.R')

### Data import from csv files
# get list of all data files
filelist <- list.files(path="../data/rawdata/", full.names = TRUE)

# load the first file -> data frame has right number of columns
rawdata <- read.csv(file=filelist[1], header = TRUE, sep = '\t', blank.lines.skip = TRUE, stringsAsFactors=FALSE, encoding = "UTF-8", skip=5)

# load all the remaining files, merge them
for (file in filelist[2:length(filelist)]) {
  tempdata <- read.csv(file=file, header = TRUE, sep = '\t', blank.lines.skip = TRUE, stringsAsFactors=FALSE, encoding = "UTF-8", skip=5)
  rawdata <- rbind(rawdata, tempdata)
}


### Load respondents data (containing expression info = StimulusName)
wsefep <- read.csv(file = "../data/respdata/WSEFEP_Data.csv", header = TRUE, sep = ';', blank.lines.skip = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8")
adfes <- read.csv(file = "../data/respdata/Resp_stim_ADFES.csv", header = FALSE, sep = ';', blank.lines.skip = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8")
rafd <- read.csv(file = "../data/respdata/Resp_stim_RafD.csv", header = FALSE, sep = ';', blank.lines.skip = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8")

# convert to long format
adfes <- reshape(adfes, varying = c("V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11"), v.names = "StimulusName",
                 direction = "long")
rafd <- reshape(rafd, varying = c("V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9"), v.names = "StimulusName",
                direction = "long")

# order, rename/remove cols, remove empty rows
adfes <- adfes %>% arrange(V1) %>% rename(Name = V1) %>% select(-id) %>% mutate(Dataset = "ADFES") %>% filter(!StimulusName == "") 
rafd <- rafd   %>% arrange(V1) %>% rename(Name = V1) %>% select(-id) %>% mutate(Dataset = "RAFD")

# wsefep is long already, add time column manually
wsefep <- wsefep %>%
  group_by(Displayer.ID) %>% mutate(time = 1:7) %>%
  rename(Name = Displayer.ID, StimulusName = Display) %>%
  select(Name, time, StimulusName) %>% mutate(Dataset = "WSEFEP") %>%
  as.data.frame(.) 


### Regex on the StimulusName, merge into respdata
adfes_regex <- '^[[:alnum:]]{3}-([[:alnum:]]*)[[:punct:]]'  # ex: "F01-Anger-Apex.jpg"
rafd_regex  <- '[[:alnum:]_]{4}([[:alnum:]]*)_frontal.jpg'  # ex: "Rafd090_27_Caucasian_female_surprised_frontal.jpg"
adfes <- adfes %>% mutate(StimulusName = stringr::str_match(StimulusName, adfes_regex)[,2])
rafd <- rafd %>% mutate(StimulusName = stringr::str_match(StimulusName, rafd_regex)[,2])

respdata <- rbind(adfes, rafd, wsefep)

save(rawdata, file = '../data/rawdata.Rdata')
save(respdata, file = '../data/respdata.Rdata')