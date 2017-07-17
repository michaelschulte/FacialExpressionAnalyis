# Facial Expression Analysis with AFFDEX and FACET: A Validation Study
# Authors:
# Michael Schulte-Mecklenbeck, Sabrina Stöckli, Stefan Borer

# clean slate
rm(list = ls())

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

# load survey data
surveydata <- read.csv(file="../data/surveydata/Validierungsstudie_Analysis - MERGED_SURVEY_RESPONSE_MATRIX.txt",
                       sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8", skip = 108)

# save
save(rawdata, file = '../data/rawdata.Rdata')
save(surveydata, file = '../data/surveydata.Rdata')