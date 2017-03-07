### extract baseline values from 5th line of Sensor Data files
# return new dataframe or rbind to existing (if available)
iMotionsBaseline <- function(filename, respName, dataframe=NULL) {
  
  connection = file(filename)
  line <- readLines(con=connection, n=5)[5]
  close(connection)
  
  # remove first part of line
  temp <- substr(line,19,nchar(line))
  temp <- unlist(strsplit(temp, ", ", fixed=TRUE))
  
  # match first parenthesis: emotion, second parenthesis: value
  pattern <- "([[:alpha:]]+)=\\((.*)\\)"
  
  # get emotion and value separated by space character
  baseline <- sub(pattern,"\\1 \\2",temp)
  emotions <- sub(pattern,"\\1",temp)
  values <- sub(pattern,"\\2",temp)
  
  # separate emotion and value
  baseline <- unlist(strsplit(baseline," "))
  
  if (is.null(dataframe)) {
    temp <- data.frame()
    temp <- rbind(temp,append(c(respName),values))
    colnames(temp) <- append(c("Name"), emotions)
  } else {
    temp <- data.frame()
    temp <- rbind(temp,append(respName,values))
    colnames(temp) <- append(c("Name"), emotions)
    temp <- rbind(dataframe, temp[1,])
  }
  return(temp)
}

### extract baseline values from custom StimulusName entries in Sensor Data files
# return new dataframe or rbind to existing (if available)
iMotionsBaselineCustom <- function(dataframe, stimulusName, baselinedata=NULL) {
  
  emotions <- emotionColsLong()
  
  # get rows
  temp <- dataframe[which(dataframe$StimulusName == "BASELINE_1"), emotions]
  values <- colMeans(na.omit(temp))
  respName <- dataframe$Name[1]
  
  if (is.null(baselinedata)) {
    temp <- data.frame()
    temp <- rbind(temp,append(c(respName), values))
    colnames(temp) <- append(c("Name"), emotions)
  } else {
    temp <- data.frame()
    temp <- rbind(temp,append(respName, values))
    colnames(temp) <- append(c("Name"), emotions)
    temp <- rbind(baselinedata, temp[1,])
  }
  return(temp)
}

# convert Evidence <-> Probability (Intensity)
EvidenceToProbability <- function(data) {
  return (1/(1+10^(-data)))
}
ProbabilityToEvidence <- function(data) {
  return (-log10((1/data) - 1))
}

### subtract baseline from original values
ApplyIMotionsBaseline <- function(nonBaselinedEvidence, baselineAverageEvidence, returnEvidenceValue=FALSE) {
  
  baselineAverageProbability = EvidenceToProbability(baselineAverageEvidence)
  nonBaselinedProbability = EvidenceToProbability(nonBaselinedEvidence)
  baselinedProbability = nonBaselinedProbability - baselineAverageProbability
  
  # Keep a lid on the max unlikeliness value. 
  if (!is.na(baselinedProbability) && baselinedProbability < 0.0001) 
    baselinedProbability = 0.0001
  
  returnValue = baselinedProbability
  
  # Check if we are building an evidence graph 
  if (returnEvidenceValue)
    returnValue = ProbabilityToEvidence(returnValue)
  
  return(returnValue)
}

ApplyBaselineEvidence <- function(nonBaselinedEvidence, baselineAverageEvidence) {
  baselinedEvidence <- nonBaselinedEvidence - baselineAverageEvidence
  
  return(baselinedEvidence)
}

# original emotion columns
emotionColsLong <- function() {
  return( c("Joy.Evidence", "Anger.Evidence", "Surprise.Evidence", "Fear.Evidence", "Contempt.Evidence",
            "Disgust.Evidence", "Sadness.Evidence", "Neutral.Evidence", "Positive.Evidence", "Negative.Evidence"))
}

emotionColsOriginalAffectiva <- function() {
  return(c("Anger", "Sadness", "Disgust", "Joy", "Surprise", "Fear", "Contempt"))
}

# customized emotion columns
emotionCols <- function() {
  return(c("Joy", "Anger", "Surprise", "Fear", "Contempt", "Disgust",
    "Sadness", "Neutral", "Positive", "Negative"))
}

emotionColsFacet <- function() {
  return(paste0(emotionCols(),".Facet"))
}

emotionColsAffectiva <- function() {
  return(paste0(emotionColsOriginalAffectiva(),".Affectiva"))
}


### Aliases for backwards compatibility
iMotionsBaselineCustomFacet <- iMotionsBaselineCustom
emotionColsOriginalFacet <- emotionColsLong
