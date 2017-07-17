# clean slate
rm(list = ls())
source('R_Packages+OwnFunctions.R')
source('iMotionsHelperFunctions.R')
#library(plotflow)
#library(cowplot)

load('../data/osdata_baselined_mean.Rdata')
load('../data/osdata_baselined_median.Rdata')
load('../data/idata_baselined_mean.Rdata')
load('../data/idata_baselined_median.Rdata')

# prepare some lists...
dfs <- list(idata_baselined_mean,idata_baselined_median,
            osdata_baselined_mean,osdata_baselined_median)
summaries <- list()
summaries[[1]] <- list()  # idata mean
summaries[[2]] <- list()  # idata median
summaries[[3]] <- list()  # osdata mean
summaries[[4]] <- list()  # osdata median

emotions <- c(emotionColsAffectiva(),emotionColsFacet())

for(i in 1:4) {
  # delete unnecessary baselines and FrameNos (>46)
  # dfs[[i]] <- dfs[[i]][-which(substr(dfs[[i]]$StimulusName,1,8) == "BASELINE"),]
  # dfs[[i]] <- dfs[[i]][which(dfs[[i]]$FrameNo %in% 0:46),]
  # remove column NewNames
  # if("NewNames" %in% colnames(dfs[[i]])) {
  #   dfs[[i]]$NewNames <- NULL
  # }
  
  # # split StimulusName into multiple columns
  # dfs[[i]] <- cbind(dfs[[i]],data.frame(Stimulus.Type = substr(dfs[[i]]$StimulusName,1,1)))
  # dfs[[i]] <- cbind(dfs[[i]],data.frame(Stimulus.Sex = substr(dfs[[i]]$StimulusName,2,2)))
  # dfs[[i]] <- cbind(dfs[[i]],data.frame(Stimulus.Emotion = substr(dfs[[i]]$StimulusName,4,5)))
  # 
  # # remove, sort columns
  # dfs[[i]]$StimulusName <- NULL
  # dfs[[i]] <- dfs[[i]][,c(1,25:27,2:24)]
  
  for(emotion in emotions) {
      if(i <= 2) { #idata
          summaries[[i]][[emotion]] <- summarySE(dfs[[i]],
                                                 measurevar=emotion,
                                                 groupvars=c("FrameNo.Facet", "StimulusName"),
                                                 na.rm=TRUE)
      } else { #osdata
          summaries[[i]][[emotion]] <- summarySE(dfs[[i]],
                                                 measurevar=emotion,
                                                 groupvars=c("FrameNo.os", "Marker", "Gamble"),
                                                 na.rm=TRUE)
      }
  }
  # # summaries: create distinct data frames with only needed variables (dv)
  # summaries[[i]][['Joy']]      <- summarySE(dfs[[i]], measurevar='Joy', groupvars=c('FrameNo', 'Stimulus.Type', 'Stimulus.Emotion'), na.rm=TRUE)
  # summaries[[i]][['Anger']]    <- summarySE(dfs[[i]], measurevar='Anger', groupvars=c('FrameNo', 'Stimulus.Type', 'Stimulus.Emotion'), na.rm=TRUE)
  # summaries[[i]][['Surprise']] <- summarySE(dfs[[i]], measurevar='Surprise', groupvars=c('FrameNo', 'Stimulus.Type', 'Stimulus.Emotion'), na.rm=TRUE)
  # summaries[[i]][['Fear']]     <- summarySE(dfs[[i]], measurevar='Fear', groupvars=c('FrameNo', 'Stimulus.Type', 'Stimulus.Emotion'), na.rm=TRUE)
  # summaries[[i]][['Contempt']] <- summarySE(dfs[[i]], measurevar='Contempt', groupvars=c('FrameNo', 'Stimulus.Type', 'Stimulus.Emotion'), na.rm=TRUE)
  # summaries[[i]][['Disgust']]  <- summarySE(dfs[[i]], measurevar='Disgust', groupvars=c('FrameNo', 'Stimulus.Type', 'Stimulus.Emotion'), na.rm=TRUE)
  # summaries[[i]][['Sadness']]  <- summarySE(dfs[[i]], measurevar='Sadness', groupvars=c('FrameNo', 'Stimulus.Type', 'Stimulus.Emotion'), na.rm=TRUE)
  # summaries[[i]][['Neutral']]  <- summarySE(dfs[[i]], measurevar='Neutral', groupvars=c('FrameNo', 'Stimulus.Type', 'Stimulus.Emotion'), na.rm=TRUE)
  # summaries[[i]][['Negative']] <- summarySE(dfs[[i]], measurevar='Negative', groupvars=c('FrameNo', 'Stimulus.Type', 'Stimulus.Emotion'), na.rm=TRUE)
  # summaries[[i]][['Positive']] <- summarySE(dfs[[i]], measurevar='Positive', groupvars=c('FrameNo', 'Stimulus.Type', 'Stimulus.Emotion'), na.rm=TRUE)
}


# copy back
idata_baselined_mean <- dfs[[1]]
idata_baselined_median <- dfs[[2]]
osdata_baselined_mean <- dfs[[3]]
osdata_baselined_median <- dfs[[4]]

### aggregate summaries for all AVs (without Positive, Negative)
AVs <- emotions
plotsummaries <- list()
stimuli <- unique(idata_baselined_mean$StimulusName)
plots <- list()

for(stim in stimuli) {
  plotsummaries[[stim]] <- list()
  plots[[stim]]         <- list()
  for(emo in emotions) { 
    plotsummaries[[stim]][[emo]] <- rbind(
      cbind(summaries[[1]][[emo]][which(summaries[[1]][[emo]]$StimulusName == stim),],
            data.frame(Baseline = "SimpleMean")),
      cbind(summaries[[2]][[emo]][which(summaries[[2]][[emo]]$StimulusName == stim),],
            data.frame(Baseline = "SimpleMedian")))
    
    # plot
    plots[[stim]][[emo]] <-
      ggplot(plotsummaries[[stim]][[emo]], aes_string(x = "FrameNo.Facet", y = emo, colour = "Baseline")) +
      geom_line(aes(group = Baseline)) +
      theme_bw() +
      #theme_apa() +
      ggtitle(paste0("Bedingung", stim, "AV:", emo))
    #print(plotDha[[emo]])

    # save plot
    ggsave(paste0(stim,"_",emo,".png"), plot = plots[[stim]][[emo]], path = "../plots/")
  }
}
