# clean slate
rm(list = ls())
source('R_Packages+OwnFunctions.R')
source('iMotionsHelperFunctions.R')
#library(plotflow)
#library(cowplot)

load('../data/idata_clean.Rdata')
load('../data/idata_baselined_median.Rdata')

# get median over all respondents for each frame
summary_nb <- idata %>%
    group_by(StimulusName, FrameNo.Affectiva) %>%
    summarise(Anger.Affectiva=median(Anger.Affectiva, na.rm = TRUE), 
              Sadness.Affectiva=median(Sadness.Affectiva, na.rm = TRUE),
              Disgust.Affectiva=median(Disgust.Affectiva, na.rm = TRUE),
              Joy.Affectiva=median(Joy.Affectiva, na.rm = TRUE),
              Surprise.Affectiva=median(Surprise.Affectiva, na.rm = TRUE),
              Fear.Affectiva=median(Fear.Affectiva, na.rm = TRUE),
              Contempt.Affectiva=median(Contempt.Affectiva, na.rm = TRUE),
              Joy.Facet=median(Joy.Facet, na.rm = TRUE),
              Anger.Facet=median(Anger.Facet, na.rm = TRUE),
              Surprise.Facet=median(Surprise.Facet, na.rm = TRUE),
              Fear.Facet=median(Fear.Facet, na.rm = TRUE),
              Contempt.Facet=median(Contempt.Facet, na.rm = TRUE),
              Disgust.Facet=median(Disgust.Facet, na.rm = TRUE),
              Sadness.Facet=median(Sadness.Facet, na.rm = TRUE)) %>%
    mutate(Baseline = "Non_Baselined")

summary_b <- idata_baselined_median %>%
    group_by(StimulusName, FrameNo.Affectiva) %>%
    summarise(Anger.Affectiva=median(Anger.Affectiva, na.rm = TRUE), 
              Sadness.Affectiva=median(Sadness.Affectiva, na.rm = TRUE),
              Disgust.Affectiva=median(Disgust.Affectiva, na.rm = TRUE),
              Joy.Affectiva=median(Joy.Affectiva, na.rm = TRUE),
              Surprise.Affectiva=median(Surprise.Affectiva, na.rm = TRUE),
              Fear.Affectiva=median(Fear.Affectiva, na.rm = TRUE),
              Contempt.Affectiva=median(Contempt.Affectiva, na.rm = TRUE),
              Joy.Facet=median(Joy.Facet, na.rm = TRUE),
              Anger.Facet=median(Anger.Facet, na.rm = TRUE),
              Surprise.Facet=median(Surprise.Facet, na.rm = TRUE),
              Fear.Facet=median(Fear.Facet, na.rm = TRUE),
              Contempt.Facet=median(Contempt.Facet, na.rm = TRUE),
              Disgust.Facet=median(Disgust.Facet, na.rm = TRUE),
              Sadness.Facet=median(Sadness.Facet, na.rm = TRUE)) %>%
    mutate(Baseline = "Baselined")

### aggregate summaries for all AVs (without Positive, Negative)
emo_measures <- colnames(summary_nb)[3:16]
stimuli <- unique(summary_nb$StimulusName)
plots <- list()

for(stim in stimuli) {
    plots[[stim]] <- list()
    for(emo in emo_measures) {
        
        temp <- summary_nb %>% 
            filter_(~StimulusName == stim) %>%
            select_(~StimulusName, ~Baseline, ~FrameNo.Affectiva, emo)
        temp2 <- summary_b %>% 
            filter_(~StimulusName == stim) %>%
            select_(~StimulusName, ~Baseline, ~FrameNo.Affectiva, emo)
        
        
        # plot
        plots[[stim]][[emo]] <-
            ggplot(rbind(temp, temp2), aes_string(x = "FrameNo.Affectiva", y = emo, colour = "Baseline")) +
            geom_line() +
            geom_line(data=temp2) +
            theme_bw() +
            #theme_apa() +
            ggtitle(paste0("Stimulus:", stim)) +
            facet_grid(Baseline~., scales = "free_y", shrink = T)
        #print(plotDha[[emo]])
        
        # save plot
        ggsave(paste0(stim,"_",emo,".png"), plot = plots[[stim]][[emo]], path = "../plots_nonbaselined/")
    }
}

