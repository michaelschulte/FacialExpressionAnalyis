# Facial Expression Analysis with AFFDEX and FACET: A Validation Study
# Authors:
# Michael Schulte-Mecklenbeck, Sabrina Stöckli, Stefan Borer

# clean slate
rm(list = ls())
source('iMotionsHelperFunctions.R')
source('R_Packages+OwnFunctions.R')

baselined = F
# load (non-)baselined data
if(baselined) {
  load(file = '../data/adfes_baselined.Rdata')
  load(file = '../data/rafd_baselined.Rdata')
  load(file = '../data/wsefep_baselined.Rdata')
  adfes <- adfes_baselined
  rafd <- rafd_baselined
  wsefep <- wsefep_baselined
  rm(adfes_baselined, rafd_baselined, wsefep_baselined)
} else {
  load(file = '../data/adfes.Rdata')
  load(file = '../data/rafd.Rdata')
  load(file = '../data/wsefep.Rdata')
}

adfes <- adfes %>% 
  group_by(Name, StimulusName) %>%
  mutate(FrameNo.os = seq(0,length(StimulusName)-1))

rafd <- rafd %>% 
  group_by(Name, StimulusName) %>%
  mutate(FrameNo.os = seq(0,length(StimulusName)-1))

wsefep <- wsefep %>% 
  group_by(Name, StimulusName) %>%
  mutate(FrameNo.os = seq(0,length(StimulusName)-1))

# save according to baseliend flag
save(adfes, file = paste0('../data/adfes', ifelse(baselined, '_baselined', '_nb'), '.Rdata'))
save(rafd, file = paste0('../data/rafd', ifelse(baselined, '_baselined', '_nb'), '.Rdata'))
save(wsefep, file = paste0('../data/wsefep', ifelse(baselined, '_baselined', '_nb'), '.Rdata'))