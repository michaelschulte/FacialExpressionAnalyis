# Facial Expression Analysis with AFFDEX and FACET: A Validation Study
# Authors:
# Michael Schulte-Mecklenbeck, Sabrina Stöckli, Stefan Borer

# clean slate
rm(list = ls())
source('iMotionsHelperFunctions.R')
source('R_Packages+OwnFunctions.R')

load(file = '../data/adfes_baselined.Rdata')
load(file = '../data/rafd_baselined.Rdata')
load(file = '../data/wsefep_baselined.Rdata')

adfes_baselined <- adfes_baselined %>% 
  group_by(Name, StimulusName) %>%
  mutate(FrameNo.os = seq(0,length(StimulusName)-1))

rafd_baselined <- rafd_baselined %>% 
  group_by(Name, StimulusName) %>%
  mutate(FrameNo.os = seq(0,length(StimulusName)-1))

wsefep_baselined <- wsefep_baselined %>% 
  group_by(Name, StimulusName) %>%
  mutate(FrameNo.os = seq(0,length(StimulusName)-1))

#save
save(adfes_baselined, file = '../data/adfes_baselined.Rdata')
save(rafd_baselined, file = '../data/rafd_baselined.Rdata')
save(wsefep_baselined, file = '../data/wsefep_baselined.Rdata')