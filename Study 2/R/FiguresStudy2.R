# Facial Expression Analysis with AFFDEX and FACET: A Validation Study
# Authors:
# Michael Schulte-Mecklenbeck, Sabrina Stöckli, Stefan Borer

# clean slate
rm(list = ls())
source('iMotionsHelperFunctions.R')
source('R_Packages+OwnFunctions.R')

# load df for figure 3
df_f2 <-  read.csv(file = ('../data/output/algorithmwise_classified_4.csv'), sep = ';')

df_f2 <- df_f2 %>%
  #filter(!(StimulusName == 'Contempt')) %>%
  mutate(Algorithm = case_when(
    Algorithm == "Affectiva" ~ "AFFDEX",
    Algorithm == "Facet" ~ "FACET"))

df_f2$StimulusName <- factor(df_f2$StimulusName,
                             levels = c('Surprise', 'Sadness', 'Joy', 'Fear', 'Disgust','Contempt','Anger'),
                             ordered = TRUE)
levels(df_f2$StimulusName)[levels(df_f2$StimulusName)=="Joy"] <- "Happiness"

#  plot
ggplot(aes(x = StimulusName, y = MS, fill = Algorithm), data = df_f2) + 
  geom_bar(stat = 'identity', position = 'dodge') + 
  coord_flip() +
  scale_fill_brewer(palette = 'Oranges', guide = guide_legend(title = "Algorithm"),
                    breaks = c("FACET", "AFFDEX"),
                    labels = c("FACET", "AFFDEX")) +
  # scale_fill_brewer(palette = 'Blues', guide = guide_legend(title = "Algorithm")) +
  theme_bw() +
  ylab('Matching Score') +
  xlab('Emotion')

ggsave(file = '../data/figures/Figure2.pdf')


# load df for figure D1
df_d1 <-  read.csv(file = ('../data/output/algorithmwise_classified_4_nb.csv'), sep = ';')

df_d1 <- df_d1 %>%
  #filter(!(StimulusName == 'Contempt')) %>%
  mutate(Algorithm = case_when(
    Algorithm == "Affectiva" ~ "AFFDEX",
    Algorithm == "Facet" ~ "FACET"))

df_d1$StimulusName <- factor(df_d1$StimulusName, 
                             levels = c('Surprise', 'Sadness', 'Joy', 'Fear', 'Disgust','Contempt' ,'Anger'),
                             ordered = TRUE)
levels(df_d1$StimulusName)[levels(df_d1$StimulusName)=="Joy"] <- "Happiness"

#  plot
ggplot(aes(x = StimulusName, y = MS, fill = Algorithm), data = df_d1) + 
  geom_bar(stat = 'identity', position = 'dodge') + 
  coord_flip() +
  scale_fill_brewer(palette = 'Blues', guide = guide_legend(title = "Algorithm"),
                    breaks = c("FACET", "AFFDEX"),
                    labels = c("FACET", "AFFDEX")) +
  # scale_fill_brewer(palette = 'Blues', guide = guide_legend(title = "Algorithm")) +
  theme_bw() +
  ylab('Matching Score') +
  xlab('Emotion')


ggsave(file = '../data/figures/FigureD1.pdf')
