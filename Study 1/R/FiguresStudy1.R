# Facial Expression Analysis with AFFDEX and FACET: A Validation Study
# Authors:
# Michael Schulte-Mecklenbeck, Sabrina Stöckli, Stefan Borer

# clean slate
rm(list = ls())
source('iMotionsHelperFunctions.R')
source('R_Packages+OwnFunctions.R')

# load df for figure 1
df_f1 <- read.csv(file = ('../data/output/algorithmwise_classified_nb.csv'), sep = ';')


df_f1 <- df_f1 %>%
  filter(!(StimulusName == 'Contempt')) %>%
  mutate(Algorithm = case_when(
    Algorithm == "Affectiva" ~ "AFFDEX",
    Algorithm == "Facet" ~ "FACET"))

df_f1$StimulusName <- factor(df_f1$StimulusName,
                             levels = c('Surprise', 'Sadness', 'Joy', 'Fear', 'Disgust', 'Anger'),
                             ordered = TRUE)
levels(df_f1$StimulusName)[levels(df_f1$StimulusName)=="Joy"] <- "Happiness"
#  plot
ggplot(aes(x = StimulusName, y = MS, fill = Algorithm), data = df_f1) + 
  geom_bar(stat = 'identity', position = 'dodge') + 
  coord_flip() +
  scale_fill_brewer(palette = 'Blues', guide = guide_legend(title = "Algorithm"),
                    breaks = c("FACET", "AFFDEX"),
                    labels = c("FACET", "AFFDEX")) +
  # scale_fill_brewer(palette = 'Blues', guide = guide_legend(title = "Algorithm")) +
  theme_bw() +
  ylab('Matching Score') +
  xlab('Emotion')

ggsave(file = '../data/figures/Figure1.pdf')


# load df for figure C1
df_c1 <-  read.csv(file = ('../data/output/algorithmwise_classified_baselined.csv'), sep = ';')

df_c1 <- df_c1 %>%
  filter(!(StimulusName == 'Contempt')) %>%
  mutate(Algorithm = case_when(
    Algorithm == "Affectiva" ~ "AFFDEX",
    Algorithm == "Facet" ~ "FACET"))

df_c1$StimulusName <- factor(df_c1$StimulusName, levels = c('Surprise', 'Sadness', 'Joy', 'Fear', 'Disgust', 'Anger'))
levels(df_c1$StimulusName)[levels(df_c1$StimulusName)=="Joy"] <- "Happiness"
#  plot
ggplot(aes(x = StimulusName, y = MS, fill = Algorithm), data = df_c1) + 
  geom_bar(stat = 'identity', position = 'dodge') + 
  coord_flip() +
  scale_fill_brewer(palette = 'Oranges', guide = guide_legend(title = "Algorithm"),
                    breaks = c("FACET", "AFFDEX"),
                    labels = c("FACET", "AFFDEX")) +
  theme_bw() +
  ylab('Matching Score') +
  xlab('Emotion')

ggsave(file = '../data/figures/FigureC1.pdf')
