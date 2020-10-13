rm(list = ls())
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)

##### OPTIONS #####
options(scipen = 3)

##### LOADING PACKAGES #####
### General libraries
library(tidyverse)
library(psych)
library(plyr)

### Multilevel modeling
library(lme4)
library(nlme)

##### FUNCTIONS #####
### APA style charts
apatheme <- theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        text = element_text(family = 'Times'))

### Function that counts the number if NA's in a given row:
countNA <- function(df)
  apply(df, MARGIN = 1, FUN = function(x) length(x[is.na(x)]))

##### DATA #####
dat_attention <- read.csv(file.choose())
dat_master <- read.csv(file.choose())
dat_attention[dat_attention == -999 | dat_attention == -888 | dat_attention == -777] <- NA

##### CLEANING #####
### We were only interested in participants with with Autism Syndrom Disorder
dat_attention$Child_ID <- factor(dat_attention$Child_ID)
dat_attention <-dat_attention %>%
  dplyr::filter(Child_Type == "ASD")
nrow(count(unique(dat_attention$Child_ID)))

### Missing values 
dat_attention_window <- subset(dat_attention, select = -c(frame_001:frame_096, frame_149:frame_200))
dat_attention_window_trialNA <- dat_attention_window[countNA(subset(dat_attention_window, select = frame_097:frame_148)) < (ncol(subset(dat_attention_window, select = frame_097:frame_148))/2),]

# Table 1 - Missing values
Process <- c("Before cleaning", "Number of excluded trials", "After cleaning")
n <- c(nrow(dat_attention_window), nrow(dat_attention_window) - nrow(dat_attention_window_trialNA), nrow(dat_attention_window_trialNA))
number_of_observations <- cbind(Process, n)
as.data.frame(number_of_observations)

# Table 2 - Overview missing values overall
dat_attention_window_descriptive <- dat_attention_window_trialNA %>%
  dplyr::select("Condition", starts_with("frame_"))
dat_attention_window_descriptive$NAcount <- countNA(dat_attention_window_descriptive)
describe(dat_attention_window_descriptive$NAcount)

# Table 4 - Overview missing values per condition
descriptive_missing <- describeBy(dat_attention_window_descriptive$NAcount,
                                  dat_attention_window_descriptive$Condition,
                                  mat = TRUE, digits = 2)

### Underperforming kids (trials)
`%notin%` <- Negate(`%in%`)

childxconditioncount_pre <- count(dat_attention_window_trialNA, c("Child_ID", "Condition"))
dat_attention_critical_trialNA <- dat_attention_window_trialNA %>%
  dplyr::filter(Child_ID %notin% c("8014", "8016", "8022", "8031", "8036", "8044", "8047", "8049", "8050", "8055", "8057", "8058", "8059", "8061",  "8062", "8063", "8072", "8074", "8076", "8077"))
childxconditioncount_post <- count(dat_attention_window_trialNA, c("Child_ID", "Condition"))

# Table 5 - Excluding kids with missing trials
Process <- c("Before cleaning", "After cleaning")
N <- c(length(unique(childxconditioncount_pre$Child_ID)), length(unique(childxconditioncount_post$Child_ID)))
number_of_participants <- cbind(Process, N)
as.data.frame(number_of_participants)

# Table 6 - Number of trials per kid per condition
dat_attention_window_trialNA$Child_Condition <- interaction(dat_attention_window_trialNA$Child_ID, dat_attention_window_trialNA$Condition)
dat_counts <- ddply(dat_attention_window_trialNA, ~Child_Condition, summarise, Counts_TR = length(unique(Tr_Num)))
dat_attention_window_trialNA <- merge(dat_attention_window_trialNA, dat_counts, by = "Child_Condition")
descriptive_trial <- describeBy(dat_attention_window_trialNA$Counts_TR,
                                dat_attention_window_trialNA$Condition,
                                mat = TRUE, digits = 2)

##### TRANSFORMING #####

### Aliasing data
dat_analysiswindow <- dat_attention_window_trialNA

### Analysis window only
dat_analysiswindow$Child_ID <- factor(dat_analysiswindow$Child_ID)
dat_analysiswindow_long <- gather(dat_analysiswindow, frame, accuracy, frame_097:frame_148, factor_key = TRUE)

### Overall
# Subsetting full data based on prior cleaning procedures
dat_attention_full <- subset(dat_attention, Child_ID %in% dat_analysiswindow$Child_ID)

# Subsetting for the pre-analysis window
dat_attention_pre <- subset(dat_attention_full, select = -c(frame_097:frame_200))
dat_attention_pre$prepost <- "pre-analysis"
dat_attention_pre$Child_ID <- factor(dat_attention_pre$Child_ID)
dat_attention_pre_long <- gather(dat_attention_pre, frame, accuracy, frame_001:frame_096, factor_key = TRUE)

# Subsetting for the analysis window
dat_attention_analysis <- subset(dat_attention_full, select = -c((frame_001:frame_096), (frame_149:frame_200)))
dat_attention_analysis$prepost <- "analysis"
dat_attention_analysis$Child_ID <- factor(dat_attention_analysis$Child_ID)
dat_attention_analysis_long <- gather(dat_attention_analysis, frame, accuracy, frame_097:frame_148, factor_key = TRUE)

# Subsetting for the post-analysis window
dat_attention_post <- subset(dat_attention_full, select = -c((frame_001:frame_148)))
dat_attention_post$prepost <- "post-analysis"
dat_attention_post$Child_ID <- factor(dat_attention_post$Child_ID)
dat_attention_post_long <- gather(dat_attention_post, frame, accuracy, frame_149:frame_200, factor_key = TRUE)

# Combining
dat_attention_long <- rbind(dat_attention_pre_long, dat_attention_analysis_long, dat_attention_post_long)

##### MEASURING #####
### For the analysis window dataframe
dat_analysiswindow_long$accuracy <- as.numeric(as.character(dat_analysiswindow_long$accuracy))
dat_analysiswindow_long <- dat_analysiswindow_long %>%
  dplyr::group_by(Child_ID, frame, Condition) %>%
  dplyr::mutate(mean_accuracy = mean(accuracy, na.rm = TRUE))

### For the overall dataframe
dat_attention_long$accuracy <- as.numeric(as.character(dat_attention_long$accuracy))
dat_attention_long <- dat_attention_long %>%
  dplyr::group_by(Child_ID, frame, Condition) %>%
  dplyr::mutate(mean_accuracy = mean(accuracy, na.rm = TRUE))

##### DESCRIPTIVES AND PLOTTING #####

### Descriptives
dat_attention_plot <- describeBy(dat_attention_long$mean_accuracy,
                                 list(dat_attention_long$Condition,
                                      dat_attention_long$frame),
                                 mat = TRUE, digits = 2)
names(dat_attention_plot)[names(dat_attention_plot) == 'group1'] = 'condition'
names(dat_attention_plot)[names(dat_attention_plot) == 'group2'] = 'frame'
dat_attention_plot

### Plotting over time
ggplot(dat_attention_plot, aes(x = sort(frame, decrease = FALSE), y = mean, group = condition, linetype = condition)) +
  geom_line(stat = "identity") +
  geom_ribbon(aes(ymin = mean - se, ymax = mean + se, fill = "Mean +/- 1SE"), alpha = 0.2) +
  scale_fill_manual("", values = "grey12") +
  ylab("Looks to Target") +
  xlab("") +
  apatheme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks = c("frame_087","frame_097", "frame_148"),
                   labels = c("0 ms", "300 ms", "2000 ms")) +
  geom_vline(xintercept = 97, linetype = "dashed") +
  geom_vline(xintercept = 148, linetype = "dashed")

##### TIME SERIES ANALYSIS #####
### Transforming Frame
dat_attention_long$frame <- as.character(dat_attention_long$frame)
dat_attention_long$frame.n <- substr(dat_attention_long$frame, 7, nchar(dat_attention_long$frame))
dat_attention_long$frame.n <- as.numeric(dat_attention_long$frame.n)

### Modelling
# Standardizing
dat_attention_long$Tr_Num <- as.factor(dat_attention_long$Tr_Num)
dat_attention_long$mean_accuracy.c <- scale(dat_attention_long$mean_accuracy, center = TRUE, scale = FALSE)

# Model
model_time_sequence <- lmer(mean_accuracy.c ~ Condition * frame.n + ( 1 | Child_ID) + ( 1 | Tr_Num), data = dat_attention_long, REML = FALSE)
summary(model_time_sequence)