library(ggplot2)
library(ggspatial)
library(sf)
library(rnaturalearth)
library(dplyr)

rm(list = ls())
dev.off()

setwd("F:/School/SeagrassRecovery/LitSurvey")
dat <- read.csv("LitSurvey_Data.csv")

lat_long <- dat %>%
  group_by(Order) %>%
  summarise(Lat = mean(Lat),
            Long = mean(Long))

world <- rnaturalearth::ne_countries(scale = "medium",
                                     returnclass = "sf")

sf_all <- st_as_sf(lat_long, 
                   coords = c("Long", "Lat"),
                   crs = 4269)

ggplot() +
  geom_sf(data = world,
          mapping = aes(geometry = geometry),
          color = "black",
          fill = "gray89") +
  geom_sf(data = sf_all,
          color = "black",
          fill = "red",
          shape = 21,
          size = 3,
          alpha = 0.8) +
  coord_sf(ylim = c(-90, 90),
           xlim = c(-180, 180),
           expand = FALSE) +
  labs(x = "Longitude",
       y = "Latitude") +
  scale_y_continuous(breaks = c(seq(-75,75,25))) +
  scale_x_continuous(breaks = c(seq(-150,150,25))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 14, color = "black", angle = 35, hjust = 1),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.5),
        panel.background = element_rect(fill = "white"))

dat2 <- dat[!duplicated(dat$Order),]
test <- as.data.frame(table(dat2$Country))
colnames(test)[1] <- "Country"
sum(test$Freq)

ggplot(data = test, aes(x = reorder(Country, -Freq), y = Freq)) +
  geom_bar(stat = "identity", color = "black", fill = "red") +
  labs(x = "Study Location",
       y = "Num. of Studies (n = 60)") +
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16, color = "black", angle = 45, hjust = 1),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 16, color = "black"),
        panel.grid = element_blank(),
        panel.background = element_blank())

library(wordcloud)
library(wordcloud2)
library(tm)

text <- dat[!duplicated(dat$Order),]
text <- text[,2]
docs <- Corpus(VectorSource(text))

docs <- docs %>%
  tm_map(removePunctuation)

docs <- docs %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)

# docs <- tm_map(docs, content_transformer(tolower))
# docs <- tm_map(docs, removeWords, stopwords("english"))

dtm <- TermDocumentMatrix(docs)
matrix <- as.matrix(dtm)
words <- sort(rowSums(matrix), decreasing = TRUE)
df <- data.frame(word = names(words), freq=words)

set.seed(1234)

dff <- df[df$freq >= 2,]

wordcloud(words = df$word, freq = df$freq, min.freq = 2,
          max.words = 200, random.order = FALSE, rot.per = 0,
          colors = brewer.pal(5, "Dark2"))

wordcloud2(data = dff, backgroundColor = "black",
           shape = "diamond", color=rep_len(c("#253494","#2c7fb8","#41b6c4", "#a1dab4", "#ffffcc"), nrow(df)),
           rotateRatio = 0, minSize = 2, shuffle = TRUE,
           fontWeight = 'normal')

###########


dat$Region <- ifelse(dat$Lat < 23.5 & dat$Lat > -23.5, "Tropics",
                     ifelse(dat$Lat > 23.5 & dat$Lat < 35, "Subtropics",
                            ifelse(dat$Lat < -23.5 & dat$Lat > -35, "Subtropics",
                                   ifelse(dat$Lat > 35, "Temperate",
                                          ifelse(dat$Lat < 35, "Temperate", NA)))))

dat <- dat[,c(1:7,35,8:34)]
table(dat$Region)
dat2 <- dat[!duplicated(dat$Order),]
table(dat2$Region)

num_region <- c("Tropics (n = 27)", "Subtropics (n = 48)", "Temperate (n = 63)")

dat %>%
  mutate(Region = factor(Region, levels = c("Tropics","Subtropics","Temperate"))) %>%
  ggplot(aes(x = Region, y = Recovery_months)) +
  scale_y_log10() +
  scale_x_discrete(labels = num_region) +
  geom_boxplot(outlier.colour="white", outlier.fill = "white", outlier.shape = 1, outlier.size = 0) +
  geom_jitter(shape=1, position=position_jitter(0.2), color = "black", fill = "white", size = 2) +
  labs(x = "Latitudinal Regions",
       y = "Recovery Time (months)") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 16, color = "black"),
        axis.title.x = element_text(vjust = -0.5),
        panel.grid = element_blank(),
        panel.background = element_blank())

dat %>% group_by(Region) %>% summarise(Median = median(Recovery_months, na.rm = T))

dat_100 <- dat[dat$Perc_RecoveryDuringStudy >= 99,]
dat_100 <- dat_100[!is.na(dat_100$Perc_RecoveryDuringStudy),]

dat_100 %>%
  mutate(Region = factor(Region, levels = c("Tropics","Subtropics","Temperate"))) %>%
  ggplot(aes(x = Region, y = Recovery_months)) +
  scale_y_log10() +
  geom_boxplot(outlier.colour="white", outlier.fill = "white", outlier.shape = 1, outlier.size = 0) +
  geom_jitter(shape=1, position=position_jitter(0.2), color = "black", fill = "white", size = 2) +
  labs(x = "Latitudinal Regions",
       y = "Recovery Time (months)") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 16, color = "black"),
        axis.title.x = element_text(vjust = -0.5),
        panel.grid = element_blank(),
        panel.background = element_blank())

dat_100 %>% group_by(Region) %>% summarise(Median = median(Recovery_months, na.rm = T))

dat_exp <- dat2[dat2$StudyType == "Experiment",]
dat_obs <- dat2[dat2$StudyType == "Observation",]
dat_obs$DisturbanceArea_km2 <- dat_obs$DisturbanceArea_m2/1000000
dat_mono <- dat2[dat2$Mixed_Mono == "Mono",]
dat_mono_table <- as.data.frame(table(unlist(strsplit(dat_mono$Species,', '))))
dat_species_table <- as.data.frame(table(unlist(strsplit(dat$Species,', '))))
dat_nonlin <- dat2[dat2$Recovery_Trajectory == "Nonlinear",]
dat_lin <- dat2[dat2$Recovery_Trajectory == "Linear",]

p1 <- ggplot(data = dat_exp, aes(x = StudyType, y = DisturbanceArea_m2)) +
  geom_boxplot(outlier.color = "black") +
  scale_y_continuous(breaks = seq(0,14,2),
                     limits = c(0,14)) +
  labs(x = "",y = expression(paste("Disturbance Area ( ", m^-2,")"))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 16, color = "black"),
        axis.title.x = element_text(vjust = -0.5),
        panel.grid = element_blank(),
        panel.background = element_blank())

p2 <- ggplot(data = dat_obs, aes(x = StudyType, y = DisturbanceArea_km2)) +
  geom_boxplot(outlier.color = "black") +
  # scale_y_continuous(breaks = seq(0,5,1),
  #                    limits = c(0,5)) +
  labs(x = "",y = expression(paste("Disturbance Area ( ", km^-2,")"))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 16, color = "black"),
        axis.title.x = element_text(vjust = -0.5),
        panel.grid = element_blank(),
        panel.background = element_blank())

library(patchwork)

p1 + p2 + plot_layout(ncol = 2)
  
current_lrg_disArea = round(max(dat_exp$DisturbanceArea_m2),1)
our_study_distArea = 3.14*(3*3)
round((our_study_distArea/current_lrg_disArea),2)

table(dat2$StudyType)

adj_studyType <- dat2
adj_studyType[adj_studyType == "Experiment"] <- "Experiment (n = 27)"
adj_studyType[adj_studyType == "Observation"] <- "Observation (n = 33)"

adj_studyType %>%
  mutate(DistShape = factor(DistShape, levels = c("Irregular","Square","Circle","Rectagle","Ellipse","NA"))) %>%
ggplot(aes(DistShape)) +
  geom_bar(color = "black",
           fill = "gray75") +
  labs(x = "Disturbance Shape",
       y = "Number of Studies") +
  scale_y_continuous(breaks = seq(0,25,5), limits = c(0,25)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 14, color = "black", angle = 30, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        strip.text.x = element_text(size = 14, color = "black")) +
  facet_wrap(~StudyType)

dat2 %>%
  arrange(-desc(DistType)) %>%
  mutate(DistType = factor(DistType, levels = c("Physical","Chemical","Light",
                                                "NA","Climate","Disease","Natural"))) %>%
ggplot(aes(DistType)) +
  geom_bar(color = "black",
           fill = "gray75") +
  labs(x = "Disturbance Type",
       y = "Number of Studies") +
  scale_y_continuous(breaks = seq(0,35,5), limits = c(0,35)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 14, color = "black", angle = 30, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        strip.text.x = element_text(size = 14, color = "black"))

ggplot(data = dat2, aes(Recovery_Trajectory)) +
  geom_bar(color = "black",
           fill = "gray75") +
  labs(x = "Recovery Trajectory",
       y = "Number of Studies") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        panel.grid = element_blank(),
        panel.background = element_blank())

dat2 %>%
  mutate(Recovery_Mechanism = factor(Recovery_Mechanism, levels = c("Clonal","Seed","Both"))) %>%
ggplot(aes(Recovery_Mechanism)) +
  geom_bar(color = "black",
           fill = "gray75") +
  labs(x = "Recovery Mechanism",
       y = "Number of Studies") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        panel.grid = element_blank(),
        strip.text.x = element_text(size = 14, color = "black"),
        panel.background = element_blank()) +
  facet_wrap(~StudyType)

# ggplot(data = dat_exp, aes(Replicates)) +
#   geom_bar(color = "black",
#            fill = "gray75") +
#   scale_x_binned(n.breaks = 30) +
#   theme_bw() +
#   theme(axis.text.x = element_text(size = 12, color = "black", angle = 45, hjust = 1),
#         axis.text.y = element_text(size = 12, color = "black"),
#         panel.grid = element_blank(),
#         panel.background = element_blank())

ggplot(data = dat2, aes(NumSpecies)) +
  geom_bar(color = "black",
           fill = "gray75") +
  scale_x_continuous(breaks = seq(1,7,1)) +
  labs(x = "Number of Species",
       y = "Number of Studies") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        strip.text.x = element_text(size = 14, color = "black")) +
  facet_wrap(~StudyType)

# dat_100 <- dat[dat$Perc_RecoveryDuringStudy >= 100,]
# dat_100 <- dat_100[!is.na(dat_100$Perc_RecoveryDuringStudy),]
# dat_100$Recovery_months <- as.numeric(dat_100$Recovery_months)
# dat_100$DisturbanceArea_m2 <- as.numeric(dat_100$DisturbanceArea_m2)
# dat_100$MeadowSizeStart_m2 <- as.numeric(dat_100$MeadowSizeStart_m2)
# dat_100$MeadowSizeEnd_m2 <- as.numeric(dat_100$MeadowSizeEnd_m2)
# dat_100$Perimeter <- as.numeric(dat_100$DisturbancePerimeter_m)
# dat_100$PerimAreaRatio <- dat_100$Perimeter/dat_100$DisturbanceArea_m2
# dat_100_small <- dat_100[dat_100$DistArea_m2 <= 100,]
# dat_100_small <- dat_100_small[!is.na(dat_100_small$Recov.),]

dat2$StudyDuration_years <- dat2$StudyDuration_months/12
dat$StudyDuration_years <- dat$StudyDuration_months/12
dat_exp <- dat[dat$StudyType == "Experiment",]
dat_obs <- dat[dat$StudyType == "Observation",]

round(mean(dat_exp$StudyDuration_months, na.rm = T))
round(mean(dat_obs$StudyDuration_months, na.rm = T))

dat$DisturbanceArea_m2 <- as.numeric(dat$DisturbanceArea_m2)
dat$Recovery_months <- as.numeric(dat$Recovery_months)
dat$DisturbancePerimeter_m <- as.numeric(dat$DisturbancePerimeter_m)
dat$PerimRatio <- dat$DisturbancePerimeter_m/dat$DisturbanceArea_m2
dat$Perc_AboveGroundReduction <- as.numeric(dat$Perc_AboveGroundReduction)
dat$MeadowSizeStart_m2 <- as.numeric(dat$MeadowSizeStart_m2)
dat$MeadowDistRatio <- dat$DisturbanceArea_m2/dat$MeadowSizeStart_m2
dat$Perc_MeadowDist <- dat$MeadowDistRatio*100
dat$abs_lat <- abs(dat$Lat)

library(ggpubr)

cols <- c("Tropics" = "#000000", "Subtropics" = "#969696", "Temperate" = "#ffffff")

ggplot(data = dat, aes(x = DisturbanceArea_m2, y = Recovery_months)) +
  geom_point(size = 2) +
  geom_point(color = "black", size = 2, shape = 1) +
  scale_color_manual(values = cols) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x=expression(paste("Disturbance Area ( ", m^-2,")")),
       y = "Recovery Length (months)") +
  stat_smooth(method = 'lm') +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 16, color = "black"),
        panel.grid = element_blank(),
        panel.background = element_blank()) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 3, label.y = -0.5, size = 5)

ggplot(data = dat, aes(x = PerimRatio, y = Recovery_months)) +
  geom_point(color = "black",
             fill = "gray75") +
  scale_y_log10(labels = function(x) format(x, scientific = FALSE)) +
  labs(x = "Disturbance Perimeter/Area Ratio",
       y = "Recovery Length (months)") +
  stat_smooth(method = 'lm') +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 16, color = "black"),
        panel.grid = element_blank(),
        panel.background = element_blank()) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 25, label.y = 2.75, size = 5)

ggplot(data = dat, aes(x = Perc_MeadowDist, y = Recovery_months)) +
  geom_point(color = "black",
             fill = "gray75") +
  scale_y_log10() +
  scale_x_log10(labels = function(x) format(x, scientific = FALSE)) +
  labs(x = "%Meadow Disturbed",
       y = "Recovery Length (months)") +
  stat_smooth(method = 'lm') +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 16, color = "black"),
        panel.grid = element_blank(),
        panel.background = element_blank()) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = -7, label.y = 2.75, size = 5)

dat %>%
  mutate(Region = factor(Region, levels = c("Tropics","Subtropics","Temperate"))) %>%
ggplot(aes(x = factor(NumSpecies), y = Recovery_months)) +
  geom_boxplot(outlier.colour = "white", outlier.fill = "white") +
  geom_jitter(shape=1, position=position_jitter(0.2), color = "black", fill = "white", size = 2) +
  scale_y_log10() +
  scale_x_discrete(limits= factor(1:6)) +
  labs(x = "Number of Species",
       y = "Recovery Time (months)") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 16, color = "black"),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        strip.text.x = element_text(size = 14, color = "black")) +
  facet_wrap(~Region)

library(MASS)

lm_testset <- dat[,c(38:39,10,20,23:25,29,34,36)] ###Need to double-check what is getting removed here
lm_testset <- na.omit(lm_testset)
lm_testset$log_recov <- log10(lm_testset$Recovery_months)
lm_testset <- lm_testset[,c(1:7,9:11)]

full.model <- lm(log_recov~., data = lm_testset)

step.model <- stepAIC(full.model, direction = "backward",
                      trace = F)
summary(step.model)
round(summary(step.model)$coefficients[1],3)
round(summary(step.model)$coefficients[2],3)
round(summary(step.model)$coefficients[3],3)
round(summary(step.model)$coefficients[4],3)
round(summary(step.model)$coefficients[5],3)
round(summary(step.model)$coefficients[6],3)
