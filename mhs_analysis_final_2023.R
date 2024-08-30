# install packages ----
#install.packages("cowplot")
packages <- c("WRS2", "tidyr", "ggplot2", "haven", "plyr", "dplyr", "reshape", "ez", "lme4", "stringr", "cowplot")
sapply(packages, require, character.only = TRUE, quietly = TRUE)

# helper functions ----
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}


# load in data ----
setwd("C:/Users/geron/Dropbox/Research/Mental Health Stigma")

path = file.path("C:/", "Users/geron/Dropbox/Research/Mental Health Stigma", "Mentalhealthpilot_1_recoded_09_06_2021.sav")
df <- data.frame(read_sav(path))

# get subset of dataframe ----
df <- df %>% select(PATID, Knowledge_SZ, Knowledge_BD, Knowledge_ASD, 
                    Attitude_SZ, Attitude_BD, Attitude_ASD, 
                    Behaviour_SZ, Behaviour_BD, Behaviour_ASD, 
                    Affected_by_SZ_BD_or_ASD)
#df <- within(df, Attitude_ASD[PATID == '223'] <- Attitude_ASD[PATID == '223']-20)
df <- within(df, Attitude_ASD[PATID == '223'] <- NA)

# Conduct regular and trimmed mean ANOVAs, with line plots ----
# Knowledge ANOVA
longdf <- df %>% select(PATID, Knowledge_SZ, Knowledge_BD, Knowledge_ASD, Affected_by_SZ_BD_or_ASD) 
longdf <- longdf[complete.cases(longdf),] %>% 
  melt(id = c('PATID', 'Affected_by_SZ_BD_or_ASD'), measured = c('Knowledge_SZ', 'Knowledge_BD', 'Knowledge_ASD'))
names(longdf) <- c('PATID', 'Affected', 'Disorder', 'Knowledge')
longdf$Disorder <- gsub("^.*_", "", longdf$Disorder)
#longdf$Knowledge <- factor(longdf$Knowledge) only use this if doing clm
longdf$Affected <- factor(longdf$Affected)
longdf$Disorder <- factor(longdf$Disorder)
longdf <- longdf[order(longdf$PATID), ]
longdf_k <- longdf # ugly code!

print('**********************Knowledge************************')
# Trimmed mean ANOVA, with 20% trimmed means
bwtrim(formula = Knowledge ~ Affected * Disorder, id = PATID, data = longdf)

df_se <- summarySE(longdf, measurevar="Knowledge", groupvars=c("Disorder","Affected"))
(df_se2 <- summarySE(longdf, measurevar="Knowledge", groupvars=c("Disorder")))

pd <- position_dodge(0.1) # move them .05 to the left and right
# Line plot
#scale_color_manual(labels = c("Not Affected", "Affected"), values = c("blue", "red"))
#facet_wrap(~Disorder,dir="v")
#windows();
g1 <- ggplot(df_se, aes(x=Disorder, y=Knowledge, group=Affected, colour=Affected))  + 
  scale_color_hue(labels = c("Not Affected", "Affected")) + 
  geom_errorbar(aes(ymin=Knowledge-se, ymax=Knowledge+se), colour="black", width=.1, position=pd, size=1) +
  geom_line(position=pd, size=1) +
  geom_point(position=pd, size=6) +
  theme(text = element_text(size = 16)) + 
  labs(title="Knowledge Scores Across Diagnosis", subtitle="Scores out of 5")
#ggsave("knowledge.png")

# Attitude ANOVA
longdf <- df %>% select(PATID, Attitude_SZ, Attitude_BD, Attitude_ASD, Affected_by_SZ_BD_or_ASD) 
longdf <- longdf[complete.cases(longdf),] %>% 
  melt(id = c('PATID', 'Affected_by_SZ_BD_or_ASD'), measured = c('Attitude_SZ', 'Attitude_BD', 'Attitude_ASD'))
names(longdf) <- c('PATID', 'Affected', 'Disorder', 'Attitude')
longdf$Disorder <- gsub("^.*_", "", longdf$Disorder)
#longdf$Attitude <- factor(longdf$Attitude)
longdf$Affected <- factor(longdf$Affected)
longdf$Disorder <- factor(longdf$Disorder)
longdf <- longdf[order(longdf$PATID), ]
longdf_a <- longdf # ugly code!

print('**********************Attitude************************')
# Trimmed mean ANOVA
bwtrim(formula = Attitude ~ Affected * Disorder, id = PATID, data = longdf)

# Just test unaffected
longdf_unaff <- longdf[longdf$Affected == 0,]
with(longdf_unaff, rmanova(y = Attitude, groups = Disorder, block = PATID))
# Just test affected
longdf_aff <- longdf[longdf$Affected == 1,]
with(longdf_aff, rmanova(y = Attitude, groups = Disorder, block = PATID))

df_se <- summarySE(longdf, measurevar="Attitude", groupvars=c("Disorder","Affected"))
(df_se2 <- summarySE(longdf, measurevar="Attitude", groupvars=c("Disorder")))

pd <- position_dodge(0.1) # move them .05 to the left and right
# Line plot
#windows();
g2 <- ggplot(df_se, aes(x=Disorder, y=Attitude, group=Affected, colour=Affected))  + 
  scale_color_hue(labels = c("Not Affected", "Affected")) + 
  geom_errorbar(aes(ymin=Attitude-se, ymax=Attitude+se), colour="black", width=.1, position=pd, size=1) +
  geom_line(position=pd, size=1) +
  geom_point(position=pd, size=6) +
  theme(text = element_text(size = 16)) + 
  labs(title="Attitude Scores Across Diagnosis", subtitle="Scores out of 24")
#ggsave("attitude.png")

# Behaviour ANOVA
longdf <- df %>% select(PATID, Behaviour_SZ, Behaviour_BD, Behaviour_ASD, Affected_by_SZ_BD_or_ASD) 
longdf <- longdf[complete.cases(longdf),] %>% 
  melt(id = c('PATID', 'Affected_by_SZ_BD_or_ASD'), measured = c('Behaviour_SZ', 'Behaviour_BD', 'Behaviour_ASD'))
names(longdf) <- c('PATID', 'Affected', 'Disorder', 'Behaviour')
longdf$Disorder <- gsub("^.*_", "", longdf$Disorder)
longdf$Affected <- factor(longdf$Affected)
longdf$Disorder <- factor(longdf$Disorder)
longdf <- longdf[order(longdf$PATID), ]
longdf_b <- longdf # ugly code!

print('**********************Behaviour************************')
# Trimmed mean ANOVA
bwtrim(formula = Behaviour ~ Affected * Disorder, id = PATID, data = longdf)

# Just test unaffected
longdf_unaff <- longdf[longdf$Affected == 0,]
with(longdf_unaff, rmanova(y = Behaviour, groups = Disorder, block = PATID))
# Just test affected
longdf_aff <- longdf[longdf$Affected == 1,]
with(longdf_aff, rmanova(y = Behaviour, groups = Disorder, block = PATID))

df_se <- summarySE(longdf, measurevar="Behaviour", groupvars=c("Disorder","Affected"))
(df_se2 <- summarySE(longdf, measurevar="Behaviour", groupvars=c("Disorder")))

pd <- position_dodge(0.1) # move them .05 to the left and right
# Line plot
#windows();
g3 <- ggplot(df_se, aes(x=Disorder, y=Behaviour, group=Affected, colour=Affected))  + 
  scale_color_hue(labels = c("Not Affected", "Affected")) + 
  geom_errorbar(aes(ymin=Behaviour-se, ymax=Behaviour+se), colour="black", width=.1, position=pd, size=1) +
  geom_line(position=pd, size=1) +
  geom_point(position=pd, size=6) +
  theme(text = element_text(size = 16)) + 
  labs(title="Behaviour Scores Across Diagnosis", subtitle="Scores out of 9")
#ggsave("behaviour.png")

#pdf("foo.pdf")
#grid.arrange(g1, g2, g3, ncol=2)
#dev.off()

#windows();
p <- plot_grid(g1, g2, g3, labels = "AUTO", label_size=12)
#save_plot("scores_across_diag.jpg", p, ncol = 2, nrow = 2, dpi = 300)

windows();
p
# turn off graphics
#graphics.off()

# post hoc tests
print("********************Post-hoc tests*********************")
print('**********************Knowledge************************')
print("ASD vs BD")
longdf_2dis <- longdf_k[longdf_k$Disorder != "SZ",]
with(longdf_2dis, rmanova(y = Knowledge, groups = Disorder, block = PATID))
print("ASD vs SZ")
longdf_2dis <- longdf_k[longdf_k$Disorder != "BD",]
with(longdf_2dis, rmanova(y = Knowledge, groups = Disorder, block = PATID))
print("BD vs SZ")
longdf_2dis <- longdf_k[longdf_k$Disorder != "ASD",]
with(longdf_2dis, rmanova(y = Knowledge, groups = Disorder, block = PATID))

print('**********************Attitude************************')
print("ASD vs BD")
longdf_2dis <- longdf_a[longdf_a$Disorder != "SZ",]
with(longdf_2dis, rmanova(y = Attitude, groups = Disorder, block = PATID))
print("ASD vs SZ")
longdf_2dis <- longdf_a[longdf_a$Disorder != "BD",]
with(longdf_2dis, rmanova(y = Attitude, groups = Disorder, block = PATID))
print("BD vs SZ")
longdf_2dis <- longdf_a[longdf_a$Disorder != "ASD",]
with(longdf_2dis, rmanova(y = Attitude, groups = Disorder, block = PATID))

print('**********************Behaviour************************')
print("ASD vs BD")
longdf_2dis <- longdf_b[longdf_b$Disorder != "SZ",]
with(longdf_2dis, rmanova(y = Behaviour, groups = Disorder, block = PATID))
print("ASD vs SZ")
longdf_2dis <- longdf_b[longdf_b$Disorder != "BD",]
with(longdf_2dis, rmanova(y = Behaviour, groups = Disorder, block = PATID))
print("BD vs SZ")
longdf_2dis <- longdf_b[longdf_b$Disorder != "ASD",]
with(longdf_2dis, rmanova(y = Behaviour, groups = Disorder, block = PATID))

# plot each variable histogram ----
dfk <- df %>% select(Knowledge_SZ, Knowledge_BD, Knowledge_ASD) %>% gather(key=Disorder, value=Knowledge)
dfa <- df %>% select(Attitude_SZ, Attitude_BD, Attitude_ASD) %>% gather(key=Disorder, value=Attitude)
dfb <- df %>% select(Behaviour_SZ, Behaviour_BD, Behaviour_ASD) %>% gather(key=Disorder, value=Behaviour)

dfk_mean <- dfk %>%
  group_by(Disorder) %>%
  summarize(mean=mean(Knowledge, na.rm=TRUE))
dfa_mean <- dfa %>%
  group_by(Disorder) %>%
  summarize(mean=mean(Attitude, na.rm=TRUE))
dfb_mean <- dfb %>%
  group_by(Disorder) %>%
  summarize(mean=mean(Behaviour, na.rm=TRUE))

# Knowledge
windows();
g <- ggplot(data=dfk, aes(x=Knowledge, fill=Disorder)) + 
  geom_histogram(alpha=1, position="identity", binwidth = 1, 
                 col="black",
                 size=1) + 
  geom_vline(data = dfk_mean, aes(xintercept = mean, color = Disorder), color="black", linetype="dashed", size=2) +
  facet_wrap(~Disorder,dir="v")

g + labs(title="Knowledge Scores", 
         subtitle="Scores out of 5")
#ggsave("knowledge_dist.png")

#g <- ggplot(data=dfk, aes(x=Disorder, y=Knowledge))
#g + geom_violin(adjust = 2.5)

# Attitude
windows();
g <- ggplot(data=dfa, aes(x=Attitude, fill=Disorder)) + 
  geom_histogram(alpha=1, position="identity", binwidth = 1, 
                 col="black",
                 size=1) + 
  geom_vline(data = dfa_mean, aes(xintercept = mean, color = Disorder), color="black", linetype="dashed", size=2) +
  facet_wrap(~Disorder,dir="v")

g + labs(title="Attitude Scores", 
         subtitle="Scores out of 24")
#ggsave("attitude_dist.png")

# Behaviour
windows();
g <- ggplot(data=dfb, aes(x=Behaviour, fill=Disorder)) + 
  geom_histogram(alpha=1, position="identity", binwidth = 1, 
                 col="black",
                 size=1) + 
  geom_vline(data = dfb_mean, aes(xintercept = mean, color = Disorder), color="black", linetype="dashed", size=2) +
  facet_wrap(~Disorder,dir="v")

g + labs(title="Behaviour Scores", 
         subtitle="Scores out of 9")
#ggsave("behaviour_dist.png")

# turn off graphics
graphics.off()

# linear regressions----
longdfk <- df %>% select(PATID, Knowledge_SZ, Knowledge_BD, Knowledge_ASD, Affected_by_SZ_BD_or_ASD) 
longdfk <- melt(longdfk, id = c('PATID', 'Affected_by_SZ_BD_or_ASD'), measured = c('Knowledge_SZ', 'Knowledge_BD', 'Knowledge_ASD'))
names(longdfk) <- c('PATID', 'Affected', 'Disorder', 'Knowledge')
longdfk$Disorder <- gsub("^.*_", "", longdfk$Disorder)
longdfk$Affected <- factor(longdfk$Affected)
longdfk$Disorder <- factor(longdfk$Disorder)

longdfa <- df %>% select(PATID, Attitude_SZ, Attitude_BD, Attitude_ASD) 
longdfa <- melt(longdfa, id = c('PATID'), measured = c('Attitude_SZ', 'Attitude_BD', 'Attitude_ASD'))
names(longdfa) <- c('PATID', 'Disorder', 'Attitude')
longdfa$Disorder <- gsub("^.*_", "", longdfa$Disorder)
longdfa$Disorder <- factor(longdfa$Disorder)

longdfb <- df %>% select(PATID, Behaviour_SZ, Behaviour_BD, Behaviour_ASD) 
longdfb <- melt(longdfb, id = c('PATID'), measured = c('Behaviour_SZ', 'Behaviour_BD', 'Behaviour_ASD'))
names(longdfb) <- c('PATID', 'Disorder', 'Behaviour')
longdfb$Disorder <- gsub("^.*_", "", longdfb$Disorder)
longdfb$Disorder <- factor(longdfb$Disorder)

longdf_all <- merge(longdfk, longdfa, by = c("PATID", "Disorder"))
longdf_all <- merge(longdf_all, longdfb, by = c("PATID", "Disorder"))
longdf_all <- longdf_all[order(longdf_all$PATID), ]

# Knowledge vs Attitude
summary(lme1 <-lmer(Attitude ~ Knowledge*Affected + (Knowledge*Affected|Disorder), data = longdf_all, REML=FALSE, na.action = na.omit)) #  + (1|PATID)
# extract coefficients
coefs <- data.frame(coef(summary(lme1)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
# Bootstrapping
FUN <- function(fit) {
  #return(coef(summary(fit))[, "t value"]) # fixef(fit)
  return(fixef(fit))
}
#summary(result <- bootMer(lme1, FUN, use.u=TRUE, type = c("parametric"), nsim = 2000))
#df_boot <- result[2]$t[, 2]
#hist(df_boot)

(ci <- confint(lme1, level = 0.95, method = "boot", nsim = 2000, parm = 'beta_'))

# Knowledge vs Behaviour
summary(lme2 <-lmer(Behaviour ~ Knowledge*Affected + (Knowledge*Affected|Disorder), data = longdf_all, REML=FALSE, na.action = na.omit))
# extract coefficients
coefs <- data.frame(coef(summary(lme2)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

(ci <- confint(lme2, level = 0.95, method = "boot", nsim = 2000, parm = 'beta_'))

# Behaviour vs Attitude
summary(lme3 <-lmer(Behaviour ~ Attitude*Affected + (Attitude*Affected|Disorder), data = longdf_all, REML=FALSE, na.action = na.omit))
# extract coefficients
coefs <- data.frame(coef(summary(lme3)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

(ci <- confint(lme3, level = 0.95, method = "boot", nsim = 2000, parm = 'beta_'))

#summary(lme1 <-lmer(Knowledge ~ Disorder + (1|PATID), data = longdfk, REML=FALSE, na.action = na.omit))
#summary(lme2 <-lmer(Attitude ~ Disorder + (1|PATID), data = longdfa, REML=FALSE, na.action = na.omit))
#summary(lme3 <-lmer(Behaviour ~ Disorder + (1|PATID), data = longdfb, REML=FALSE, na.action = na.omit))

# Heatmaps!

#ggplot(df_ka, aes(Knowledge, Attitude)) +
#  geom_tile(aes(fill = Freq), colour = "black") +
#  scale_fill_gradient(low = "white", high = "steelblue") # , limits = c(0,90)

# Knowledge vs Attitude
df_short <- longdf_all %>% select(Knowledge, Attitude)
df_ka <- as.data.frame(table(df_short))

theme_nogrid <- function (base_size = 12, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace% 
    theme(panel.grid = element_blank())   
}

#windows();
g1 <- ggplot(df_ka, aes(Knowledge, Attitude)) +
  geom_point(aes(color = Freq), size = 4, shape = 16) + # size = Freq, 
  #scale_size_continuous(range = c(3,10)) + 
  scale_color_gradient(name="Frequency", low = "white", high = "black") +
  labs(title="Knowledge vs Attitude") + 
  theme_nogrid() + theme(text = element_text(size = 16))

#ggsave("knowledge_vs_attitude.png")


# Knowledge vs Behaviour
df_short <- longdf_all %>% select(Knowledge, Behaviour)
df_kb <- as.data.frame(table(df_short))

#windows();
g2 <- ggplot(df_kb, aes(Knowledge, Behaviour)) +
  geom_point(aes(color = Freq), size = 8, shape = 16) +
  scale_color_gradient(name="Frequency", low = "white", high = "black") +
  labs(title="Knowledge vs Behaviour") + 
  theme_nogrid() + theme(text = element_text(size = 16))

#ggsave("knowledge_vs_behaviour.png")

# Attitude vs Behaviour
df_short <- longdf_all %>% select(Attitude, Behaviour)
df_ab <- as.data.frame(table(df_short))

#windows();
g3 <- ggplot(df_ab, aes(Attitude, Behaviour)) +
  geom_point(aes(color = Freq), size = 6, shape = 16) +
  scale_color_gradient(name = "Frequency", low = "white", high = "black") +
  labs(title="Behaviour vs Attitude") + 
  theme_nogrid() + theme(text = element_text(size = 16))

#ggsave("attitude_vs_behaviour.png")

p <- plot_grid(g1, g2, g3, labels = "AUTO", label_size=12)
save_plot("scatter_across_diag.jpg", p, ncol = 2, nrow = 2, dpi = 300)

windows();
p

# turn off graphics
graphics.off()

# Code graveyard---------------------

#df_ka$Knowledge <- as.numeric(as.character(df_ka$Knowledge))
#df_ka$Attitude <- as.numeric(as.character(df_ka$Attitude))
#df_ka$Freq <- as.numeric(as.character(df_ka$Freq))

#for (i in seq(from=1, to=max(df_ka$Attitude)-2, by=3)) {
#  df_ka$Attitude[df_ka$Attitude %in% c(i,i+1,i+2)] <- i # or i+1 for middle value
#}

#df_ka <- df_ka %>% 
#  group_by(Knowledge, Attitude) %>% 
#  summarise(Freq = sum(Freq, na.rm = TRUE))

#hangctr <- subset(hangover, subset = group == "alcoholic")
#bwtrim(symptoms ~ group*time, id = id, data = hangover)



g <- ggplot(data=dfk, aes(x=Disorder, y=Knowledge))
g + geom_violin(adjust = 1.5)





dfk <- df %>% select(Knowledge_SZ, Knowledge_BD, Knowledge_ASD) %>% gather(key=Disorder, value=Knowledge)
g <- ggplot(dfk, aes(x=Disorder, y=Knowledge))
g + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .2, 
               fill="red")






# Histogram on Knowledge
theme_set(theme_classic())

g <- ggplot(data=dfk, aes(x=Knowledge, color=Disorder)) + 
  geom_freqpoly(alpha=1, bins = 6, center = 0, size = 2) + 
  scale_fill_brewer(palette = "Spectral")

g + labs(title="Knowledge Scores", 
         subtitle="Scores out of 5")

# Histogram on Attitude
g <- ggplot(data=dfa, aes(x=Attitude, color=Disorder)) + 
  geom_freqpoly(alpha=1, bins = 8, center = 0, size = 2) + 
  scale_fill_brewer(palette = "Spectral")

g + labs(title="Attitude Scores", 
         subtitle="Scores out of 24")

# Histogram on Behaviour
g <- ggplot(data=dfb, aes(x=Behaviour, color=Disorder)) + 
  geom_freqpoly(alpha=1, bins = 3, center = 0, size = 2) + 
  scale_fill_brewer(palette = "Spectral")

g + labs(title="Behaviour Scores", 
         subtitle="Scores out of 9")


# link function technique for ordinal data
summary(mm1 <- clmm(Knowledge ~ Disorder + (1|PATID), data=longdf_k))

# Bootstrapping
FUN <- function(fit) {
  return(fixef(fit))
}

(ci <- confint(mm1, level = 0.95, method = "boot", nsim = 100))

summary(mm1 <- clmm(Attitude ~ Disorder + (1|PATID), data=longdf_a))


