# load packages -----------------------------------------------------------

library(openxlsx)
library(tidyverse)
library(lme4)
library(lmerTest)
library(Rmisc)
library(Cairo)
library(strengejacke)
library(ggplot2)
library(knitr)
library(influence.ME)
library(sjPlot)

options(scipen=999)

rm(list = ls())

today <- Sys.Date()
today <- format(today, format="%y%m%d") 


# read data ---------------------------------------------------------------

df <- read.xlsx(here::here("data", "verbal_CSI", "full_df.xlsx"))


# remove NA + incorrect ---------------------------------------------------------------
df_full <- df # 4800 observations (= 160 Trials * 30 participants)
save(df_full, file = here::here("data", "verbal_CSI", "full_df.RData"))

df <- df[(df$cat_code != "100"),] # kick out Filler -> 3600 observations (= 120 Trials * 30 participants)
df <- df[!is.na(df$VOT),] # 3549 observations -> 51 Trials lost

ntrials_participant <- df %>%
  group_by(subject) %>%
  dplyr::summarise (ntrials = length(trial)) %>%
  dplyr::ungroup %>%
  dplyr::mutate(percent = (ntrials / 120)*100) 


df_correct <- df[df$correct == "1",] ## only correct items 3264 observations

ntrials_participant_correct <- df_correct %>%
  group_by(subject) %>%
  dplyr::summarise (ntrials = length(trial)) %>%
  ungroup %>%
  dplyr::mutate(percent = (ntrials / 120)*100) 

save(df_correct,file = here::here("data", "verbal_CSI", "CSI_online_Conny_df_correct.RData"))

# descriptives ------------------------------------------------------------

means <- summarySEwithin(df_correct,"VOT",idvar = "subject",withinvars = "Pos")
means_t <- data.frame(t(means))
colnames(means_t) <- c(1,2,3,4,5)
means_t <- means_t[-1,]
write.csv(means_t, file = here::here("data", "verbal_CSI", "descriptive_means.csv"))
kable(means_t)

# plots -------------------------------------------------------------------

### makes plots suitable for APA format, font sizes can be adjusted
apatheme <- theme_bw()+
  theme(plot.title=element_text(family="Arial",size=22,hjust = .5),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        panel.border=element_blank(),axis.line=element_line(),
        text=element_text(family="Arial",size=16))

plot <- ggplot(data=means, aes(x=Pos, y=VOT)) +
  geom_point( size = 2)+
  geom_errorbar(aes(ymin=VOT-se, ymax=VOT+se), width =.1) +
  stat_summary(fun=mean,  geom="line", size = 1, group = 1) +
  apatheme+
  scale_y_continuous(breaks = seq(900, 1100, by = 20))+
  labs(x="Ordinal Position ",y ="RTs [ms]")+
  annotate(geom="text", x=2, y=980, label="n = 30", color="black", size = 8)
plot

ggsave(plot = plot, file = here::here("data", "verbal_CSI", "Plots", paste0("plot_CSI_online_",today,".pdf")), width = 18, height = 13, units = "cm",device = cairo_pdf,dpi = 300)
embedFonts(file = here::here("data", "verbal_CSI", "Plots", paste0("plot_CSI_online_",today,".pdf")))
ggsave(plot = plot, file = here::here("data", "verbal_CSI", "Plots", paste0("plot_CSI_online_",today,".png")), width = 18, height = 13, units = "cm"
       ,dpi = 300)



### plot by category
plot_facet <- ggplot(data=df_correct, aes(x=Pos, y=VOT)) +
  stat_summary(fun=mean,  geom="point", size = 2)+
  stat_summary(fun=mean,  geom="line", size = 1, group = 1) +
  facet_wrap(~subcat) +
  apatheme+
  labs(x="Ordinal Position ",y ="RT ms") 
ggsave(plot = plot_facet, file = here::here("data", "verbal_CSI", "Plots", "CSI_online_subcats.pdf"), device = cairo_pdf,dpi = 300)
embedFonts(file =  here::here("data", "verbal_CSI", "Plots", "CSI_online_subcats.pdf"))

### plot by Subject
plot_facet <- ggplot(data=df_correct, aes(x=Pos, y=VOT)) +
  stat_summary(fun=mean,  geom="point", size = 2)+
  stat_summary(fun=mean,  geom="line", size = 1, group = 1) +
  facet_wrap(~subject) +
  apatheme+
  labs(x="Ordinal Position ",y ="RT ms") 
plot_facet

ggsave(plot = plot_facet, file =  here::here("data", "verbal_CSI", "Plots", "CSI_online_subcats.pdf"), device = cairo_pdf,dpi = 300)
embedFonts(file = here::here("data", "verbal_CSI", "Plots", "CSI_online_subcats.pdf"))


# lmms --------------------------------------------------------------------

### with continuous predictor
df_correct$Pos_cont <- scale(as.numeric(as.character(df_correct$Pos)), center = T, scale = F)


m1_cont <- glmer(VOT ~ Pos_cont + (Pos_cont|subject) +(Pos_cont|subcat) , 
            data =df_correct, family =Gamma(link ="identity"), control=glmerControl(optimizer = "bobyqa"))

summary(m1_cont)
tab_model(m1_cont,transform = NULL,show.re.var = F, show.stat = T, string.stat = "t-Value", 
          pred.labels = c("Intercept","Ordinal Position"), show.r2 = F,show.icc = F,
          file = here::here("data", "verbal_CSI", "Plots", paste0("glmm_CSI_online_cont_",today,".html")))

infFit_subject <- influence(m1_cont, group = "subject")

plot(infFit_subject, cutoff = 2/sqrt(length(unique(df_correct$subject))), which = "dfbetas") 
# no influental participants for our main effect

infFit_subcat <- influence(m1_cont, group = "subcat")

plot(infFit_subcat, cutoff = 2/sqrt(length(unique(df_correct$subcat))), which = "dfbetas") 
# category "Heimwerker" might be slightly influential for our main effect



### with filelength as covariate

m1_cont_length <- glmer(VOT ~ Pos_cont * scale(filelength) + (Pos_cont|subject) +(Pos_cont|subcat) , 
                 data =df_correct, family =Gamma(link ="identity"), control=glmerControl(optimizer = "bobyqa"))
summary(rePCA(m1_cont_length))
VarCorr(m1_cont_length)
summary(m1_cont_length)



