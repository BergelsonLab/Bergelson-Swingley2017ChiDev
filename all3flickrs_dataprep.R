# this is the data prep file for the combined all3flickrs paper

# running this file in its entirety will produce the files the .rmd needs for the paper

#first let's get our libraries and our functions

# libraries and helper function -------------------------------------------
library(tidyverse)

library(forcats)
options(tibble.width = 100)
options(dplyr.width = 50)

onesamp_npstats<-
  function(data=NULL){
    #this function is to get the wilcoxon test p value, pseudomedian, mean,sd and n pos and ntotal for a vector of data
    wilcoxtestoutput<-wilcox.test(data, conf.int=T, alt="two.sided")
    noutput<-length(data)#assumes no NAs
    posnoutput<-sum(data>0)
    meanoutput<-mean(data, na.rm=T)
    sdoutput<-sd(data, na.rm=T)
    onetailttestoutput<-t.test(data, conf.int=T, alt="greater")#one tailed!
    shapirotestoutput<-shapiro.test(data)
    return((cbind(signif(wilcoxtestoutput$estimate,2), 
             wilcox_p=signif(wilcoxtestoutput$p.value,3), 
             pos=posnoutput,
             ntotal=noutput, 
             m=signif(meanoutput, 2),
             sd=signif(sdoutput, 2),
             proppos = paste(posnoutput, "/", noutput, sep = ""),
             binomp = signif(binom.test(posnoutput, noutput, .5)$p.value,3),
             otT_p = signif(onetailttestoutput$p.value,3),
             shap_p = signif(shapirotestoutput$p.value,3))))%>%
      tidy()%>%
      rename(pseudomed = V1)%>%
      dplyr::select(-.rownames)%>%
      as.tbl()%>%
      mutate(pseudomed = as.numeric(pseudomed),
             wilcox_p = as.numeric(wilcox_p),
             pos = as.numeric(pos),
             ntotal = as.numeric(ntotal),
             m = as.numeric(m),
             sd = as.numeric(sd),
             binomp = as.numeric(binomp),
             otT_p = as.numeric(otT_p),
             shap_p = as.numeric(shap_p))
  }


# load the data -----------------------------------------------------------

#demographic files by condition
#i only use read_csv (and equivalent) these days but am doing the old ones to make sure nothing breaks
flickrvamp_demo <- read.delim("data/flickrvamp_edcat_eth_gender.txt", header= T)
flickrvoice_demo <- read.delim("data/flickrv_edcat_eth_gender.txt", header= T)
vampvoice_cdi <- read.csv("data/vamp_voice_cdi.csv", header=T)


#now the data files. 
###flickr_dsmelt_all3 has each pair each subject
###flickr_ds_all3 has subject means
###flickr_ds_pairs has pair means
###flickrvs_gr is each item each subject, grouped
###flickrvs_gr_subj has subject means, grouped
###flickrvs_gr_item has item means, grouped based on n>7
###flickvs_gr_items_basedonDS_Ss, excludes based on DS excludes
flickr_dsmelt_all3 <- read.table("data/flickr_dsmelt_all3.txt", header = T)
flickr_ds_all3 <- read.table("data/flickr_ds_all3.txt", header=T)
flickr_ds_pairs <- read.table("data/flickr_ds_pairs_all3.txt", header = T)

flickrvs_gr <- read.table("data/flickrvs_gr.txt", header=T)
flickrvs_gr_subj <- read.table("data/flickrvs_gr_subj.txt", header=T)
flickrvs_gr_item <- read.table("data/flickrvs_gr_item.txt", header=T)
flickvs_gr_items_basedonDS_Ss <- read.table("data/flickvs_gr_items_basedonDS_Ss.txt", header=T)


# #Ss ---------------------------------------------------------------------

##here's how many Ss there are for each agebin for each trial type, with the >3 pairs, >7 items rule  
flickr_ds_all3 <- flickr_ds_all3 %>%
  mutate(SubjectNumber = factor(SubjectNumber),
         AgeGroup3 = factor(AgeGroup3, levels = c("(5.9,8]", "(8,11]", "(11,15]")))%>%
  group_by(AgeGroup3, study)

flickr_ds_pairs <- flickr_ds_pairs %>%
  mutate(AgeGroup3 = factor(AgeGroup3, levels = c("(5.9,8]", "(8,11]", "(11,15]")))%>%
  group_by(AgeGroup3, study)

flickr_ds_pairs<-flickr_ds_pairs%>%
  mutate(pair = factor(pair, 
                       levels=c("ear_spoon","hair_banana","hand_yogurt","mouth_apple",
                                "foot_milk","eyes_cookie","leg_bottle","nose_juice")))


flickr_dsmelt_all3 <- flickr_dsmelt_all3 %>%
  mutate(SubjectNumber = factor(SubjectNumber),
         AgeGroup3 = factor(AgeGroup3, levels = c("(5.9,8]", "(8,11]", "(11,15]")))%>%
  group_by(AgeGroup3, study)


#these are the 245 Ss in the paired-picture data: grouped needs to be same babies
flickr_vs_Ss <- flickr_dsmelt_all3 %>%
  unite(studysubj, study, SubjectNumber, remove = F)%>%
  filter(numpairs>3)%>%
  ungroup()%>%
  dplyr::select(studysubj)%>%
  distinct(studysubj)


summary(flickrvs_gr)
flickrvs_gr <- flickrvs_gr %>%
  mutate(target = factor(target, levels = c("face","feet","hands","legs","ear","eyes",
                                            "hair","mouth","apple","bottle","cookie","yogurt","banana",
                                            "juice","milk","spoon")),
         AgeGroup3 = factor(AgeGroup3, levels = c("(5.9,8]", "(8,11]", "(11,15]")))%>%
  filter(studysubj %in% flickr_vs_Ss$studysubj)


flickrvs_gr_subj <- flickrvs_gr_subj %>%
  mutate(AgeGroup3 = factor(AgeGroup3, levels = c("(5.9,8]", "(8,11]", "(11,15]")))%>%
  filter(studysubj %in% flickr_vs_Ss$studysubj)

flickvs_gr_items_basedonDS_Ss <- flickvs_gr_items_basedonDS_Ss %>%
  mutate(target = factor(target, levels = c("face","feet","hands","legs","ear","eyes",
                                            "hair","mouth","apple","bottle","cookie","yogurt","banana",
                                            "juice","milk","spoon")),
         AgeGroup3 = factor(AgeGroup3, levels = c("(5.9,8]", "(8,11]", "(11,15]")))


# demographics and cdi analysis -------------------------------------------

flickrvs_demo <- rbind(flickrvamp_demo, flickrvoice_demo)


summary(vampvoice_cdi)
cdi_flickrs_ds <- vampvoice_cdi%>%
  mutate(SubjectNumber = factor(SubjectNumber))%>%
  left_join(flickr_ds_all3)%>%
  filter(numpairs>3)%>%
  ungroup()%>%
  mutate(AgeMonthsCtr = AgeMonths - mean(AgeMonths))


# Ss and Pairs -----------------------------------------------------

#6-7.9s Ss:
#all groups, overall, SBS
#these aren't actually used in the markdown but they're a conventient descriptor
by3conditions_Ss <- flickr_ds_all3%>%
  filter(numpairs>3)%>%
  group_by(AgeGroup3, study)%>%
  summarise(mean_group = round(mean(ds_mean, na.rm=T),3),
            sd_group = round(sd(ds_mean, na.rm=T),3),
            numpos = sum(ds_mean>0),
            numneg = sum(ds_mean<=0),
            n = length(ds_mean),
            wilcox.pval= round(wilcox.test(ds_mean)$p.value,5),
            binom.test.pval = round(binom.test(numpos, n, .5)$p.value,3),
            shapiro.test.pval= round(shapiro.test(ds_mean)$p.value,3))
# 
by3conditions_pairs <- flickr_ds_pairs%>%
  group_by(AgeGroup3, study)%>%
  summarise(mean_group = round(mean(pair_agebin_ds, na.rm=T),3),
            sd_group = round(sd(pair_agebin_ds, na.rm=T),3),
            numpos = sum(pair_agebin_ds>0),
            numneg = sum(pair_agebin_ds<=0),
            n = length(pair_agebin_ds),
            wilcox.pval= round(wilcox.test(pair_agebin_ds)$p.value,5),
            binom.test.pval = round(binom.test(numpos, n, .5)$p.value,2))

no_eyes_by3conditions_pairs <- flickr_ds_pairs%>%
  filter(pair!="eyes_cookie")%>%
  group_by(AgeGroup3, study)%>%
  summarise(mean_group = round(mean(pair_agebin_ds, na.rm=T),3),
            sd_group = round(sd(pair_agebin_ds, na.rm=T),3),
            numpos = sum(pair_agebin_ds>0),
            numneg = sum(pair_agebin_ds<=0),
            n = length(pair_agebin_ds),
            wilcox.pval= round(wilcox.test(pair_agebin_ds)$p.value,5),
            binom.test.pval = round(binom.test(numpos, n, .5)$p.value,2))

