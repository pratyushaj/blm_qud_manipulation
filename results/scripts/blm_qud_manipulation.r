library(tidyverse)
library(brms)
library(forcats)
library(stringr)
library(lme4)
library(languageR)
require(plyr)

theme_set(theme_bw(18))
source("helpers.r")

d = read.table(file="../data/experiment.csv",sep=",", header=T)
d = as.data.frame(lapply(d, function(x) {gsub('\"',"",x)}))

#hard-coded filtering for participants who reported a native language other than English --> cut out 2 participants 
d = d %>%
   filter(language != 'russian' & language != 'urdu') %>%
   droplevels()

#basic tenets of the dataset
head(d)
nrow(d)
summary(d)

d$Trial = as.numeric(as.character(d$slide_number)) - 2
d$Answer.time_in_minutes = as.numeric(as.character(d$Answer.time_in_minutes))
d$age = as.numeric(as.character(d$age))
d$response = as.numeric(as.character(d$response))
d$response_other = as.numeric(as.character(d$response_other))

#N = 158
length(unique(d$workerid))

# # look at turker comments
unique(d$comments)
# 
summary(d$Answer.time_in_minutes)

#plot of full range of response times (for whole experiment in minutes) by trial not by participant
ggplot(d, aes(Answer.time_in_minutes)) +
   geom_histogram()
ggsave(file="../graphs/answer_time_in_minutes.png")

#gender breakdown of trials (not by participant)
ggplot(d, aes(gender)) +
   stat_count()
ggsave(file="../graphs/participant_gender.png")

#did they understand the HIT?
ggplot(d, aes(asses)) +
   stat_count()
ggsave(file="../graphs/participant_understood_task.png")

# age by trial
ggplot(d, aes(age)) +
   geom_histogram()
ggsave(file="../graphs/age_by_trial.png")

table(d$age)

#education by trial 
ggplot(d, aes(education)) +
   stat_count()
ggsave(file="../graphs/education_by_trial.png")

#language
ggplot(d, aes(language)) +
   stat_count() +
   theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))
# 
ggplot(d, aes(enjoyment)) +
  stat_count()

#getting all critical qud manipulation trials
cover_stories = d %>%
   filter(block == "cover_stories") %>%
   droplevels()

#filtering for people whose variance wasn't good enough 
func2 <- function(xx)
{
  priors$michelle_var = abs(max(xx$response,xx$response_other)-min(xx$response,xx$response_other))
}

individualPriorVar <- function(xx)
{
  priors$priors_var = abs(max(xx$response)-min(xx$response))
}

cover_stories = cover_stories %>%
  right_join(ddply(cover_stories, .(workerid), func2),by=c('workerid'))%>%
  right_join(ddply(priors, .(workerid), individualPriorVar),by=c('workerid'))%>%
  filter(V1.x > 0.1 & V1.y > 0.1)

#getting all critical prior collection trials (no experimental manipulation) 
priors = d %>%
   filter(block == "priors") %>%
   droplevels() %>%
  right_join(ddply(cover_stories, .(workerid), func2),by=c('workerid'))%>%
  right_join(ddply(priors, .(workerid), individualPriorVar),by=c('workerid'))%>%
  filter(V1.x > 0.1 & V1.y > 0.1)

nrow(cover_stories)
nrow(priors)
# nrow(identity)

#Plotting means for critical noun vs other noun in terms of qud with error bars - separated by noun
detach(package:plyr)
agr = cover_stories %>%                                   
  group_by(qud,nounclass,crit_noun) %>%                    
  summarize(mean_response=mean(response), 
    mean_response_other=mean(response_other), 
    cilow_response=ci.low(response),
    cilow_response_other=ci.low(response_other),cihigh_response=ci.high(response), 
    cihigh_response_other=ci.high(response_other)) %>%
  ungroup() %>%
  mutate(YMin_response=mean_response-cilow_response,YMax_response=mean_response+cihigh_response,YMin_response_other=mean_response_other-cilow_response_other,YMax_response_other=mean_response_other+cihigh_response_other)

agr_means = agr %>%
  select(qud,nounclass,crit_noun,mean_response,mean_response_other) %>%
  gather(ResponseType,Mean,-qud,-nounclass,-crit_noun) %>%
  mutate(ResponseType=case_when(ResponseType == "mean_response" ~ "target",
                                ResponseType == "mean_response_other" ~ "other"))

agr_ymin = agr %>%
  select(qud,nounclass,crit_noun,YMin_response,YMin_response_other) %>%
  gather(ResponseType,YMin,-qud,-nounclass,-crit_noun) %>%
  mutate(ResponseType=case_when(ResponseType == "YMin_response" ~ "target",
                                ResponseType == "YMin_response_other" ~ "other"))

agr_ymax = agr %>%
  select(qud,nounclass,crit_noun,YMax_response,YMax_response_other) %>%
  gather(ResponseType,YMax,-qud,-nounclass,-crit_noun) %>%
  mutate(ResponseType=case_when(ResponseType == "YMax_response" ~ "target",
                                ResponseType == "YMax_response_other" ~ "other"))

toplot = left_join(agr_means,agr_ymin,by=c("qud","nounclass","crit_noun","ResponseType")) %>%
  left_join(agr_ymax,by=c("qud","nounclass","crit_noun","ResponseType"))

dodge=position_dodge(.9)

p=ggplot(toplot, aes(x=qud,y=Mean,fill=ResponseType)) +
  geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),position=dodge,width=.25) +
  facet_wrap(~crit_noun)
ggsave(p,file="../graphs/qud_means_noun.png",width=10,height=10)


#Plotting means for crit noun versus other noun with error bars, separated by noun class 
agr = cover_stories %>%                                   
  group_by(qud,nounclass) %>%                    
  summarize(mean_response=mean(as.numeric(as.character(response))), 
            mean_response_other=mean(as.numeric(as.character(response_other))), 
            cilow_response=ci.low(as.numeric(as.character(response))),
            cilow_response_other=ci.low(as.numeric(as.character(response_other))),cihigh_response=ci.high(as.numeric(as.character(response))), 
            cihigh_response_other=ci.high(as.numeric(as.character(response_other)))) %>%
  ungroup() %>%
  mutate(YMin_response=mean_response-cilow_response,YMax_response=mean_response+cihigh_response,YMin_response_other=mean_response_other-cilow_response_other,YMax_response_other=mean_response_other+cihigh_response_other)

agr_means = agr %>%
  select(qud,nounclass,mean_response,mean_response_other) %>%
  gather(ResponseType,Mean,-qud,-nounclass) %>%
  mutate(ResponseType=case_when(ResponseType == "mean_response" ~ "target",
                                ResponseType == "mean_response_other" ~ "other"))

agr_ymin = agr %>%
  select(qud,nounclass,YMin_response,YMin_response_other) %>%
  gather(ResponseType,YMin,-qud,-nounclass) %>%
  mutate(ResponseType=case_when(ResponseType == "YMin_response" ~ "target",
                                ResponseType == "YMin_response_other" ~ "other"))

agr_ymax = agr %>%
  select(qud,nounclass,YMax_response,YMax_response_other) %>%
  gather(ResponseType,YMax,-qud,-nounclass) %>%
  mutate(ResponseType=case_when(ResponseType == "YMax_response" ~ "target",
                                ResponseType == "YMax_response_other" ~ "other"))

toplot = left_join(agr_means,agr_ymin,by=c("qud","nounclass","ResponseType")) %>%
  left_join(agr_ymax,by=c("qud","nounclass","ResponseType"))

dodge=position_dodge(.9)

p=ggplot(toplot, aes(x=qud,y=Mean,fill=ResponseType)) +
  geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),position=dodge,width=.25) +
  facet_wrap(~nounclass)
ggsave(p,file="../graphs/qud_means_nounclass.png",width=10,height=10)

#Plotting means for crit noun and other noun with error bars in terms of qud
agr = cover_stories %>%                                   
  group_by(qud) %>%                    
  summarize(mean_response=mean(as.numeric(as.character(response))), 
            mean_response_other=mean(as.numeric(as.character(response_other))), 
            cilow_response=ci.low(as.numeric(as.character(response))),
            cilow_response_other=ci.low(as.numeric(as.character(response_other))),cihigh_response=ci.high(as.numeric(as.character(response))), 
            cihigh_response_other=ci.high(as.numeric(as.character(response_other)))) %>%
  ungroup() %>%
  mutate(YMin_response=mean_response-cilow_response,YMax_response=mean_response+cihigh_response,YMin_response_other=mean_response_other-cilow_response_other,YMax_response_other=mean_response_other+cihigh_response_other)

agr_means = agr %>%
  select(qud,mean_response,mean_response_other) %>%
  gather(ResponseType,Mean,-qud) %>%
  mutate(ResponseType=case_when(ResponseType == "mean_response" ~ "target",
                                ResponseType == "mean_response_other" ~ "other"))

agr_ymin = agr %>%
  select(qud,YMin_response,YMin_response_other) %>%
  gather(ResponseType,YMin,-qud) %>%
  mutate(ResponseType=case_when(ResponseType == "YMin_response" ~ "target",
                                ResponseType == "YMin_response_other" ~ "other"))

agr_ymax = agr %>%
  select(qud,YMax_response,YMax_response_other) %>%
  gather(ResponseType,YMax,-qud) %>%
  mutate(ResponseType=case_when(ResponseType == "YMax_response" ~ "target",
                                ResponseType == "YMax_response_other" ~ "other"))

toplot = left_join(agr_means,agr_ymin,by=c("qud","ResponseType")) %>%
  left_join(agr_ymax,by=c("qud","ResponseType"))

dodge=position_dodge(.9)

p = ggplot(toplot, aes(x=qud,y=Mean,fill=ResponseType)) +
  geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),position=dodge,width=.25)
ggsave(p,file="../graphs/qud_means.png",width=10,height=10)

#Plotting the correlation between judgments for a certain noun and priors of that same noun 
agr2_crit = cover_stories %>%                                   
  group_by(workerid,qud,predicate,nounclass,crit_noun) %>%    
  select(workerid,qud,predicate,nounclass,crit_noun,response,response_other)%>%
  ungroup() %>%
  arrange(nounclass)%>%
  gather(ResponseType,Response,-qud,-nounclass,-crit_noun,-workerid,-predicate)%>%
  filter(ResponseType == "response")

agr2_other = cover_stories %>%                                   
  group_by(workerid,qud,predicate,nounclass,other_noun) %>%    
  select(workerid,qud,predicate,nounclass,other_noun,response,response_other)%>%
  ungroup() %>%
  arrange(nounclass)%>%
  gather(ResponseType,Response,-qud,-nounclass,-other_noun,-workerid,-predicate)%>%
  filter(ResponseType == "response_other")

agr2 = bind_rows(agr2_crit,agr2_other) %>%
  mutate(Noun = ifelse(is.na(crit_noun), as.character(other_noun), as.character(crit_noun)))%>%
  select(workerid,qud,predicate,nounclass,Response,ResponseType,Noun)
#%>%
#  gather(NounType,Noun,-ResponseType,-Response,-qud,-nounclass,-workerid,-predicate) 

agr2$Noun <- factor(agr2$Noun)

priors_truncated = priors %>%
  select(workerid,predicate,response,noun)%>%
  mutate(response_prior=response)%>%
  mutate(Noun=noun)%>%
  mutate(Noun=case_when(Noun == "people in their 20&quotechars" ~ "People in their 20&quotechars",
                        Noun == "people in their 50&quotechars" ~ "People in their 50&quotechars",
                        Noun == "Americans" ~ "Americans",
                        Noun == "Europeans" ~ "Europeans",
                        Noun == "men" ~ "men",
                        Noun == "women" ~ "women",
                        Noun == "Democrats" ~ "Democrats",
                        Noun == "Republicans" ~ "Republicans"))

priors_truncated$Noun <- factor(priors_truncated$Noun)

priors_responses = agr2 %>%
  select(workerid,qud,predicate,Response,ResponseType,Noun) %>%
  right_join(priors_truncated,by=c("workerid","predicate","Noun"))
options(digits=4)
priors_responses$Response <- as.double(priors_responses$Response)

p=ggplot(priors_responses, aes(x=Response,y=response_prior,color=Noun)) +
  geom_point() +
  geom_smooth(method="lm")
ggsave(p,file="../graphs/priors_responses_noun.png",width=10,height=10)

# Attempting to make a scatterplot of slider responses and prior responses per individual 
ggplot(priors_responses, aes(x=Response,y=response_prior,color=Noun)) +
  geom_point() +
  geom_smooth(method="lm")

p = ggplot(priors_responses, aes(x=Response,y=response_prior,color=ResponseType,group=1)) +
  geom_point() +
  geom_smooth(method="lm")+
  facet_wrap(~workerid) + 
  ylim(0,1)
ggsave(p,file="../graphs/priors_responses_subject.png",width=40,height=40)


ggsave(file="../graphs/priors_responses.png",width=400,height=200)

#Correlation between Michelle and prior judgments
res <- cor.test(priors_responses$Response, priors_responses$response_prior, 
                method = "pearson")

# Pearson's product-moment correlation
# 
# data:  priors_responses$Response and priors_responses$response_prior
# t = 5.8, df = 1300, p-value = 6e-09
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# 0.1082 0.2156
# sample estimates:
# cor 
# 0.1624 

#Run correlations on individual subjects' data between Michelle and prior - separate into low and high thresholds that way, create mean response/qud bar plot with error bars 
require(plyr)
individualCors <- function(xx)
{
  priors_responses$individualcor = cor(xx$Response,xx$response_prior)
}

priors_responses = priors_responses %>%
  right_join(ddply(priors_responses, .(workerid), individualCors),by=c("workerid"))%>%
  mutate(individualcor_4bins = cut(V1, 4))

ggplot(priors_responses%>%select(workerid,V1)%>%unique(), aes(V1)) +
  geom_histogram()

##Plotting the same plots with error bars in terms of 4 bins of correlation
detach(package:plyr)
agr = cover_stories %>% 
  right_join(priors_responses%>%select(workerid,individualcor_4bins),by=c("workerid"))%>%
  group_by(qud,individualcor_4bins) %>%                    
  summarize(mean_response=mean(as.numeric(as.character(response))), 
            mean_response_other=mean(as.numeric(as.character(response_other))), 
            cilow_response=ci.low(as.numeric(as.character(response))),
            cilow_response_other=ci.low(as.numeric(as.character(response_other))),cihigh_response=ci.high(as.numeric(as.character(response))), 
            cihigh_response_other=ci.high(as.numeric(as.character(response_other)))) %>%
  ungroup() %>%
  mutate(YMin_response=mean_response-cilow_response,YMax_response=mean_response+cihigh_response,YMin_response_other=mean_response_other-cilow_response_other,YMax_response_other=mean_response_other+cihigh_response_other)

agr_means = agr %>%
  select(qud,individualcor_4bins,mean_response,mean_response_other) %>%
  gather(ResponseType,Mean,-qud,-individualcor_4bins) %>%
  mutate(ResponseType=case_when(ResponseType == "mean_response" ~ "target",
                                ResponseType == "mean_response_other" ~ "other"))

agr_ymin = agr %>%
  select(qud,individualcor_4bins,YMin_response,YMin_response_other) %>%
  gather(ResponseType,YMin,-qud,-individualcor_4bins) %>%
  mutate(ResponseType=case_when(ResponseType == "YMin_response" ~ "target",
                                ResponseType == "YMin_response_other" ~ "other"))

agr_ymax = agr %>%
  select(qud,individualcor_4bins,YMax_response,YMax_response_other) %>%
  gather(ResponseType,YMax,-qud,-individualcor_4bins) %>%
  mutate(ResponseType=case_when(ResponseType == "YMax_response" ~ "target",
                                ResponseType == "YMax_response_other" ~ "other"))

toplot = left_join(agr_means,agr_ymin,by=c("qud","individualcor_4bins","ResponseType")) %>%
  left_join(agr_ymax,by=c("qud","individualcor_4bins","ResponseType"))

dodge=position_dodge(.9)

p=ggplot(toplot, aes(x=qud,y=Mean,fill=ResponseType)) +
  geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),position=dodge,width=.25) +
  facet_wrap(~individualcor_4bins)
ggsave(p,file="../graphs/qud_means_cors.png",width=10,height=10)

###ANALYSIS
ad = priors_responses %>%
  droplevels() %>%
  mutate(Noun = as.factor(as.character(Noun))) %>%
  mutate(ResponseType = as.factor(as.character(ResponseType)))%>%
  mutate(cqud = myCenter(qud), cResponseType = myCenter(ResponseType),cPrior = myCenter(response_prior))
nrow(ad)
summary(ad)

ad$Item = as.factor(paste(ad$Noun,ad$predicate))

library(lme4)
m = lmer(Response ~ cqud*cResponseType+V1 + (1 + cqud*cResponseType+V1 |Item) + (1 + cqud*cResponseType+V1|workerid), data=ad)
summary(m)

#bayesian regression - run this again with response_prior as an interaction
#compare the two models with Bayes Factor (looks at how much more likely one model is compared to another)

m1 = brm(Response ~ cqud * cResponseType + cPrior + (1 + cqud * cResponseType+cPrior|Item) + (1 + cqud * cResponseType+cPrior|workerid), data=ad,save_all_pars=TRUE,control = list(adapt_delta = 0.9999),iter = 3000)
summary(m1)
plot(m1)

m2 = brm(Response ~ cqud * cResponseType * cPrior + (1 + cqud * cResponseType * cPrior|Item) + (1 + cqud * cResponseType * cPrior|workerid), save_all_pars=TRUE,data=ad,control = list(adapt_delta = 0.9999),iter = 3000)
summary(m2)
plot(m2)

#what is the probability that the effect being examined is nonzero (slightly different than the p-value)
mean(posterior_samples(m1,pars='b_cqud:cResponseType') < 0)
mean(posterior_samples(m1,pars='b_cqud') < 0)

#Figure out what to do with the model not converging - i.e. make it converge
#incorporate prior as a fixed effect 

#Analysis: predict difference between target and other, or predict slider ratings for the other noun 
#diff=target~response~other_response (do this later)
#lmer(diff~qud + (1~qud|item) + (1+qud|workerid)
#can also add +target_prior + other_prior, ratio of the two 

#brms package - Inference algorithm for lmer is MLE, use brms for bayesian regression 

#getting things from wide to long format: workerid, variable, value of the variable per row 
#one column of michelle judgments, one column of prior judgments --> run a correlation between those 
#what do histograms look like over priors for different nouns? 

#How correlated are values given in one task versus the other, control ho wmuch judgments of michelle are 
# affected by their judgments, may want to regress them out (can include their prior in the model as a variable)
#can also regress michelle judgment onto individual judgment, use residual error as new dependent variable 
# scatterplot of priors versus judgments assigned to michelle for each data point (own on X, Michelle on Y), color points differently based
#on nounclass (or by noun directly)

#06/05/2018: Exclude anyone whose min and max isn't more than 0.1 apart (max - min > 0.1)
#Cut the prior/Michelle correlation into 3 bins instead of 2 

