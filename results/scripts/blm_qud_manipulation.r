library(tidyverse)
library(forcats)
library(stringr)
library(ggplot2)

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

#getting all critical prior collection trials (no experimental manipulation) 
priors = d %>%
   filter(block == "priors") %>%
   droplevels()

#playing around - turning cover story sliders to 2 and 3 bins 
cover_stories_cut = d %>%
  filter(block == "cover_stories") %>%
  droplevels() %>%
  mutate(response = as.numeric(as.character(response))) %>%
  mutate(response_2bins = cut(response, 2, labels = c("low", "high"))) %>%
  mutate(response_3bins = cut(response, 3, labels = c("low", "medium", "high"))) %>%
  mutate(response_other = as.numeric(as.character(response_other))) %>%
  mutate(response_2bins_other = cut(response_other, 2, labels = c("low", "high"))) %>%
  mutate(response_3bins_other = cut(response_other, 3, labels = c("low", "medium", "high")))
# 
# View(likeability)
# 
nrow(cover_stories)
nrow(priors)
# nrow(identity)

#Getting only relevant columns
covers_crit = cover_stories %>% 
    select(workerid,response,predicate,qud)
covers_crit$which_noun <- rep("critical",nrow(covers_crit))

covers_other = cover_stories %>% 
  select(workerid,response_other,predicate,qud)%>%
  mutate(response = response_other) %>%
  select(-response_other)
covers_other$which_noun <- rep("other",nrow(covers_other))

covers = rbind(covers_crit,covers_other)

# 
# ggplot(identity, aes(x=response, fill=nounclass)) +
#   geom_histogram(stat="count") +
#   facet_wrap(~noun)
# 
ggplot(cover_stories_cut, aes(x=qud, fill=response_2bins)) +
   geom_histogram(stat="count",position=position_dodge())

ggplot(cover_stories, aes(x=qud, y=response)) + stat_summary(fun.y="mean", geom="bar")

ggplot(cover_stories_cut, aes(x=qud)) +
  geom_histogram(aes(fill=response_2bins_other),stat="count",position=position_dodge()) +
  geom_histogram(aes(fill=response_2bins),stat="count",position=position_dodge())

ggplot(cover_stories_cut, aes(x=qud)) + 
  geom_line(aes(y = response, colour = "critical_noun")) + 
  geom_line(aes(y = response_other, colour = "other_noun"))

cover_stories %>%                                   
  group_by(qud) %>%                    
  summarize(mean_response=mean(as.numeric(as.character(response))), 
    mean_response_other=mean(as.numeric(as.character(response_other))))

# ggplot(likeability, aes(x=response, fill=nounclass)) +
#   geom_density() +
#   facet_wrap(~noun)
# 
# ggplot(likeability, aes(x=response, fill=noun)) +
#   geom_density(alpha=.5) +
#   facet_wrap(~nounclass)
# 
# ggplot(production, aes(x=class, fill=response)) +
#   geom_histogram(stat="count",position="dodge") +
#   facet_wrap(~noun) +
#   theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))
# 
# View(identity)
# 
# # get likeability means
# agr = likeability %>%
#   group_by(noun,nounclass) %>%
#   summarize(Mean=mean(response),CILow=ci.low(response),CIHigh=ci.high(response)) %>%
#   mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)
# 
# ggplot(agr, aes(x=noun,y=Mean,fill=nounclass)) +
#   geom_bar(stat="identity") +
#   geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
#   facet_wrap(~nounclass,scales="free_x",nrow=1) +
#   theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))
# 
# ggplot(likeability, aes(x=noun,y=response,fill=nounclass)) +
#   # geom_violin() +
#   geom_boxplot() +
#   facet_wrap(~nounclass,scales="free_x",nrow=1) +
#   theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))
# 
# # merge identity & likeability
# like = likeability %>%
#   select(workerid,noun,response,nounclass) %>%
#   mutate(response_likeable = response) %>%
#   select(-response)
# 
# id = identity %>%
#   select(workerid,noun,response,nounclass) %>%
#   mutate(response_identity = response) %>%
#   left_join(like,by=c("workerid","noun","nounclass"))
# 
# View(id)
# 
# ggplot(id %>% filter(response_identity != "Confused"), aes(x=response_likeable, fill=response_identity)) +
#   geom_density(alpha=.5) +
#   facet_wrap(~noun)
# 
# #merge identity and production 
# prod = production %>%
#   select(workerid,noun,response,class) %>%
#   mutate(response_prod = response) %>%
#   select(-response)
# 
# #Resolving incompatible values for 'Black lives matter' condition between identity and production
# prod$noun <- gsub('Black lives', 'Black people', prod$noun)
# #prod$noun_class <- gsub('black_lives', 'black', prod$nounclass)
# 
# ######### PRODUCTION X IDENTITY ##########
# id_collapsed_pred = identity %>%
#   select(workerid,response,noun) %>%
#   rename(response_identity = response) %>%
#   right_join(prod,by=c("workerid","noun"))
# 
# #identity collapsed pred only 
# ggplot(id_collapsed_pred %>% filter(response_identity != 'Confused'), aes(x=class, fill=response_prod)) +
#   geom_histogram(stat="count",position="dodge") +
#   facet_grid(noun~response_identity, scales="free_y") +
#   theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))
# ggsave(file="../graphs/ingroupoutgroup.png", height=30,width=20,limitsize=FALSE)
# 
# #identity collapsed pred only in terms of proportions - NOT QUITE RIGHT 
# ggplot(id_collapsed_pred %>% filter(response_identity != 'Confused'), aes(x=class, fill=response_prod)) +
#   geom_bar(stat="count",position="fill") +
#   facet_grid(noun~response_identity, scales="free_y") +
#   theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1)) +
#   scale_y_continuous(labels=scales::percent) +
#   labs(y="Proportion of Responses", x="class")
# ggsave(file="../graphs/ingroupoutgroup_proportions.png",height=30,width=20,limitsize=FALSE)
# 
# #identity collapsed pred and noun 
# ggplot(id_collapsed_pred %>% filter(response_identity != 'Confused'), aes(x=class, fill=response_prod)) +
#   geom_histogram(stat="count",position="dodge") +
#   facet_wrap(~response_identity, scales="free_y") +
#   theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))
# ggsave(file="../graphs/ingroupoutgroup_collapsed.png",height=20,width=20,limitsize=FALSE)
# 
# #identity collapsed pred and noun in terms of proportions rather than counts - NOT QUITE RIGHT
# ggplot(id_collapsed_pred %>% filter(response_identity != 'Confused'), aes(x=class, fill=response_prod)) +
#   geom_bar(stat="count",position="fill") +
#   facet_wrap(~response_identity, scales="free_y") +
#   theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))+
#   scale_y_continuous(labels=scales::percent) +
#   labs(y="Proportion of Responses", x="class") 
# ggsave(file="../graphs/ingroupoutgroup_collapsed_proportions.png",height=10,width=20,limitsize=FALSE)
# 
# ###### PRODUCTION X LIKEABILITY 3 BINS#######
# 
# like_collapsed_pred = likeability %>%
#   select(workerid,response_3bins,noun) %>%
#   right_join(prod,by=c("workerid","noun"))
# 
# #likabiity 3 bins collapsed by pred, not noun
# ggplot(like_collapsed_pred, aes(x=class, fill=response_prod)) +
#   geom_histogram(stat="count",position="dodge") +
#   facet_grid(noun~response_3bins, scales="free_y") +
#   theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))
# ggsave(file="../graphs/likeability3bins.png",height=20,width=20,limitsize=FALSE)
# 
# #likabiity 3 bins collapsed by pred, not noun PROPORTIONS
# ggplot(like_collapsed_pred, aes(x=class, fill=response_prod)) +
#   geom_bar(stat="count",position="fill") +
#   facet_grid(noun~response_3bins, scales="free_y") +
#   scale_y_continuous(labels=scales::percent) +
#   theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))
# ggsave(file="../graphs/likeability3bins_proportions.png",height=30,width=20,limitsize=FALSE)
# 
# #likability 3 bins collapsed pred and noun 
# ggplot(like_collapsed_pred, aes(x=class, fill=response_prod)) +
#   geom_histogram(stat="count",position="dodge") +
#   facet_wrap(~response_3bins, scales="free_y") +
#   theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))
# ggsave(file="../graphs/likeability3bins_collapsed.png",height=10,width=20,limitsize=FALSE)
# 
# #likability 3 bins collapsed pred and noun proportions 
# ggplot(like_collapsed_pred, aes(x=class, fill=response_prod)) +
#   geom_bar(stat="count",position="fill") +
#   facet_wrap(~response_3bins, scales="free_y") +
#   scale_y_continuous(labels=scales::percent) +
#   theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))
# ggsave(file="../graphs/likeability3bins_collapsed_proportions.png",height=10,width=20,limitsize=FALSE)
# 
# ###### PRODUCTION X LIKEABILITY 2 BINS#######
# 
# like_collapsed_pred = likeability %>%
#   select(workerid,response_2bins,noun) %>%
#   right_join(prod,by=c("workerid","noun"))
# 
# #likabiity 2 bins collapsed by pred, not noun
# ggplot(like_collapsed_pred, aes(x=class, fill=response_prod)) +
#   geom_histogram(stat="count",position="dodge") +
#   facet_grid(noun~response_2bins, scales="free_y") +
#   theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))
# ggsave(file="../graphs/likeability2bins.png",height=20,width=20,limitsize=FALSE)
# 
# #likabiity 2 bins collapsed by pred, not noun PROPORTIONS
# ggplot(like_collapsed_pred, aes(x=class, fill=response_prod)) +
#   geom_bar(stat="count",position="fill") +
#   facet_grid(noun~response_2bins, scales="free_y") +
#   scale_y_continuous(labels=scales::percent) +
#   theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))
# ggsave(file="../graphs/likeability2bins_proportions.png",height=30,width=20,limitsize=FALSE)
# 
# #likability 2 bins collapsed pred and noun 
# ggplot(like_collapsed_pred, aes(x=class, fill=response_prod)) +
#   geom_histogram(stat="count",position="dodge") +
#   facet_wrap(~response_2bins, scales="free_y") +
#   theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))
# ggsave(file="../graphs/likeability2bins_collapsed.png",height=10,width=20,limitsize=FALSE)
# 
# #likability 3 bins collapsed pred and noun proportions 
# ggplot(like_collapsed_pred, aes(x=class, fill=response_prod)) +
#   geom_bar(stat="count",position="fill") +
#   facet_wrap(~response_2bins, scales="free_y") +
#   scale_y_continuous(labels=scales::percent) +
#   theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))
# ggsave(file="../graphs/likeability2bins_collapsed_proportions.png",height=10,width=20,limitsize=FALSE)
# 
# #######DEMOCRATS LIKEABILITY ##########
# 
# #getting responses to questions about Democrats 
# prod_pred = production %>% 
#   filter(noun == 'Democrats') %>%
#   select(workerid,noun,response,predicate,class) %>%
#   mutate(response_prod = response) %>%
#   select(-response)
# 
# prod_pred$noun <- gsub('Black lives', 'Black people', prod_pred$noun)
# 
# #joining responses to questions about men with info about whether they identify as a man  
# 
# like_collapsed_pred_prod = likeability %>%
#   select(workerid,response_2bins,noun) %>%
#   right_join(prod_pred,by=c("workerid","noun"))
# 
# #Housekeeping for per-predicate plots: ordering in terms of predicate valence (done manually)#
# like_collapsed_pred_prod$predicate2 = factor(like_collapsed_pred_prod$predicate, levels=c('competent','generous','honest','kind','smart','strong','incompetent','lie','rude','sexist','unintelligent','weak','eat','sleep','breathe','walk'))
# 
# ggplot(like_collapsed_pred_prod, aes(x=response_2bins, fill = response_prod)) +
#   geom_bar(stat="count",position="fill") +
#   facet_wrap(~predicate2, scales="free_y") +
#   theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))
# ggsave(file="../graphs/democrats_likability_predicates_2bins.png",height=20,width=20,limitsize=FALSE)
# 
# ########## REPUBLICANS LIKEABILITY ##########
# 
# #getting responses to questions about Democrats 
# prod_pred = production %>% 
#   filter(noun == 'Republicans') %>%
#   select(workerid,noun,response,predicate,class) %>%
#   mutate(response_prod = response) %>%
#   select(-response)
# 
# prod_pred$noun <- gsub('Black lives', 'Black people', prod_pred$noun)
# 
# like_collapsed_pred_prod = likeability %>%
#   select(workerid,response_2bins,noun) %>%
#   right_join(prod_pred,by=c("workerid","noun"))
# 
# #Housekeeping for per-predicate plots: ordering in terms of predicate valence (done manually)#
# like_collapsed_pred_prod$predicate2 = factor(like_collapsed_pred_prod$predicate, levels=c('competent','generous','honest','kind','smart','strong','incompetent','lie','rude','sexist','unintelligent','weak','eat','sleep','breathe','walk'))
# 
# ggplot(like_collapsed_pred_prod, aes(x=response_2bins, fill = response_prod)) +
#   geom_bar(stat="count",position="fill") +
#   facet_wrap(~predicate2, scales="free_y") +
#   theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))
# ggsave(file="../graphs/republicans_likability_predicates_2bins.png",height=20,width=20,limitsize=FALSE)
# 
# ####### MEN LIKEABILITY ##########
# 
# #getting responses to questions about men 
# prod_pred = production %>% 
#   filter(noun == 'men') %>%
#   select(workerid,noun,response,predicate,class) %>%
#   mutate(response_prod = response) %>%
#   select(-response)
# 
# prod_pred$noun <- gsub('Black lives', 'Black people', prod_pred$noun)
# 
# #joining responses to questions about men with info about whether they identify as a man  
# 
# like_collapsed_pred_prod = likeability %>%
#   select(workerid,response_2bins,noun) %>%
#   right_join(prod_pred,by=c("workerid","noun"))
# 
# #Housekeeping for per-predicate plots: ordering in terms of predicate valence (done manually)#
# like_collapsed_pred_prod$predicate2 = factor(like_collapsed_pred_prod$predicate, levels=c('competent','generous','honest','kind','smart','strong','incompetent','lie','rude','sexist','unintelligent','weak','eat','sleep','breathe','walk'))
# 
# ggplot(like_collapsed_pred_prod, aes(x=response_2bins, fill = response_prod)) +
#   geom_bar(stat="count",position="fill") +
#   facet_wrap(~predicate2, scales="free_y") +
#   theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))
# ggsave(file="../graphs/men_likability_predicates_2bins.png",height=20,width=20,limitsize=FALSE)
# 
# ########## WOMEN LIKEABILITY ##########
# 
# #getting responses to questions about women 
# prod_pred = production %>% 
#   filter(noun == 'women') %>%
#   select(workerid,noun,response,predicate,class) %>%
#   mutate(response_prod = response) %>%
#   select(-response)
# 
# prod_pred$noun <- gsub('Black lives', 'Black people', prod_pred$noun)
# 
# like_collapsed_pred_prod = likeability %>%
#   select(workerid,response_2bins,noun) %>%
#   right_join(prod_pred,by=c("workerid","noun"))
# 
# #Housekeeping for per-predicate plots: ordering in terms of predicate valence (done manually)#
# like_collapsed_pred_prod$predicate2 = factor(like_collapsed_pred_prod$predicate, levels=c('competent','generous','honest','kind','smart','strong','incompetent','lie','rude','sexist','unintelligent','weak','eat','sleep','breathe','walk'))
# 
# ggplot(like_collapsed_pred_prod, aes(x=response_2bins, fill = response_prod)) +
#   geom_bar(stat="count",position="fill") +
#   facet_wrap(~predicate2, scales="free_y") +
#   theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))
# ggsave(file="../graphs/women_likability_predicates_2bins.png",height=20,width=20,limitsize=FALSE)
# 
# ########## DEMOCRATS IDENTITY ##########
# prod_pred = production %>% 
#   filter(noun == 'Democrats') %>%
#   select(workerid,noun,response,predicate,class) %>%
#   mutate(response_prod = response) %>%
#   select(-response)
# 
# prod_pred$noun <- gsub('Black lives', 'Black people', prod_pred$noun)
# 
# #joining responses to questions about men with info about whether they identify as a Democrat  
# 
# id_collapsed_pred_prod = identity %>%
#   select(workerid,response,noun) %>%
#   rename(response_identity = response) %>%
#   right_join(prod_pred,by=c("workerid","noun"))
# 
# #Housekeeping for per-predicate plots: ordering in terms of predicate valence (done manually)#
# id_collapsed_pred_prod$predicate2 = factor(id_collapsed_pred_prod$predicate, levels=c('competent','generous','honest','kind','smart','strong','incompetent','lie','rude','sexist','unintelligent','weak','eat','sleep','breathe','walk'))
# 
# ggplot(id_collapsed_pred_prod%>% filter(response_identity != 'Confused'), aes(x=response_identity, fill = response_prod)) +
#   geom_bar(stat="count",position="fill") +
#   facet_wrap(~predicate2, scales="free_y") +
#   theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))
# ggsave(file="../graphs/democrats_identity_predicates.png",height=20,width=20,limitsize=FALSE)
# 
# ########## REPUBLICANS IDENTITY ##########
# prod_pred = production %>% 
#   filter(noun == 'Republicans') %>%
#   select(workerid,noun,response,predicate,class) %>%
#   mutate(response_prod = response) %>%
#   select(-response)
# 
# prod_pred$noun <- gsub('Black lives', 'Black people', prod_pred$noun)
# 
# #joining responses to questions about men with info about whether they identify as a Democrat  
# 
# id_collapsed_pred_prod = identity %>%
#   select(workerid,response,noun) %>%
#   rename(response_identity = response) %>%
#   right_join(prod_pred,by=c("workerid","noun"))
# 
# #Housekeeping for per-predicate plots: ordering in terms of predicate valence (done manually)#
# id_collapsed_pred_prod$predicate2 = factor(id_collapsed_pred_prod$predicate, levels=c('competent','generous','honest','kind','smart','strong','incompetent','lie','rude','sexist','unintelligent','weak','eat','sleep','breathe','walk'))
# 
# ggplot(id_collapsed_pred_prod%>% filter(response_identity != 'Confused'), aes(x=response_identity, fill = response_prod)) +
#   geom_bar(stat="count",position="fill") +
#   facet_wrap(~predicate2, scales="free_y") +
#   theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))
# ggsave(file="../graphs/republicans_identity_predicates.png",height=20,width=20,limitsize=FALSE)
# 
# ########## MEN IDENTITY ##########
# prod_pred = production %>% 
#   filter(noun == 'men') %>%
#   select(workerid,noun,response,predicate,class) %>%
#   mutate(response_prod = response) %>%
#   select(-response)
# 
# prod_pred$noun <- gsub('Black lives', 'Black people', prod_pred$noun)
# 
# #joining responses to questions about men with info about whether they identify as a Democrat  
# 
# id_collapsed_pred_prod = identity %>%
#   select(workerid,response,noun) %>%
#   rename(response_identity = response) %>%
#   right_join(prod_pred,by=c("workerid","noun"))
# 
# #Housekeeping for per-predicate plots: ordering in terms of predicate valence (done manually)#
# id_collapsed_pred_prod$predicate2 = factor(id_collapsed_pred_prod$predicate, levels=c('competent','generous','honest','kind','smart','strong','incompetent','lie','rude','sexist','unintelligent','weak','eat','sleep','breathe','walk'))
# 
# ggplot(id_collapsed_pred_prod%>% filter(response_identity != 'Confused'), aes(x=response_identity, fill = response_prod)) +
#   geom_bar(stat="count",position="fill") +
#   facet_wrap(~predicate2, scales="free_y") +
#   theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))
# ggsave(file="../graphs/men_identity_predicates.png",height=20,width=20,limitsize=FALSE)
# 
# ########## WOMEN IDENTITY ##########
# prod_pred = production %>% 
#   filter(noun == 'women') %>%
#   select(workerid,noun,response,predicate,class) %>%
#   mutate(response_prod = response) %>%
#   select(-response)
# 
# prod_pred$noun <- gsub('Black lives', 'Black people', prod_pred$noun)
# 
# #joining responses to questions about men with info about whether they identify as a Democrat  
# 
# id_collapsed_pred_prod = identity %>%
#   select(workerid,response,noun) %>%
#   rename(response_identity = response) %>%
#   right_join(prod_pred,by=c("workerid","noun"))
# 
# #Housekeeping for per-predicate plots: ordering in terms of predicate valence (done manually)#
# id_collapsed_pred_prod$predicate2 = factor(id_collapsed_pred_prod$predicate, levels=c('competent','generous','honest','kind','smart','strong','incompetent','lie','rude','sexist','unintelligent','weak','eat','sleep','breathe','walk'))
# 
# ggplot(id_collapsed_pred_prod%>% filter(response_identity != 'Confused'), aes(x=response_identity, fill = response_prod)) +
#   geom_bar(stat="count",position="fill") +
#   facet_wrap(~predicate2, scales="free_y") +
#   theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))
# ggsave(file="../graphs/women_identity_predicates.png",height=20,width=20,limitsize=FALSE)
# 
# 
# ### ANALYSIS
# 
# # merge identity and likeability ratings into production
# prod = production %>%
#   select(workerid,noun,response,class,predicate) %>%
#   mutate(response_prod = response, noun=gsub('Black lives', 'Black people', prod$noun)) %>%
#   select(-response)
# dd = identity %>%
#   select(workerid,response,noun) %>%
#   rename(response_identity = response) %>%
#   right_join(prod,by=c("workerid","noun")) 
# d = likeability %>%
#   select(workerid,response,noun) %>%
#   rename(response_like = response) %>%
#   right_join(dd,by=c("workerid","noun")) %>%
#   mutate(yes = response_prod == "yes", no = response_prod == "no", notall = response_prod == "notall", all = response_prod == "everyone")
# summary(d)
# 
# # exclude fillers from analysis and cases where people didn't unambiguously identify with category or not
# ad = d %>%
#   filter(class %in% c("negative","positive","neutral") & response_identity != "Confused") %>%
#   droplevels() %>%
#   mutate(clike = myCenter(response_like), cidentity = myCenter(response_identity), noun = as.factor(as.character(noun)))
# nrow(ad)
# summary(ad)
# 
# contrasts(ad$response_identity)
# pairscor.fnc(ad[,c("class","response_identity","response_like")])
# 
# ad$Item = as.factor(paste(ad$noun,ad$predicate))
# ad$num_yes = ifelse(ad$yes, 1, 0)
# ad$num_no = ifelse(ad$no, 1, 0)
# ad$num_notall = ifelse(ad$notall, 1, 0)
# ad$num_all = ifelse(ad$all, 1, 0)
# 
# summary(ad)
# 
# # "yes" responses: simple effects analysis to see effect of identity and likeability for the three different classes of predicates separately
# m.yes = glmer(yes ~ class + class:cidentity + class:clike + (1+ class + class:cidentity + class:clike|Item) + (1+ class + class:cidentity + class:clike|workerid), data=ad, family="binomial")
# m.yes.brm = brm(num_yes ~ class + class:cidentity + class:clike + (1|Item) + (1|workerid), data=ad, family="bernoulli")
# summary(m)
# # generally: more "yes" responses for neutral and positive predicates than for negative ones.
# # likeability*predicate type interaction: negative efffect of likeability on "yes" responses for negative predicates, but positive effect for both neutral and positive predicates
# # identity*predicate type interaction: positive effect of identity on "yes" responses for positive predicates; identity doesn't matter for negative or neutral predicates
# 
# # "no" responses: simple effects analysis to see effect of identity and likeability for the three different classes of predicates separately
# m.no = glmer(no ~ class + class:cidentity + class:clike + (1|Item) + (1|workerid), data=ad, family="binomial")
# m.no.brm = brm(num_no ~ class + class:cidentity + class:clike + (1|Item) + (1|workerid), data=ad, family="bernoulli")
# summary(m)
# # generally: fewer "no" responses for neutral and positive predicates than for negative ones.
# # likeability*predicate type interaction: positive efffect of likeability on "no" responses for negative predicates, but negative effect for both neutral and positive predicates
# # identity*predicate type interaction: positive effect of identity on "no" responses for negative and neutral predicates; identity doesn't matter for positive predicates
# 
# # "notall" responses: simple effects analysis to see effect of identity and likeability for the three different classes of predicates separately
# m.notall = glmer(notall ~ class + class:cidentity + class:clike + (1|Item) + (1|workerid), data=ad, family="binomial")
# m.notall.brm = brm(num_notall ~ class + class:cidentity + class:clike + (1|Item) + (1|workerid), data=ad, family="bernoulli")
# summary(m)
# # generally: way fewer "notall" responses for neutral predicates than for negative ones, but no diff between negative and positive.
# # likeability*predicate type interaction: positive efffect of likeability on "notall" responses for negative predicates (as predicted!! #notallmen effect --sort of, because it's not showing up in identity, just likeability), but no other effects of likeability
# # identity*predicate type interaction: no identity effects
# 
# # "all" responses: simple effects analysis to see effect of identity and likeability for the three different classes of predicates separately
# m.all = glmer(all ~ class + class:cidentity + class:clike + (1|Item) + (1|workerid), data=ad, family="binomial")
# m.all.brm = brm(num_all ~ class + class:cidentity + class:clike + (1|Item) + (1|workerid), data=ad, family="bernoulli")
# summary(m)
# # generally: way more "all" responses for neutral predicates than for negative ones, but no diff between negative and positive.
# # likeability*predicate type interaction: positive efffect of likeability on "all" responses for positive predicates and marginal negative effect for negative predicates (hm, this is the strange one)
# # identity*predicate type interaction: marginal negative effect of identity with positive predicates (ie marginal "all lives matter" effect in predicted direction)


