#BACI simulations
rm(list=ls(all=TRUE)) #clear environment
library(dplyr) #to wrangle
library(ggplot2) #to plot
library(lme4) #to fit lme models
library(emmeans) #to estimate factor specific marginal means  


#simulate a different number of observations in a factor group#######################################################
sd <- 3 #define your standard deviation 
obs_out <- NULL #empty object to poulate with loop

for(i in 1:100){ #can increase loop number to add observations to the simulaiton 
  #create a set of balanced observaitons
  n_level <- 20 #the number of observations per level combo - even among periods
  Balanced <- data.frame(period = factor(c(rep("before", n_level*2), 
                                           rep("after", n_level*2)), levels= c("before", "after"))) %>%
    group_by(period) %>%
    mutate(treatment = factor(c(rep("impact", n_level), rep("control", n_level)))) %>%
    ungroup() %>%
    mutate(response = ifelse(treatment=="control", rnorm(n_level*2, mean=10, sd=sd), 
                             ifelse(period=="before", rnorm(n_level, mean=10, sd=sd), 
                                    rnorm(n_level, mean=15, sd=3)))) #impact response after increases by 5
  
  #make a set of unbalanced observaitons
  n_level <- c(20, 20, 20, 80) # BC, BI, AC, AI factor order 
  More_AI <- data.frame(period = factor(c(rep("before", sum(n_level[1:2])), 
                                          rep("after", sum(n_level[3:4]))), levels= c("before", "after"))) %>%
    mutate(treatment = factor(c(rep("control", n_level[1]), rep("impact", n_level[2]),
                                rep("control", n_level[3]), rep("impact", n_level[4])), levels= c("control", "impact")))  %>%
    mutate(response = ifelse(treatment=="control", rnorm(sum(n_level[1:2]), mean=10, sd=sd), 
                             ifelse(period=="before", rnorm(n_level[3], mean=10, sd=sd), 
                                    rnorm(n_level[4], mean=15, sd=3))))
  
  #build a linear model for both sets of observations
  Balanced_mod <-  lm(response~period*treatment, data=Balanced)
  Unbalanced_mod <-  lm(response~period*treatment, data=More_AI)
  
  #test diffecence in effect size 
  obs_out <- rbind(obs_out, data.frame(balanced_effect = summary(Balanced_mod)$coefficients[4],
                              balanced_emm = confint(contrast(emmeans(Balanced_mod,  specs= ~period:treatment), list(baci=c(1,-1,-1,1))))[[2]], 
                              unbalanced_effect = summary(Unbalanced_mod)$coefficients[4], 
                              unbalanced_emm = confint(contrast(emmeans(Unbalanced_mod,  specs= ~period:treatment), list(baci=c(1,-1,-1,1))))[[2]])) 
}

#plot the BACI effect size to see if unbalanced changed the estimate
  #can repeat this multiple times to see - or increase number of observations by increasing loop length. 
par(mfrow=c(2,1))
hist(obs_out$balanced_effect)
hist(obs_out$unbalanced_effect)


#simulate different numbers of sites#########################################################################
sd <- 3
sites_out <- NULL 
for(i in 1:100){
  n_level <- 20 #the number of observations per level combo
  Balanced <- data.frame(period = factor(c(rep("before", n_level*2), 
                                           rep("after", n_level*2)), levels= c("before", "after"))) %>%
    group_by(period) %>%
    mutate(treatment = factor(c(rep("impact", n_level), rep("control", n_level)))) %>%
    ungroup() %>%
    mutate(response = ifelse(treatment=="control", rnorm(n_level*2, mean=10, sd=sd), 
                             ifelse(period=="before", rnorm(n_level, mean=10, sd=sd), 
                                    rnorm(n_level, mean=15, sd=3)))) #impact response after increases by 5
  
  
  #2 impacts, 1 control site  
  More_I <- data.frame(period = factor(c(rep("before", n_level*3), 
                                          rep("after", n_level*3)), levels= c("before", "after"))) %>%
    group_by(period) %>%
    mutate(site = factor(c(rep("A", n_level), rep("B", n_level), rep("C", n_level)))) %>%
    ungroup() %>%
    mutate(response = case_when(site=="A" ~ rnorm(n(), mean=10, sd=sd), 
                                site %in% c("B", "C") & period=="before" ~ rnorm(n(), mean=10, sd=sd),
                                site %in% c("B", "C") & period=="after" ~ rnorm(n(), mean=15, sd=sd)), 
           treatment = ifelse(site=="A", "control", "impact"))
  
  
  Balanced_mod <-  lm(response~period*treatment, data=Balanced)
  Unbalanced_mod <-  lm(response~period*treatment, data=More_I)
  
  #test diffecence in effect size 
  sites_out <- rbind(sites_out, data.frame(balanced_effect = summary(Balanced_mod)$coefficients[4],
                                       balanced_emm = confint(contrast(emmeans(Balanced_mod,  specs= ~period:treatment), list(baci=c(1,-1,-1,1))))[[2]], 
                                       unbalanced_effect = summary(Unbalanced_mod)$coefficients[4], 
                                       unbalanced_emm = confint(contrast(emmeans(Unbalanced_mod,  specs= ~period:treatment), list(baci=c(1,-1,-1,1))))[[2]])) 
}
hist(sites_out$balanced_effect)
hist(sites_out$unbalanced_effect)



#difference in response in lm with length different at impacts###########################################################################################
  #this is now astimating a BACI in a linear model - similar to an increase in intercept
n_level <- c(200, 200, 200, 200) #observations per site - stays same in each period
slope <- -0.025 #the global slope mean
int <- 4 #global mean intercept
lens_out <- NULL #empty ogject to populate with loop output

#generate fake data for balanced and unbalanced (restrict range of x values at 1 impact)
  #then estimate BACI with the coefficient and using emmeans 
for(i in 1:100){
  balancedMult_data <- data.frame(period = factor(c(rep("before", sum(n_level)), 
                                                    rep("after", sum(n_level))), levels= c("after", "before"))) %>%
    group_by(period) %>%
    mutate(site = factor(c(rep("A", n_level[1]), rep("B", n_level[2]), rep("C", n_level[3]), rep("D", n_level[4])))) %>%
    mutate(treatment= factor(ifelse(site %in% c("A", "B"), "control", "impact"), levels=c("impact", "control"))) %>% #partition treatments
    ungroup() %>%
    mutate(length = rnorm(sum(n_level*2), mean= 150, sd=80), 
           error = rnorm(sum(n_level*2), mean=0, sd=2)) %>% 
    #manipulate slope and intercept to get responses - make impacts intercept double
    mutate(response = case_when(treatment == "control" & period== "before" ~ (length*(slope)) + int + error, #these need to lm 
                                treatment == "control" & period== "after" ~  (length*(slope)) + int + error, 
                                treatment == "impact" & period== "before" ~  (length*slope) + int + error,
                                treatment == "impact" & period== "after" ~  (length*slope) + (int+int) + error)) #double the int
  
  unbalancedMult_data <- data.frame(period = factor(c(rep("before", sum(n_level)), 
                                                      rep("after", sum(n_level))), levels= c("after", "before"))) %>%
    group_by(period) %>%
    mutate(site = factor(c(rep("A", n_level[1]), rep("B", n_level[2]), rep("C", n_level[3]), rep("D", n_level[4])))) %>%
    mutate(treatment= factor(ifelse(site %in% c("A", "B"), "control", "impact"), levels=c("impact", "control"))) %>% #partition treatments
    ungroup() %>%
    mutate(length = rnorm(sum(n_level*2), mean= 150, sd=80), 
           error = rnorm(sum(n_level*2), mean=0, sd=2)) %>% 
    #change length category subsampled at one of the impacts
    mutate(length= ifelse(site=="C", rnorm(sum(n_level[1]*2), mean= 80, sd=40), length)) %>% #restrict length range at C
    mutate(response = case_when(treatment == "control" & period== "before" ~ (length*(slope)) + int + error, 
                                treatment == "control" & period== "after" ~  (length*(slope)) + int + error, 
                                treatment == "impact" & period== "before" ~  (length*slope) + int + error,
                                treatment == "impact" & period== "after" ~  (length*slope) + (int+int) + error)) #double the int
  
  #models
  Balanced_mod <-  lm(response ~ length + period*treatment, data=balancedMult_data)
  Unbalanced_mod <-  lm(response ~ length + period*treatment, data=unbalancedMult_data)
  
  #print margianl means and effect sizes 
  lens_out <- rbind(lens_out, data.frame(balanced_effect = summary(Balanced_mod)$coefficients[5],
                                           balanced_emm = confint(contrast(emmeans(Balanced_mod,  specs= ~period:treatment), list(baci=c(1,-1,-1,1))))[[2]], 
                                           unbalanced_effect = summary(Unbalanced_mod)$coefficients[5], 
                                           unbalanced_emm = confint(contrast(emmeans(Unbalanced_mod,  specs= ~period:treatment), list(baci=c(1,-1,-1,1))))[[2]]))
  
}

hist(lens_out$balanced_effect, breaks=30)
mean(lens_out$balanced_effect)
hist(lens_out$unbalanced_effect, breaks=30)
mean(lens_out$unbalanced_effect)

hist(lens_out$balanced_emm, breaks=30)
mean(lens_out$balanced_emm)
hist(lens_out$unbalanced_emm, breaks=30)
mean(lens_out$unbalanced_emm)

#an example of the unbalanced data
ggplot(unbalancedMult_data, aes(length, response)) +
  geom_point(aes(color=site, shape=treatment), alpha=0.4) +
  stat_smooth(method="lm", se=FALSE, aes(color=site)) +
  facet_wrap(~period)



#does unbalanced number of obs with a different intercept change the result?###################################################
  #i.e. is low number of obs in a not strongly influenced group change the estimate?
  #make one of them not respond as much and see if BACI is the mean ignoring sample size or not. 
slope <- -0.025 #the global slope mean
int <- 4 #global mean intercept
lens_out <- NULL #empty ogject to populate with loop output

for(i in 1:100){
  n_level <- c(200, 200, 200, 200) #observations per site - stays same in each period
  balancedMult_data <- data.frame(period = factor(c(rep("before", sum(n_level)), 
                                                    rep("after", sum(n_level))), levels= c("after", "before"))) %>%
    group_by(period) %>%
    mutate(site = factor(c(rep("A", n_level[1]), rep("B", n_level[2]), rep("C", n_level[3]), rep("D", n_level[4])))) %>%
    mutate(treatment= factor(ifelse(site %in% c("A", "B"), "control", "impact"), levels=c("impact", "control"))) %>% #partition treatments
    ungroup() %>%
    mutate(length = rnorm(sum(n_level*2), mean= 150, sd=80), 
           error = rnorm(sum(n_level*2), mean=0, sd=2)) %>% 
    #manipulate slope and intercept to get responses - make impacts intercept double
    mutate(response = case_when(treatment == "control" & period== "before" ~ (length*(slope)) + int + error, #these need to lm 
                                treatment == "control" & period== "after" ~  (length*(slope)) + int + error, 
                                
                                site == "C" & period== "before" ~  (length*slope) + int + error,
                                site == "C" & period== "after" ~  (length*slope) + (int+int/2) + error, #1.5 x the response
                                
                                site == "D" & period== "before" ~  (length*slope) + int + error,
                                site == "D" & period== "after" ~  (length*slope) + (int+int) + error)) #double the int
  
  n_level <- c(200, 200, 80, 200) #observations per site - stays same in each period
  unbalancedMult_data <- data.frame(period = factor(c(rep("before", sum(n_level)), 
                                                      rep("after", sum(n_level))), levels= c("after", "before"))) %>%
    group_by(period) %>%
    mutate(site = factor(c(rep("A", n_level[1]), rep("B", n_level[2]), rep("C", n_level[3]), rep("D", n_level[4])))) %>%
    mutate(treatment= factor(ifelse(site %in% c("A", "B"), "control", "impact"), levels=c("impact", "control"))) %>% #partition treatments
    ungroup() %>%
    mutate(length = rnorm(sum(n_level*2), mean= 150, sd=80), 
           error = rnorm(sum(n_level*2), mean=0, sd=2)) %>% 
    #change length category subsampled at one of the impacts
    mutate(response = case_when(treatment == "control" & period== "before" ~ (length*(slope)) + int + error, 
                                treatment == "control" & period== "after" ~  (length*(slope)) + int + error, 
                                
                                site == "C" & period== "before" ~  (length*slope) + int + error,
                                site == "C" & period== "after" ~  (length*slope) + (int+int/2) + error, #1.5 x the response
                                
                                site == "D" & period== "before" ~  (length*slope) + int + error,
                                site == "D" & period== "after" ~  (length*slope) + (int+int) + error)) #double the int
  
  #models
  Balanced_mod <-  lm(response ~ length + period*treatment, data=balancedMult_data)
  Unbalanced_mod <-  lm(response ~ length + period*treatment, data=unbalancedMult_data)
  
  #print margianl means and effect sizes 
  lens_out <- rbind(lens_out, data.frame(balanced_effect = summary(Balanced_mod)$coefficients[5],
                                         balanced_emm = confint(contrast(emmeans(Balanced_mod,  specs= ~period:treatment), list(baci=c(1,-1,-1,1))))[[2]], 
                                         unbalanced_effect = summary(Unbalanced_mod)$coefficients[5], 
                                         unbalanced_emm = confint(contrast(emmeans(Unbalanced_mod,  specs= ~period:treatment), list(baci=c(1,-1,-1,1))))[[2]]))
  
}
#if n obs matters effect should be higher at unbalanced because site C is a smaller effect and has smaller sample 
hist(lens_out$balanced_effect, breaks=30)
mean(lens_out$balanced_effect)
hist(lens_out$unbalanced_effect, breaks=30)
mean(lens_out$unbalanced_effect)

#having less observations in a smaller group messes up est (e.g. different BACI estimate if one response is half as strong 
#and has less observations, as it does when it has more observations)
#can this be fixed with mixed effects? 

#Same scenatio as previous but compare unbalanced FE with unbalanced ME##############################################################
n_level <- c(300, 300, 80, 300) #observations per site - stays same in each period# estimates #3.6 with levels like this
n_level <- c(100,100,300,100) #2.5 with an estimate like this 
slope <- -0.025 #the global slope mean
int <- 4 #global mean intercept
lens_out <- NULL #empty ogject to populate with loop output

for(i in 1:100){
  unbalancedMult_data <- data.frame(period = factor(c(rep("before", sum(n_level)), 
                                                      rep("after", sum(n_level))), levels= c("after", "before"))) %>%
    group_by(period) %>%
    mutate(site = factor(c(rep("A", n_level[1]), rep("B", n_level[2]), rep("C", n_level[3]), rep("D", n_level[4])))) %>%
    mutate(treatment= factor(ifelse(site %in% c("A", "B"), "control", "impact"), levels=c("impact", "control"))) %>% #partition treatments
    ungroup() %>%
    mutate(length = rnorm(sum(n_level*2), mean= 150, sd=80), 
           error = rnorm(sum(n_level*2), mean=0, sd=2)) %>% 
    #change length category subsampled at one of the impacts
    mutate(response = case_when(treatment == "control" & period== "before" ~ (length*(slope)) + int + error, 
                                treatment == "control" & period== "after" ~  (length*(slope)) + int + error, 
                                
                                site == "C" & period== "before" ~  (length*slope) + int + error,
                                site == "C" & period== "after" ~  (length*slope) + (int+int/2) + error, #1.5 x the response
                                
                                site == "D" & period== "before" ~  (length*slope) + int + error,
                                site == "D" & period== "after" ~  (length*slope) + (int+int) + error)) #double the int
  
  #models
  Unbalanced_FE_mod <-  lm(response ~ length + period*treatment, data=unbalancedMult_data)
  Unbalanced_ME_mod <-  lmer(response ~ length + period*treatment + (1|site), data=unbalancedMult_data)
  
  #print margianl means and effect sizes 
  lens_out <- rbind(lens_out, data.frame(unbalanced_FE_effect = summary(Unbalanced_FE_mod)$coefficients[5],
                                         unbalanced_FE_emm = confint(contrast(emmeans(Unbalanced_FE_mod,  specs= ~period:treatment), list(baci=c(1,-1,-1,1))))[[2]], 
                                         unbalanced_ME_effect = summary(Unbalanced_ME_mod)$coefficients[5], 
                                         unbalanced_ME_emm = confint(contrast(emmeans(Unbalanced_ME_mod,  specs= ~period:treatment), list(baci=c(1,-1,-1,1))))[[2]]))
  
}

hist(lens_out$unbalanced_FE_effect, breaks=30)
mean(lens_out$unbalanced_FE_effect) #expect to be biased toward 4
hist(lens_out$unbalanced_ME_effect, breaks=30)
mean(lens_out$unbalanced_ME_effect) #want to be 3.5

# effect estimate doesn't change i dont get it
ggplot(unbalancedMult_data, aes(response, length, color=period)) +
  geom_point(alpha=0.4) +
  stat_smooth(method="lm", se=FALSE) +
  facet_wrap(~site)

#effect size estimate (i.e. mean intercept by group) is "driven" more by sites with more data. 