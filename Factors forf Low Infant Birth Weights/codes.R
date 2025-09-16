library(tidyverse)
library(haven)
library(gtsummary)
library(kableExtra)
library(patchwork)
#Reading the data set
lbwdata <- tibble(read_dta('lbwdata.dta'))

#Converting the columns to their right data types
lbwdata <- transform(lbwdata,
                     low   = as.factor(lbwdata$low),
                     age   = as.numeric(lbwdata$age),
                     lwt   = as.numeric(lbwdata$lwt),
                     race  = as.factor(str_trim(lbwdata$race)), #To remove the trailing spaces as well
                     smoke = as.factor(lbwdata$smoke),
                     ht    = as.factor(lbwdata$ht),
                     ui    = as.factor(lbwdata$ui),
                     ftv   = as.factor(lbwdata$ftv),
                     ptl   = as.factor(lbwdata$ptl),
                     bwt   = as.numeric(lbwdata$bwt))


#Labeling the values in the categorical columns
lbwdata <- lbwdata %>% 
  mutate(low = factor(low,labels = c('Normal','low_weight'),levels=c(0,1)),
         smoke = factor(smoke,labels = c('No','Yes'),levels=c(0,1)),
         ht = factor(ht,labels = c('No','Yes'),levels=c(0,1)),
         ui = factor(ui,labels = c('No','Yes'),levels=c(0,1)),
         ftv = factor(ftv,labels = c('No','Yes'),levels=c(0,1)),
         ptl = factor(ptl,labels = c('No','Yes'),levels=c(0,1))
         )

#Renaming some of the variables
lbwdata <- lbwdata %>% 
  rename(uterine_irritability='ui',
         mother.weight = 'lwt',
         hypertension.history = 'ht',
         prev.premature.labours = 'ptl',
         physician.visits = 'ftv',
         birth_weight = 'bwt',
         b.Weight.category = 'low')

  
#Describing the demographics in the data
#Categorical variables
demographics = lbwdata %>% 
  select(-c(age,mother.weight,birth_weight)) %>% 
  tbl_summary(by=b.Weight.category,type = all_dichotomous()~'categorical')

demographics_race = lbwdata %>% 
  select(race,b.Weight.category) %>% 
  tbl_summary(by='b.Weight.category',type = all_dichotomous()~'categorical')

#Numerical variables
p1 = ggplot(data = lbwdata,aes(x=age,fill = b.Weight.category)) + 
  geom_density(alpha=.5)+ xlim(10,50) +
  theme_bw()+
  labs(title = "Distribution of mothers' ages",
       x='Weight in pounds')+
  guides(fill=guide_legend(title = 'Birth weight:'))+
  scale_fill_manual(values = c('red','blue')) +
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = .5,colour = 'black'),
        panel.grid = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.title = element_text(face='bold',colour = 'black'))

p2 = ggplot(data = lbwdata,aes(x=b.Weight.category,y=age,fill = b.Weight.category))+
  geom_boxplot() + ylim(10,50)+
  theme_bw()+
  labs(x='Birth weight',y='Weigt in pounds',title = "Distribution of mothers' ages")+
  guides(fill=guide_legend(title = 'Birth weight:'))+
  scale_fill_manual(values = c('red','blue'))+
  theme(axis.text = element_text(colour = 'black'),
        plot.title = element_text(hjust = .5,colour = 'black'),
        legend.position = 'bottom',
        legend.title = element_text(face = 'bold',colour = 'black'))
p1+p2


ggplot(data = lbwdata,aes(x=birth_weight,fill=b.Weight.category)) + 
  geom_density(alpha=.5) + xlim(500,5000)+
  theme_bw()+
  labs(title = 'Distribution of birth weights',x='Birth weight')+
  scale_x_continuous(breaks = seq(500,5000,500))+
  scale_fill_manual(values=c('red','blue'))+
  theme(panel.grid = element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = .5),
        axis.text.x=element_text(color='black'))

#Distribution of birth weights
lbwdata %>% select(birth_weight,b.Weight.category) %>% group_by(b.Weight.category) %>%
  summarise(Min. = min(birth_weight),
            Q1 = quantile(birth_weight,.25),
            Median = median(birth_weight),
            Mean = mean(birth_weight),
            Q3 = quantile(birth_weight,.75),
            Max. = max(birth_weight)
            ) %>% 
  kbl(digits = 3,
      col.names = c('Birth Weight','Min.','Q1','Median','Mean','Q3','Max.')) %>% 
  kable_styling(position = 'center',latex_options = c('stripped','hold_position')) %>% 
  row_spec(1,background = 'gray')



#Bivariate Analysis
#Numerical variables

#Mean ages
age.test = t.test(age~b.Weight.category,data = lbwdata)
lwt.test = t.test(mother.weight~b.Weight.category,data = lbwdata)

Age <- data.frame(
  Variable = "Mother's Age",
  #t.statistic = age.test$statistic,
  #df = age.test$parameter,
  p.value = age.test$p.value,
  conf.int.birth_weighter = age.test$conf.int[1],
  conf.int.upper = age.test$conf.int[2]
  #mean.normal.weight = age.test$estimate[1],
  #mean.birth_weight.weight = age.test$estimate[2]
)

lweight = data.frame(
  Variable = "mother's Weight",
  #t.statistic = lwt.test$statistic,
  #df = lwt.test$parameter,
  p.value = lwt.test$p.value,
  conf.int.birth_weighter =  lwt.test$conf.int[1],
  conf.int.upper = lwt.test$conf.int[2]
  #mean.normal.weight = lwt.test$estimate[1],
  #mean.birth_weight.weight = lwt.test$estimate[2]
)

combined1 = bind_rows(Age,lweight)
row.names(combined1) <- NULL
combined1_formated <- combined1 %>% 
  kbl() %>% 
  kable_styling(position = 'center',latex_options = c('stripped','hold_position'))


#Categorical variables
bth_race <- table(lbwdata$b.Weight.category,lbwdata$race)
bth_race.test <- chisq.test(bth_race,correct = F)

bth_smoke <- table(lbwdata$b.Weight.category,lbwdata$smoke)
bth_smoke.test <- chisq.test(bth_smoke,correct = F)

bth_hptsn <- table(lbwdata$b.Weight.category,lbwdata$hypertension.history)
bth_hptsn.test <- chisq.test(bth_hptsn,correct = F)

bth_iriit <- table(lbwdata$b.Weight.category,lbwdata$uterine_irritability)
bth_iriit.test <- chisq.test(bth_iriit,correct = F)

bth_physic <- table(lbwdata$b.Weight.category,lbwdata$physician.visits)
bth_physic.test <- chisq.test(bth_physic,correct = F)

bth_premat <- table(lbwdata$b.Weight.category,lbwdata$prev.premature.labours)
bth_premat.test <- chisq.test(bth_premat,correct = F)

#Creating data frames of the results
d1 <- data.frame(Variable = "Race",
                 p_value = bth_race.test$p.value)
d2 <- data.frame(Variable = "Smoke",
                 p_value = bth_smoke.test$p.value)
d3 <- data.frame(Variable = "Hypert.History",
                 p_value = bth_hptsn.test$p.value)
d4 <- data.frame(Variable = "Uterine irritability",
                 p_value = bth_iriit.test$p.value)
d5 <- data.frame(Variable = "Physician visits",
                 p_value = bth_physic.test$p.value)
d6 <- data.frame(Variable = "Prev.premat.labours",
                 p_value = bth_premat.test$p.value)
merged_dataframe <- bind_rows(d1,d2,d3,d4,d5,d6)
merged_dataframe %>% 
  kbl() %>% 
  kable_styling(position = 'center',latex_options = c('stripped','hold_position'))



#RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR
#Logistic regression
lbwdata <- lbwdata %>% 
  mutate(b.Weight.category = ifelse(b.Weight.category=='Normal',1,0))

model <- glm(b.Weight.category~smoke+hypertension.history+uterine_irritability+prev.premature.labours+mother.weight,family = 'binomial',data = lbwdata)
summary(model)
gtsummary::tbl_regression(model,exponentiate = T,intercept = T)
