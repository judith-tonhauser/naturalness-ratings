# this R script is a modification of the RMD script downloaded from the OSF repo provided in
# Kalomoiros & Schwarz 2024 (https://osf.io/3p68r/)
# it calls the data from Exp 1

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load libraries
# library(plyr)
library(lme4)
library(lmerTest)
# library(ggplot2)
library(tidyverse)
# library(broom)
# library(emmeans)
# library(ordinal)
# library(brms)

# load helpers
source('../../../results/helpers.R')

# set theme
theme_set(theme_bw())

# load data
results <- read.csv("../data/resultsExp1.csv")
#view(results)
table(results$Condition)

# filter to data of interest (Support = support simple ps, SimplePs = explicit ignorance simple ps)
d = results %>%
  filter(Condition == "Support" | Condition == "SimplePs") %>%
  droplevels()
table(d$Condition)

# recode Value from 1-7 to 0-6 to compare to Mandelkern et al 2020
table(d$Value)
d$Value = d$Value -1
table(d$Value)

# graphs ----

#### Fig A: plot of mean naturalness ratings in explicit ignorance context ----

# target data: explicit ignorance context
t = d %>%
  filter(Condition == "SimplePs")
nrow(t)
#view(t)
table(t$Trigger)
#  again    aware continue find out    happy     stop

# calculate mean naturalness rating by expression in explicit ignorance context
nat.meansEIC = t %>%
  group_by(Trigger) %>%
  summarize(Mean = mean(Value), CILow = ci.low(Value), CIHigh = ci.high(Value)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, Trigger = fct_reorder(as.factor(Trigger),Mean))
nat.meansEIC
levels(nat.meansEIC$Trigger)

# color code the expressions
ps <- c("again","continue","happy","find out","aware","stop")
noPs <- c()
#noPs <- c("enjoy", "frown on","hoping","sure")

nat.meansEIC$ps = ifelse(nat.meansEIC$Trigger %in% noPs, "noPs",
                         ifelse(nat.meansEIC$Trigger %in% ps, "ps", "other"))
table(nat.meansEIC$ps)

t$expression = t$Trigger
t$ps = ifelse(t$expression %in% noPs, "noPs",
              ifelse(t$expression %in% ps, "ps", "other"))
table(t$ps)

table(nat.meansEIC$ps, nat.meansEIC$Trigger)

text.color <- ifelse(nat.meansEIC$Trigger[order(nat.meansEIC$Mean)] %in% ps, '#D55E00',
                     ifelse(nat.meansEIC$Trigger[order(nat.meansEIC$Mean)] %in% noPs, "black", "#009E73"))
text.color

t$expression = factor(t$expression, levels = nat.meansEIC$Trigger[order(nat.meansEIC$Mean)], ordered = TRUE)

# plot of naturalness means, with participants' individual responses
ggplot(nat.meansEIC, aes(x=Trigger, y=Mean)) +
  geom_violin(data=t[t$Condition == "SimplePs",],aes(x=Trigger, y=Value),
              scale="width",color="gray80", fill = "gray80") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1,color="black") +
  geom_point(aes(group = ps, fill = ps), shape=21,stroke=.5,size=3, color="black") +
  scale_fill_manual(values=c('black','#D55E00')) + 
  #scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0","1"")) +
  guides(fill=FALSE) +
  theme(legend.position="top", panel.grid.major.x = element_blank()) +
  ylab("Mean naturalness rating \n in explicit ignorance context") +
  xlab("Expression") +  
  #theme_dark() +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color = text.color)) 
ggsave("../graphs/explicit-ignorance-naturalness-by-expression.pdf",height=3,width=6)


#### Fig B: plot of mean naturalness ratings by context ----

# calculate mean naturalness rating by trigger and context
nat.means = d %>%
  group_by(Trigger,Condition) %>%
  summarize(Mean = mean(Value), CILow = ci.low(Value), CIHigh = ci.high(Value)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  ungroup %>%
  select(-c(CILow,CIHigh)) %>%
  mutate(Condition = as.factor(Condition))
nat.means
table(nat.means$Condition)
nat.means$Trigger <- as.factor(nat.means$Trigger)
levels(nat.means$Trigger)

# order predicates by mean naturalness rating in EIC
tmp <- d %>%
  filter(Condition == "SimplePs") %>%
  group_by(Trigger) %>%
  summarize(Mean = mean(Value)) %>%
  mutate(Trigger = fct_reorder(as.factor(Trigger),Mean))
tmp
levels(tmp$Trigger)

# order predicates by mean difference in EIC and Support context 
# because the claim by Mandelkern et al is that this is how ps differ from non-ps
tmp.EIC <- d %>%
  filter(Condition == "SimplePs") %>%
  group_by(Trigger) %>%
  summarize(Mean.EIC = mean(Value))
tmp.EIC

tmp.SUP <- d %>%
  filter(Condition == "Support") %>%
  group_by(Trigger) %>%
  summarize(Mean.SUP = mean(Value))
tmp.SUP

tmp = left_join(tmp.EIC,tmp.SUP)
tmp
tmp$Diff = tmp$Mean.EIC-tmp$Mean.SUP
tmp
tmp = tmp %>%
  mutate(Trigger = fct_reorder(as.factor(Trigger),Diff))
tmp

# now order expressions based on tmp (two options above)
levels(tmp$Trigger)

nat.means$Trigger = factor(nat.means$Trigger, levels=tmp$Trigger[order(tmp$Trigger)], ordered=TRUE)
d$Trigger = factor(d$Trigger, levels=tmp$Trigger[order(tmp$Trigger)], ordered=TRUE)
levels(nat.means$Trigger)
levels(d$Trigger)

# order the contexts
levels(nat.means$Condition)
nat.means$Condition = factor(nat.means$Condition, levels = c("SimplePs", "Support"))

fill.color <- ifelse(levels(nat.means$Trigger) %in% ps, '#D55E00', "black")
fill.color

# to color the facets differently
library(ggh4x)

strip <- strip_themed(background_x = elem_list_rect(fill = fill.color))

nat.means

# violinplot
ggplot() +
  geom_violin(data=d, aes(x=Condition, y=Value, fill = Condition), scale="width", linewidth = 0) +
  geom_point(data=nat.means, aes(x=Condition, y=Mean, fill = Condition), shape=21,stroke=.5,size=2, color="black") +
  geom_errorbar(data=nat.means,aes(x=Condition,ymin=YMin,ymax=YMax),width=0.1,color="black") +
  scale_fill_manual(values=c('gray80',"#56B4E9",'#F0E442'),
                    name = "Context",
                    labels=c('explicit ignorance', 'support')) +
  scale_x_discrete(breaks = NULL) +
  #scale_y_continuous(limits = c(-.15,1), breaks = seq(0,1,.2), labels = c("0",".2",".4",".6",".8","1")) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank()) +
  theme(legend.position="top") +
  guides(linetype = "none") +
  ylab("Mean naturalness rating") +
  xlab("Context") +
  facet_wrap2(. ~ Trigger, nrow = 1, strip = strip) +
  theme(panel.grid.minor = element_blank()) +
  theme(strip.background = element_rect(fill="white")) +
  theme(strip.text = element_text(color = "white"))
ggsave("../graphs/naturalness-by-context-and-expression.pdf",height=3,width=9)

# to compare to Fig 2 in Kalomoiros & Schwarz 2024 ----

# calculate mean naturalness rating by context
nat.means = d %>%
  group_by(Condition) %>%
  summarize(Mean = mean(Value), CILow = ci.low(Value), CIHigh = ci.high(Value)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  ungroup %>%
  select(-c(CILow,CIHigh)) %>%
  mutate(Condition = as.factor(Condition))
nat.means
table(nat.means$Condition)

# order the contexts
levels(nat.means$Condition)
nat.means$Condition = factor(nat.means$Condition, levels = c("SimplePs", "Support"))

# violinplot
ggplot() +
  geom_violin(data=d, aes(x=Condition, y=Value, fill = Condition), scale="width", linewidth = 0) +
  geom_point(data=nat.means, aes(x=Condition, y=Mean, fill = Condition), shape=21,stroke=.5,size=2, color="black") +
  geom_errorbar(data=nat.means,aes(x=Condition,ymin=YMin,ymax=YMax),width=0.1,color="black") +
  scale_fill_manual(values=c('gray80',"#56B4E9",'#F0E442'),
                    name = "Context",
                    labels=c('explicit ignorance', 'support')) +
  scale_x_discrete(breaks = NULL) +
  #scale_y_continuous(limits = c(-.15,1), breaks = seq(0,1,.2), labels = c("0",".2",".4",".6",".8","1")) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank()) +
  theme(legend.position="top") +
  guides(linetype = "none") +
  ylab("Mean naturalness rating") +
  xlab("Context") 

# analysis ----

# Mandelkern et al 2020 used linear regression with their data, so we will do the same here

summary(d)
table(d$Trigger)
table(d$Condition)
table(d$Value)
table(d$ID)
# no column for item in raw data
table(d[d$Trigger == "again",]$ID,d[d$Trigger == "again",]$Condition)
# also can't use by-participant random effect because each participant
# saw each trigger only in one condition

m = lm(Value ~ Condition, data=subset(d[d$Trigger == "happy",]))
summary(m)

#again: **
#continue: **
#find out: **
#aware: marginal
#stop: --
#happy: --

# JT's code ends here -----

# first plots
results$Condition <- (as.factor(as.character(results$Condition)))

levels(results$Condition) <- c("NoPsFirst", "NoPsSecond", "PsFirst", "PsSecond", "EISimplePs", "SSimplePs")

results <- as_tibble(results)

results <- results %>%
          mutate(Condition = fct_relevel(Condition, 
             c("PsFirst", "PsSecond", "NoPsFirst", "NoPsSecond", "EISimplePs", "SSimplePs")))

resultsPlot <- results %>%
  group_by(Condition) %>%
  dplyr::summarize(n = n(), MeanValue = mean(Value), sd = sd(Value))%>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ci=se * qt((1-0.05)/2 + .5, n-1))

colours <- c("#727272", "#f1595f", "#79c36a", "#599ad3", "#f9a65a" , "#9e66ab")

ggplot(resultsPlot, aes(x = Condition, y = MeanValue, fill=Condition))+
  geom_bar(stat = "identity") +
  ylim("1", "2", "3", "4", "5", "6", "7")+
  geom_errorbar(aes(ymin=MeanValue-se, ymax=MeanValue+se), width=.1) +
  scale_fill_manual(values=colours)+
  theme(axis.text.x = element_text(angle = 45)) +
  theme_classic() +
  labs(title = "Mean rating per condition",
       y = "Mean rating",
       x = "Condition")
        

```

Setting up factors for analyses:

```{r}

results$Condition <- as_factor(results$Condition)
results$ID <- as_factor(results$ID)
results$Trigger <- as_factor(results$Trigger)
results$Value <- as_factor(results$Value)

results <- results %>%
      mutate(PsType = fct_collapse(Condition,
        Ps = c("SSimplePs", "EISimplePs", "PsFirst", "PsSecond"),
        NoPs = c("NoPsFirst", "NoPsSecond")
      )) %>%
      mutate(Order = fct_collapse(Condition,
        First = c("PsFirst", "NoPsFirst"),
        Second = c("PsSecond", "NoPsSecond"),
      ))%>%
      mutate(Order = fct_relevel(Order, c("First", "Second"))) %>%
      mutate(SupType = fct_collapse(Condition,
        NoPriorSupp = c("PsFirst", "EISimplePs"),
        PriorSupp = c("PsSecond", "SSimplePs")
      ))%>%
      mutate(CompType = fct_collapse(Condition,
                    Simple = c("SSimplePs", "EISimplePs"),
                    Complex = c("PsFirst", "PsSecond", "NoPsFirst", "NoPsSecond")
                                      )) %>%
      mutate(SupType = fct_relevel(SupType, c("PriorSupp", "NoPriorSupp"))) %>%
      mutate(OrderNum = as.numeric(Order)-1.5) %>%       ##### here and below, using this to numerically force the first two levels of the factor to be -0.5 and 0.5; any other levels will be removed via filter() in the relevant 2x2 analyses, so no harm done.
      mutate(PsTypeNum = as.numeric(PsType)-1.5) %>%
      mutate(SupTypeNum = as.numeric(SupType)-1.5) %>%
      mutate(CompTypeNum = as.numeric(CompType)-1.5)

filter(results, Order %in% c("First", "Second"))
filter(results, SupType %in% c("PriorSupp", "NoPriorSupp"))



```

Check difference between EISimplePs and SSimplePs:

```{r}

EIvSSimple.ordinal <- clmm(Value ~ Condition + (1|ID) + (1+Condition|Trigger), filter(results, Condition %in% c("EISimplePs", "SSimplePs")), link = "logit")

summary(EIvSSimple.ordinal)
```


Testing for Ps-NoPs interaction (predicted by asymmetry-based accounts):

```{r}

PsNoPs.ordinal <- clmm(Value ~ PsTypeNum*OrderNum + (1|ID) + (1+PsTypeNum*OrderNum|Trigger), filter(results, Order %in% c("First", "Second")), link = "logit")

summary(PsNoPs.ordinal)

PsNoPs.ordinal2 <- clmm(Value ~ PsTypeNum*OrderNum + (1|ID) + (1+PsTypeNum+OrderNum|Trigger), filter(results, Order %in% c("First", "Second")), link = "logit")


anova(PsNoPs.ordinal,PsNoPs.ordinal2)


PsNoPs.ordinal3 <- clmm(Value ~ PsTypeNum*OrderNum + (1|ID) + (1+PsTypeNum|Trigger), filter(results, Order %in% c("First", "Second")), link = "logit")

anova(PsNoPs.ordinal,PsNoPs.ordinal3)

PsNoPs.ordinal4 <- clmm(Value ~ PsTypeNum*OrderNum + (1|ID) + (1+OrderNum|Trigger), filter(results, Order %in% c("First", "Second")), link = "logit")

anova(PsNoPs.ordinal,PsNoPs.ordinal4)

```

Final model is PsNoPs.ordinal3

```{r}
summary(PsNoPs.ordinal3)
```

No interaction, no significant condition effects. Note: OrderNum slope for Trigger also converges, but accounts for much less variance; signifance pattern doesn't change.

The Bayesian part of the analysis:

```{r}
head(filter(results, Order %in% c("First", "Second")))
results$Value <- as.numeric(as.character(results$Value)) #make Value a factor (model below is cumulative and requires it)
# Priors from Mandelkern et al. brm analysis (newly conducted; no Bayesian stats in original)
priors <- c(
    prior(normal(-2.26, 0.22), class = "Intercept",
        coef = "1"),
  prior(normal(-1.18, 0.21), class = "Intercept",
        coef = "2"),
  prior(normal(-0.52, 0.21), class = "Intercept",
        coef = "3"),
  prior(normal(-0.06, 0.21), class = "Intercept",
        coef = "4"),
  prior(normal(0.79, 0.21), class = "Intercept",
        coef = "5"),
  prior(normal(2.17, 0.22), class = "Intercept",
        coef = "6"),
  prior(normal(0.79, 0.32), class = "b", 
        coef = "PsTypeNum"),
  prior(normal(1.04, 0.22), class = "b",
        coef = "OrderNum"),
  prior(normal(-1.49, 0.38), class = "b",
        coef = "PsTypeNum:OrderNum")
)


PsNoPs.brm <- brm(Value ~ PsTypeNum*OrderNum + (1|ID) + (1+PsTypeNum|Trigger),
                 family=cumulative("logit"),  
                 control = list(adapt_delta = 0.98),
                 save_pars = save_pars(all = TRUE),
                 iter = 20000,
                 prior =  priors,
                 filter(results, Order %in% c("First", "Second")), backend = "cmdstanr")

PsNoPs.brm

PsNoPs.brmStan <- brm(Value ~ PsTypeNum*OrderNum + (1|ID) + (1+PsTypeNum|Trigger),
                 family=cumulative("logit"),  
                 control = list(adapt_delta = 0.98),
                 save_pars = save_pars(all = TRUE),
                 iter = 0,
                 chains = 0,
                 prior =  priors,
                 filter(results, Order %in% c("First", "Second")), backend = "rstan")

priors <- c(
    prior(normal(-2.26, 0.22), class = "Intercept",
        coef = "1"),
  prior(normal(-1.18, 0.21), class = "Intercept",
        coef = "2"),
  prior(normal(-0.52, 0.21), class = "Intercept",
        coef = "3"),
  prior(normal(-0.06, 0.21), class = "Intercept",
        coef = "4"),
  prior(normal(0.79, 0.21), class = "Intercept",
        coef = "5"),
  prior(normal(2.17, 0.22), class = "Intercept",
        coef = "6"),
  prior(normal(0.79, 0.32), class = "b", 
        coef = "PsTypeNum"),
  prior(normal(1.04, 0.22), class = "b",
        coef = "OrderNum")
)


PsNoPsNullInt.brm <- brm(Value ~ PsTypeNum+OrderNum + (1|ID) + (1+PsTypeNum|Trigger),
                 family=cumulative("logit"),  
                 control = list(adapt_delta = 0.98), 
                 save_pars = save_pars(all = TRUE),
                 iter = 20000,
                 prior =  priors,                 
                 filter(results, Order %in% c("First", "Second")))



margLogLik_linear <- bridge_sampler(PsNoPs.brm,  silent = TRUE)

margLogLik_null <- bridge_sampler(PsNoPsNullInt.brm, silent = TRUE)

(BF_ln <- bayes_factor(margLogLik_linear, margLogLik_null))

```


Testing for condition differences relative to Support Control:

```{r}

PsNoPs.emm = emmeans(PsNoPs.ordinal3, specs = pairwise ~ OrderNum|PsTypeNum)

PsNoPs.emm$contrasts %>%
  rbind()

```

No significant effects of Order in either Ps or No-Ps

Next: testing interaction between CompType and SupType:

```{r}

Support.ordinal <- clmm(Value ~ CompTypeNum * SupTypeNum + (1|ID) + (1+CompTypeNum*SupTypeNum|Trigger), filter(results, PsType =="Ps"), link = "logit")

summary(Support.ordinal)

Support.ordinal2 <- clmm(Value ~ CompTypeNum * SupTypeNum + (1|ID) + (1+CompTypeNum+SupTypeNum|Trigger), filter(results, PsType =="Ps"), link = "logit")

Support.ordinal3 <- clmm(Value ~ CompTypeNum * SupTypeNum + (1|ID) + (1+SupTypeNum|Trigger), filter(results, PsType =="Ps"), link = "logit")

Support.ordinal4 <- clmm(Value ~ CompTypeNum * SupTypeNum + (1|ID) + (1+CompTypeNum|Trigger), filter(results, PsType =="Ps"), link = "logit")

anova(Support.ordinal,Support.ordinal2)
anova(Support.ordinal,Support.ordinal3)
anova(Support.ordinal,Support.ordinal4)
```

Including by-trigger random slope for interaction doesn't significantly improve model fit. Including by-trigger random slope for CompType does make a difference. Including by-trigger random slope for SupType doesn't make a difference. So, our final model is Support.ordinal4:

```{r}
summary(Support.ordinal4)

```

Significant interaction (plus main effect dominated by interaction)

Follow-up pairwise comparisons:

```{r}
Support.emm <- emmeans(Support.ordinal4, specs = pairwise ~ SupTypeNum|CompTypeNum)



Support.emm$contrasts %>%
  rbind()


```

Support-Ps-Cond is rated significantly higher than EI-Ps-Cond. The Disj conditions are NOT significantly lower in rating than S-Ps-Cond.

