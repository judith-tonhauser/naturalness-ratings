# this R script is a modification of the RMD script downloaded from the OSF repo provided in
# Kalomoiros & Schwarz 2024 (https://osf.io/3p68r/)
# it calls the data from Exp 2 ---

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
theme_set(theme_bw())

## Data import

#We first load the necessary R packages and import the data: initially, we create 4 results files representing presuppositional vs non-presuppositional stimuli for each connective:

#1.  CONJPs
#2.  CONJNoPs
#3.  DISJPs
#4.  DISJNoPs

# load libraries
#```{r Data import, message=FALSE, warning=FALSE, linewidth=20}
library(tidyverse)
#library(emmeans)
#library(sjPlot)
#library(emmeans)
#library(lmerTest)
#library(lme4)
#library(ordinal)

#Read in the results

resultsCONJPs <- read.csv("https://159d3d98-11fc-4642-8e29-43c994fa07d0.usrfiles.com/ugd/159d3d_2f0abc5ea2284053ba89b24fe8b72cee.csv")
resultsCONJNoPs <- read.csv("https://159d3d98-11fc-4642-8e29-43c994fa07d0.usrfiles.com/ugd/159d3d_13a280890ffb4013b862d92f83bb240c.csv")
resultsDISJPs <- read.csv("https://159d3d98-11fc-4642-8e29-43c994fa07d0.usrfiles.com/ugd/159d3d_f9d3c9bf402e4065b333c17291788436.csv")
resultsDISJNoPs <- read.csv("https://159d3d98-11fc-4642-8e29-43c994fa07d0.usrfiles.com/ugd/159d3d_f9041e82b68c4ed8b5f5f9072ca28b4a.csv")


# We then bind the results files into results for conjunction (resultsCONJ), results for disjunction (resultsDISJ), and an overall results file (results):

#```{r, warning = FALSE, message = FALSE}
resultsCONJ <- rbind(resultsCONJPs, resultsCONJNoPs)

resultsDISJ <- rbind(resultsDISJPs, resultsDISJNoPs)

results <- rbind(resultsCONJ, resultsDISJ)
#```

#Check number of observations in each critical condition:

#```{r, warning = FALSE, message = FALSE}
check <- results %>%
  filter(`Item` != "ID", Condition != "Filler")%>%
  group_by(Condition) %>%
  dplyr::summarise(Count = n())
check
#```

#Check the total number of participants:

#```{r, warning = FALSE, message = FALSE}
check2 <- results %>%
  group_by(ID)%>%
  dplyr::summarise(count = n())
check2
#```

## Visualizing the data

### Conjunction

#Make the Value fields in the results files numeric. Then, filter out fillers, leaving only critical conditions

#```{r, warning = FALSE, message = FALSE}

resultsCONJ$Value <- as.numeric(as.character(resultsCONJ$Value))
resultsDISJ$Value <- as.numeric(as.character(resultsDISJ$Value))
results$Value <- as.numeric(as.character(results$Value))
table(results$Value) # values from 1-9

# turn into values from 0-8 for better comparison with Mandelkern et al 2020
table(results$Value)
results$Value = results$Value -1
table(results$Value)

resultsCrit <- results %>%
  filter(Condition != "Filler") %>%
  mutate(Condition = factor(Condition, c("PsFirst", "PsSecond","NoPsFirst", "NoPsSecond", "EISimplePs", "SSimplePs")))

# graphs ----

d = resultsCrit %>%
  filter(Condition == "EISimplePs" | Condition == "SSimplePs") %>%
  droplevels()

#### Fig A: plot of mean naturalness ratings in explicit ignorance context ----

# target data: explicit ignorance context
t = d %>%
  filter(Condition == "EISimplePs")
nrow(t)
#view(t)
table(t$Trigger)
#  again continue     stop

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
  geom_violin(data=t[t$Condition == "EISimplePs",],aes(x=Trigger, y=Value),
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
ggsave("../graphs/explicit-ignorance-naturalness-by-expression.pdf",height=3,width=3)


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
  filter(Condition == "EISimplePs") %>%
  group_by(Trigger) %>%
  summarize(Mean = mean(Value)) %>%
  mutate(Trigger = fct_reorder(as.factor(Trigger),Mean))
tmp
levels(tmp$Trigger)

# order predicates by mean difference in EIC and Support context 
# because the claim by Mandelkern et al is that this is how ps differ from non-ps
tmp.EIC <- d %>%
  filter(Condition == "EISimplePs") %>%
  group_by(Trigger) %>%
  summarize(Mean.EIC = mean(Value))
tmp.EIC

tmp.SUP <- d %>%
  filter(Condition == "SSimplePs") %>%
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
nat.means$Condition = factor(nat.means$Condition, levels = c("EISimplePs", "SSimplePs"))

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
ggsave("../graphs/naturalness-by-context-and-expression.pdf",height=3,width=4)

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

m = lm(Value ~ Condition, data=subset(d[d$Trigger == "continue",]))
summary(m)

#again: ***
#continue: ***
#stop: ***

# JT's code ends here -----

Summarize the conjunction part of the data by finding mean, standard deviation, standard error, and confidence intervals for the Value field for each condition:

```{r, warning = FALSE, message = FALSE}

#CONJ

resultsCondCONJ <- resultsCrit %>%
  filter(Connective == "CONJ") %>%
  group_by(Condition) %>%
  dplyr::summarize(n = n(), MeanValue = mean(Value), sd = sd(Value))%>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ci=se * qt((1-0.05)/2 + .5, n-1))

```

Plot mean Value per condition for conjunction:

```{r, echo=FALSE}

colours <- c("#727272", "#f1595f", "#79c36a", "#599ad3", "#f9a65a" , "#9e66ab")

ggplot(resultsCondCONJ, aes(x = Condition, y = MeanValue, fill = Condition))+
  geom_bar(stat = "identity") +
  ylim("1", "2", "3", "4", "5", "6", "7", "8", "9")+
  geom_errorbar(aes(ymin=MeanValue-se, ymax=MeanValue+se), width=.1) +
  scale_fill_manual(values=colours)+
  theme(axis.text.x = element_text(angle = 45)) +
  theme_classic() +
  labs(title = "Mean rating per condition for conjunction",
       y = "Mean rating",
       x = "Condition")
```

### Disjunction

We now repeat for disjunction:

```{r, warning = FALSE, message = FALSE}
#DISJ

resultsCondDISJ <- resultsCrit %>%
  filter(Connective == "DISJ") %>%
  group_by(Condition) %>%
  dplyr::summarize(n = n(), MeanValue = mean(Value), sd = sd(Value))%>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ci=se * qt((1-0.05)/2 + .5, n-1))

```

Plot mean Value per condition for disjunction:

```{r, echo=FALSE}

ggplot(resultsCondDISJ, aes(x = Condition, y = MeanValue, fill = Condition))+
  geom_bar(stat = "identity") +
  ylim("1", "2", "3", "4", "5", "6", "7", "8", "9")+
  geom_errorbar(aes(ymin=MeanValue-se, ymax=MeanValue+se), width=.1) +
  scale_fill_manual(values=colours)+
  theme(axis.text.x = element_text(angle = 45)) +
  theme_classic() +
  labs(title = "Mean rating per condition for disjunction",
       y = "Mean rating",
       x = "Condition")
```

### Fillers

We also check that the filler manipulation worked. First we isolate the rating of the filler conditions:

```{r, warning = FALSE, message = FALSE}
#Filter out critical items

resultsFillers <- results %>%
  filter(Condition == "Filler") %>%
  mutate(Trigger = factor(Trigger, c("GoodCond", "BadCond")))


```

Then we summarize the mean rating (together with its standard deviation, standard error, and confidence intervals) for the filler conditions:

```{r, warning = FALSE, message = FALSE}
resultsFiller <- resultsFillers %>%
  group_by(Trigger) %>%
  dplyr::summarize(n = n(), MeanValue = mean(Value), sd = sd(Value))%>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ci=se * qt((1-0.05)/2 + .5, n-1))

```

Finally, we plot the mean rating per filler variant:

```{r, echo = FALSE}
ggplot(resultsFiller, aes(x = Trigger, y = MeanValue, fill = Trigger))+
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin=MeanValue-se, ymax=MeanValue+se), width=.1) +
  scale_fill_manual(values=colours)+
  theme(axis.text.x = element_text(angle = 45)) +
  theme_classic() +
  labs(title = "Mean rating per filler variant",
       y = "Mean rating",
       x = "Filler variant")
```

## Statistics

### General

Since we are going to be running ordinal models we make Value a factor:

```{r}
resultsCONJ$Value <- as.factor(resultsCONJ$Value)
resultsDISJ$Value <- as.factor(resultsDISJ$Value)
results$Value <- as.factor(results$Value)
```

First, we test the difference test between EISimplePs and SSimplePs for CONJ, to make sure that our design can detect projection-related infelicities.

```{r}
mod1 <- clmm(Value ~ Condition + (1|ID) + (1|Item), filter(resultsCONJ, Condition ==
  "EISimplePs"| Condition=="SSimplePs"))

summary(mod1)

```

We also fit a model with by-item random intercept and by-item random slope:

```{r}
mod1.1 <- clmm(Value ~ Condition + (1|ID) + (1+Condition|Item), filter(resultsCONJ, Condition ==
  "EISimplePs"| Condition=="SSimplePs"))

summary(mod1.1)
```

We also test if the addition of the by-item random slope improves anything:

```{r}
anova(mod1, mod1.1)
```

It does, so we keep our mod1.1.

We see that in mod1.1, to go from EISimplePs to SSimplePs the rating must jump by approx 2.95 points, an extremely significant difference. Therefore, our design can detect the effects of presupposition projection.

Then, we test the same difference EISimplePs and SSimplePs for DISJ.

```{r}
mod2 <- clmm(Value ~ Condition + (1|ID) + (1|Item), filter(resultsDISJ, Condition ==
  "EISimplePs"| Condition=="SSimplePs"))

summary(mod2)

```

We also fit a model with by-item random intercept and by-item random slope:

```{r}
mod2.1 <- clmm(Value ~ Condition + (1|ID) + (1+Condition|Item), filter(resultsDISJ, Condition ==
  "EISimplePs"| Condition=="SSimplePs"))

summary(mod2.1)
```

We also test if the addition of the by-item random slope improves anything:

```{r}
anova(mod2, mod2.1)
```

It does, so we keep our mod2.1. Again, the model shows significant difference between EI vs S SimplePs.

### Disjunction

A theoretically relevant questions is whether the presence of material supporting the presupposition in PsFirst and PsSecond conjunctions has any effect at all on acceptability, compared to cases where no such material is available, i.e. EISimplePs vs SSimplePs.

On the Hirsch & Hackl account, PsFirst and EISimplePs are both taken to be fine to the extent that Local Accommodation is allowed. So they should be on par. PsSecond and SSimplePs both have some kind of support for the presupposition, so they should be on par. Thus, no interaction is predicted between the presence of Support (Support vs No Support) and the type of Sentence (Ps vs Simple).

To test this, first we filter the data so that only the Ps and SimplePs conditions remain:

```{r}


resultsDISJSimplevsComplex <- resultsDISJ %>%
  filter(!(Condition %in% c("NoPsFirst", "NoPsSecond", "Filler")))
```

Then, we set up two two-level factors: CompType categorizes the conditions as Simple (EI/S-SimplePs) vs Complex (PsFirst/Second):

```{r}
resultsDISJSimplevsComplex$CompType <- case_when(resultsDISJSimplevsComplex$Condition == "PsFirst" | resultsDISJSimplevsComplex$Condition == "PsSecond" ~ "Complex",
                                       resultsDISJSimplevsComplex$Condition == "EISimplePs" | resultsDISJSimplevsComplex$Condition == "SSimplePs" ~ "Simple")

resultsDISJSimplevsComplex$CompType <- as.factor(resultsDISJSimplevsComplex$CompType)
```

The other factor, SupType, categorizes the conditions on the basis of whether the presupposition is supported in its local or global context. Thus, SSimplePs and PsSecond are categorized as "S" (for "Support"), whereas PsFirst and EISimplePs are categorized as "NoS":

```{r}
resultsDISJSimplevsComplex$SupType <- case_when(resultsDISJSimplevsComplex$Condition == "PsFirst" | resultsDISJSimplevsComplex$Condition == "EISimplePs" ~ "NoS",
                                          resultsDISJSimplevsComplex$Condition == "PsSecond" | resultsDISJSimplevsComplex$Condition == "SSimplePs" ~ "S")

resultsDISJSimplevsComplex$SupType <- as.factor(resultsDISJSimplevsComplex$SupType)
```

Again, the factors are sum-coded:

```{r}
contrasts(resultsDISJSimplevsComplex$SupType) <- contr.sum(2)
contrasts(resultsDISJSimplevsComplex$CompType) <- contr.sum(2)
```

We also rename the contrasts for interpretability:

```{r}
colnames(contrasts(resultsDISJSimplevsComplex$SupType)) <- c('NoS')
colnames(contrasts(resultsDISJSimplevsComplex$CompType)) <- c('Complex')
```

We then fit a model predicting rating from SupType, CompType and their interaction. For participants who saw SSimplePs, this was the only SupType sentence they saw. So, these participants did not see both kinds of SupType sentences. Therefore, we do not include a by-participant random slope for SupType. The maximal model we can try to fit is:

```{r}
mod3 <- clmm(Value ~ CompType*SupType + (1+CompType|ID) + (1+CompType*SupType|Item), resultsDISJSimplevsComplex, link = "logit")
```

```{r}
summary(mod3)
```

We test if removing the by-item random slope for the interaction matters:

```{r}
mod3.1 <- clmm(Value ~ CompType*SupType + (1+CompType|ID) + (1+CompType+SupType|Item), resultsDISJSimplevsComplex, link = "logit")
anova(mod3, mod3.1)
```

It does. Does including by-participant random slope for CompType matter?

```{r}
mod3.2 <- clmm(Value ~ CompType*SupType + (1|ID) + (1+CompType*SupType|Item), resultsDISJSimplevsComplex, link = "logit")
anova(mod3, mod3.2)
```

It does. Final model:

```{r}
mod3_fin <- clmm(Value ~ CompType*SupType + (1+CompType|ID) + (1+CompType*SupType|Item), resultsDISJSimplevsComplex, link = "logit")
```

Let's look at the outcome of the model:

```{r}
summary(mod3_fin)
```

Hugely significant interaction. Let's test with emmeans as well:

```{r}
emm1 <- emmeans(mod3_fin, ~CompType | SupType)
contrast(emm1, 'pairwise')
```

Clearly, PsFirst and EISimple Ps are not on par, against the prediction that a local accommodation account of bathroom disjunctions makes.

### Testing the difference between conjunction and disjunction

If disjunction and conjunction really do differ in terms of the availability of symmetric filtering, then we should see the following picture: For each connective, the effect of ORDER on the presence of a presupposition should be different. CONJ should show an interaction between CONJType (Ps vs NoPs) and Order (First vs Second), whereas DISJ should show no such interaction. This should lead to a three-way interaction (different two-by-two interactions) between Connective (CONJ VS DISJ), PsType (Ps vs NoPs) and Order (First vs Second).

Let's test this. First take the full results:

```{r}
resultsConnOrderPs <- results %>%
  filter(!(Condition %in% c("EISimplePs", "SSimplePs", "Filler")))
```

Then set up a PsType factor:

```{r}
resultsConnOrderPs$PsType <- case_when(resultsConnOrderPs$Condition == "PsFirst" | resultsConnOrderPs$Condition == "PsSecond" ~ "Ps",
                                          resultsConnOrderPs$Condition == "NoPsFirst" | resultsConnOrderPs$Condition == "NoPsSecond" ~ "NoPs")

resultsConnOrderPs$PsType <- as.factor(resultsConnOrderPs$PsType)
```

Set up an Order factor:

```{r}
resultsConnOrderPs$Order <- case_when(resultsConnOrderPs$Condition == "PsFirst" | resultsConnOrderPs$Condition == "NoPsFirst" ~ "First",
                                       resultsConnOrderPs$Condition == "PsSecond" | resultsConnOrderPs$Condition == "NoPsSecond" ~ "Second")

resultsConnOrderPs$Order <- as.factor(resultsConnOrderPs$Order)
```

Finally, set up a Connective factor:

```{r}
resultsConnOrderPs$Connective <- case_when(resultsConnOrderPs$Connective == "CONJ" ~ "CONJ", resultsConnOrderPs$Connective == "DISJ" ~ "DISJ")

resultsConnOrderPs$Connective <- as.factor(resultsConnOrderPs$Connective)

```

Sum-code the factors:

```{r}
contrasts(resultsConnOrderPs$PsType) <- contr.sum(2)
contrasts(resultsConnOrderPs$Order) <- contr.sum(2)
contrasts(resultsConnOrderPs$Connective) <- contr.sum(2)
```

Re-level the PsType factor:

```{r}
resultsConnOrderPs <- resultsConnOrderPs %>%
  mutate(PsType = relevel(PsType, 'Ps'))
```

```{r}
colnames(contrasts(resultsConnOrderPs$PsType)) <- c('Ps')
colnames(contrasts(resultsConnOrderPs$Order)) <- c('First')
colnames(contrasts(resultsConnOrderPs$Connective)) <- c('CONJ')
```

Fit a model predicting rating (Value) from PsType, Order, Connective and their interaction. The maximal model we can try to fit is, the following (since each participant saw only one connective and one PsType, only a by-participant random slope for Order makes sense here):

```{r}
mod4 <- clmm(Value ~ PsType*Order*Connective + (1+Order|ID) + (1+PsType*Order*Connective|Item), resultsConnOrderPs, link = "logit")
```

The model converges. Let's take a look at it:

```{r}
summary(mod4)

#Cumulative Link Mixed Model fitted with the Laplace approximation

#formula: Value ~ PsType * Order * Connective + (1 + Order | ID) + (1 +      PsType * Order * Connective | Item)
#data:    resultsConnOrderPs

# link  threshold nobs logLik   AIC      niter         max.grad cond.H 
# logit flexible  3248 -6077.05 12262.09 18120(126518) 6.83e-02 2.5e+04

#Random effects:
# Groups Name                                  Variance Std.Dev. Corr                                             
# ID     (Intercept)                           2.20821  1.4860                                                    
        #OrderSecond                           1.71324  1.3089   -0.509                                           
# Item   (Intercept)                           0.87680  0.9364                                                    
#        PsTypeNoPs                            0.59188  0.7693    0.586                                           
#        OrderSecond                           0.08985  0.2998   -0.004 -0.198                                    
#        ConnectiveDISJ                        0.90658  0.9521   -0.625 -0.559  0.644                             
#        PsTypeNoPs:OrderSecond                0.07415  0.2723   -0.627 -0.281  0.068  0.108                      
#        PsTypeNoPs:ConnectiveDISJ             0.30741  0.5544   -0.086 -0.198 -0.742 -0.529  0.297               
#        OrderSecond:ConnectiveDISJ            0.32330  0.5686   -0.181  0.155 -0.904 -0.620  0.263  0.674        
#        PsTypeNoPs:OrderSecond:ConnectiveDISJ 0.33302  0.5771    0.655  0.418  0.678  0.072 -0.393 -0.596 -0.794 
#Number of groups:  ID 203,  Item 24 

#Coefficients:
#                                   Estimate Std. Error z value Pr(>|z|)    
#PsTypePs                            0.01343    0.25087   0.054 0.957294    
#OrderFirst                         -0.37034    0.08243  -4.493 7.02e-06 ***
#ConnectiveCONJ                     -0.48505    0.16018  -3.028 0.002460 ** 
#PsTypePs:OrderFirst                 0.41964    0.11878   3.533 0.000411 ***
#PsTypePs:ConnectiveCONJ             0.22601    0.19827   1.140 0.254309    
#OrderFirst:ConnectiveCONJ          -0.50350    0.08645  -5.824 5.73e-09 ***
#PsTypePs:OrderFirst:ConnectiveCONJ  0.42868    0.11812   3.629 0.000284 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Threshold coefficients:
#    Estimate Std. Error z value
#1|2  -3.5524     0.2155 -16.483
#2|3  -2.5222     0.2079 -12.129
#3|4  -1.8439     0.2051  -8.990
#4|5  -1.4173     0.2039  -6.951
#5|6  -0.9289     0.2030  -4.577
#6|7  -0.2957     0.2023  -1.462
#7|8   0.6172     0.2023   3.051
#8|9   1.7575     0.2046   8.592
```

Let's do some model comparison. First, we check the extent to which including the random slope for the three-way interaction improves model fit:

```{r}
mod4.1 <- clmm(Value ~ PsType*Order*Connective + (1+Order|ID) + (1+PsType+Order+Connective|Item), resultsConnOrderPs, link = "logit")
summary(mod4.1)
```

```{r}
anova(mod4, mod4.1)
```

Including the interaction random slope doesn't significantly improve model fit, so we'll simplify the structure and exclude it from the final model. Let's check if the Order by-participant random slope makes a difference:

```{r}
mod4.2 <- clmm(Value ~ Connective*PsType*Order + (1|ID) + (1+PsType*Order*Connective|Item), resultsConnOrderPs, link = "logit")

```

```{r}
summary(mod4.2)
```

```{r}
anova(mod4, mod4.2)
```

This makes a difference, so we keep it. Next we remove by-item Connective random slope:

```{r}
mod4.3 <- clmm(Value ~ Connective*PsType*Order + (1+Order|ID) + (1+PsType+Order|Item), resultsConnOrderPs, link = "logit")

anova(mod4, mod4.3)
```

It makes a difference so we keep it. Next we remove Order by-item random slope:

```{r}
mod4.4 <- clmm(Value ~ Connective*PsType*Order + (1+Order|ID) + (1+PsType+Connective|Item), resultsConnOrderPs, link = "logit")


```

```{r}
anova(mod4, mod4.4)
```

Including Order by-item random slope doesn't produce a significantly better model, so we can leave it out. Let's check the importance of leaving PsType by-item random slope:

```{r}
mod4.5 <- clmm(Value ~ Connective*PsType*Order + (1+Order|ID) + (1+Order+Connective|Item), resultsConnOrderPs, link = "logit")


```

```{r}
anova(mod4, mod4.5)
```

Including PsType makes a difference, so we keep it in. So, our final model is:

```{r}

mod4_fin <- clmm(Value ~ Connective*PsType*Order + (1+Order|ID) + (1+PsType+Connective|Item), resultsConnOrderPs, link = "logit")

```

Look at the model's output:

```{r}
summary(mod4_fin)
```

The three-way interaction comes out extremely strongly. Let's look at the interaction using emmeans:

```{r}
emm2 <- emmeans(mod4_fin, ~PsType*Order| Connective, pbkrtest.limit = 3248)
contrast(emm2, interaction = "pairwise")
```

The above makes it clear that the three-way interaction is driven by the contrast between PsFirst vs NoPsFirst for CONJ.

As noted in fn 17 in the main text, some conjunction items contained the potentially problematic item "only" in what was supposed to be the non-presuppositional conjunct. We check whether the three way interaction still comes out when these items are removed (we remove from the both the conjunction and the disjunction part of the data):

```{r}

mod4_fin.1 <- clmm(Value ~ Connective*PsType*Order + (1+Order|ID) + (1+PsType+Connective|Item), filter(resultsConnOrderPs, !(Item %in% c("3", "4"))), link = "logit")
summary(mod4.1_fin)

```

The three way interaaction still comes out extremely significant. Therefore, our initial analyses that included these cases stand.

We also assess the evidence in favor of the null hypothesis with respect to the 2-way interaction term for disjunction. To do so, we conduct a Bayesian analysis parallel to the one we conducted for Experiment 1. We start with a Bayesian analysis of the conjunction part of the data in Exp2, in order to get parameter expectation to use as priors for the Bayesian analysis of the disjunction data.

```{r}

resultsCONJ$Value <- as.numeric(as.character(resultsCONJ$Value)) #make Value an integer (bayesian models below are cumulative and requires it)

resultsCONJPsvsNoPs <- resultsCONJ %>% filter(!(Condition %in% c("EISimplePs", "SSimplePs", "Filler")))

#Set up ConjType factor:

resultsCONJPsvsNoPs$CONJType <- case_when(resultsCONJPsvsNoPs$Condition == "PsFirst" | resultsCONJPsvsNoPs$Condition == "PsSecond" ~ "Ps",
                                          resultsCONJPsvsNoPs$Condition == "NoPsFirst" | resultsCONJPsvsNoPs$Condition == "NoPsSecond" ~ "NoPs")

resultsCONJPsvsNoPs$CONJType <- as.factor(resultsCONJPsvsNoPs$CONJType)

# Set up Order factor:

resultsCONJPsvsNoPs$Order <- case_when(resultsCONJPsvsNoPs$Condition == "PsFirst" | resultsCONJPsvsNoPs$Condition == "NoPsFirst" ~ "First",
                                       resultsCONJPsvsNoPs$Condition == "PsSecond" | resultsCONJPsvsNoPs$Condition == "NoPsSecond" ~ "Second")

resultsCONJPsvsNoPs$Order <- as.factor(resultsCONJPsvsNoPs$Order)

#Reference levels

resultsCONJPsvsNoPs <- within(resultsCONJPsvsNoPs, resultsCONJPsvsNoPs$Order <- relevel(resultsCONJPsvsNoPs$Order, ref = "First"))

resultsCONJPsvsNoPs <- within(resultsCONJPsvsNoPs, resultsCONJPsvsNoPs$CONJType <- relevel(resultsCONJPsvsNoPs$CONJType, ref = "Ps"))

## Adding myCenter coding for comparison of estimates with Expt 1 BRM analysis

myCenter= function(x) {
	if (is.numeric(x)) { return(x - mean(x, na.rm=T)) }
	if (is.factor(x)) {
		x= as.numeric(x)
		return(x - mean(x, na.rm=T))
	}
	if (is.data.frame(x) || is.matrix(x)) {
		m= matrix(nrow=nrow(x), ncol=ncol(x))
		colnames(m)= paste("c", colnames(x), sep="")
		for (i in 1:ncol(x)) {
			m[,i]= myCenter(x[,i])
		}
		return(as.data.frame(m))
	}
}

resultsCONJPsvsNoPs$OrderC <- myCenter(resultsCONJPsvsNoPs$Order)
unique(resultsCONJPsvsNoPs$OrderC)
resultsCONJPsvsNoPs$CONJTypeC <- myCenter(resultsCONJPsvsNoPs$CONJType)
unique(resultsCONJPsvsNoPs$CONJTypeC)



```

```{r}
library(brms)

CONJPsNoPs.brm <- brm(Value ~ CONJTypeC*OrderC + (1+OrderC|ID) 
                  + (1+CONJTypeC+OrderC|Item),
                 family=cumulative("logit"),  
                 control = list(adapt_delta = 0.95),
                 resultsCONJPsvsNoPs)

 CONJPsNoPs.brm
# # 
# Population-Level Effects: 
#                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept[1]        -3.10      0.27    -3.63    -2.57 1.01      809     1439
# Intercept[2]        -2.14      0.26    -2.65    -1.64 1.01      797     1375
# Intercept[3]        -1.45      0.26    -1.95    -0.95 1.01      784     1260
# Intercept[4]        -1.00      0.26    -1.50    -0.50 1.01      798     1406
# Intercept[5]        -0.46      0.26    -0.96     0.04 1.01      781     1339
# Intercept[6]         0.12      0.26    -0.38     0.63 1.01      772     1306
# Intercept[7]         1.08      0.26     0.58     1.59 1.01      757     1377
# Intercept[8]         2.10      0.26     1.58     2.61 1.01      774     1481
# CONJTypeC            0.28      0.32    -0.37     0.90 1.00      936     1342
# OrderC               0.95      0.22     0.52     1.38 1.00     1338     2089
# CONJTypeC:OrderC    -1.76      0.44    -2.63    -0.90 1.00     1275     1968
 # estimate quite close to Mandelkern et al. (-1.49, .38); 



#####
```

As a sanity-check, we compare with the equivalent clmm model:

```{r}
resultsCONJPsvsNoPs$Value <- as.factor(resultsCONJPsvsNoPs$Value)

CONJPsNoPs.clmm <- clmm(Value ~ CONJTypeC*OrderC + (1+OrderC|ID) 
                  + (1+CONJTypeC+OrderC|Item), resultsCONJPsvsNoPs, link = "logit")

summary(CONJPsNoPs.clmm)
```
Estimates are comparable.


We now compare a model of the disjunction data WITH the interaction vs a model WITHOUT the interaction (using the priors from the CONJ part of the data):

```{r}
resultsDISJ$Value <- as.numeric(as.character(resultsDISJ$Value)) #make Value an integer (bayesian models below require it)
resultsDISJPsvsNoPs <- resultsDISJ %>% filter(!(Condition %in% c("EISimplePs", "SSimplePs", "Filler")))

#Set up DISJType factor:

resultsDISJPsvsNoPs$DISJType <- case_when(resultsDISJPsvsNoPs$Condition == "PsFirst" | resultsDISJPsvsNoPs$Condition == "PsSecond" ~ "Ps",
                                          resultsDISJPsvsNoPs$Condition == "NoPsFirst" | resultsDISJPsvsNoPs$Condition == "NoPsSecond" ~ "NoPs")

resultsDISJPsvsNoPs$DISJType <- as.factor(resultsDISJPsvsNoPs$DISJType)

# Set up Order factor:

resultsDISJPsvsNoPs$Order <- case_when(resultsDISJPsvsNoPs$Condition == "PsFirst" | resultsDISJPsvsNoPs$Condition == "NoPsFirst" ~ "First",
                                       resultsDISJPsvsNoPs$Condition == "PsSecond" | resultsDISJPsvsNoPs$Condition == "NoPsSecond" ~ "Second")

resultsDISJPsvsNoPs$Order <- as.factor(resultsDISJPsvsNoPs$Order)

#Reference levels

resultsDISJPsvsNoPs <- within(resultsDISJPsvsNoPs, resultsDISJPsvsNoPs$Order <- relevel(resultsDISJPsvsNoPs$Order, ref = "First"))

resultsDISJPsvsNoPs <- within(resultsDISJPsvsNoPs, resultsDISJPsvsNoPs$DISJType <- relevel(resultsDISJPsvsNoPs$DISJType, ref = "Ps"))

#Centering factors

resultsDISJPsvsNoPs$OrderC <- myCenter(resultsDISJPsvsNoPs$Order)

resultsDISJPsvsNoPs$DISJTypeC <- myCenter(resultsDISJPsvsNoPs$DISJType)


```

```{r}
library(brms)


priors <- c(
    prior(normal(-3.1, 0.27), class = "Intercept",
        coef = "1"),
  prior(normal(-2.14, 0.26), class = "Intercept",
        coef = "2"),
  prior(normal(-1.45, 0.26), class = "Intercept",
        coef = "3"),
  prior(normal(-1, 0.26), class = "Intercept",
        coef = "4"),
  prior(normal(-0.46, 0.26), class = "Intercept",
        coef = "5"),
  prior(normal(0.12, 0.26), class = "Intercept",
        coef = "6"),
  prior(normal(1.08, 0.26), class = "Intercept",
        coef = "7"),
  prior(normal(2.1, 0.26), class = "Intercept",
        coef = "8"),
  prior(normal(0.28, 0.32), class = "b", 
        coef = "DISJTypeC"),
  prior(normal(0.95, 0.22), class = "b",
        coef = "OrderC"),
  prior(normal(-1.76, 0.44), class = "b",
        coef = "DISJTypeC:OrderC")
)



DISJPsNoPs.brm <- brm(Value ~ DISJTypeC*OrderC + (1+OrderC|ID) 
                  + (1+DISJTypeC+OrderC|Item),
                 family=cumulative("logit"),  
                 control = list(adapt_delta = 0.95),
                 save_pars = save_pars(all = TRUE),
                 iter = 20000,
                 prior =  priors,
                 resultsDISJPsvsNoPs)

DISJPsNoPs.brm

```
```{r}
priors <- c(
    prior(normal(-3.1, 0.27), class = "Intercept",
        coef = "1"),
  prior(normal(-2.14, 0.26), class = "Intercept",
        coef = "2"),
  prior(normal(-1.45, 0.26), class = "Intercept",
        coef = "3"),
  prior(normal(-1, 0.26), class = "Intercept",
        coef = "4"),
  prior(normal(-0.46, 0.26), class = "Intercept",
        coef = "5"),
  prior(normal(0.12, 0.26), class = "Intercept",
        coef = "6"),
  prior(normal(1.08, 0.26), class = "Intercept",
        coef = "7"),
  prior(normal(2.1, 0.26), class = "Intercept",
        coef = "8"),
  prior(normal(0.28, 0.32), class = "b", 
        coef = "DISJTypeC"),
  prior(normal(0.95, 0.22), class = "b",
        coef = "OrderC")
)

DISJPsNoPsNullInt.brm <- brm(Value ~ DISJTypeC+OrderC + (1+OrderC|ID) 
                  + (1+DISJTypeC+OrderC|Item),
                 family=cumulative("logit"),  
                 control = list(adapt_delta = 0.95),
                 save_pars = save_pars(all = TRUE),
                 iter = 20000,
                 prior = priors,
                 resultsDISJPsvsNoPs)

```

```{r}
margLogLik_linear <- bridge_sampler(DISJPsNoPs.brm, silent = TRUE)
margLogLik_null <- bridge_sampler(DISJPsNoPsNullInt, silent = TRUE)


```

```{r}
(BF_ln <- bayes_factor(margLogLik_linear, margLogLik_null))
# Estimated Bayes factor in favor of x1 over x2: 0.00117
```

### Local Accommodation for both DISJ-PsFirst and DISJ-PsSecond?

We want to test if there's any evidence for the presence of Local Accommodation in both PsFirst and PsSecond. On that view PsFirst, PsSecond and EISimplePs involve Local Accommodation (by assumption), while NoPsFirst, NoPsSecond, and SSimplePs involve no Local Acc. Thus, all the conditionals and disjunctions that involve Local Acc should be on par, while the conditionals and disjunction that involve no local acc should be on par (or if there's an effect of disjunctions being more complex this should have equal effect for both Local Acc and non-Local Acc disjunctions). Hence, if we set up a LocalAcc categorizing EISimplePs, PsFirst, PsSecond as LocalAcc, while SSimplePs, NoPsFirst/Second and NoLocalAcc, and another factor CompType categorizing sentences as disjunctions or conditionals, then we expect no interaction between these two factors.

We set up the LocAcc factor:

```{r}
resultsDISJ$LocAcc <- case_when(resultsDISJ$Condition == "PsFirst" | resultsDISJ$Condition == "PsSecond" | resultsDISJ$Condition == "EISimplePs" ~ "LocAcc", 
            resultsDISJ$Condition == "NoPsFirst" | resultsDISJ$Condition == "NoPsSecond" | resultsDISJ$Condition == "SSimplePs" ~ "NoLocAcc")


resultsDISJ$LocAcc <- as.factor(resultsDISJ$LocAcc)
```

We then set up a CompType factor:

```{r}
resultsDISJ$CompType <- case_when(resultsDISJ$Condition == "PsFirst" | resultsDISJ$Condition == "NoPsFirst" ~ "DISJ1", resultsDISJ$Condition == "PsSecond" | resultsDISJ$Condition == "NoPsSecond" ~ "DISJ2", resultsDISJ$Condition == "EISimplePs"| resultsDISJ$Condition == "SSimplePs" ~ "COND")


resultsDISJ$CompType <- as.factor(resultsDISJ$CompType)
```

Sum-code the factors:

```{r}
contrasts(resultsDISJ$LocAcc) <- contr.sum(2)
contrasts(resultsDISJ$CompType) <- contr.sum(3)

```

Fit a model predicting Value from LocAcc, CompType and their interaction. Since each participant saw only one kind of LocAcc sentence, it doesn't make sense to include the LocAcc factor in the by-participant random effect structure. So, the maximal model is:

```{r}
mod5 <- clmm(Value ~ LocAcc*CompType + (1+CompType|ID) + (1+LocAcc*CompType|Item), resultsDISJ, link = "logit")
```

```{r}
summary(mod5)
```

Check if including the by-item random slope for the interaction improves model fit:

```{r}
mod5.1 <- clmm(Value ~ LocAcc*CompType + (1+CompType|ID) + (1+LocAcc+CompType|Item), resultsDISJ, link = "logit")
```

```{r}
summary(mod5.1)
```

```{r}
anova(mod5, mod5.1)
```

Including the radom slope for the interaction doesn't significantly improve fit. So, eventually we'll leave it out.

Check the the effect of LocAcc random slope for Item on model fit:

```{r}
mod5.2 <- clmm(Value ~ LocAcc*CompType + (1+CompType|ID) + (1+CompType|Item), resultsDISJ, link = "logit")
```

```{r}
summary(mod5.2)
anova(mod5, mod5.2)
```

It makes a differnece in terms of model fit. Now, check the the effect of CompType random slope for Item on model fit:

```{r}
mod5.3 <- clmm(Value ~ LocAcc*CompType + (1+CompType|ID) + (1+LocAcc|Item), resultsDISJ, link = "logit")
```

```{r}
summary(mod5.3)
anova(mod5, mod5.3)
```

It does, so we keep it. Now, check the the effect of CompType random slope for ID on model fit:

```{r}
mod5.4 <- clmm(Value ~ LocAcc*CompType + (1|ID) + (1+LocAcc*CompType|Item), resultsDISJ, link = "logit")
```

```{r}
summary(mod5.4)
anova(mod5, mod5.4)
```

It does, so we keep it.

Final model:

```{r}
mod5_fin <- clmm(Value ~ LocAcc*CompType + (1+CompType|ID) + (1+LocAcc+CompType|Item), resultsDISJ, link = "logit")
```

```{r}
summary(mod5_fin)
```

Look at the interaction via emmeans:

```{r}
emm3 <- emmeans(mod5_fin, ~LocAcc|CompType, pbkrtest.limit = 3248)
contrast(emm3, interaction = "pairwise")
```
