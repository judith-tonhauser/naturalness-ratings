# this R script is a modification of the R script downloaded from the OSF repo provided in
# Mandelkern et al 2020 (https://osf.io/jwcvr)
# it calls the data from Exp 3

# load packages
library(tidyverse)
library(reshape2)
library(lme4)

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
theme_set(theme_bw())

# load helpers
source('../../results/helpers.R')

# read data from exp 3 in Mandelkern et al 2020
results <- read.csv("../data/IncrSymAccResults.txt", comment.char="#", header=F)
#view(results)

# assign column names
names(results) <- c("ReceptionTime", "IP", "Controller", "It", "Elements", "Type", "Group", "Question", "Answer", "Time")
#view(results)

# subset the data into meta (?, something about experiment coding?) and results
meta <- subset(results, Controller!="DynamicQuestion")
results <- subset(results, Controller=="DynamicQuestion")

# '+' was used as a separating character in 'Question', so the 'p+' in 'SupportP+' in 'Question' was replaced by "p+" (not sure what this is about)
#table(results$Question)
#results$Question <- sub("[+][+]", "p+", results$Question)

# now the 'Question' column is split into separate columns based on +
results <- cbind(results, colsplit(results$Question, "[+]", c("item","group","condition","context", "trigger", "sentence", "inference")))

# Creating subject IDs by combining IP and time
results$subject <- paste(results$IP, results$ReceptionTime)

length(unique(results$subject))
#[1] 126

# remove practice trials
practice <- subset(results, Type=="practice")
results <- subset(results, Type!="practice")

# make ordinal Answers numeric
results$Answer <- as.numeric(as.character(results$Answer))
table(results$Answer) # answers between 0 and 6 (ordinal)

# context is Explicit Ignorance or Support
table(results$context)

# "trigger" codes the expression (- means no trigger)
table(results$trigger)

# select relevant columns
results = results %>%
  select(c("Answer","item","condition","context","trigger","sentence","inference","subject"))
#view(results)
# Answer: rating given by the participant
# item: item number
# condition: Acceptable (filler?), P (ps but no conjunction), TrF (trigger first), TrL (trigger last), Unacceptable (filler?)
# context: ExplicitIgnoranceP, SupportP (whether the context is an EI context or a context that satisfies the ps)
# trigger: aware, continue, enjoy, frown on, happy, hoping, stop, sure and --
# sentence: the sentence that participants read
# inference: the sentence that represents the inference of interest in the sentence that participants read
# subject: participant ID

# code whether something was a ps trigger or not
results$ps <- "not presupposition"
results$ps[results$trigger=="aware"|results$trigger=="happy"|results$trigger=="stop"|results$trigger=="continue"] <- "presupposition"
table(results$trigger, results$ps)

# change "frown on" to "now frown on"
results = results %>%
  mutate(trigger = recode(trigger, "frown on" = "now frown on"))
table(tmp$trigger)

# item coding: items with numbers above 40 don't have "If..."-sentence (filler)
results$item <- as.numeric(results$item)

# remove such rows from the dataset
results <- results %>%
  filter(item <= 40)

# remove the rows without a trigger
results <- results %>%
  filter(trigger != "-")
table(results$trigger)

# graphs ----

#### Fig A: plot of mean naturalness ratings in explicit ignorance context ----

# target data: explicit ignorance context
t = results %>%
  filter(context == "ExplicitIgnoranceP")
nrow(t)
table(t$trigger)
str(t)
table(t$Answer) #0-6

# calculate mean naturalness rating by expression in explicit ignorance context
nat.meansEIC = t %>%
  group_by(trigger) %>%
  summarize(Mean = mean(Answer), CILow = ci.low(Answer), CIHigh = ci.high(Answer)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, expression = fct_reorder(as.factor(trigger),Mean))
nat.meansEIC
levels(nat.meansEIC$expression)

# color code the expressions
ps <- c("aware", "continue", "happy", "stop")
noPs <- c("enjoy", "now frown on","hoping","sure")

nat.meansEIC$ps = ifelse(nat.meansEIC$expression %in% noPs, "noPs",
                         ifelse(nat.meansEIC$expression %in% ps, "ps", "other"))
table(nat.meansEIC$ps)

t$expression = t$trigger
t$ps = ifelse(t$expression %in% noPs, "noPs",
              ifelse(t$expression %in% ps, "ps", "other"))
table(t$ps)

table(nat.meansEIC$ps, nat.meansEIC$expression)

text.color <- ifelse(nat.meansEIC$expression[order(nat.meansEIC$Mean)] %in% ps, '#D55E00',
                     ifelse(nat.meansEIC$expression[order(nat.meansEIC$Mean)] %in% noPs, "black", "#009E73"))
text.color

t$expression = factor(t$expression, levels = nat.meansEIC$expression[order(nat.meansEIC$Mean)], ordered = TRUE)

# plot of naturalness means, with participants' individual responses
ggplot(nat.meansEIC, aes(x=expression, y=Mean)) +
  geom_violin(data=t[t$context == "ExplicitIgnoranceP",],aes(x=expression, y=Answer),
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
nat.means = results %>%
  group_by(trigger,context) %>%
  summarize(Mean = mean(Answer), CILow = ci.low(Answer), CIHigh = ci.high(Answer)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  ungroup %>%
  select(-c(CILow,CIHigh)) %>%
  mutate(context = as.factor(context))
nat.means
table(nat.means$context)
nat.means$trigger <- as.factor(nat.means$trigger)
levels(nat.means$trigger)

# order predicates by mean naturalness rating in EIC
tmp <- results %>%
  filter(context == "ExplicitIgnoranceP") %>%
  group_by(trigger) %>%
  summarize(Mean = mean(Answer)) %>%
  mutate(expression = fct_reorder(as.factor(trigger),Mean))
tmp
levels(tmp$expression)

# order predicates by mean difference in EIC and Support context 
# because the claim by Mandelkern et al is that this is how ps differ from non-ps
tmp.EIC <- results %>%
  filter(context == "ExplicitIgnoranceP") %>%
  group_by(trigger) %>%
  summarize(Mean.EIC = mean(Answer))
tmp.EIC

tmp.SUP <- results %>%
  filter(context == "SupportP") %>%
  group_by(trigger) %>%
  summarize(Mean.SUP = mean(Answer))
tmp.SUP

tmp = left_join(tmp.EIC,tmp.SUP)
tmp
tmp$Diff = tmp$Mean.EIC-tmp$Mean.SUP
tmp
tmp = tmp %>%
  mutate(expression = fct_reorder(as.factor(trigger),Diff))
tmp

# now order expressions based on tmp (two options above)
levels(tmp$expression)

nat.means$trigger = factor(nat.means$trigger, levels=tmp$expression[order(tmp$expression)], ordered=TRUE)
results$trigger = factor(results$trigger, levels=tmp$expression[order(tmp$expression)], ordered=TRUE)
levels(nat.means$trigger)
levels(results$trigger)

# order the contexts
levels(nat.means$context)
nat.means$context = factor(nat.means$context, levels = c("ExplicitIgnoranceP", "SupportP"))

fill.color <- ifelse(levels(nat.means$trigger) %in% ps, '#D55E00', "black")
fill.color

# to color the facets differently
library(ggh4x)

strip <- strip_themed(background_x = elem_list_rect(fill = fill.color))

nat.means

# violinplot
ggplot() +
  geom_violin(data=results, aes(x=context, y=Answer, fill = context), scale="width", linewidth = 0) +
  geom_point(data=nat.means, aes(x=context, y=Mean, fill = context), shape=21,stroke=.5,size=2, color="black") +
  geom_errorbar(data=nat.means,aes(x=context,ymin=YMin,ymax=YMax),width=0.1,color="black") +
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
  facet_wrap2(. ~ trigger, nrow = 1, strip = strip) +
  theme(panel.grid.minor = element_blank()) +
  theme(strip.background = element_rect(fill="white")) +
  theme(strip.text = element_text(color = "white"))
ggsave("../graphs/naturalness-by-context-and-expression.pdf",height=3,width=9)

# analysis ----

# Mandelkern et al 2020 used linear regression with their data, so we will do the same

m = lmer(Answer ~ context + (1+context|subject) + (1+context|item), data=subset(results[results$trigger == "sure",]))
summary(m)

# continue: ***
# stop: **
# happy: ***
# frown on: --
# hoping: --
# aware: *
# enjoy: --
# sure: --


# BELOW HERE NOT JT CODE ----


# overall summary
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")

results$condition.labels <- as.character(results$condition)
results$condition.labels[results$condition=="TrF"&results$ps=="ps"] <- "Ps-1st"
results$condition.labels[results$condition=="TrL"&results$ps=="ps"] <- "Ps-2nd"
results$condition.labels[results$condition=="TrF"&results$ps=="no-ps"] <- "NoPs-1st"
results$condition.labels[results$condition=="TrL"&results$ps=="no-ps"] <- "NoPs-2nd"
results$condition.labels[results$condition=="P"&results$ps=="ps"] <- "Simple Ps"
results$condition.labels[results$condition=="P"&results$ps=="no-ps"] <- "Simple NoPs"

results$condition.labels <- factor(results$condition.labels, 
                                   levels=c("Acceptable", "Simple NoPs", "Simple Ps", "NoPs-1st", "NoPs-2nd", "Ps-1st", "Ps-2nd", "Unacceptable"))

levels(results$condition.labels)[c(1,8)] <- c("Accept.", "Unacc.")

ggplot(results, aes(condition.labels, Answer, fill=inference_about))+
  stat_summary(fun.y=mean, geom="bar", position="dodge")+
  scale_x_discrete(name="")+scale_y_continuous(name="Mean Ratings")+
  scale_fill_manual(values=cbPalette, name="Context", labels=c("Support","Explicit Ignorance"))+
  facet_wrap(~ps, scales="free_x")+
  theme(strip.background = element_blank(), strip.text = element_blank(), legend.position="top")


# JT: this is the figure from the appendix ----
ggplot(subset(results, item < 25), aes(condition, Answer, fill=order))+
  stat_summary(fun.y=mean, geom="bar", position="dodge")+
  scale_x_discrete(name="")+scale_y_continuous(name="Mean Ratings")+
  scale_fill_manual(values=cbPalette.grayFirst, name="Embedding conjunct")+
  #  facet_grid(context~., scales="free_x")+
  facet_wrap(~context*trigger, nrow=4)+
  theme(legend.position="top")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# RESULTS (UNFILTERED): effect of internal_order, but also in filler_presuppositional about pp
ddply(results, .(trigger, condition, inference_about), summarize, N=length(Answer), Answer=mean(Answer))

#     trigger    condition    inference_about    N    Answer
# 1         -   Acceptable ExplicitIgnoranceP 1008 4.4464286
# 2         - Unacceptable           SupportP 1008 1.2410714
# 3     aware          TrL           SupportP   64 4.3125000
# 4     aware          TrL ExplicitIgnoranceP   66 4.0757576
# 5     aware   Acceptable           SupportP  252 5.4166667
# 6     aware            P           SupportP   64 4.4218750
# 7     aware            P ExplicitIgnoranceP   66 3.6060606
# 8     aware          TrF           SupportP   64 4.2187500
# 9     aware          TrF ExplicitIgnoranceP   66 3.4848485
# 10    aware Unacceptable ExplicitIgnoranceP  252 2.6230159
# 11 continue          TrL           SupportP   64 4.0781250
# 12 continue          TrL ExplicitIgnoranceP   66 3.2272727
# 13 continue   Acceptable           SupportP  252 4.9484127
# 14 continue            P           SupportP   64 4.9843750
# 15 continue            P ExplicitIgnoranceP   66 1.1212121
# 16 continue          TrF           SupportP   64 3.7343750
# 17 continue          TrF ExplicitIgnoranceP   66 1.1969697
# 18 continue Unacceptable ExplicitIgnoranceP  252 0.9404762
# 19    enjoy          TrL           SupportP   62 3.8225806
# 20    enjoy          TrL ExplicitIgnoranceP   60 3.7333333
# 21    enjoy            P           SupportP   62 3.5967742
# 22    enjoy            P ExplicitIgnoranceP   60 3.7833333
# 23    enjoy          TrF           SupportP   62 3.5806452
# 24    enjoy          TrF ExplicitIgnoranceP   60 3.7166667
# 25 frown on          TrL           SupportP   62 3.4677419
# 26 frown on          TrL ExplicitIgnoranceP   60 3.3500000
# 27 frown on            P           SupportP   62 4.2096774
# 28 frown on            P ExplicitIgnoranceP   60 3.3500000
# 29 frown on          TrF           SupportP   62 2.8870968
# 30 frown on          TrF ExplicitIgnoranceP   60 2.4833333
# 31    happy          TrL           SupportP   64 4.3906250
# 32    happy          TrL ExplicitIgnoranceP   66 4.3030303
# 33    happy   Acceptable           SupportP  252 5.5992063
# 34    happy            P           SupportP   64 4.7500000
# 35    happy            P ExplicitIgnoranceP   66 1.6515152
# 36    happy          TrF           SupportP   64 4.0625000
# 37    happy          TrF ExplicitIgnoranceP   66 1.9848485
# 38    happy Unacceptable ExplicitIgnoranceP  252 1.2777778
# 39   hoping          TrL           SupportP   62 3.0161290
# 40   hoping          TrL ExplicitIgnoranceP   60 3.4833333
# 41   hoping            P           SupportP   62 3.0322581
# 42   hoping            P ExplicitIgnoranceP   60 2.8833333
# 43   hoping          TrF           SupportP   62 3.7258065
# 44   hoping          TrF ExplicitIgnoranceP   60 3.2000000
# 45     stop          TrL           SupportP   64 3.2343750
# 46     stop          TrL ExplicitIgnoranceP   66 3.0303030
# 47     stop   Acceptable           SupportP  252 4.8253968
# 48     stop            P           SupportP   64 3.3906250
# 49     stop            P ExplicitIgnoranceP   66 1.3484848
# 50     stop          TrF           SupportP   64 3.0000000
# 51     stop          TrF ExplicitIgnoranceP   66 1.7272727
# 52     stop Unacceptable ExplicitIgnoranceP  252 1.3809524
# 53     sure          TrL           SupportP   62 4.1612903
# 54     sure          TrL ExplicitIgnoranceP   60 3.9500000
# 55     sure            P           SupportP   62 4.3387097
# 56     sure            P ExplicitIgnoranceP   60 4.5333333
# 57     sure          TrF           SupportP   62 4.5645161
# 58     sure          TrF ExplicitIgnoranceP   60 3.9500000


# code whether something was a ps or not
results$ps <- "no-ps"
results$ps[results$trigger=="aware"|results$trigger=="happy"|results$trigger=="stop"|results$trigger=="continue"] <- "ps"

results$item <- as.numeric(results$item)


ddply(results, .(item, ps, condition), summarize, N=length(Answer))

results$ps[results$item > 40] <- "ps"
results$ps <- as.factor(results$ps)
levels(results$ps)


ddply(results, .(ps, condition, inference_about), summarize, N=length(Answer), Answer=mean(Answer))

#       ps    condition    inference_about    N   Answer
# 1     ps          TrL           SupportP  256 4.003906
# 2     ps          TrL ExplicitIgnoranceP  264 3.659091
# 3     ps   Acceptable           SupportP 1008 5.197421
# 4     ps            P           SupportP  256 4.386719
# 5     ps            P ExplicitIgnoranceP  264 1.931818
# 6     ps          TrF           SupportP  256 3.753906
# 7     ps          TrF ExplicitIgnoranceP  264 2.098485
# 8     ps Unacceptable ExplicitIgnoranceP 1008 1.555556
# 9  no-ps          TrL           SupportP  248 3.616935
# 10 no-ps          TrL ExplicitIgnoranceP  240 3.629167
# 11 no-ps   Acceptable ExplicitIgnoranceP 1008 4.446429
# 12 no-ps            P           SupportP  248 3.794355
# 13 no-ps            P ExplicitIgnoranceP  240 3.637500
# 14 no-ps          TrF           SupportP  248 3.689516
# 15 no-ps          TrF ExplicitIgnoranceP  240 3.337500
# 16 no-ps Unacceptable           SupportP 1008 1.241071


# overall summary
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")

results$condition.labels <- as.character(results$condition)
results$condition.labels[results$condition=="TrF"&results$ps=="ps"] <- "Ps-1st"
results$condition.labels[results$condition=="TrL"&results$ps=="ps"] <- "Ps-2nd"
results$condition.labels[results$condition=="TrF"&results$ps=="no-ps"] <- "NoPs-1st"
results$condition.labels[results$condition=="TrL"&results$ps=="no-ps"] <- "NoPs-2nd"
results$condition.labels[results$condition=="P"&results$ps=="ps"] <- "Simple Ps"
results$condition.labels[results$condition=="P"&results$ps=="no-ps"] <- "Simple NoPs"

results$condition.labels <- factor(results$condition.labels, 
        levels=c("Acceptable", "Simple NoPs", "Simple Ps", "NoPs-1st", "NoPs-2nd", "Ps-1st", "Ps-2nd", "Unacceptable"))

levels(results$condition.labels)[c(1,8)] <- c("Accept.", "Unacc.")

ggplot(results, aes(condition.labels, Answer, fill=inference_about))+
	stat_summary(fun.y=mean, geom="bar", position="dodge")+
  scale_x_discrete(name="")+scale_y_continuous(name="Mean Ratings")+
  scale_fill_manual(values=cbPalette, name="Context", labels=c("Support","Explicit Ignorance"))+
  facet_wrap(~ps, scales="free_x")+
  theme(strip.background = element_blank(), strip.text = element_blank(), legend.position="top")


results$order <- "NA"
results$order[results$condition=="TrF"] <- "First"
results$order[results$condition=="TrL"] <- "Second"
results$order <- factor(results$order, levels=c("NA","First","Second"))


unique(results$condition[results$item > 24 & results$item < 41])

results$condition.ps <- as.character(results$condition)
results$condition.ps[results$ps=="ps"&grepl("Tr",results$condition)] <- "Conj. (Ps)"
results$condition.ps[results$ps=="no-ps"&grepl("Tr", results$condition)] <- "Conj. (no Ps)"
results$condition.ps[results$ps=="ps"&results$condition=="P"] <- "Simple (Ps)"
results$condition.ps[results$ps=="no-ps"&results$condition=="P"] <- "Simple (no Ps)"

results$condition.ps[results$item > 24 & results$item < 41] <- paste("Cond.No.Ps-",results$condition[results$item > 24 & results$item < 41], sep="")
results$condition.ps[results$item > 40] <- paste("Conj.Ps-",results$condition[results$item > 40], sep="")

results$condition.ps <- as.factor(results$condition.ps)
levels(results$condition.ps)[1] <- "Cond-No-Ps-Acc."
levels(results$condition.ps)[2] <- "Cond-No-Ps-Unacc."
levels(results$condition.ps)[5] <- "Conj-Ps-Acc."
levels(results$condition.ps)[6] <- "Conj-Ps-Unacc."

results$condition.ps <- factor(results$condition.ps, 
  levels=c("Cond-No-Ps-Acc.", "Cond-No-Ps-Unacc.", "Conj-Ps-Acc.", "Simple (Ps)",  "Conj. (Ps)", "Conj. (no Ps)", "Simple (no Ps)",  "Conj-Ps-Unacc."))

 unique(results$condition.ps)


results$context <- results$inference_about
results$context <- as.factor(results$context)
levels(results$context) <- c( "Context: Expl-Ign", "Context: Support")
cbPalette.grayFirst <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# General results
ggplot(subset(results, item < 25), aes(condition.ps, Answer, fill=order))+
  stat_summary(fun.y=mean, geom="bar", position="dodge")+
  stat_summary(fun.data = mean_se, geom = "errorbar", pos=position_dodge(width=0.9), aes(width=0.5))+
  scale_x_discrete(name="")+scale_y_continuous(name="Mean Ratings",limits=c(0,6),labels=c(1,3,5,7))+
  scale_fill_manual(values=cbPalette.grayFirst, name="Embedding conjunct", breaks=c("First","Second"))+
#  facet_grid(context~., scales="free_x")+
  facet_wrap(~context)+
  theme(legend.position="top")

# items where adjective+noun vs others
items.adjN <- c(8,13,14,15,19,23)
ggplot(subset(results, item %in%items.adjN), aes(condition.ps, Answer, fill=order))+
  stat_summary(fun.y=mean, geom="bar", position="dodge")+
  stat_summary(fun.data = mean_se, geom = "errorbar", pos=position_dodge(width=0.9), aes(width=0.5))+
  scale_x_discrete(name="")+scale_y_continuous(name="Mean Ratings")+
  scale_fill_manual(values=cbPalette.grayFirst, name="Embedding conjunct", breaks=c("First","Second"))+
  #  facet_grid(context~., scales="free_x")+
  facet_wrap(~context)+
  theme(legend.position="top")
ggplot(subset(results, item < 25 & !(item%in%items.adjN)), aes(condition.ps, Answer, fill=order))+
  stat_summary(fun.y=mean, geom="bar", position="dodge")+
  stat_summary(fun.data = mean_se, geom = "errorbar", pos=position_dodge(width=0.9), aes(width=0.5))+
  scale_x_discrete(name="")+scale_y_continuous(name="Mean Ratings")+
  scale_fill_manual(values=cbPalette.grayFirst, name="Embedding conjunct", breaks=c("First","Second"))+
  #  facet_grid(context~., scales="free_x")+
  facet_wrap(~context)+
  theme(legend.position="top")
# per item
ggplot(subset(results, item %in%items.adjN), aes(condition.ps, Answer, fill=order))+
  stat_summary(fun.y=mean, geom="bar", position="dodge")+
  stat_summary(fun.data = mean_se, geom = "errorbar", pos=position_dodge(width=0.9), aes(width=0.5))+
  scale_x_discrete(name="")+scale_y_continuous(name="Mean Ratings")+
  scale_fill_manual(values=cbPalette.grayFirst, name="Embedding conjunct", breaks=c("First","Second"))+
  #  facet_grid(context~., scales="free_x")+
  facet_wrap(item~context)+
  theme(legend.position="top")
ggplot(subset(results, item < 25 & !(item%in%items.adjN)), aes(condition.ps, Answer, fill=order))+
  stat_summary(fun.y=mean, geom="bar", position="dodge")+
  stat_summary(fun.data = mean_se, geom = "errorbar", pos=position_dodge(width=0.9), aes(width=0.5))+
  scale_x_discrete(name="")+scale_y_continuous(name="Mean Ratings")+
  scale_fill_manual(values=cbPalette.grayFirst, name="Embedding conjunct", breaks=c("First","Second"))+
  #  facet_grid(context~., scales="free_x")+
  facet_wrap(item~context)+
  theme(legend.position="top")



levels(results$condition)
results$condition <- factor(results$condition,
        levels=c("P", "TrF", "TrL", "Acceptable", "Unacceptable"))

results$trigger <- factor(results$trigger,
        levels=c("aware", "happy", "continue", "stop", "sure", "hoping", "enjoy", "frown on"))

       
# JT: this is the figure from the appendix ----
ggplot(subset(results, item < 25), aes(condition, Answer, fill=order))+
  stat_summary(fun.y=mean, geom="bar", position="dodge")+
  scale_x_discrete(name="")+scale_y_continuous(name="Mean Ratings")+
  scale_fill_manual(values=cbPalette.grayFirst, name="Embedding conjunct")+
#  facet_grid(context~., scales="free_x")+
  facet_wrap(~context*trigger, nrow=4)+
  theme(legend.position="top")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())




# performance of different critical context groups on ps controls?

results$groupCritContext <- "Crit-Support"
results$groupCritContext[results$group > 3 & results$group < 7] <- "Crit-Expl-Ign"
results$groupCritContext[results$group > 9] <- "Crit-Expl-Ign"

results$groupPs <- "Ps"
results$groupPs[results$group > 6] <- "No-Ps"



ggplot(subset(results, item >40), aes(context, Answer, fill=groupCritContext))+
  stat_summary(fun.y=mean, geom="bar", position="dodge")+
  scale_x_discrete(name="")+scale_y_continuous(name="Mean Ratings")+
  scale_fill_manual(values=cbPalette.grayFirst[c(3,4)], name="Contexts for Critical Item")


ggplot(subset(results, item > 24), aes(condition.ps, Answer, fill=groupPs))+
  stat_summary(fun.y=mean, geom="bar", position="dodge")+
  scale_x_discrete(name="")+scale_y_continuous(name="Mean Ratings")+
  scale_fill_manual(values=cbPalette.grayFirst[c(7,4)], name="Type of Critical Item Group")+
#  facet_grid(context~., scales="free_x")+
  facet_wrap(~groupCritContext)+
  theme(legend.position="top")



# check on fillers overall



# rough check of subject 'accuracy' for controls

ggplot(subset(results, condition=="Acceptable"|condition=="Unacceptable"), aes(condition.ps, Answer, fill=inference_about))+
	stat_summary(fun.y=mean, geom="bar", position="dodge")+
	facet_wrap(~subject)



summary(lmer(Answer~condition*ps*inference_about + (1+condition|subject)+(1+ps*inference_about|item), data=subset(results, condition!="Acceptable"&condition!="Unacceptable")))


# Random effects:
 # Groups   Name                         Variance Std.Dev. Corr             
 # subject  (Intercept)                  0.46992  0.6855                    
          # conditionTrF                 0.04837  0.2199    0.93            
          # conditionTrL                 0.46016  0.6783   -0.12  0.26      
 # item     (Intercept)                  0.40000  0.6325                    
          # psps                         0.51606  0.7184   -0.35            
          # inference_aboutSupportP      0.01375  0.1173    0.09  0.00      
          # psps:inference_aboutSupportP 0.61512  0.7843    0.04 -0.63 -0.77
 # Residual                              2.93865  1.7142                    
# Number of obs: 3024, groups:  subject, 126; item, 24

# Fixed effects:
                                           # Estimate Std. Error t value
# (Intercept)                                3.637500   0.211128  17.229
# conditionTrF                              -0.300000   0.161558  -1.857
# conditionTrL                              -0.008333   0.199568  -0.042
# psps                                      -1.692922   0.273501  -6.190
# inference_aboutSupportP                    0.159100   0.235572   0.675
# conditionTrF:psps                          0.445843   0.223364   1.996
# conditionTrL:psps                          1.718150   0.275857   6.228
# conditionTrF:inference_aboutSupportP       0.185364   0.226660   0.818
# conditionTrL:inference_aboutSupportP      -0.166025   0.279973  -0.593
# psps:inference_aboutSupportP               2.293248   0.363472   6.309
# conditionTrF:psps:inference_aboutSupportP -0.982140   0.315671  -3.111
# conditionTrL:psps:inference_aboutSupportP -1.939108   0.389879  -4.974

results$ps <- as.factor(results$ps)
results$ps <- relevel(results$ps, "ps")


Random effects:
 Groups   Name                            Variance Std.Dev. Corr             
 subject  (Intercept)                     0.46992  0.6855                    
          conditionTrF                    0.04837  0.2199    0.93            
          conditionTrL                    0.46016  0.6783   -0.12  0.26      
 item     (Intercept)                     0.59786  0.7732                    
          psno-ps                         0.51606  0.7184   -0.64            
          inference_aboutSupportP         0.48701  0.6979   -0.61  0.70      
          psno-ps:inference_aboutSupportP 0.61512  0.7843    0.55 -0.63 -0.99
 Residual                                 2.93865  1.7142                    
Number of obs: 3024, groups:  subject, 126; item, 24

Fixed effects:
                                             Estimate Std. Error t value
(Intercept)                                    1.9446     0.2243   8.670
conditionTrF                                   0.1458     0.1542   0.946
conditionTrL                                   1.7098     0.1904   8.978
psno-ps                                        1.6929     0.2735   6.190
inference_aboutSupportP                        2.4523     0.2681   9.149
conditionTrF:psno-ps                          -0.4458     0.2234  -1.996
conditionTrL:psno-ps                          -1.7181     0.2759  -6.228
conditionTrF:inference_aboutSupportP          -0.7968     0.2197  -3.626
conditionTrL:inference_aboutSupportP          -2.1051     0.2713  -7.759
psno-ps:inference_aboutSupportP               -2.2932     0.3635  -6.309
conditionTrF:psno-ps:inference_aboutSupportP   0.9821     0.3157   3.111
conditionTrL:psno-ps:inference_aboutSupportP   1.9391     0.3899   4.974


results$condition <- relevel(as.factor(results$condition), "TrL")

Random effects:
 Groups   Name                            Variance Std.Dev. Corr             
 subject  (Intercept)                     0.8193   0.9051                    
          conditionP                      0.4602   0.6783   -0.66            
          conditionTrF                    0.4306   0.6562   -0.38  0.95      
 item     (Intercept)                     0.5979   0.7732                    
          psno-ps                         0.5161   0.7184   -0.64            
          inference_aboutSupportP         0.4870   0.6979   -0.61  0.70      
          psno-ps:inference_aboutSupportP 0.6151   0.7843    0.55 -0.63 -0.99
 Residual                                 2.9386   1.7142                    
Number of obs: 3024, groups:  subject, 126; item, 24

Fixed effects:
                                             Estimate Std. Error t value
(Intercept)                                   3.65439    0.24676  14.810
conditionP                                   -1.70982    0.19045  -8.978
conditionTrF                                 -1.56397    0.18808  -8.316
psno-ps                                      -0.02523    0.31151  -0.081
inference_aboutSupportP                       0.34722    0.30554   1.136
conditionP:psno-ps                            1.71815    0.27586   6.228
conditionTrF:psno-ps                          1.27231    0.27242   4.670
conditionP:inference_aboutSupportP            2.10513    0.27133   7.759
conditionTrF:inference_aboutSupportP          1.30836    0.26796   4.883
psno-ps:inference_aboutSupportP              -0.35414    0.42016  -0.843
conditionP:psno-ps:inference_aboutSupportP   -1.93911    0.38988  -4.974
conditionTrF:psno-ps:inference_aboutSupportP -0.95697    0.38503  -2.485


results$inference_about <- relevel(as.factor(results$inference_about), "SupportP")

Random effects:
 Groups   Name                                      Variance Std.Dev. Corr             
 subject  (Intercept)                               0.8193   0.9051                    
          conditionP                                0.4602   0.6783   -0.66            
          conditionTrF                              0.4306   0.6562   -0.38  0.95      
 item     (Intercept)                               0.4271   0.6535                    
          psno-ps                                   0.4237   0.6509   -0.50            
          inference_aboutExplicitIgnoranceP         0.4870   0.6979   -0.35  0.42      
          psno-ps:inference_aboutExplicitIgnoranceP 0.6151   0.7843    0.41 -0.51 -0.99
 Residual                                           2.9386   1.7142                    
Number of obs: 3024, groups:  subject, 126; item, 24

Fixed effects:
                                                       Estimate Std. Error t value
(Intercept)                                              4.0016     0.2343  17.081
conditionP                                               0.3953     0.1933   2.045
conditionTrF                                            -0.2556     0.1909  -1.339
psno-ps                                                 -0.3794     0.3050  -1.244
inference_aboutExplicitIgnoranceP                       -0.3472     0.3055  -1.136
conditionP:psno-ps                                      -0.2210     0.2755  -0.802
conditionTrF:psno-ps                                     0.3153     0.2721   1.159
conditionP:inference_aboutExplicitIgnoranceP            -2.1051     0.2713  -7.759
conditionTrF:inference_aboutExplicitIgnoranceP          -1.3084     0.2680  -4.883
psno-ps:inference_aboutExplicitIgnoranceP                0.3541     0.4202   0.843
conditionP:psno-ps:inference_aboutExplicitIgnoranceP     1.9391     0.3899   4.974
conditionTrF:psno-ps:inference_aboutExplicitIgnoranceP   0.9570     0.3850   2.485



results$ps <- relevel(results$ps, "ps")
results$condition <- relevel(as.factor(results$condition), "TrF")


summary(lmer(Answer~condition*ps*inference_about + (1+condition|subject)+(1+ps*inference_about|item), data=subset(results, condition!="Acceptable"&condition!="Unacceptable")))

# Random effects:
 # Groups   Name                            Variance Std.Dev. Corr             
 # subject  (Intercept)                     0.79788  0.8932                    
          # conditionTrL                    0.43059  0.6562   -0.35            
          # conditionP                      0.04837  0.2199   -0.96  0.07      
 # item     (Intercept)                     0.59786  0.7732                    
          # psno-ps                         0.51606  0.7184   -0.64            
          # inference_aboutSupportP         0.48701  0.6979   -0.61  0.70      
          # psno-ps:inference_aboutSupportP 0.61512  0.7843    0.55 -0.63 -0.99
 # Residual                                 2.93865  1.7142                    
# Number of obs: 3024, groups:  subject, 126; item, 24

# Fixed effects:
                                             # Estimate Std. Error t value
# (Intercept)                                    2.0904     0.2454   8.517
# conditionTrL                                   1.5640     0.1881   8.316
# conditionP                                    -0.1458     0.1542  -0.946
# psno-ps                                        1.2471     0.3093   4.032
# inference_aboutSupportP                        1.6556     0.3034   5.457
# conditionTrL:psno-ps                          -1.2723     0.2724  -4.670
# conditionP:psno-ps                             0.4458     0.2234   1.996
# conditionTrL:inference_aboutSupportP          -1.3084     0.2680  -4.883
# conditionP:inference_aboutSupportP             0.7968     0.2197   3.626
# psno-ps:inference_aboutSupportP               -1.3111     0.4169  -3.145
# conditionTrL:psno-ps:inference_aboutSupportP   0.9570     0.3850   2.485
# conditionP:psno-ps:inference_aboutSupportP    -0.9821     0.3157  -3.111



results$ps <- relevel(results$ps, "no-ps")
results$condition <- relevel(as.factor(results$condition), "TrF")




#### USE THE ONE BELOW FOR APPENDIX; PS, TrF, Expl-Ign as baseline, giving us t-values for all the simple effects we need, plus 3- and 2-way interactions

results$condition <- relevel(results$condition, "TrF")
summary(lmer(Answer~condition*ps*inference_about + (1|subject)+(1+ps*inference_about|item), data=subset(results, condition!="Acceptable"&condition!="Unacceptable")))

# Random effects:
 # Groups   Name                            Variance Std.Dev. Corr             
 # subject  (Intercept)                     0.5866   0.7659                    
 # item     (Intercept)                     0.6001   0.7747                    
          # psno-ps                         0.5202   0.7213   -0.63            
          # inference_aboutSupportP         0.4909   0.7007   -0.60  0.72      
          # psno-ps:inference_aboutSupportP 0.6452   0.8033    0.53 -0.64 -0.99
 # Residual                                 3.0441   1.7447                    
# Number of obs: 3024, groups:  subject, 126; item, 24

# Fixed effects:
                                             # Estimate Std. Error t value
# (Intercept)                                    2.0881     0.2331   8.958
# conditionTrL                                   1.5681     0.1520  10.314 * asymmetry
# conditionP                                    -0.1429     0.1520  -0.940   no R2L
# psno-ps                                        1.2494     0.2885   4.331 * ps-specific
# inference_aboutSupportP                        1.6571     0.2829   5.858 * effect of context
# conditionTrL:psno-ps                          -1.2764     0.2202  -5.797 * key 2x2 @ Expl-Ign [blue/yellow in left panel]
# conditionP:psno-ps                             0.4429     0.2202   2.011 * 2x2 with control - conjunctions show smaller effects?
# conditionTrL:inference_aboutSupportP          -1.3116     0.2166  -6.056 * key 2x2 @ ps [blue/yellow on left in both panels]
# conditionP:inference_aboutSupportP             0.7956     0.2166   3.673 * 2x2 with control, due to Simple-Ps-Support boost
# psno-ps:inference_aboutSupportP               -1.3126     0.3871  -3.391 * key 2x2 @ TrF [yellow bars]
# conditionTrL:psno-ps:inference_aboutSupportP   0.9612     0.3112   3.089 * key 3-way interaction amongs conjunctions
# conditionP:psno-ps:inference_aboutSupportP    -0.9819     0.3112  -3.155 * 3-way with control: different effects of context by ps for TrF vs P



# 2x2 centered follow-ups to test for significance by model comparison

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


# @ Expl-Ign - TrLF

resultsEI_TrLF <- subset(results, context=="Context: Expl-Ign" & item < 25 & condition !="P")
summary(resultsEI_TrLF)
nrow(resultsEI_TrLF)

resultsEI_TrLF$condC <- myCenter(resultsEI_TrLF$condition)
resultsEI_TrLF$psC <- myCenter(resultsEI_TrLF$ps)

EI_TrLF_lmer <- lmer(Answer~condC*psC + (1+condC|subject)+(1+psC*condC|item), REML="ML", data=resultsEI_TrLF)
summary(EI_TrLF_lmer)
# Random effects:
 # Groups   Name        Variance Std.Dev. Corr             
 # subject  (Intercept) 0.7242   0.8510                    
          # condC       0.8574   0.9259   0.07             
 # item     (Intercept) 0.3280   0.5727                    
          # psC         0.3415   0.5843   -0.18            
          # condC       0.1791   0.4232   -0.26  0.02      
          # psC:condC   0.6308   0.7942   -0.13 -0.51 -0.45
 # Residual             2.7918   1.6709                    
# Number of obs: 1008, groups:  subject, 63; item, 24

# Fixed effects:
            # Estimate Std. Error t value
# (Intercept)   3.1637     0.1672  18.927
# condC         0.9576     0.1794   5.338
# psC           0.6102     0.2673   2.283
# condC:psC    -1.2714     0.3541  -3.590

EI_TrLF_lmer2 <- lmer(Answer~condC+psC + (1+condC|subject)+(1+psC*condC|item), data=resultsEI_TrLF)

anova(EI_TrLF_lmer,EI_TrLF_lmer2)
# 11.546      1  0.0006789 ***



# @ Ps - TrLF

resultsPs_TrLF <- subset(results, ps=="ps" & item < 25 & condition !="P")
summary(resultsPs_TrLF)
nrow(resultsPs_TrLF)

resultsPs_TrLF$contextC <- myCenter(resultsPs_TrLF$context)
resultsPs_TrLF$condC <- myCenter(resultsPs_TrLF$condition)

Ps_TrLF_lmer <- lmer(Answer~condC*contextC + (1+condC|subject)+(1+contextC*condC|item),  data=resultsPs_TrLF)
summary(Ps_TrLF_lmer)
# Random effects:
 # Groups   Name           Variance Std.Dev. Corr             
 # subject  (Intercept)    0.6878   0.8293                    
          # condC          0.6900   0.8307   0.15             
 # item     (Intercept)    0.4117   0.6416                    
          # contextC       0.1964   0.4432   -0.07            
          # condC          0.2043   0.4520   -0.15  0.41      
          # contextC:condC 0.3528   0.5940   -0.22 -0.68 -0.86
 # Residual                2.5027   1.5820                    
# Number of obs: 1040, groups:  subject, 65; item, 24

# Fixed effects:
               # Estimate Std. Error t value
# (Intercept)      3.3656     0.1736  19.382
# condC            0.9219     0.1697   5.432
# contextC         1.0028     0.2453   4.088
# condC:contextC  -1.3036     0.3095  -4.212

Ps_TrLF_lmer2 <- lmer(Answer~condC+contextC + (1+condC|subject)+(1+contextC*condC|item), data=resultsPs_TrLF)

anova(Ps_TrLF_lmer,Ps_TrLF_lmer2)
# 15.533      1  8.109e-05 ***




# @ TrF - ps/no-ps * EI/Support

resultsTrF <- subset(results,  item < 25 & condition =="TrF")
summary(resultsTrF)
nrow(resultsTrF)

resultsTrF$contextC <- myCenter(resultsTrF$context)
resultsTrF$psC <- myCenter(resultsTrF$ps)

TrF_lmer <- lmer(Answer~contextC*psC + (1+contextC|subject)+(1+psC*contextC|item), data=resultsTrF)
# large eigenvalue 

TrF_lmer <- lmer(Answer~contextC*psC + (1|subject)+(1+psC*contextC|item), data=resultsTrF)
summary(TrF_lmer)
# Random effects:
 # Groups   Name         Variance Std.Dev. Corr             
 # subject  (Intercept)  0.7614   0.8726                    
 # item     (Intercept)  0.4871   0.6979                    
          # psC          0.4217   0.6494    0.21            
          # contextC     0.1136   0.3370    0.22 -0.08      
          # psC:contextC 0.6129   0.7829    0.28 -0.44 -0.66
 # Residual              2.7268   1.6513                    
# Number of obs: 1008, groups:  subject, 126; item, 24

# Fixed effects:
             # Estimate Std. Error t value
# (Intercept)    3.2033     0.1705  18.792
# contextC       1.0145     0.1994   5.088
# psC            0.5858     0.2294   2.553
# contextC:psC  -1.3151     0.4071  -3.230

TrF_lmer2 <- lmer(Answer~contextC+psC + (1|subject)+(1+psC*contextC|item), data=resultsTrF)

anova(TrF_lmer,TrF_lmer2)
# 10.036      1   0.001535 **

