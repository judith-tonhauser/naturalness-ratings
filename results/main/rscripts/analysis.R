# 13_explicitIgnorance
# analysis.R
# analysis 1: section 2.2.2 
# analysis 2: section 2.2.1 
# this script also creates the visual outputs of the analyses reported in the paper

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)
this.dir

# load required packages
library(tidyverse)
library(tidybayes)
library(dplyr)
library(dichromat)
library(forcats)
library(ggrepel)
library(brms)
library(knitr)
library(emmeans)
library(lme4)
library(lmerTest)
library(padr)
library(performance)
library(MuMIn)
library(xtable)

# load clean data
d = read_tsv("../data/cd.tsv")

# analysis 1: pairwise comparison of ratings in explicit ignorance context ----

# target data and set reference levels
t = d %>% 
  filter(context == "explicitIgnorance") %>%
  filter(expression != "practice" & expression != "controlGood1" & expression != "controlGood2" & expression != "controlGood3" & expression != "controlGood4")
  
# set reference level
t = t %>%
  mutate(expression = fct_relevel(expression, "continue"))
levels(t$expression)

# response distribution before transformation
summary(t$response)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.1000  0.6000  0.5065  0.8600  1.0000

# first, because response assumes values of 0 and 1, which beta regression cannot handle, transform: (Smithson & Verkuilen 2006)
# y_new = (y_old * (n−1) + 0.5) / n (where n is the sample size)
# note: first rescaling of y'=(y-a)/(b-a) not necessary because highest and lowest value are 0 and 1 already
t$betaresponse = (t$response*(nrow(t)-1) + .5)/nrow(t)
summary(t$betaresponse)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.0000751 0.1000601 0.5999850 0.5064720 0.8599459 0.9999249 

# fit the model
prior = get_prior(betaresponse ~ expression + (1|participantID) + (1|cc),family = Beta(),data=t)
prior

betamodel = bf(betaresponse ~ expression + (1|participantID) + (1|cc),
               phi ~ expression + (1|participantID) + (1|cc), # beta distribution's precision 
               family = Beta())

m.b = brm(formula = betamodel,
          family=Beta(),
          data=t, 
          cores = 4, iter = 3000, warmup = 500,
          control = list(adapt_delta = .95,max_treedepth=15))

# model summary
summary(m.b)

# save the model
saveRDS(m.b,file="../models/analysis1/beta-model-mixed1.rds")

# read the model
m.b <- readRDS(file="../models/analysis1/beta-model-mixed1.rds")
m.b

# run posterior predictive checks
p1 <- pp_check(m.b, type = "dens_overlay_grouped", group = "expression", ndraws = 100) +
  scale_x_continuous(breaks = seq(0,1,by=.25)) 
p1

# load the model
m.b = readRDS(file="../models/analysis1/beta-model-mixed1.rds")

# draws of posterior distributions of estimated marginal means of pairwise differences
pairwise <- m.b %>%
  emmeans(~ expression) %>%
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  mean_hdi() %>%
  # create new column of "first" and "second" element in contrast
  mutate(first = gsub(" -.*", "", contrast)) %>%
  mutate(second = gsub(".* -", "", contrast)) %>%
  # sort by mean value
  mutate(contrast = fct_reorder(as.factor(contrast),.value))
pairwise

# save the pairwise comparison
write_csv(pairwise,file="../models/analysis1/pairwise1.csv")

# load the pairwise comparison
pairwise = read_csv(file="../models/analysis1/pairwise1.csv")
pairwise

# which differences are observed?
summary(pairwise$.value)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-2.6943 -1.0429 -0.2822 -0.2895  0.3865  2.7232 

# select relevant columns for printing
pairwise_reduced = pairwise %>%
  select(c(contrast, .value, .lower, .upper))
pairwise_reduced

#### full model output for online supplement ----

tableApp1 = print(xtable(pairwise_reduced),
               #only.contents = T,
               include.rownames=FALSE,
               include.colnames=TRUE,
               tabular.environment="longtable",
               floating=FALSE,
               hline.after = NULL,
               latex.environments=NULL,
               booktabs=TRUE,
               sanitize.text.function = function(x){x},
               comment = F
)

# write the table, print in latex document in supplement
write(tableApp1, "../models/analysis1/fullModelOutput/analysis1.tex")

#### create latex input for Table 1 in paper ----

# select needed columns from the pairwise comparison for the table input
tableInput = pairwise %>%
  select(c(contrast, .value, .lower, .upper, first, second)) %>%
  select(-c(contrast))
tableInput$second = trimws(tableInput$second)
tableInput

# create separate dataframes for each expression
predicates = unique(as.character(t$expression))
predicates
predicates <- replace(predicates, 9, "be.annoyed") 
predicates <- replace(predicates, 14, "be.right") 
predicates

# make tableInput a dataframe
tableInput <- as.data.frame(tableInput)
tableInput = tableInput %>%
  mutate(first = recode(first,"be annoyed" = "be.annoyed","be right" = "be.right")) %>%
  mutate(second = recode(second,"be annoyed" = "be.annoyed","be right" = "be.right"))
tableInput

# create a separate dataframe for each predicate
for (p in predicates) {
  assign(paste("data.", p, sep=""), subset(tableInput, tableInput$first == p | tableInput$second == p))
  assign(paste("data.", p, sep=""), get(paste("data.", p, sep="")) %>% mutate(expression = c(p)))
  write(paste("data.",p,sep=""),file=paste("../models/analysis1/data.",p,sep=""))
}

# change dataframes such that value, lower and upper is consistent by expression in first position

# create a tableData dataframe
tableData = data.frame(expression = character(), comparisonExpression = character(), value = numeric(), lower = numeric(), upper = numeric())
tableData

# fill tableData with the relevant information from the individual predicates' dataframes
for (p in predicates) {
  for (i in 1:nrow(get(paste("data.",p,sep="")))) {
    print(p)
    # define some expressions
    valueOld = get(paste("data.",p,sep=""))[i,]$.value
    lowerOld = get(paste("data.",p,sep=""))[i,]$.lower
    upperOld = get(paste("data.",p,sep=""))[i,]$.upper
    first = get(paste("data.",p,sep=""))[i,]$first
    second = get(paste("data.",p,sep=""))[i,]$second
    expression = get(paste("data.",p,sep=""))[i,]$expression
    # now fill the dataframe
    comparisonExpression = ifelse(expression == first, second, first)
    value = ifelse(expression == first, valueOld, -valueOld)
    lower = ifelse(expression == first, lowerOld, -upperOld)
    upper = ifelse(expression == first, upperOld, -lowerOld)
    tableData = tableData %>%
      add_row(expression = p, comparisonExpression = comparisonExpression, value = value, lower = lower, upper = upper)
  }
}

tableData

# sort dataframe by expression mean naturalness rating in explicit ignorance context

# get the mean naturalness for each predicate (tmp dataframe)
tmp = t %>%
  filter(context == "explicitIgnorance") %>%
  group_by(expression) %>%
  summarize(Mean = mean(response)) %>%
  mutate(expression = fct_reorder(as.factor(expression),Mean))
tmp
tmp = tmp %>%
  mutate(expression = recode(expression,"be annoyed" = "be.annoyed","be right" = "be.right"))
tmp
levels(tmp$expression)
tableData$expression = factor(tableData$expression, levels=tmp$expression[order(tmp$expression)], ordered=TRUE)
tableData
levels(tableData$expression)
tableData$expression = factor(tableData$expression, ordered = FALSE )
str(tableData$expression)
str(tmp$expression)

# join the tmp dataframe with tableData
tableData = left_join(tableData, tmp)
tableData

tableData$comparisonExpression = factor(tableData$comparisonExpression, levels=tmp$expression[order(tmp$expression)], ordered=TRUE)

# sort by mean (first column) and comparisonExpression (second column)
tableData <- tableData %>% arrange(Mean, comparisonExpression)
tableData

# colorcode the cells

tableData$cellColor = ifelse(tableData$lower <= 0 & tableData$upper >= 0, "\\cellcolor{white}",
                       ifelse(tableData$lower < 0 & tableData$upper < 0 & tableData$value <= -1.5, "\\cellcolor{yellow1}",
                              ifelse(tableData$lower < 0 & tableData$upper < 0 & -1.5 < tableData$value & tableData$value <= -0.5, "\\cellcolor{yellow2}",
                                     ifelse(tableData$lower < 0 & tableData$upper < 0 & -.5 < tableData$value & tableData$value <= 0, "\\cellcolor{yellow3}",
                                            ifelse(tableData$lower > 0 & tableData$upper > 0 & tableData$value >= 1.5, "\\cellcolor{purple1}",
                                                   ifelse(tableData$lower > 0 & tableData$upper > 0 & 1.5 > tableData$value & tableData$value > 0.5, "\\cellcolor{purple2}",
                                                          ifelse(tableData$lower > 0 & tableData$upper > 0 & .5 > tableData$value & tableData$value >= 0, "\\cellcolor{purple3}", "error")))))))
#view(tableData)

# select relevant columsn to make the latex table
tableData = tableData %>%
  select(c(expression,comparisonExpression,cellColor))

# spread the data wide
tableData = tableData %>%
  spread(comparisonExpression,cellColor)

# replace NA with gray cells and expressions with color coded versions
tableData = tableData %>% mutate(across(everything(), ~replace_na(.x, "\\cellcolor{black}")))
tableData = tableData %>%
  mutate(expression = recode(expression,
                             "continue" = "{\\bf continue}",
                             "too" = "{\\bf too}",
                             "also" = "{\\bf also}",
                             "again" = "{\\bf again}",
                             "stop" = "{\\bf stop}",
                             "cleft" = "{\\bf cleft}",
                             "be.annoyed" = "\\color{orange}{\\bf be annoyed}\\color{black}",
                             "know" = "\\color{orange}{\\bf know}\\color{black}",
                             "demonstrate" = "\\color{green}{\\bf demonstrate}\\color{black}",
                             "pretend" = "\\color{green}{\\bf pretend}\\color{black}",
                             "inform" = "\\color{green}{\\bf inform}\\color{black}",
                             "confess" = "\\color{green}{\\bf confess}\\color{black}",
                             "see" = "\\color{orange}{\\bf see}\\color{black}",
                             "acknowledge" = "\\color{green}{\\bf acknowledge}\\color{black}",
                             "discover" = "\\color{orange}{\\bf discover}\\color{black}",
                             "admit" = "\\color{green}{\\bf admit}\\color{black}",
                             "hear" = "\\color{green}{\\bf hear}\\color{black}",
                             "prove" = "\\color{green}{\\bf prove}\\color{black}",
                             "reveal" = "\\color{orange}{\\bf reveal}\\color{black}",
                             "announce" = "\\color{green}{\\bf announce}\\color{black}",
                             "be.right" = "\\color{green}{\\bf be right}\\color{black}",
                             "establish" = "\\color{green}{\\bf establish}\\color{black}",
                             "confirm" = "\\color{green}{\\bf confirm}\\color{black}",
                             "suggest" = "\\color{green}{\\bf suggest}\\color{black}",
                             "think" = "\\color{green}{\\bf think}\\color{black}",
                             "say" = "\\color{green}{\\bf say}\\color{black}"                             
                             ))

#view(tableData)

# now create the table to include in the paper
table1 = print(xtable(tableData),
               only.contents = T,
               include.rownames=FALSE,
               include.colnames=FALSE,
               floating=FALSE,
               hline.after = NULL,
               latex.environments=NULL,
               booktabs=TRUE,
               sanitize.text.function = function(x){x},
               comment = F
)

write(table1, "../models/analysis1/table1.tex")


# analysis 2: effect of context by expression ----

# load clean data
d = read_tsv("../data/cd.tsv")
names(d)

# target data: 20 predicates, all three contexts
t = d %>% 
  filter(expression != "practice" & expression != "controlGood1" & expression != "controlGood2" & expression != "controlGood3" & expression != "controlGood4") %>%
  filter(cc != "noCC" )

# set reference levels
t = t %>%
  mutate(context = factor(context, levels = c("explicitIgnorance", "factL", "factH")))

table(t$expression)
table(t$context)

# first, because response assumes values of 0 and 1, which beta regression cannot handle, transform: (Smithson & Verkuilen 2006)
# y'' = (y' ?? (n ??? 1) + 0.5)/n
# note: first rescaling of y'=(y-a)/(b-a) not necessary because highest and lowest value are 0 and 1 already
t$betaresponse = (t$response*(nrow(t)-1) + .5)/nrow(t)

# wrapper that fits a beta model for each predicate

predicates = unique(t$expression) 
predicates
predicates <- replace(predicates, 12, "be.annoyed") 
predicates <- replace(predicates, 15, "be.right") 
predicates

t = t %>%
  mutate(expression = recode(expression,"be annoyed" = "be.annoyed","be right" = "be.right"))
  
theme_set(theme_bw()) 

# fit beta models
# no by-participant intercept because each participant saw each predicate only once
# no by-context slope for the CC random effect in the precision because of conversion issues
for (p in predicates) {
  print(p)
  betamodel = bf(betaresponse ~ context + (1+context|cc),
                 phi ~ context + (1|cc), # beta distribution's precision 
                 family = Beta())
  saveRDS(assign(paste("beta-model-mixed.", p, ".rds", sep=""), brm(formula = betamodel,
                                                                    family=Beta(),
                                                                    data=t[t$expression == p,], 
                                                                    cores = 4, iter = 4000, warmup = 700,
                                                                    control = list(adapt_delta = .95,max_treedepth=15))),
          file=paste("../models/analysis2/beta-model-mixed-",p,".rds",sep=""))
}

# run posterior predictive checks
for (p in predicates) {
  model=readRDS(paste("../models/analysis2/beta-model-mixed-",p,".rds",sep=""))
  p1=pp_check(model, type = "dens_overlay_grouped", group = "context", ndraws = 100) +
  scale_x_continuous(breaks = seq(0,1,by=.25)) 
  ggsave(paste("../models/analysis2/posterior-checks/posterior-check-",p,".pdf",sep=""),height=5,width=8)
}


# pairwise comparisons between contexts for each predicate
for (p in predicates) {
    readRDS(paste("../models/analysis2/beta-model-mixed-",p,".rds",sep="")) %>%
    emmeans(~ context) %>%
    contrast(method = "pairwise") %>%
    gather_emmeans_draws() %>%
    mean_hdi() %>%
    write_csv(file=paste("../models/analysis2/pairwise.",p,".csv",sep=""))
}

# plot the pairwise comparisons for each predicate
for (p in predicates) {
  print(paste("pairwise.", p, sep=""))
  d = read_csv(paste("../models/analysis2/pairwise.",p,".csv",sep="")) 
  ggplot(d, aes(x = .value, y = contrast)) +
    geom_point() +
    geom_linerange(aes(xmin = .lower, xmax = .upper)) +
    geom_vline(xintercept = 0, color = "red") +
    xlim(-2.5,2.5) +
    labs(x = p, y = NULL)
  ggsave(paste("../models/analysis2/plots-of-pairwise-comparisons/",p,".pdf",sep=""),height=5,width=8)
}

# pairwise.know[pairwise.know$contrast == "explicitIgnorance - factH",]$.value
# pairwise.see[pairwise.see$contrast == "explicitIgnorance - factH",]$.value
# 
# pairwise.know[pairwise.know$contrast == "explicitIgnorance - factH",]$.upper - pairwise.know[pairwise.know$contrast == "explicitIgnorance - factH",]$.lower
# pairwise.see[pairwise.see$contrast == "explicitIgnorance - factH",]$.upper - pairwise.see[pairwise.see$contrast == "explicitIgnorance - factH",]$.lower


# extract pairwise differences for inclusion in Fig X
values = data.frame(predicate = character(), contrast = character(), mean = numeric(), lower = numeric(), upper = numeric())
values

for (p in predicates) {
  data = read_csv(paste("../models/analysis2/pairwise.",p,".csv",sep=""))
  for (i in 1:nrow(data)) {
    cntrst = data$contrast[i]
    value = data[data$contrast == cntrst,]$.value
    lower = data[data$contrast == cntrst,]$.lower
    upper = data[data$contrast == cntrst,]$.upper
    values = values %>%
      add_row(predicate = p, contrast = cntrst, mean = value, lower = lower, upper = upper)                    
  }
}
values
write_csv(values, file= "../models/analysis2/values.csv")

# plot the HDIs
values = read_csv(file= "../models/analysis2/values.csv")
values = as.data.frame(values)
values

# gather the dataframe for plotting
values.long <- gather(values,label,value,mean:upper)
values.long

# this plot already shows us the results
# EIC-factH different positive: confirm, establish, pretend, prove, say (?)
# EIC-factH different negative: be annoyed, discover (?), know, 
# EIC-factH no difference: acknowledge, admit, announce, be right, confess, hear, 
# inform, reveal, 
ggplot(values.long, aes(x = predicate, y = value)) +
  geom_point() +
  facet_grid(contrast ~ .) +
  geom_hline(yintercept = 0, color = "red")
ggsave("../models/analysis2/plots-of-pairwise-comparisons/0_all-predicates.pdf",height=8,width=14)

#### full model output  ----

analysis2 = print(xtable(values),
                  #only.contents = T,
                  include.rownames=FALSE,
                  include.colnames=TRUE,
                  tabular.environment="longtable",
                  floating=FALSE,
                  hline.after = NULL,
                  latex.environments=NULL,
                  booktabs=TRUE,
                  sanitize.text.function = function(x){x},
                  comment = F
)

write(analysis2, "../models/analysis2/fullModelOutput/analysis2.tex")

### create data to plot into the relevant figure in the paper ----
# we just differentiate whether HDI includes 0 or not (as in text)
# hdi = 0 (hdi includes 0)
# hdi = 1 (hdi does not include 0)
values
values = values %>%
  mutate(hdi = case_when(lower < 0 & upper < 0 ~ 1,
                         lower > 0 & upper > 0 ~ 1,
                         TRUE ~ 0))
values

# now bring the data in the right format for plotting

# > nat.means
# # A tibble: 60 × 5
# expression  context            Mean  YMin  YMax
# <ord>       <fct>             <dbl> <dbl> <dbl>
#   1 acknowledge explicitIgnorance 0.620 0.575 0.667
# 2 acknowledge factH             0.701 0.646 0.758
# 3 acknowledge factL             0.284 0.212 0.360

# get rid of unneeded columns (mean, lower, upper)
values = values %>%
  select(-c(mean, lower, upper))
values

# change column "predicate" to "expression"
values = values %>%
  rename("expression" = "predicate")
values
  
# make column for context based on contrast column
values = values %>%
  mutate(context = case_when(contrast == "explicitIgnorance - factH" ~ "explicitIgnorance",
                             contrast == "explicitIgnorance - factL" ~ "factL",
                             contrast == "factL - factH" ~ "factH",
                             TRUE ~ "error")) %>%
  select(-c(contrast))
values
                         
# now add the needed line values based on context and hdi
# explicitIgnorance (EIC-factH): x=1,xend=3,y=-.15,yend=-.15
# factL (EIC-factL): x=1,xend=2,y=-.05,yend=-.05
# factH (factL-factH): x=2,xend=3,y=-.1,yend=-.1
values = values %>%
  mutate(x = case_when(context == "explicitIgnorance" ~ 1,
                       context == "factL" ~ 1,
                       context == "factH" ~ 2,
                       TRUE ~ 666)) %>%
  mutate(xend = case_when(context == "explicitIgnorance" ~ 3,
                          context == "factL" ~ 2,
                          context == "factH" ~ 3,
                          TRUE ~ 666)) %>%
  mutate(y = case_when(context == "explicitIgnorance" ~ -.15,
                       context == "factL" ~ -.05,
                       context == "factH" ~ -.1,
                       TRUE ~ 666)) %>%
  mutate(yend = case_when(context == "explicitIgnorance" ~ -.15,
                          context == "factL" ~ -.05,
                          context == "factH" ~ -.1,
                          TRUE ~ 666))
values

write_csv(values, file="../data/values.csv")

# ## THIS CODE WAS USED IN THE FIRST SUBMISSION
# #### create data for presenting results as part paper figure
# 
# # there are three lines in each facet (that is, for each expression)
# # one line for each context (x = context)
# # geom_segment(aes(x=1,xend=2,y=-.05,yend=-.05), linetype = "solid")
# # x, xend, y, yend, linetype: depend on expression and contrast
# # create data called "contrasts"
# # with columns "expression" and "context" (for binding and faceting with nat.means)
# # and with columns "x", "xend", "y", "yend", and "linetype"
# 
# predicates = unique(t$expression) 
# predicates
# predicates <- replace(predicates, 12, "be.annoyed") 
# predicates <- replace(predicates, 15, "be.right") 
# predicates
# 
# contrasts = data.frame(expression = character(), contrast = character(), linetype = numeric())
# contrasts
# 
# # linetypes
# # 0 blank: hdi crosses 0
# # 1 solid: biggest value
# # 2 dashed: lower value
# # 3 dotted: lost value (but hdi still doesn't cross 0)
# for (p in predicates) {
#   data = read_csv(paste("../models/analysis2/pairwise.",p,".csv",sep=""))
#   for (i in 1:nrow(data)) {
#     print(i)
#     cntrst = data$contrast[i]
#     lower = data[data$contrast == cntrst,]$.lower
#     upper = data[data$contrast == cntrst,]$.upper
#     value = data[data$contrast == cntrst,]$.value
#     l = ifelse(lower <= 0 & upper >= 0, 0,
#           ifelse(lower < 0 & upper < 0 & value <= -1.5, 1,
#             ifelse(lower < 0 & upper < 0 & -1.5 < value & value <= -0.5, 6,
#               ifelse(lower < 0 & upper < 0 & -.5 < value & value <= 0, 3,
#                 ifelse(lower > 0 & upper > 0 & value >= 1.5, 1,
#                   ifelse(lower > 0 & upper > 0 & 1.5 > value & value > 0.5, 6,
#                     ifelse(lower > 0 & upper > 0 & .5 > value & value >= 0, 3, 666)))))))
#     contrasts = contrasts %>%
#       add_row(expression = p, contrast = cntrst, linetype = l)
#   }
# }
# contrasts
# 
# contrasts = data.frame(contrasts)
# names(contrasts)
# 
# # make column for context
# contrasts = contrasts %>%
#   mutate(context = case_when(contrast == "explicitIgnorance - factH" ~ "explicitIgnorance",
#                              contrast == "explicitIgnorance - factL" ~ "factL",
#                              contrast == "factL - factH" ~ "factH",
#                              TRUE ~ "error"))
# 
# # now add the needed line values based on context
# # explicitIgnorance (EIC-factH): x=1,xend=3,y=-.15,yend=-.15
# # factL (EIC-factL): x=1,xend=2,y=-.05,yend=-.05
# # factH (factL-factH): x=2,xend=3,y=-.1,yend=-.1
# contrasts = contrasts %>%
#   mutate(x = case_when(context == "explicitIgnorance" ~ 1,
#                        context == "factL" ~ 1,
#                        context == "factH" ~ 2,
#                        TRUE ~ 666)) %>%
#   mutate(xend = case_when(context == "explicitIgnorance" ~ 3,
#                        context == "factL" ~ 2,
#                        context == "factH" ~ 3,
#                        TRUE ~ 666)) %>%
#   mutate(y = case_when(context == "explicitIgnorance" ~ -.15,
#                        context == "factL" ~ -.05,
#                        context == "factH" ~ -.1,
#                        TRUE ~ 666)) %>%
#   mutate(yend = case_when(context == "explicitIgnorance" ~ -.15,
#                        context == "factL" ~ -.05,
#                        context == "factH" ~ -.1,
#                        TRUE ~ 666))
# write_csv(contrasts, file="../data/contrasts.csv")
# 
# contrasts = read_csv("../data/contrasts.csv")
# view(contrasts)


# the remainder of this code is to create Table 2 (which has been removed from the paper because
# the results of the statistical comparison have been integrated to Fig 4)

# # what is the distribution of the values?
# ggplot(values, aes(x=value)) +
#   geom_histogram() 
#     
#     
# diff2 = data.frame(predicate = character(), contrast = character(), significant = character())
# diff2
# 
# for (p in predicates) {
#   for (i in 1:nrow(get(paste("pairwise.",p,sep="")))) {
#     print(i)
#     cntrst = get(paste("pairwise.",p,sep=""))$contrast[i]
#     lower = get(paste("pairwise.",p,sep=""))[get(paste("pairwise.",p,sep=""))$contrast == cntrst,]$.lower
#     upper = get(paste("pairwise.",p,sep=""))[get(paste("pairwise.",p,sep=""))$contrast == cntrst,]$.upper
#     value = get(paste("pairwise.",p,sep=""))[get(paste("pairwise.",p,sep=""))$contrast == cntrst,]$.value
#     # significant = ifelse(get(paste("pairwise.",p,sep=""))[get(paste("pairwise.",p,sep=""))$contrast == cntrst,]$.lower < 0 
#     #                      & get(paste("pairwise.",p,sep=""))[get(paste("pairwise.",p,sep=""))$contrast == cntrst,]$.upper > 0, "n.d.", 
#     #                      ifelse(get(paste("pairwise.",p,sep=""))[get(paste("pairwise.",p,sep=""))$contrast == cntrst,]$.lower < 0 
#     #                             & get(paste("pairwise.",p,sep=""))[get(paste("pairwise.",p,sep=""))$contrast == cntrst,]$.upper < 0
#     #                             & get(paste("pairwise.",p,sep=""))[get(paste("pairwise.",p,sep=""))$contrast == cntrst,]$.value < -1.5, "--", "+"))
#     significant = ifelse(lower <= 0 & upper >= 0, "\\cellcolor{white}",
#                          ifelse(lower < 0 & upper < 0 & value <= -1.5, "\\cellcolor{yellow1}",
#                                 ifelse(lower < 0 & upper < 0 & -1.5 < value & value <= -0.5, "\\cellcolor{yellow2}",
#                                        ifelse(lower < 0 & upper < 0 & -.5 < value & value <= 0, "\\cellcolor{yellow3}",
#                                               ifelse(lower > 0 & upper > 0 & value >= 1.5, "\\cellcolor{purple1}",
#                                                      ifelse(lower > 0 & upper > 0 & 1.5 > value & value > 0.5, "\\cellcolor{purple2}",
#                                                             ifelse(lower > 0 & upper > 0 & .5 > value & value >= 0, "\\cellcolor{purple3}", "error")))))))
#     diff2 = diff2 %>%
#       add_row(predicate = p, contrast = cntrst, significant = significant)
#   }
# }
# 
# 
# 
# diff2
# diff2 = diff2 %>%
#   spread(key = predicate, value = significant)
# 
# # order the columns by mean naturalness rating in explicit ignorance context, as in Table 1
# tmp = t %>%
#   filter(context == "explicitIgnorance") %>%
#   group_by(expression) %>%
#   summarize(Mean = mean(response)) %>%
#   mutate(expression = fct_reorder(as.factor(expression),Mean))
# tmp
# levels(tmp$expression)
# col_order <- levels(tmp$expression)
# diff2 <- diff2[, col_order]
# diff2$contrast = c("higher prior - EIC", "lower prior - EIC", "higher - lower prior")
# diff2 = diff2 %>%
#   select(contrast,everything())
# diff2
# 
# #### Table 2 output 
# table2 = print(xtable(diff2),
#                only.contents = T,
#                include.rownames=FALSE,
#                include.colnames=FALSE,
#                floating=FALSE,
#                hline.after = NULL,
#                latex.environments=NULL,
#                booktabs=TRUE,
#                sanitize.text.function = function(x){x},
#                comment = F
#                #hline.after = c(2,2,22,42)
# )
# 
# write(table2, "../models/analysis2/table2.tex")

# # combine all the plots
# library(ggpubr)
# 
# combined_plot <- ggarrange(plot.pairwise.acknowledge,
#                            plot.pairwise.admit,
#                            plot.pairwise.announce,
#                            plot.pairwise.be.annoyed,
#                            plot.pairwise.be.right,
#                            plot.pairwise.confess,
#                            plot.pairwise.confirm,
#                            plot.pairwise.demonstrate,
#                            plot.pairwise.discover,
#                            plot.pairwise.establish,
#                            plot.pairwise.hear,
#                            plot.pairwise.inform,
#                            plot.pairwise.know,
#                            plot.pairwise.pretend,
#                            plot.pairwise.prove,
#                            plot.pairwise.reveal,
#                            plot.pairwise.say,
#                            plot.pairwise.see,
#                            plot.pairwise.suggest,
#                            plot.pairwise.think,
#                            ncol = 2,
#                            nrow = 10)
# combined_plot
# ggsave("../graphs/context-comparisons.pdf",height=10,width=6)

