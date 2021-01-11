# Data Application Lab - copyright

binary <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
head(binary)
str(binary)  
summary(binary)

## convert rank to a factor
binary$rank = factor(binary$rank)
str(binary) 


###########################################
####### Explore and Visualize Data  #######
###########################################
require(dplyr)
require(ggplot2)

#### Admit vs. GRE
binary %>%
  group_by(admit) %>%
  summarise(gre = mean(gre))

ggplot(binary, aes(x = factor(admit), y=gre)) +
  geom_bar(stat = "summary", fun.y = mean, aes(fill = factor(admit))) +
  ylab("Average GRE") +
  scale_x_discrete(label=c("0"="Not Adimission", "1"="Admission")) +
  theme(legend.position="none")
# Average GRE is higher for students who have an offer.



#### Admit vs. GPA
binary %>%
  group_by(admit) %>%
  summarise(gpa=mean(gpa))

ggplot(binary, aes(x=factor(admit), y=gpa)) +
  geom_bar(stat="summary", fun.y=mean, aes(fill=factor(admit))) +
  ylab("Average GPA") +
  scale_x_discrete(label=c("0"="Not Adimission", "1"="Admission")) +
  theme(legend.position="none")
# Average GRE is slightly higher for students who have an offer.



## Logistic Regression
model = glm(admit ~., data = binary, family = "binomial")
summary(model)
## Deviance residuals: a measure of model fit. 
## Coefficients interpretation:
## The log odds of admission increases by 0.002 when gre increase in 1 unit;
## The log odds of admission increases by 0.804 when gpa increase in 1 unit;
## Having attended an institution with rank of 2 versus an instituion with rank of 1, 
## changes the log odds of admission by -0.675;
## Having attended an institution with rank of 3 versus an institution with rank of 1, 
## changes the log odds of admission by -1.34;
## Having attended an institution with rank of 4 versus an institution with rank of 1, 
## changes the log odds of admission by -1.551;

## odds ratio
OR = exp(coef(model))
OR
## OR interpretation:
## one unit increase in gre, the odds of being admitted (versus not being admitted) increase by a factor of 1.002
## one unit increase in gpa, the odds of being admitted increase by a factor of 2.234
## rank 2 school versus rank 1 school, 
## the odds of being admitted increase by a fator of 0.5089
## rank 3 school versus rank 1 school, 
## the odds of being admitted increase by a factor of 0.2618
## rank 4 school versus rank 1 school, 
## the odds of being admitted increase by a factor of 0.2119

## Misclassification Rate
pred = predict(model, newdata = binary, type = "response")
pred2 = ifelse(pred > 0.5, 1, 0)
accuracy = table(pred2, binary$admit)
accuracy
1 - sum(diag(accuracy))/sum(accuracy)


## Model Performance Evaluation
## ROC Curve
## We are concerned about the area under the ROC curve (AUROC)
## The metric ranges from 0.5 to 1
## Values above 0.8 indicate that the model does a good job 
require("ROCR")
pred3 = prediction(pred, binary$admit)
roc = performance(pred3, "tpr", "fpr")
plot(roc,colorize=T)
abline(0,1)

## AUROC 
auc = performance(pred3,'auc')
auc@y.values
auc = slot(auc, 'y.values')[[1]]
legend(0.4,0.3,round(auc,4), title="AUC", cex=1)



## Identify Best classifier
eval = performance(pred3, "acc")
plot(eval)
max = which.max(slot(eval, "y.values")[[1]])
acc = max(slot(eval, "y.values")[[1]])
cutoff = slot(eval, "x.values")[[1]][max]
cutoff
abline(h=acc, v=cutoff)

## use the best classifier to re-classify the outcome
pred_best = ifelse(pred > cutoff, 1, 0)
cutoff
accuracy = table(pred_best, binary$admit)
accuracy
1-sum(diag(accuracy))/sum(accuracy)




#### Rank vs. Admission Rate
p2 = binary %>% 
  count(rank, admit) %>%
  ungroup %>%
  group_by(rank) %>%
  mutate(pct = n/sum(n))
p2

ggplot(p2, aes(x=factor(admit), y=pct)) +
  geom_bar(stat='identity', aes(fill=factor(admit))) +
  facet_grid(~rank) +
  geom_text(aes(label=round(pct,2), y=pct+0.02)) +
  xlab("Rank")

# Rank 1 has the highest admission rate;
# Rank 4 has the lowest admission rate.