###################################
### Model Building & Evaluation ###
###################################

# # Load packages
# library('ggplot2')
# library('ggthemes')
# library('scales')
# library('dplyr')
# library('mice')
library('randomForest')
# library('data.table')
library('FSelector')
# library('corrplot')
# library('MLmetrics')
library('ROCR')
# library('plyr')
set.seed(29127)
base = './Data/' # Data directory /
load(paste0(base, 'feat.Rdata'))


###################
### Build Model ###
###################

# # Feature exclusion after testing
# ex = c('min_months_last_30_plus', 'enq_purpose', 'count_enquiry_recency_90', 'worst_dpd', 
# 	'pay_dpd_ratio')
# train = train[, !(colnames(train) %in% ex)]
# test  = test[, !(colnames(test) %in% ex)]

# Sample to balance target feature
ind1     = which(train[,"bad_flag_worst6"] == 1)
ind0     = which(train[,"bad_flag_worst6"] == 0)
sampind1 = sample(ind1, length(ind1), replace = TRUE)
sampind0 = sample(ind0, length(ind1), replace = TRUE) # undersampling
sampind  = c(sampind1, sampind0)
train    = train[sampind,]

print(information.gain(bad_flag_worst6 ~ . , train))
write.csv(information.gain(bad_flag_worst6 ~.,train), file = 'ig.csv')

# Build random forest model
model = randomForest(bad_flag_worst6 ~ . ,
                      data = train, 
                      importance = TRUE,
                      do.trace = 500,
                      ntree = 1000)
# imp = varImpPlot(model)


fit        = predict(model, test[,2:ncol(test)], type = "prob")
prediction = prediction(fit[,2], test$bad_flag_worst6)


################
### Evaluate ###
################

auroc      = round(performance(prediction, measure = "auc")@y.values[[1]]*100, 2)
Gini       = (2*auroc - 100)
print(Gini)
score       = data.frame(score = apply(fit[,1:2],1, max))
score$class = colnames(fit)[apply((fit[,1:2] == score[,1]), 1, which.max)]
score       = score[order(score[,1], decreasing = TRUE),]
rank = data.frame(decile = c(10:1))
win = nrow(score)/10
for (x in c(1:10)){
	rank[x,2] = sum(score[(win*x-(win-1)):(win*x),'class'] == 1)/win
}

roc.perf = performance(prediction, measure = "tpr", x.measure = "fpr")
plot(roc.perf)
abline(a=0, b= 1)
# precision = sum(score$class == test$bad_flag_worst6 & score$class == '1') / (sum(score$class != test$bad_flag_worst6 & score$class == '1') + sum(score$class == test$bad_flag_worst6 & score$class == '1'))
# recall = sum(score$class == test$bad_flag_worst6 & score$class == '1') / (sum(score$class != test$bad_flag_worst6 & score$class == '0') + sum(score$class == test$bad_flag_worst6 & score$class == '1'))
# fmeasure = (2 * precision * recall) / (precision + recall)
specificity = sum(score$class == test$bad_flag_worst6 & score$class == '0') / (sum(score$class == test$bad_flag_worst6 & score$class == '0')
 + sum(score$class != test$bad_flag_worst6 & score$class == '1'))
sensitivity = sum(score$class == test$bad_flag_worst6 & score$class == '1') / (sum(score$class == test$bad_flag_worst6 & score$class == '1')
 + sum(score$class != test$bad_flag_worst6 & score$class == '0'))
print(rank)
print(specificity)
print(sensitivity)
write.csv(rank, file = 'rank.csv')
# write.csv(imp, file = 'imp.csv')
write.csv(information.gain(bad_flag_worst6 ~.,train), file = 'ig.csv')

print('Done!')