library(lme4)
library(reshape2)
library(tidyverse)
library(readxl)
library(Matrix)
library(glmm)




colnames(tbi_data) = gsub(" ","",colnames(tbi_data))


weight_bearing_data = select(tbi_data,SubjectID,weightbearingpre,Weightbearingpost,Age,Height,Weight,BMI,starts_with("EOPre"),starts_with("ECPre"))

head(weight_bearing_data)


weight_bearing_data = weight_bearing_data[order(weight_bearing_data$SubjectID),]
table(is.na(weight_bearing_data))

weight_bearing_data = na.omit(weight_bearing_data)
table(is.na(weight_bearing_data))


demographics_data = weight_bearing_data[,which(names(weight_bearing_data) %in% c("SubjectID","Age","Height","Weight","BMI"))]
EOPre_data = weight_bearing_data[ ,!names(weight_bearing_data) %in% c("Age","Height","Weight","BMI","weightbearingpre","Weightbearingpost")]
EOPre_data = select(EOPre_data,SubjectID,starts_with("EOPre"))
EOPre_data = melt(EOPre_data)
colnames(EOPre_data)[which(colnames(EOPre_data) == "variable")] = "EOPre_examination"
colnames(EOPre_data)[which(colnames(EOPre_data) == "value")] = "EOPre_time"
response_data = weight_bearing_data[,which(names(weight_bearing_data) %in% c("SubjectID","weightbearingpre","Weightbearingpost"))]
response_data = melt(response_data)
colnames(response_data)[which(colnames(response_data) == "variable")] = "Pre_Post_weightbearing"
complete_data = cbind(demographics_data,EOPre_data,response_data)
complete_data = complete_data[,!duplicated(colnames(complete_data))]

head(complete_data)

complete_data$score = ifelse(complete_data$value < mean(complete_data$value),0,1)

head(complete_data)

EOPre_model = glmm(score ~ 0 + EOPre_examination,random = list(~0 + Age,~0 + Height,~0 + Weight,~0 + BMI,~0+EOPre_time),varcomps.names = c("Age", "Height","Weight","BMI","EOPre_time"),data = complete_data,family.glmm = bernoulli.glmm, m = 10^4)


ECPre_data = weight_bearing_data[ ,!names(weight_bearing_data) %in% c("Age","Height","Weight","BMI","weightbearingpre","Weightbearingpost")]
ECPre_data = select(ECPre_data,SubjectID,starts_with("ECPre"))
ECPre_data = melt(ECPre_data)
colnames(ECPre_data)[which(colnames(ECPre_data) == "variable")] = "ECPre_examination"
colnames(ECPre_data)[which(colnames(ECPre_data) == "value")] = "ECPre_time"
complete_data = cbind(ECPre_data,complete_data)
complete_data = complete_data[,!duplicated(colnames(complete_data))]

str(activity_data)
head(complete_data)


complete_data$SubjectID = factor(complete_data$SubjectID)
ECPre_model = glmm(score ~ 0 + ECPre_examination,random = list(~0 + Age,~0 + Height,~0 + Weight,~0 + BMI,~0+ECPre_time),varcomps.names = c("Age", "Height","Weight","BMI","EOPre_time"),data = complete_data,family.glmm = bernoulli.glmm, m = 10^4)
summary(ECPre_model)
anova(ECPre_model)

anova(ECPre_model,EOPre_model)
