setwd("C:\\Users\\Ouyangd\\Dropbox\\Echo Research\\Talks\\Blog Post")

library(ggplot2)
library(ggthemes)
library(pROC)

data <- read.csv("data.csv")

str(data)

model <- lm(FinalEF ~ InitialEF, data[data$ActualArm == "Sonographer",])
summary(model)$r.squared
mean(abs(data[data$ActualArm == "Sonographer",]$FinalEF - data[data$ActualArm == "Sonographer",]$InitialEF))

qplot(data = data[data$ActualArm == "Sonographer",], x = InitialEF, y = FinalEF) + 
theme_bw() + coord_equal() + theme(axis.title = element_text()) +
ggtitle("R2 = 0.82, MAE  = 3.8%") + xlab("Initial Sonographer Evaluation") +
ylab("Blinded Cardiologist Evaluation") + 
theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 15)) +xlim(0,100) + ylim(0,100)

SonographerROC <- roc(data[data$ActualArm == "Sonographer",]$FinalEF > 80, data[data$ActualArm == "Sonographer",]$InitialEF)
SonographerROC 
ggroc(SonographerROC )+ theme_fivethirtyeight() + ggtitle("AUC = 0.96") + coord_equal()

LVEF60 <- roc(data[data$ActualArm == "Sonographer",]$FinalEF > 60, data[data$ActualArm == "Sonographer",]$InitialEF)
LVEF60 
LVEF50 <- roc(data[data$ActualArm == "Sonographer",]$FinalEF > 50, data[data$ActualArm == "Sonographer",]$InitialEF)
LVEF50 
LVEF35 <- roc(data[data$ActualArm == "Sonographer",]$FinalEF > 35, data[data$ActualArm == "Sonographer",]$InitialEF)
LVEF35 
ggroc(list(LVEF60, LVEF50,LVEF35), size = 1)+ theme_bw() + ggtitle("AUC 0.98 for LVEF >35%, 0.96 >50%, 0.92 >60%") + 
coord_equal()+ scale_color_brewer(palette = "Set1", labels = c(">60%", ">50%", ">35%")) + 
theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 15)) + labs(x = "Specificity of Sonographer", y = "Sensitivity of Sonographer", color = "Threshold")






data$FinalEF <- abs(data$FinalEF)


model <- lm(FinalEF ~ PriorClinicalEF, data)
summary(model)$r.squared
mean(abs(data$FinalEF - data$PriorClinicalEF))

qplot(data = data, x = PriorClinicalEF, y = FinalEF) + 
theme_bw() + coord_equal() + theme(axis.title = element_text()) +
ggtitle("R2 = 0.64, MAE  = 6.7%") + xlab("Historical Cardiologist Evaluation") +
ylab("Blinded Cardiologist Evaluation") + 
theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 15)) +xlim(0,100) + ylim(0,100)

hxLVEF60 <- roc(data$FinalEF > 60, data$PriorClinicalEF)
hxLVEF60 
hxLVEF50 <- roc(data$FinalEF > 50, data$PriorClinicalEF)
hxLVEF50 
hxLVEF35 <- roc(data$FinalEF > 35, data$PriorClinicalEF)
hxLVEF35 
ggroc(list(hxLVEF60, hxLVEF50,hxLVEF35), size = 1)+ theme_bw() + ggtitle("AUC 0.97 for LVEF >35%, 0.91 >50%, 0.81 >60%") + 
coord_equal()+ scale_color_brewer(palette = "Set1", labels =  c(">60%", ">50%", ">35%")) + 
theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 15)) + labs(x = "Specificity  of Cardiologist", y = "Sensitivity of Cardiologist", color = "Threshold")



ecg <- read.csv("EF_predictions_slim.csv")

str(ecg)


model <- lm(ecg$EF_2D ~ ecg$EF_preds, ecg,])
summary(model)$r.squared
mean(abs(ecg$EF_2D - ecg$EF_preds))

qplot() + geom_point(data = ecg, aes( x = EF_preds, y = EF_2D), alpha = 0.05) + 
theme_bw()  + theme(axis.title = element_text()) +
ggtitle("R2 = 0.64, MAE  = 9.7%") + xlab("ECG prediction of LVEF") +
ylab("Ground Truth Echo LVEF") + 
theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 15))# +xlim(0,100) + ylim(0,100)+ coord_equal()


ecgLVEF60 <- roc(ecg$EF_2D > 60, ecg$EF_preds)
ecgLVEF60 
ecgLVEF50 <- roc(ecg$EF_2D > 50, ecg$EF_preds)
ecgLVEF50 
ecgLVEF35 <- roc(ecg$EF_2D > 35, ecg$EF_preds)
ecgLVEF35 
ggroc(list(ecgLVEF60, ecgLVEF50,ecgLVEF35), size = 1)+ theme_bw() + ggtitle("AUC 0.88 for LVEF >35%, 0.83 >50%, 0.75 >60%") + 
coord_equal()+ scale_color_brewer(palette = "Set1", labels =  c(">60%", ">50%", ">35%")) + 
theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 15)) + labs(x = "Specificity of AI-ECG", y = "Sensitivity of AI-ECG", color = "Threshold")








