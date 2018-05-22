# titanic
library(e1071)
tdata <- subset(data, select = c(pclass, fare, fs, survived))
svm_model <- svm(survived ~ ., data=tdata)
svm_pred <- predict(svm_model)
table(svm_pred, data$survived) 

plot(svm_model, tdata, pclass ~ fare,
     slice = list(age = 3, sibsp= 4),color.palette = terrain.colors)
plot(svm_model, tdata, survived ~ fare + pclass + fs)

