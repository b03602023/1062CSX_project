data <- read.csv("titanicTrain.csv")
# 去除NA
data <- data[1:1000,]
# 整理資料格式
data$boat <- as.character(data$boat)
summary(data)












# age 越小，越有可能上船 (但不一定會存活?)
hist(data$age[data$survived==1], main = "the age histogram for those who were survived" )


# 船票價的影響
data$fare[data$survived==1]
hist(data$fare[data$survived==1])
summary(data$fare[data$survived==1])
summary(data$fare)
plot(data$age, data$fare)
points(data$age[data$survived==1], data$fare[data$survived==1], col="red") #活下
points(data$age[data$survived==1 & data$sibsp!=0 & data$parch!=0], data$fare[data$survived==1 & data$sibsp!=0 & data$parch!=0], col="blue")
points(data$age[data$survived==1 & data$sibsp!=0 & data$parch!=0 & data$sex=="female"], data$fare[data$survived==1 & data$sibsp!=0 & data$parch!=0 & data$sex=="female"], col="green")
#活下且有兄弟姊妹親人是女性的比例是
length(data$age[data$survived==1 & data$sibsp!=0 & data$parch!=0& data$sex=="female"])/length(data$age[data$survived==1 & data$sibsp!=0 & data$parch!=0])
length(data$age[data$survived==0 & data$sibsp!=0 & data$parch!=0& data$sex=="male"])/length(data$age[data$survived==0 & data$sibsp!=0 & data$parch!=0])


#活下且有兄弟姊妹親人等等



# 推測: 買最貴的票價存活機率極高
#          

# age 越大，越有可能上船?



# 登船但不一定會存活
data$survived[data$boat != ""]
as.factor(data$survived[data$boat != ""])
summary(as.factor(data$survived[data$boat != ""])) #有登船
summary(as.factor(data$survived[data$boat != ""]))[1]/length(as.factor(data$survived[data$boat != ""]))
# 沒登船但不一定就會死
data$survived[data$boat == ""]
summary(as.factor(data$survived[data$boat == ""])) #沒登船
summary(as.factor(data$survived[data$boat == ""]))[2]/length(as.factor(data$survived[data$boat == ""]))

# changing the data type
data$pclass <- as.factor(data$pclass)
data$survived <- as.factor(data$survived)
data$sex <- as.factor(data$sex)



