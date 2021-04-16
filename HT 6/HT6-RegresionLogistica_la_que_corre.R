#Tutores: Juan De Leon, Maria Jose Castro, Jose Block
#15/04/2021

library(tidyverse)
library(caret)
library(dummy)
library(dummies)
library(corrplot)

# Set de datos

df = read.csv("./train.csv")
df$tipoDeCasa = as.integer(as.character( cut(df$SalePrice,c(0,145000,205000,410000), labels = c(1, 2, 3))))

porcentaje<-0.7
set.seed(123)

# Agregamos las variables dummy al Data Frame
# Variables Dicotomicas
df<-cbind(df,dummy(df$tipoDeCasa, verbose = T))
df

frstselect <- subset (df, select = c(2,4,5,18,19,20,21,27,35,37,38,39,44,45,46,47,48,49,50,51))
frstselect[is.na(frstselect)] <- 0
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
M <- cor(frstselect)
corrplot(M,  method = "color", col = col(200), order = "hclust", number.cex = .5,
         addCoef.col = "black", 
         tl.col = "black",
         sig.level = 0.2, 
         insig = "blank",
         diag = FALSE)

scndselect <- subset (df, select = c(52,53,55,57,60,62,63,67,68,69,70,71,72,76,77,78,82,83,84,85))
scndselect[is.na(scndselect)] <- 0
M1 <- cor(scndselect)
corrplot(M1,  method = "color", col = col(200), order = "hclust", number.cex = .5,
         addCoef.col = "black", 
         tl.col = "black",
         sig.level = 0.2, 
         insig = "blank",
         diag = FALSE)

M3 <- cor(frstselect ,scndselect)
corrplot(M3,  method = "color", col = col(200), order = "hclust", number.cex = .5,
         addCoef.col = "black", 
         tl.col = "black",
         sig.level = 0.2, 
         insig = "blank",
         diag = FALSE)


corte <- sample(nrow(df), nrow(df)*porcentaje)

# Conjutno de datos de hojas anteriores
train <- df[corte,]
test <- df[-corte,]

nums <- unlist(lapply(train, is.numeric))

modelo <- glm(train~., data = train[,nums][,-c(41,42,43)],family = binomial(), maxit=100)

# Modelo Regresion Logistica de casas Economicas
modelo

pred<-predict(modelo,newdata = test[,nums][,-c(41,42,43)], type = "response")

# La mera mera prediccion
prediccion <- ifelse(pred>=0.5, 1, 0)

test$prediccion <- prediccion

plot(test$tipoDeCasa, type="l", lty=1.8, col="blue")
lines(test$prediccion, type="l", col="red")

# Matriz de Confucion
confusionMatrix(as.factor(test$df1),as.factor(prediccion))

modelo <- glm(train$df2~., data = train[,nums][,-c(40,42,43)],family = binomial(), maxit=100)

modelo

pred<-predict(modelo,newdata = test[,nums][,-c(40,42,43)], type = "response")

# La mera mera prediccion
prediccion <- ifelse(pred>=0.5, 1, 0)

test$prediccion <- prediccion

plot(test$tipoDeCasa, type="l", lty=1.8, col="blue")
lines(test$prediccion, type="l", col="red")

# Matriz de Confucion
confusionMatrix(as.factor(test$df2),as.factor(prediccion))

modelo <- glm(train$df3~., data = train[,nums][,-c(40,41,43)],family = binomial(), maxit=100)

# Modelo Regresion Logistica de casas Economicas
modelo

pred<-predict(modelo,newdata = test[,nums][,-c(40,41,43)], type = "response")

# La mera mera prediccion
prediccion <- ifelse(pred>=0.5, 1, 0)

test$prediccion <- prediccion

plot(test$tipoDeCasa, type="l", lty=1.8, col="blue")
lines(test$prediccion, type="l", col="red")

#Matriz de Confusion
confusionMatrix(as.factor(test$df3),as.factor(prediccion))
