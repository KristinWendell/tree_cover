# load data and libraries

df = read.csv("~/Documents/MachineLearning/Tree Project/raw_data/train.csv")
source("~/Documents/MachineLearning/BabsonAnalytics.R")

library(ggridges) #density plot
library(ggplot2) #density plot
library(forcats) #reorder density plot
library(rpart) #class tree
library(rpart.plot) #class tree
library(rattle) #pretty rpart plot
library(dplyr) #used in highlighting nodes in class tree plot + density plot color
library(randomForest) #rf
library(caret) #knn
library(e1071) #naive bayes

set.seed(1234)

# manage
df$Id = NULL
df$Hillshade_9am = as.integer(df$Hillshade_9am)
df$Hillshade_Noon = as.integer(df$Hillshade_Noon)
df$Hillshade_3pm = as.integer(df$Hillshade_3pm)
df$Wilderness_Area1 = as.factor(df$Wilderness_Area1)
df$Wilderness_Area2 = as.factor(df$Wilderness_Area2)
df$Wilderness_Area3 = as.factor(df$Wilderness_Area3)
df$Wilderness_Area4 = as.factor(df$Wilderness_Area4)
df$Soil_Type1 = as.factor(df$Soil_Type1)
df$Soil_Type2 = as.factor(df$Soil_Type2)
df$Soil_Type3 = as.factor(df$Soil_Type3)
df$Soil_Type4 = as.factor(df$Soil_Type4)
df$Soil_Type5 = as.factor(df$Soil_Type5)
df$Soil_Type6 = as.factor(df$Soil_Type6)
df$Soil_Type7 = NULL
df$Soil_Type8 = as.factor(df$Soil_Type8)
df$Soil_Type9 = as.factor(df$Soil_Type9)
df$Soil_Type10 = as.factor(df$Soil_Type10)
df$Soil_Type11 = as.factor(df$Soil_Type11)
df$Soil_Type12 = as.factor(df$Soil_Type12)
df$Soil_Type13 = as.factor(df$Soil_Type13)
df$Soil_Type14 = as.factor(df$Soil_Type14)
df$Soil_Type15 = NULL
df$Soil_Type16 = as.factor(df$Soil_Type16)
df$Soil_Type17 = as.factor(df$Soil_Type17)
df$Soil_Type18 = as.factor(df$Soil_Type18)
df$Soil_Type19 = as.factor(df$Soil_Type19)
df$Soil_Type20 = as.factor(df$Soil_Type20)
df$Soil_Type21 = as.factor(df$Soil_Type21)
df$Soil_Type22 = as.factor(df$Soil_Type22)
df$Soil_Type23 = as.factor(df$Soil_Type23)
df$Soil_Type24 = as.factor(df$Soil_Type24)
df$Soil_Type25 = as.factor(df$Soil_Type25)
df$Soil_Type26 = as.factor(df$Soil_Type26)
df$Soil_Type27 = as.factor(df$Soil_Type27)
df$Soil_Type28 = as.factor(df$Soil_Type28)
df$Soil_Type29 = as.factor(df$Soil_Type29)
df$Soil_Type30 = as.factor(df$Soil_Type30)
df$Soil_Type31 = as.factor(df$Soil_Type31)
df$Soil_Type32 = as.factor(df$Soil_Type32)
df$Soil_Type33 = as.factor(df$Soil_Type33)
df$Soil_Type34 = as.factor(df$Soil_Type34)
df$Soil_Type35 = as.factor(df$Soil_Type35)
df$Soil_Type36 = as.factor(df$Soil_Type36)
df$Soil_Type37 = as.factor(df$Soil_Type37)
df$Soil_Type38 = as.factor(df$Soil_Type38)
df$Soil_Type39 = as.factor(df$Soil_Type39)
df$Soil_Type40 = as.factor(df$Soil_Type40)
df$Cover_Type = as.factor(df$Cover_Type)

#recode cover type for easier analysis
df$Cover_Type = recode_factor(df$Cover_Type, `1` = "Spruce/Fir", `2` = "Lodgepole Pine", `3` = "Ponderosa Pine",
              `4` = "Cottonwood/Willow", `5` = "Aspen" ,`6`  = "Douglas-fir", `7`  = "Krummholz" )

# elevation plot

#mutate so the y axis is sorted in the correct order, otherwise will be in reverse order
#set colors to match tableau colors
df <- df %>%
mutate(Cover_Type = fct_relevel(Cover_Type, 
                           "Krummholz", "Douglas-fir", "Aspen", 
                            "Cottonwood/Willow", "Ponderosa Pine", "Lodgepole Pine", 
                            "Spruce/Fir"))    
  ggplot(df, aes(x = Elevation, y = Cover_Type,  fill = Cover_Type)) + 
  geom_density_ridges2(scale = 5, alpha = 0.7) +
  theme_ridges(grid = FALSE, center_axis_labels = TRUE) +
  scale_fill_manual(values = c("Spruce/Fir" = "purple",
                                "Ponderosa Pine" ="yellow",
                                "Lodgepole Pine" = "green",
                                "Krummholz" = "lightblue",
                                "Douglas-fir" = "red",
                                "Cottonwood/Willow" = "orange",
                                "Aspen"="steelblue")) +
  theme(legend.position="none") +
  xlab("Elevation (meters)") +
  ylab("")

#PCA to be use on KNN and NB
df_PCA = df
df_Hillshades = df[ ,c("Aspect","Slope","Hillshade_9am","Hillshade_Noon","Hillshade_3pm")]
pca =prcomp(df_Hillshades, center=TRUE, scale=TRUE)
summary(pca) #PC1 52%, PC2 32% PC3 10% PC4 6% PC5 .06% 
pcs = predict(pca, df_Hillshades)
df_PCA = cbind(df_PCA,pcs)
df_PCA$PC3 = NULL
df_PCA$PC4 = NULL
df_PCA$PC5 = NULL
df_PCA$Hillshade_9am = NULL
df_PCA$Hillshade_Noon = NULL
df_PCA$Hillshade_3pm = NULL
df_PCA$Aspect = NULL
df_PCA$Slope = NULL
df_PCA = rename(df_PCA, PC1_hillshade=PC1)
df_PCA = rename(df_PCA, PC2_hillshade=PC2)


df_Hydrology = df_PCA[ ,c("Horizontal_Distance_To_Hydrology","Vertical_Distance_To_Hydrology" )]
pca =prcomp(df_Hydrology, center=TRUE, scale=TRUE)
summary(pca) #PC 1 83% PC 2 17%
pcs = predict(pca, df_Hydrology)
df_PCA = cbind(df_PCA,pcs)
df_PCA$Horizontal_Distance_To_Hydrology = NULL
df_PCA$Vertical_Distance_To_Hydrology = NULL
df_PCA = rename(df_PCA, PC1_hydro=PC1)
df_PCA = rename(df_PCA, PC2_hydro=PC2)



# Partition
N = nrow(df)
trainingSize = round(0.6*N)
trainingCases = sample(N,trainingSize)
training = df[trainingCases, ]
test = df[-trainingCases, ]
observations = test$Cover_Type


####TREE
# Model
model_tree = rpart(Cover_Type ~., method = "class", data=training)
fancyRpartPlot(model_tree, palettes=c("Blues", "Greens", "Greys", "Oranges", "Purples", "Reds"))

# Predictions
predictions_tree = predict(model_tree,test, type="class")
observations = test$Cover_Type

# Evaluate
error_rate_btree = sum(predictions_tree != observations)/nrow(test)
error_bench = benchmarkErrorRate(training$Cover_Type, test$Cover_Type)

# Overfitting. 
stopping_rules = rpart.control(minsplit=1,minbucket=1,cp=0) #for easier view(minsplit=25,minbucket=25,cp=.0001)
model_tree = rpart(Cover_Type ~., data=training, control=stopping_rules)
#rpart.plot(model) This will take a while to run. Wait until it is pruned.
predictions_overfit = predict(model_tree,test, type="class")
error_overfit = sum(predictions_overfit != observations)/nrow(test)

#Prune tree. Take off the unnecessary branches
model_tree = easyPrune(model_tree)
fancyRpartPlot(model_tree, palettes=c("Blues", "Greens", "Greys", "Oranges", "Purples", "Reds"))
#rules = rpart.rules(model_tree, nn = TRUE, extra = 4, cover = TRUE)
#write.csv(rules,  "rules.csv") 


# return the given node and all its ancestors - use this to see nodes/branches easier
#path.to.root <- function(node)
#{
#  if(node == 1)   
#    node
#else
#  # recurse, %/% 2 gives the parent of node
# c(node, path.to.root(node %/% 2)) }

#node = 24          # *****PUT NODE HERE****
#nodes = as.numeric(row.names(model_tree$frame))
#cols = ifelse(nodes %in% path.to.root(node), "sienna", "gray")
#prp(model_tree, nn = TRUE, 
#    col = cols, branch.col = cols, split.col = cols, nn.col = cols)

predictions_pruned = predict(model_tree,test, type="class")
error_pruned = sum(predictions_pruned != observations)/nrow(test)
table(predictions_pruned,observations)


#Random Forest
model_rf = randomForest(Cover_Type ~.,method = "class", data=training, ntree=500)
predictions_rf = predict(model_rf,test,type="class")
error_rf = sum(predictions_rf != observations) / nrow(test)
varImpPlot(model_rf) 
table(predictions_rf,observations)


#####KNN

df_KNN = df_PCA[, c("Cover_Type","PC1_hillshade","PC2_hillshade","PC1_hydro","PC2_hydro","Elevation",
                "Horizontal_Distance_To_Roadways","Horizontal_Distance_To_Fire_Points")] #only numeric
standardizer = preProcess(df_KNN ,c("center","scale")) #center to mean 0 scale so SD 1
df_KNN  = predict(standardizer, df_KNN)

# partition the data
training_KNN = df_KNN[trainingCases, ]
test_KNN = df_KNN[-trainingCases, ]

# model
k_best = kNNCrossVal(Cover_Type ~., training_KNN) # determines k = 1 brings lowest error rate
model_kbest = knn3(Cover_Type ~ ., data=training_KNN, k=k_best) #use k=3 for best stacking


#evaluate
predictions_kbest = predict(model_kbest,test_KNN, type="class")
table(predictions_kbest,observations)
error_kbest = sum(predictions_kbest != observations)/nrow(test) # new error rate ~24%

####Naive Bayes

#convert PC1 to categorical
df_NB = df_PCA
PC1_hillshade_binned = cut(df_NB$PC1_hillshade,10)
levels(PC1_hillshade_binned) #factor 
df_NB = cbind(df_NB,PC1_hillshade_binned) 
df_NB$PC1_hillshade = NULL
summary(PC1_hillshade_binned)

#convert PC2 to categorical
PC2_hillshade_binned = cut(df_NB$PC2_hillshade,10)
levels(PC2_hillshade_binned) #factor 
df_NB = cbind(df_NB,PC2_hillshade_binned) 
df_NB$PC2_hillshade = NULL
summary(PC2_hillshade_binned)

# convert elevation to factor
Elevation_binned = cut(df_NB$Elevation,c(1800,2000,2400,2640,2920,3160,3320,3440,3900)) 
levels(Elevation_binned) #factor 
df_NB = cbind(df_NB,Elevation_binned) 
df_NB$Elevation = NULL
summary(Elevation_binned)

# convert horizontal distance to fire points
Horizontal_Distance_To_Fire_Points_binned = cut(df_NB$Horizontal_Distance_To_Fire_Points,10) 
levels(Horizontal_Distance_To_Fire_Points_binned) #factor 
df_NB = cbind(df_NB,Horizontal_Distance_To_Fire_Points_binned) 
df_NB$Horizontal_Distance_To_Fire_Points = NULL
summary(Horizontal_Distance_To_Fire_Points_binned)

# convert horizontal distance to roadways
Horizontal_Distance_To_Roadways_binned = cut(df_NB$Horizontal_Distance_To_Roadways,10) 
levels(Horizontal_Distance_To_Roadways_binned) #factor 
df_NB = cbind(df_NB,Horizontal_Distance_To_Roadways_binned) 
df_NB$Horizontal_Distance_To_Roadways = NULL
summary(Horizontal_Distance_To_Roadways_binned)

#convert PC1 Hydro to categorical
PC1_hydro_binned = cut(df_NB$PC1_hydro,10)
levels(PC1_hydro_binned) #factor 
df_NB = cbind(df_NB,PC1_hydro_binned) 
df_NB$PC1_hydro = NULL
summary(PC1_hydro_binned)

#convert PC2 Hydro to categorical
PC2_hydro_binned = cut(df_NB$PC2_hydro,10)
levels(PC2_hydro_binned) #factor 
df_NB = cbind(df_NB,PC2_hydro_binned) 
df_NB$PC2_hydro = NULL
summary(PC2_hydro_binned)

training_NB = df_NB[trainingCases, ]
test_NB = df_NB[-trainingCases, ]

model_NB = naiveBayes(Cover_Type ~., data=training_NB)
predictions_NB = predict(model_NB, test_NB) 
observations = test$Cover_Type
error_NB = sum(predictions_NB != observations)/nrow(test)
table(predictions_NB,observations)
error_bench = benchmarkErrorRate(training_NB$Cover_Type, test_NB$Cover_Type)

# Particularly impactful NB tables
#model_NB$tables$Elevation_binned
  #Given the cover type is 1, the probability it is in elevation bin 5 is 44% and 38% 6
  #Given cover type is 4, the probability it is in elevation bin 2 is 98%
  #Given cover type is 5, the probability it is in elevation bin 4 is 87%
#model_NB$tables$Wilderness_Area1
  #Cover Type 3,4,6 never in wilderness area 1
#model_NB$tables$Wilderness_Area2
  #Not very common for these cover types. 3- 6 never in it, low prob for others
#model_NB$tables$Wilderness_Area3
  #All could be in wilderness 3 except 4
#model_NB$tables$Wilderness_Area4
  #Given cover type 4, 100% in area 4


####Stacking
pred_rf_full = predict(model_rf,df,type="class") 
pred_KNN_full = predict(model_kbest,df_KNN, type="class")
pred_NB_full = predict(model_NB, df_NB)
df_stacked = cbind(df,pred_rf_full,pred_KNN_full,pred_NB_full) # bind by columns 

#Random Forest as manager model 22.5% error with k=1, 17.4% with k=3
train_stacked = df_stacked[trainingCases,] 
test_stacked = df_stacked[-trainingCases,]
stacked_rf = randomForest(Cover_Type ~.,method = "class", data=train_stacked,ntree=500) 
pred_stacked = predict(stacked_rf,test_stacked,type="class")
error_stacked = sum(pred_stacked != observations)/nrow(test_stacked)
table(pred_stacked,observations)
varImpPlot(stacked_rf) #KNN predictions most impact with k=1

# Neural Net as manager model 24% error (k=1)
#standardizer_stacked = preProcess(df_stacked,c("center","scale"))
#df_stacked = predict(standardizer_stacked,df_stacked)
#train_stacked = df_stacked[trainingCases,] 
#test_stacked = df_stacked[-trainingCases,]
#stacked_nn = nnet(Cover_Type ~., data=train_stacked,size = 4) 
#pred_stacked = as.factor(predict(stacked_nn,test_stacked,type="class"))
#error_stacked = sum(pred_stacked != observations)/nrow(test_stacked) 

# Boosting as manager model - 24% error (k=1)
#train_stacked = df_stacked[trainingCases,] 
#test_stacked = df_stacked[-trainingCases,]
#library(gbm) 
#boost = gbm(Cover_Type ~., data=train_stacked, n.trees=500,cv.fold=4) 
#best_size = gbm.perf(boost)
#pred_boost = predict(boost,test_stacked,best_size,type="response") #using 187 trees
#predictions_boost = colnames(pred_boost)[apply(pred_boost, 1, which.max)]
#table(predictions_boost,test_stacked$Cover_Type)
#result = data.frame(test_stacked$Cover_Type, predictions_boost)
#error_boost = sum(predictions_boost != test_stacked$Cover_Type) / nrow(test)


