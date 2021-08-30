if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, skimr, GGally, plotly, viridis, caret, randomForest, e1071,multiROC, 
               rpart, xgboost, h2o, corrplot, rpart.plot, corrgram, lightgbm, visNetwork,Ckmeans.1d.dp)#knitr

train_test = read_csv("E:/Kaggle/Household Poverty Level/output_featured.csv")
#str(train_test)
test=train_test[is.na(train_test$Target),]

train=train_test[(!(is.na(train_test$Target)))&(train_test$parentesco_int==1),]
#划分train validate test
set.seed(1234)

rnd <- sample(dim(train)[1])

trainRatio <- 0.7

trainIndex <- rnd[c(1:(trainRatio*dim(train)[1]))]
trainIndex
train <- train[trainIndex, ]

val <- train[-trainIndex, ] 
#write.csv(val,"val.csv",row.names=FALSE)

# EDA --------------------------------------------------------------------------------------

#train %>% skim() %>% kable()
train %>% skim()

# correlation 多了画不下，选几个feature画
#train%>%select(-c("Id", "Target","idhogar")) %>% corrgram(lower.panel=panel.shade, upper.panel=panel.ellipse)
#train%>%select(-c("Id", "Target","idhogar")) %>% cor() %>% corrplot.mixed(upper = "ellipse", tl.cex=.8, tl.pos = 'lt', number.cex = .8)
train[3:10] %>% cor() %>% corrplot.mixed(upper = "ellipse", tl.cex=.8, tl.pos = 'lt', number.cex = .8)

# ###没跑通
# train %>% 
#   
#   mutate(train$Target = as.factor(train$Target)) %>% 
#   
#   select(-c("Id", "Target","idhogar")) %>% 
#   
#   ggpairs(aes(color = train$Target, alpha=0.4),
#           
#           columns=1:7,
#           
#           lower=list(continuous="points"),
#           
#           upper=list(continuous="blank"),
#           
#           axisLabels="none", switch="both")


#3D图

train %>% 
  
  plot_ly(x=~rooms,y=~escolari,z= ~r4h3, color=~Target, hoverinfo = 'text', colors = viridis(3),
          
          text = ~paste('Target:', Target,
                        
                        '<br>rooms:', rooms,
                        
                        '<br>escolari:', escolari,
                        
                        '<br>r4h3:', r4h3)) %>% 
  
  add_markers(opacity = 0.8) %>%
  
  layout(title = "3D Household Poverty",
         
         annotations=list(yref='paper',xref="paper",y=1.05,x=1.1, text="Target",showarrow=F),
         
         scene = list(xaxis = list(title = 'rooms'),
                      
                      yaxis = list(title = 'escolari'),
                      
                      zaxis = list(title = 'r4h3')))


# XGBoost---------------------------------------------------------------------------------------------

dval<- xgb.DMatrix(data = as.matrix(val%>%select(-c("Id", "Target","idhogar"))))
#train_upsample = upSample(X_train, factor(y_train$Target))
dtrain <- xgb.DMatrix(data = as.matrix(train%>%select(-c("Id", "Target","idhogar"))), label = as.numeric(train$Target-1))


parameters <- list(
  # General Parameters
  booster            = "gbtree",      
  verbosity             = 2,           
  # Booster Parameters
  eta                = 0.08,              
  gamma              = 0.7,                 
  max_depth          = 8,                
  min_child_weight   = 2,            
  subsample          = .9,                 
  colsample_bytree   = .5,                
  colsample_bylevel  = 1,          
  lambda             = 1,    
  alpha              = 0,       
  # Task Parameters
  objective          = "multi:softmax",   # default = "reg:linear"
  eval_metric        = "merror",
  num_class          = 7,
  #seed               = 1,               # reproducability seed
  tree_method = "hist",
  grow_policy = "lossguide"
)



xgb_model <- xgb.train(parameters, dtrain, nrounds = 100)

xgb_pred <- predict(xgb_model, dval)

confusionMatrix(as.factor(xgb_pred+1), as.factor(val$Target))

# predict and submission

my_preds <- predict(xgb_model, as.matrix(test %>%select(-c("Id", "Target","idhogar"))),reshape = TRUE)
# <- predict(model, as.matrix(test %>%select(-c("Id", "Target","idhogar"))),rawscore = TRUE, reshape = TRUE)
# 
# class(my_preds)
# my_preds = as.data.frame(my_preds)
# my_preds = apply(my_preds,1,which.max)

submission = data.frame(Id=test$Id, Target=my_preds)
str(submission)

write.csv(submission,"submission_XGBoost.csv",row.names=FALSE)

# feature importance
xgb_imp = xgb.importance(model = xgb_model, feature_names = names(train))
xgb.ggplot.importance(importance_matrix = xgb_imp,top_n=20)


rm(xgb_model, xgb_pred, dtrain, dval, parameters, my_preds)
# LightGBM ------------------------------------------------------------------------------------

dtrain <- lgb.Dataset(data = as.matrix(train%>%select(-c("Id", "Target","idhogar"))), 
                      label = as.numeric(train$Target-1))
dval <- lgb.Dataset.create.valid(dtrain, data = as.matrix(val %>% select(-c("Id", "Target","idhogar"))), 
                                  label = as.numeric(val$Target-1))
valids <- list(test = dval)

params <- list(objective = "multiclass", metric = "multi_error", num_class = 4)

# model <- lgb.train(
#   params
#   , dtrain
#   , nrounds=1000
#   , valids
#   , learning_rate = 0.1
#   , max_depth=3
#   , num_leaves=7
#   ,class_weight='balanced'
#   
# )

lgb_model <- lgb.train(
  params,
  dtrain,
  valids = valids,
  nrounds=100000,
  min_data = 1, 
  learning_rate = 0.06,
  verbose = 1,
  class_weight='balanced',
  early_stopping_rounds = 10)

#performance on validate set 
lgb_pred <- predict(lgb_model, data.matrix(val %>% select(-c("Id", "Target","idhogar"))),reshape = TRUE)

lgb_pred <- lgb_pred %>% as.data.frame() %>% apply(.,1,which.max)


confusionMatrix(as.factor(lgb_pred), as.factor(val$Target))

#predict
my_preds <- predict(lgb_model, as.matrix(test %>%select(-c("Id", "Target","idhogar"))),reshape = TRUE)
# <- predict(model, as.matrix(test %>%select(-c("Id", "Target","idhogar"))),rawscore = TRUE, reshape = TRUE)

#convert results from probability of each class into class labels
class(my_preds)
my_preds = as.data.frame(my_preds)
my_preds = apply(my_preds,1,which.max)

submission = data.frame(Id=test$Id, Target=my_preds)


#submission
write.csv(submission,"submission_LightGBM.csv",row.names=FALSE)

#feature importance

lgb_imp <- lgb.importance(lgb_model, percentage = TRUE)

lgb.plot.importance(lgb_imp, measure = "Gain",top_n=20)

#interpretation
tree_interpretation <- lgb.interprete(lgb_model, data = as.matrix(val %>% select(-c("Id", "Target","idhogar"))), 1)

val

lgb.plot.interpretation(tree_interpretation[[1]],top_n = 10,cols =2)


# SVM ----------------------------------------------------------------------------------------

# 
# svm_model <- svm(Target~.,train)
# 
# svm_result <- predict(svm_model, newdata = val[,!colnames(val) %in% c("idhogar","Id", "Target")])
# 
# 
# 
# confusionMatrix(svm_result, valid$quality)