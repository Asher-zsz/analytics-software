if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, skimr, GGally, plotly, viridis, caret, randomForest, e1071,multiROC, 
               rpart, xgboost, h2o, corrplot, rpart.plot, corrgram, lightgbm, visNetwork,Ckmeans.1d.dp,
               highcharter,tictoc,gmodels,dplyr,VIM,readr,ggplot2,mice,corrplot,factoextra,glmnet,
               ggthemes,plotly,Rtsne,breakDown,gridExtra,kknn,C50,pROC)#knitr

#1.import data
train = read.csv("train.csv")
test = read.csv("test.csv")
all = bind_rows(train,test)

#2.one-hot encoding turns to categorical
#2.1 list feature that need to encoding
feature_list = c(
  "pared",
  "piso",
  "techo",
  "abasta",
  "sanitario",
  "energcocinar",
  "elimbasu",
  "epared",
  "etecho",
  "eviv",
  "estadocivil",
  "parentesco",
  "instlevel",
  "tipovivi",
  "lugar",
  "area"
)

# Matrix to store our new features

new_features_integer = data.frame(matrix(ncol = length(feature_list), nrow = nrow(all)))

# Cycle through and reverse the OHE process for these

ohe_names = vector()
#take a look at new features 
feature_sum <- 0
for(i in 1:length(feature_list)){
  
  # Grab the feature
  feature_to_fix = all %>% select(starts_with(feature_list[i]))
  feature_sum <- feature_sum + length(feature_to_fix)
  # Fix and enter into our new feature matrix
  
  
  #max.col(feature_to_fix) find the column has number
  #names(feature_to_fix)[max.col(feature_to_fix)] grab name
  #factor's ordered = TRUE :
  #paredblolad < paredzocalo < paredpreb < pareddes < paredmad < paredzinc < ... < paredother
  # levels = name(feature_to_fix_1) 
  new_features_integer[,i] = as.integer(factor(names(feature_to_fix)[max.col(feature_to_fix)], ordered = FALSE,
                                               levels = names(feature_to_fix)))
  # name new feature matrix
  names(new_features_integer)[i] = paste0(feature_list[i],"_int")
  #collect names
  ohe_names = c(ohe_names, as.vector(names(feature_to_fix)))
  
}
head(new_features_integer)

new_features_integer = data.frame(apply(new_features_integer,2,factor))
all <- all %>% bind_cols(new_features_integer)

#2.2 change electric features into label feature

origin_name = c('public','planpri','noelec','coopele')
feature_to_ele = all %>% select(origin_name)
all$eletr_int <- as.integer(factor(names(feature_to_ele)[max.col(feature_to_ele)], ordered = FALSE,
                                   levels = names(feature_to_ele)))
table(all$eletr_int)


#3.missing value
#aggr shows the distribution of missing value(so slow)
#aggr(all,plot = T, bras = F, only.miss = TRUE)

#more fast and convenient way
missing = is.na(all)
miss_num =sort(colSums(missing),decreasing=T)[sort(colSums(missing),decreasing=T)>0]
miss_percent <- round(miss_num/ length(all),2)

# 3.1 rez_esc: Years behind in school
# Collected for people between [7,19] of age. Formula: rez_esc = (age - escolari) - 7
# find null values that are not in range
L <- ((all$age >19 | all$age <7 )& is.na(all$rez_esc))
all[L,"rez_esc"] <- 0

#take a look at the feature of variables that have null values 
idx = which(is.na(all$rez_esc))
table(all[idx,'age'])
all[idx,"rez_esc"] <- all[idx,'age']-all[idx,'escolari']-7

all[which(all$rez_esc < 0 & all$age <18),c('age','escolari')]
table(all$rez_esc)

# check missing variables
all$rez_esc[is.na(all$rez_esc)]
#sum(is.na(all$v2a1))

#set new feature new_rec_esc
L1 <- (all$age > 7 & all$age < 19)
all$new_rez_esc <- all$rez_esc
all[L1,'new_rez_esc'] <- all[L1,'age'] - all[L1,'escolari'] -7
# equal to all$new_rez_esc[L1] <- all$age[L1] - all$escolari[L1] -7
table(all$new_rez_esc - all$rez_esc)

sum(is.na(all$rez_esc))

#3.2 v18q1
#number of tablets household owns
#find all missing v18q1 values are v18q=0
all[is.na(all$v18q1) & all$v18q != 0,c('v18q','v18q1')]

#implement
all$v18q1[is.na(all$v18q1)] <- 0

#3.3 v2a1 
#Monthly rent payment
all[is.na(all$v18q1) & all$v18q != 0,c('v18q','v18q1')]

v2a1 <- all %>% select(starts_with('tipo'))
ggplot(all[all$v2a1 %>% is.na,], aes(x=tipovivi_int)) +
  geom_bar(position="stack") 

#1 means having a house??4 means precarious??5 means assign or borrow
all$v2a1[is.na(all$v2a1)& (all$tipovivi_int == 1 | all$tipovivi_int == 5) ] <- 0
all$v2a1[all$tipovivi_int == 4] <- 0.5

sum(is.na(all$v2a1))

missing_meanedu = all[all$meaneduc %>% is.na() ,c('idhogar', 'escolari')]
no_missing_meanedu = all[!all$meaneduc %>% is.na() ,c('idhogar', 'escolari')]
intersect(missing_meanedu$idhogar, no_missing_meanedu$idhogar)

#temp = summarise(group_by(missing_meanedu,idhogar),meaneduc=mean(escolari))
temp <- missing_meanedu %>% 
  group_by(idhogar) %>% 
  summarise(meaneduc = mean(escolari))
merge(all,temp,all.x=T,suffixes=c('','.y'),by='idhogar') -> temp1
temp1[is.na(temp1['meaneduc']),'meaneduc'] <- temp1[is.na(temp1['meaneduc']),'meaneduc.y'] 
all <- temp1[,-grep('meaneduc.y',colnames(temp1))]

#check missing variables
sum(is.na(all$meaneduc))

# 3.5 complement SQBmeaned
all[(is.na(all$SQBmeaned)) ,'SQBmeaned'] <- all[(is.na(all['SQBmeaned'])) ,'meaneduc']^2
sum(is.na(all$SQBmeaned))

#4.correct the fault values 
# 4.1dependency, edjefe, edjefa <- change yes/no into 1/0    
table(all$dependency)
table(all$edjefa)
table(all$edjefe)
name <- c('dependency','edjefe','edjefa')

all$dependency[all$dependency == 'yes'] <- 1
all$dependency[all$dependency == 'no'] <- 0
all$edjefe[all$edjefe == 'no'] <- 0
all$edjefe[all$edjefe == 'yes'] <- 1
all$edjefa[all$edjefa == 'no'] <- 0
all$edjefa[all$edjefa == 'yes'] <- 1

#4.2 household Target
# some families menber have different target 

if(FALSE){
  all %>% 
    group_by(idhogar) %>% 
    summarise(Target = n_distinct(Target)) %>% 
    filter(Target != 1) %>% dim
  
  not_uni <- all %>% 
    group_by(idhogar) %>% 
    summarise(Target = n_distinct(Target)) %>% 
    filter(Target != 1)
  
  
  all %>%
    select(idhogar,parentesco1,Target) %>% 
    group_by(idhogar) %>%
    summarise(Target_unique = n_distinct(Target)) %>% 
    filter(Target_unique > 1) %>% dim()
}


#5.output
#delete dis,male,rez_esc-missing,moblie
ind_bool = c('v18q', 'female', 'estadocivil1', 'estadocivil2', 'estadocivil3', 
             'estadocivil4', 'estadocivil5', 'estadocivil6', 'estadocivil7', 
             'parentesco1', 'parentesco2',  'parentesco3', 'parentesco4', 'parentesco5', 
             'parentesco6', 'parentesco7', 'parentesco8',  'parentesco9', 'parentesco10', 
             'parentesco11', 'parentesco12', 'instlevel1', 'instlevel2', 'instlevel3', 
             'instlevel4', 'instlevel5', 'instlevel6', 'instlevel7', 'instlevel8', 
             'instlevel9'  )

ind_ordered = c('rez_esc', 'escolari', 'age')

hh_bool = c('hacdor', 'hacapo', 'v14a', 'refrig', 'paredblolad', 'paredzocalo', 
            'paredpreb','pisocemento', 'pareddes', 'paredmad',
            'paredzinc', 'paredfibras', 'paredother', 'pisomoscer', 'pisoother', 
            'pisonatur', 'pisonotiene', 'pisomadera',
            'techozinc', 'techoentrepiso', 'techocane', 'techootro', 'cielorazo', 
            'abastaguadentro', 'abastaguafuera', 'abastaguano',
            'public', 'planpri', 'noelec', 'coopele', 'sanitario1', 
            'sanitario2', 'sanitario3', 'sanitario5',   'sanitario6',
            'energcocinar1', 'energcocinar2', 'energcocinar3', 'energcocinar4', 
            'elimbasu1', 'elimbasu2', 'elimbasu3', 'elimbasu4', 
            'elimbasu5', 'elimbasu6', 'epared1', 'epared2', 'epared3',
            'etecho1', 'etecho2', 'etecho3', 'eviv1', 'eviv2', 'eviv3', 
            'tipovivi1', 'tipovivi2', 'tipovivi3', 'tipovivi4', 'tipovivi5', 
            'computer', 'television', 'lugar1', 'lugar2', 'lugar3',
            'lugar4', 'lugar5', 'lugar6', 'area1', 'area2', 'v2a1-missing')

hh_ordered = c('rooms', 'r4h1', 'r4h2', 'r4h3', 'r4m1','r4m2','r4m3', 'r4t1',  'r4t2', 
               'r4t3', 'v18q1', 'tamhog','tamviv','hhsize','hogar_nin',
               'hogar_adul','hogar_mayor','hogar_total',  'bedrooms', 'qmobilephone')

hh_cont = c('v2a1', 'dependency', 'edjefe', 'edjefa', 'meaneduc', 'overcrowding')
sqr_ = c('SQBescolari', 'SQBage', 'SQBhogar_total', 'SQBedjefe', 
         'SQBhogar_nin', 'SQBovercrowding', 'SQBdependency', 'SQBmeaned', 'agesq')

bool <- c(hh_bool,ind_bool)
charact <- c('Id','idhogar','Target')
out_data <- all[,!names(all)%in%bool]

datexpr2=as.data.frame(lapply(out_data[,!names(out_data)%in%charact],as.numeric))

out_data[,!names(out_data)%in%charact] <- datexpr2
summary(datexpr2)

write.csv(out_data,file = 'output_cleaned.csv', row.names = F, col.names = T)

#feature engineering--------------------------------------------------------------------------

#1  edjefe edjefa as house load  flag "headmale"
out_data$headmale <- 1
out_data[out_data$edjefa==0,]$headmale <- 0

#2  adults' education years/total members
out_data$aldeduhousemean <- out_data$meaneduc * out_data$hogar_adul / out_data$hhsize

#3  delete mobilephone 
out_data<-out_data%>%select(-c("mobilephone"))

#4-6  "agemeanfamily", "agemeanmale", "agemeanfemale"
feature_age <- out_data %>% select(idhogar,age,male) %>% 
  group_by(idhogar,male) %>% 
  summarise(mean_age = mean(age)) 

feature_age$male_age <- 0
feature_age$female_age <- 0	
M <- feature_age$male == 1
FM <- feature_age$male == 0
feature_age$male_age[M] <- feature_age$mean_age[M]
feature_age$female_age[FM] <- feature_age$mean_age[FM]

tempo <- feature_age %>% 
  select(idhogar,male_age,female_age,mean_age) %>% 
  group_by(idhogar) %>% 
  summarise(agemeanfamily = max(mean_age),
            agemeanmale= max(male_age),
            agemeanfemale = max(female_age))
out_data %>% merge(tempo, all.x = 1)

##7 
full_count = out_data %>%  group_by(idhogar) %>% summarise(n = n())

dis_household = out_data %>%  select_if(is.numeric) %>% 
  cbind(out_data %>% select(idhogar)) %>% 
  select(
    idhogar,
    hhsize,
    r4t1,
    dis
  ) %>% 
  group_by(idhogar) %>% 
  summarise_all((funs( sum(., na.rm = TRUE),  max(.,na.rm = TRUE)))) %>%
  left_join(full_count) %>% 
  mutate(
    disrate=dis_sum/(hhsize_sum - r4t1_sum),
    dishh=dis_max,
  ) %>% 
  mutate_all(funs(ifelse(is.nan(.), NA, .))) %>% 
  mutate_all(funs(ifelse(is.infinite(.), NA, .)))%>%select(idhogar,disrate,dishh)


out_data = out_data %>%
  left_join(dis_household, c("idhogar" = "idhogar"))

#8
out_data = out_data %>% mutate(instlevel_int=escolari/hhsize)
#9
xx  = c("r4t3","tamhog","tamviv","dis")
out_data = out_data[,!names(out_data) %in% xx]

#10
out_data <- out_data %>%
  mutate(
    personperbed=hhsize/bedrooms)
#11
out_data[c('refrig','computer','television')]=all[c('refrig','computer','television')]
#12
out_data = out_data %>% merge(all %>% group_by(idhogar) %>% summarize(instlevel1_count=sum(instlevel1)),all.x=1)
out_data = out_data %>% merge(all %>% group_by(idhogar) %>% summarize(instlevel2_count=sum(instlevel2)),all.x=1)
out_data = out_data %>% merge(all %>% group_by(idhogar) %>% summarize(instlevel3_count=sum(instlevel3)),all.x=1)
out_data = out_data %>% merge(all %>% group_by(idhogar) %>% summarize(instlevel4_count=sum(instlevel4)),all.x=1)
out_data = out_data %>% merge(all %>% group_by(idhogar) %>% summarize(instlevel5_count=sum(instlevel5)),all.x=1)
out_data = out_data %>% merge(all %>% group_by(idhogar) %>% summarize(instlevel6_count=sum(instlevel6)),all.x=1)
out_data = out_data %>% merge(all %>% group_by(idhogar) %>% summarize(instlevel7_count=sum(instlevel7)),all.x=1)
out_data = out_data %>% merge(all %>% group_by(idhogar) %>% summarize(instlevel8_count=sum(instlevel8)),all.x=1)
out_data = out_data %>% merge(all %>% group_by(idhogar) %>% summarize(instlevel9_count=sum(instlevel9)),all.x=1)
#13 generate tempanies
out_data$hhsize_diff <- out_data$tamviv-out_data$hhsize
#14 numbers of appliances
out_data <- out_data %>% mutate(elec_prod = computer + television + refrig )

#15 warning
out_data$warning <- 0
out_data$warning[out_data$sanitario_int ==1] <- out_data$warning[out_data$sanitario_int ==1]-1
out_data$warning[out_data$eletr_int == 3] <- out_data$warning[out_data$eletr_int == 3]-1
out_data$warning[out_data$abasta_int == 3] <- out_data$warning[out_data$abasta_int == 3]-1
out_data$warning[out_data$piso_int ==5] <- out_data$warning[out_data$piso_int ==5]-1
out_data$warning[out_data$cielorazo == 0] <- out_data$warning[out_data$cielorazo == 0]-1

#16 pca

full_pca = prcomp(out_data[,!names(out_data) %in% c('Target','Id','idhogar')], center = TRUE, scale. = TRUE)
# scree plot
fviz_eig(full_pca)

fviz_pca_var(full_pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             select.var = list(contrib = 15), # top 30 contributing
             repel = TRUE     # Avoid text overlapping
)
out_data = out_data %>% 
  cbind(full_pca$x[,1:5])

#17 delete square numbers

demo <- out_data %>% select(starts_with('SQ'))
drop_n <- names(demo)
out_data <- out_data[,!names(out_data)%in% drop_n ]

#18.1 Discretization meaneduc
tp <- vector()
for (i in 1:10){
  tp <- c(tp,quantile(out_data$meaneduc,i/10))
  names(tp) <- NULL
}
tp[1]<- -1
categorical=cut(out_data$meaneduc, tp, right=1) 
out_data$meaneduc <- as.integer(categorical)

#18.2 Discretization aldeduhousemean
tp <- vector()
for (i in 1:10){
  tp <- c(tp,quantile(out_data$aldeduhousemean,i/10))
  names(tp) <- NULL
}
tp[1]<- -1
categorical=cut(out_data$aldeduhousemean, tp, right=1) 
out_data$aldeduhousemean <- as.integer(categorical)

#18.3 Discretization age
tp <- vector()
for (i in 1:10){
  tp <- c(tp,quantile(out_data$age,i/10))
  names(tp) <- NULL
}
tp[1]<- -1
categorical=cut(out_data$age, tp, right=1) 
out_data$age <- as.integer(categorical)

##KNN-------------------------------------------------------------------------##

# 1 standardizing
# min-max
normalize <- function(x){
  return ((x - min(x)) / (max(x) - min(x)))
}
scale_out_data <- as.data.frame(lapply(select(out_data,-c('idhogar','Id','Target')), normalize))
str(scale_out_data)
scale_out_data[c('idhogar','Id','Target')]=out_data[c('idhogar','Id','Target')]

#change Target into Factor
scale_out_data <- scale_out_data %>%
  mutate_at(.vars = vars(Target), .fun = as.factor)

# 2 Feature to be deleted
needless_cols = c('r4t3', 'tamhog', 'tamviv', 'mobilephone','agesq','rez_esc')

# 3 remove feature and Id
#remove feature
scale_out_data = scale_out_data %>% select(-c(needless_cols))

#split model and test data?? reserve id
model_data_id = scale_out_data[!is.na(scale_out_data$Target),]
test_data_id = scale_out_data[is.na(scale_out_data$Target),]
str(scale_out_data)

#remove id
model_data = model_data_id %>% select(-c('idhogar','Id'))
test_data = test_data_id %>% select(-c('idhogar','Id'))

# 4.Sampling
labelindex <- sample(2, nrow(model_data), replace = TRUE, prob = c(0.7, 0.3))
train_set=model_data[labelindex==1,]
test_set=model_data[labelindex==2,]

train_lab=train_set$Target
test_lab=test_set$Target
# 5 F-Score Metric
F_measSummary <- function(pre, ground,lev = NULL, model = NULL) {
  cm = table(data = pre$Target_pre, reference = ground$Target)
  precision <- diag(cm) / rowSums(cm)
  recall <- diag(cm) / colSums(cm)
  f1 <-  ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))
  f1[is.na(f1)] <- 0
  out <- mean(f1)
  names(out) <- "F_meas"
  out
}
# 6 train.kknn
(fit.train <- train.kknn(Target ~ ., train_set, kmax = 15,
                         kernel = c("triangular", "rectangular", "epanechnikov", "optimal"), distance = 2))
# table(predict(fit.train1, test_set), test_set$Target)
pre=test_set
pre['Target_pre'] = predict(fit.train, test_set)
table(pre$Target_pre, test_set$Target)
confusionMatrix(data = pre$Target_pre, test_set$Target)
F_measSummary(pre=pre,ground=test_set)

# 6.submission
test_data['Target']=predict(fit.train, test_data)

submission=cbind.data.frame(test_data_id['Id'],test_data['Target'])
str(submission)

##decision tree---------------------------------------------------------------##
requiredPackages <- c("C50","gmodels", "dplyr")
if (length(setdiff(requiredPackages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(requiredPackages, rownames(installed.packages())), dependencies = TRUE)  
}

#7.real_train
drop_name <- c('Id','idhogar')
out_put  <- read.csv('output.csv')
out_put  <- out_put [,-1]
out_put $Target <- as.factor(out_put $Target)
nw_all_1 <- out_put 
out_put  <- out_put [!names(out_put ) %in% drop_name]

new_train <- out_put [!is.na(out_put $Target),]
new_test <- out_put [is.na(out_put $Target),]
new_test_1 <- nw_all_1[is.na(nw_all_1$Target),]
model_new <- C5.0(new_train[names(new_train) != 'Target'], new_train$Target, trails = 10,
                  control = C5.0Control(
                    subset = 0,
                    bands = 0,
                    winnow = 1,
                    noGlobalPruning = 1,
                    CF = 0.25,
                    minCases = 2,
                    fuzzyThreshold = FALSE,
                    earlyStopping = TRUE,
                  )
)
pred3 <- predict(model_new, new_test)
pred3

new_test_1$Target <- pred3

####
#new_test_1 %>%select(idhogar, Target) %>% 
  #group_by(idhogar) %>% 
  #summarise(Target = n_distinct(Target)) %>% 
  #filter(Target != 1)


submission <- new_test_1 %>% select(Id, Target)

submission$Target <- as.integer(submission$Target)
write.csv(submission,file = 'submission.csv',row.names = FALSE)
##random forest--------------------------------------------------------------##

newdata<-out_data
head(newdata)
str(newdata)

#split train and test
newtrain<-newdata[complete.cases(newdata),]
nrow(newtrain)
head(newtrain)
str(newtrain)
newtest<-newdata[!complete.cases(newdata),]
nrow(newtest)
head(newtest)
str(newtest)
newtrain<-select(newtrain,-Id)
newtrain<-select(newtrain,-idhogar)
head(newtrain)
##Create evaluation function 
F_measSummary <- function(data, lev = NULL, model = NULL) {
  cm = table(data = data$pred, reference = data$obs)
  precision <- diag(cm) / rowSums(cm)
  recall <- diag(cm) / colSums(cm)
  f1 <-  ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))
  f1[is.na(f1)] <- 0
  out <- mean(f1)
  names(out) <- "F_meas"
  out
}
#target change into factor

newtrain$Target <- as.factor(newtrain$Target)
table(newtrain$Target)

# Data Partition: 70 training and 30 test
set.seed(123)

# Randomly generate label index, with 1 training and 2 test
labelindex <- sample(2, nrow(newtrain), replace = TRUE, prob = c(0.7, 0.3))


table(labelindex)
# We will sample from 1 and 2 with replacement, 
# with prob 0.7 for 1 (training data), 03 for 2 (test data)
rf_train <- newtrain[labelindex==1,]
rf_test <- newtrain[labelindex==2,]
str(rf_train)
str(rf_test)
plot(rf_train$Target,col='red')
#subsampling
set.seed(9560)
up_train <- upSample(x = rf_train[, -ncol(rf_train)],
                     y = rf_train$Target)                         
table(up_train$Target) 
plot(up_train$Target, col = "blue")

down_train <- downSample(x = rf_train[, -ncol(rf_train)],
                         y = rf_train$Target)                         
table(down_train$Target) 
plot(down_train$Target, col = "green")
#use Random Forest model

rf<-randomForest(Target~.,data=rf_train)
print(rf)

# variables importance
varImpPlot(rf,
           sort = T,
           main =  "Variable Importance")

#feature selection
# Remove low variance features

#near_zero_variance = nearZeroVar(
  #x = rf_train, 
  #freqCut = 99/1, 
  #uniqueCut = 10, 
  #saveMetrics = FALSE)

#out_light = rf_train[,-near_zero_variance]

#str(out_light)
# Remove highly correlated variables

#out_cor = cor(out_light)
#out_cor[is.na(out_cor)] <- 0

#near_perfect_correlation = findCorrelation(
#x = out_cor,
#cutoff = 0.99,
#verbose = FALSE

#)
#out_light = out_light[,-near_perfect_correlation]
# Grab the right columns

#cols_to_keep = names(out_light)
#rf_train = rf_train %>% select(one_of(cols_to_keep))

xx  = c("meaneduc","dependency","overcrowding","SQBedjefe","agesq","hogar_nin")
trainset = rf_train[,!names(rf_train) %in% xx]
str(trainset)

#tuning and upsampling For Class Imbalances#
fitControl <- trainControl(method = "cv", 
                           number = 5,
                           sampling = "up",
                           summaryFunction = F_measSummary)

orifitControl <- trainControl(method = "cv", 
                              number = 5,
                              summaryFunction = F_measSummary)
set.seed(123)
#grid <- expand.grid(mtry=c(8,16,32,65))#
#after tuning,we choose mtry=32#

grid <- expand.grid(mtry=c(32))
ori_rf<-train(Target ~ ., data=trainset, 
              method = "rf", 
              metric = "F_meas",
              trControl = orifitControl,
              tuneGrid=grid)
ori_rf
randomforest_train<- train(Target ~ ., data=trainset, 
                           method = "rf", 
                           metric = "F_meas",
                           trControl = fitControl,
                           tuneGrid=grid)
randomforest_train


#Prediction in test
ori_test<-predict(ori_rf,rf_test)
prediction_test <- predict(randomforest_train,rf_test)

head(prediction_test)

#ground truth
head(rf_test$Target)
confusionMatrix(ori_test, rf_test$Target)
confusionMatrix(prediction_test, rf_test$Target)
#ROC curve
#install.packages('pROC')
prediction_test<-as.numeric(prediction_test)
head(prediction_test)
modelroc<- roc(rf_test$Target,prediction_test)

plot(modelroc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"),smooth=TRUE, max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)
#ROC curve
#install.packages('pROC')
prediction_test<-as.numeric(prediction_test)
ori_test<-as.numeric(ori_test)
head(prediction_test)
roc1<- roc(rf_test$Target,prediction_test)
roc2<-roc(rf_test$Target,ori_test)
plot(roc1, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE,smooth=TRUE)
plot(roc2, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE,smooth=TRUE)
plot.roc(roc2,add=T,col="yellow",print.auc=TRUE,print.auc.x=0.3,print.auc.y=0.3)

#final Prediction
randomforest_final<- train(Target ~ ., data=newtrain, 
                           method = "rf", 
                           metric = "F_meas",
                           trControl = fitControl,
                           tuneGrid=grid)
prediction_final <- predict(randomforest_final,newtest)
newtest$Target <-prediction_final

#Prepare submission file

submission <- newtest %>% select(Id,Target)

head(submission)
#Write results 
write_csv(submission,"submission.csv")

##take too much times!fit control of caret
#fitControl <- trainControl(method = "repeatedcv", 
# number = 10,repeats=10,
# summaryFunction = F_measSummary)

#Boosting-----------------------------------------------------------------------#
train_test = out_data
#str(train_test)
test=train_test[is.na(train_test$Target),]
train=train_test[(!(is.na(train_test$Target)))]
#train=train_test[(!(is.na(train_test$Target)))&(train_test$parentesco_int==1),]#training with only the head of each household
#divide train validate test
set.seed(1234)

rnd <- sample(dim(train)[1])

trainRatio <- 0.7

trainIndex <- rnd[c(1:(trainRatio*dim(train)[1]))]

train <- train[trainIndex, ]

val <- train[-trainIndex, ] 
#XGBoost&LGBM---------------------------------------------------------------------------------------------
# Xgboost with 100 rounds

exclude=c("Id", "Target","idhogar","agesq"
)
dval<- xgb.DMatrix(data = as.matrix(val%>%select(-exclude)))
dtrain <- xgb.DMatrix(data = as.matrix(train%>%select(-exclude)), 
                      label = as.numeric(train$Target-1))
parameters <- list(
  # General Parameters
  booster            = "gbtree",      
  verbosity          = 1,           
  # Booster Parameters
  eta                = 0.1,              
  #   gamma              = 0.7,                 
  max_depth          = 6,                
  #   min_child_weight   = 2,            
  #   subsample          = .9,                 
  #   colsample_bytree   = .5,                
  #   colsample_bylevel  = 1,          
  #   lambda             = 1,    
  #   alpha              = 0,       
  # Task Parameters
  objective          = "multi:softmax",   # default = "reg:linear"
  eval_metric        = "merror",
  num_class          = 4,
  #seed               = 1,               # reproducability seed
  #   tree_method = "hist",
  grow_policy = "lossguide"
)
tic("Total time for xgboost training --->")
xgb_model <- xgb.train(parameters, dtrain, nrounds = 100)
toc()
xgb_pred <- predict(xgb_model, dval)

confusionMatrix(as.factor(xgb_pred+1), as.factor(val$Target))
# predict and submission

my_preds <- predict(xgb_model, as.matrix(test %>%select(-exclude)),reshape = TRUE)
# <- predict(model, as.matrix(test %>%select(-c("Id", "Target","idhogar"))),rawscore = TRUE, reshape = TRUE)
# 
# class(my_preds)
# my_preds = as.data.frame(my_preds)
# my_preds = apply(my_preds,1,which.max)

submission = data.frame(Id=test$Id, Target=my_preds+1)
str(submission)

# write.csv(submission,"submission.csv",row.names=FALSE)
# feature importance
xgb_imp = xgb.importance(model = xgb_model, feature_names = names(train))
xgb.ggplot.importance(importance_matrix = xgb_imp,top_n=20)


#rm(xgb_model, xgb_pred, dtrain, dval, parameters, my_preds)

# lightGBM with all features
exclude=c("Id", "Target","idhogar","agesq"
)
dtrain <- lgb.Dataset(data = as.matrix(train%>%select(-exclude)), 
                      label = as.numeric(train$Target-1))

params <- list(objective = "multiclass", metric = "multi_error", num_class = 4)
tic("Total time for Lightgbm training --->")
lgb_model <- lgb.train(
  params,
  dtrain,
  nrounds=100,
  min_data = 20, 
  learning_rate = 0.1,
  verbose = 1,
  num_leaves=31,
  class_weight='balanced',
)
toc()
#performance on validate set 
lgb_pred <- predict(lgb_model, data.matrix(val %>% select(-exclude)),reshape = TRUE)

lgb_pred <- lgb_pred %>% as.data.frame() %>% apply(.,1,which.max)


confusionMatrix(as.factor(lgb_pred), as.factor(val$Target))

#predict
my_preds <- predict(lgb_model, as.matrix(test %>%select(-exclude)),reshape = TRUE)

#convert results from probability of each class into class labels
class(my_preds)
my_preds = as.data.frame(my_preds)
my_preds = apply(my_preds,1,which.max)

submission = data.frame(Id=test$Id, Target=my_preds)
# #submission
# write.csv(submission,"submission.csv",row.names=FALSE)

#feature importance

lgb_imp <- lgb.importance(lgb_model, percentage = TRUE)

lgb.plot.importance(lgb_imp, measure = "Gain",top_n=20)
# lightGBM with top 20 features
include=c("aldeduhousemean","age","meaneduc","v2a1","instlevel_int","overcrowding","rooms",
          "dependency","escolari","pared_int","lugar_int","qmobilephone","estadocivil_int",
          "disrate","eviv_int","edjefe","etecho_int","edjefa","v18q1","piso_int")
dtrain <- lgb.Dataset(data = as.matrix(train%>%select(include)), 
                      label = as.numeric(train$Target-1))

params <- list(objective = "multiclass", metric = "multi_error", num_class = 4)
lgb_model <- lgb.train(
  params,
  dtrain,
  nrounds=200,
  min_data = 1, 
  learning_rate = 0.1,
  verbose = 0,
  class_weight='balanced',
  
)

#performance on validate set 
lgb_pred <- predict(lgb_model, data.matrix(val %>% select(include)),reshape = TRUE)

lgb_pred <- lgb_pred %>% as.data.frame() %>% apply(.,1,which.max)
confusionMatrix(as.factor(lgb_pred), as.factor(val$Target))

#predict
my_preds <- predict(lgb_model, as.matrix(test %>%select(include)),reshape = TRUE)

#convert results from probability of each class into class labels
class(my_preds)
my_preds = as.data.frame(my_preds)
my_preds = apply(my_preds,1,which.max)

#feature importance

lgb_imp <- lgb.importance(lgb_model, percentage = TRUE)

lgb.plot.importance(lgb_imp, measure = "Gain",top_n=10)
# lightGBM with top 20 features
include=c("aldeduhousemean",#adults' education/total members
          "instlevel_int",#education lever
          "overcrowding",#person per room
          "dependency",#non labor force/labor force
          "age"
          ,"v2a1",#rent
          "rooms",
          "lugar_int",#house location
          "qmobilephone",#mobile numbers
          "pared_int"#Exterior wall
)

dtrain <- lgb.Dataset(data = as.matrix(train%>%select(include)), 
                      label = as.numeric(train$Target-1))

params <- list(objective = "multiclass", metric = "multi_error", num_class = 4)

lgb_model <- lgb.train(
  params,
  dtrain,
  nrounds=200,
  min_data = 1, 
  learning_rate = 0.1,
  verbose = 0,
  class_weight='balanced',
  
)
#performance on validate set 
lgb_pred <- predict(lgb_model, data.matrix(val %>% select(include)),reshape = TRUE)

lgb_pred <- lgb_pred %>% as.data.frame() %>% apply(.,1,which.max)


confusionMatrix(as.factor(lgb_pred), as.factor(val$Target))

#predict
my_preds <- predict(lgb_model, as.matrix(test %>%select(include)),reshape = TRUE)
# <- predict(model, as.matrix(test %>%select(-c("Id", "Target","idhogar"))),rawscore = TRUE, reshape = TRUE)

#convert results from probability of each class into class labels
class(my_preds)
my_preds = as.data.frame(my_preds)
my_preds = apply(my_preds,1,which.max)
# get feature importance
fi = lgb.importance(lgb_model, percentage = TRUE)

highchart() %>%
  hc_title(text = "Feature importance by Cover, Gain and Frequency") %>%
  hc_xAxis(categories = fi$Feature) %>%
  hc_add_series(name = "Cover", data = fi, type = "bar", hcaes(x = Feature, y = Cover)) %>%
  hc_add_series(name = "Gain", data = fi, type = "bar", hcaes(x = Feature, y= Gain)) %>%
  hc_add_series(name = "Frequency", data = fi, type = "bar", hcaes(x = Feature, y = Frequency)) %>%
  hc_add_theme(hc_theme_538())
