library(tidyverse)
library(dplyr)
library(VIM)  #缺失值处理
library(readr)  #读取数据
library(ggplot2)
library(mice)

#1.导入数据
train = read.csv("train.csv")
test = read.csv("test.csv")
all = bind_rows(train,test)

#all[,!names(all) %in% c('Target')]
#write.csv(all,file = 'all.csv', row.names = F, col.names = T)

#2.将one-hot转成分类变量
#2.1列出现成的要修改的特征(不包括电力)
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
#观察新进来几个变量
feature_sum <- 0
for(i in 1:length(feature_list)){
  
  # Grab the feature
  feature_to_fix = all %>% select(starts_with(feature_list[i]))
  feature_sum <- feature_sum + length(feature_to_fix)
  # Fix and enter into our new feature matrix
  
  
  #max.col(feature_to_fix)表示找到有数字的那一列，因为是one-hot编码，所有最大值就是有值的
  #names(feature_to_fix)[max.col(feature_to_fix)]表示取出有值的地方的名字
  #factor将这些名字变成因子类型，便于分类。 factor 的ordered = TRUE 会在出下面这个大小关系
  #paredblolad < paredzocalo < paredpreb < pareddes < paredmad < paredzinc < ... < paredother
  # levels = name(feture_to_fix_1) 表示按照原始的顺序编码，不然会变成用字母大小顺序编码
  #as.integer 最后将这些这些factor变成数字型
  #最后将编好号码的数据传回new matrix
  new_features_integer[,i] = as.integer(factor(names(feature_to_fix)[max.col(feature_to_fix)], ordered = FALSE,
                                               levels = names(feature_to_fix)))
  #将新特征矩阵命名
  names(new_features_integer)[i] = paste0(feature_list[i],"_int")
  #收集起来
  ohe_names = c(ohe_names, as.vector(names(feature_to_fix)))
  
}
head(new_features_integer)

new_features_integer = data.frame(apply(new_features_integer,2,factor))
all <- all %>% bind_cols(new_features_integer)

#2.2将电力特征改成label feature
#强行改（被注释掉了）
if (FALSE){
  origin_name = c('public','planpri','noelec','coopele')
  feature_ele = c('ele1','ele2','ele3','ele4')
  ele_matrix = data.frame(matrix(ncol = length(1), nrow = nrow(all)))
  for (i in 1:4){
    all[,paste('ele',i,sep = '')] <- all[,origin_name[i]]
  }
  
  feature_to_ele = all %>% select(starts_with('ele'))
  ele_matrix[,1] = as.integer(factor(names(feature_to_ele)[max.col(feature_to_ele)], ordered = FALSE,
                                     levels = names(feature_to_ele)))
}

#灵活改
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
#找到不在这个区间的空值
L <- ((all$age >19 | all$age <7 )& is.na(all$rez_esc))
all[L,"rez_esc"] <- 0

#查看这个区间的空值的年龄特点
idx = which(is.na(all$rez_esc))
table(all[idx,'age'])
all[idx,"rez_esc"] <- all[idx,'age']-all[idx,'escolari']-7

all[which(all$rez_esc < 0 & all$age <18),c('age','escolari')]
table(all$rez_esc)
#（原来：小于0变成0）实际上不需要变成0
#all[which(all$rez_esc < 0),] <- 0

#判断是否有缺失，以及缺失数
all$rez_esc[is.na(all$rez_esc)]
#sum(is.na(all$v2a1))

#设置新变量new_rec_esc，用公式修改
L1 <- (all$age > 7 & all$age < 19)
all$new_rez_esc <- all$rez_esc
all[L1,'new_rez_esc'] <- all[L1,'age'] - all[L1,'escolari'] -7
# 等价于 all$new_rez_esc[L1] <- all$age[L1] - all$escolari[L1] -7
table(all$new_rez_esc - all$rez_esc)

sum(is.na(all$rez_esc))

#3.2 v18q1
#number of tablets household owns
#发现v18q1缺失的全部都是v18q为0的值
all[is.na(all$v18q1) & all$v18q != 0,c('v18q','v18q1')]

#填充
all$v18q1[is.na(all$v18q1)] <- 0

#观察
#aggr(all,plot =0)

#3.3 v2a1 
#Monthly rent payment
all[is.na(all$v18q1) & all$v18q != 0,c('v18q','v18q1')]

v2a1 <- all %>% select(starts_with('tipo'))
ggplot(all[all$v2a1 %>% is.na,], aes(x=tipovivi_int)) +
  geom_bar(position="stack") 

#可以看到房子拥有类型相关,1表示已拥有房子，4表示precarious(危楼?)，5表示指派的或者借的
all$v2a1[is.na(all$v2a1)& (all$tipovivi_int == 1 | all$tipovivi_int == 5) ] <- 0
all$v2a1[all$tipovivi_int == 4] <- 0.5

sum(is.na(all$v2a1))
#3.4meandeduc

#aggr(all,plot =0)
#table(all$meaneduc)
#name = c('escolari','rez_esc','edjefa','edjefe','instlevel_int')
#all[all$meaneduc %>% is.na(),name] %>% head()

#####################################################################################################
#####待探索问题，限制了18岁条件之后，会有三个NA值没被修正，先不管它。**********
#####################################################################################################
missing_meanedu = all[all$meaneduc %>% is.na() ,c('idhogar', 'escolari')]
no_missing_meanedu = all[!all$meaneduc %>% is.na() ,c('idhogar', 'escolari')]
intersect(missing_meanedu$idhogar, no_missing_meanedu$idhogar)

#因此,对household of records of which meaneduc= NULL求均值, 关联至原表

#temp = summarise(group_by(missing_meanedu,idhogar),meaneduc=mean(escolari))
temp <- missing_meanedu %>% 
  group_by(idhogar) %>% 
  summarise(meaneduc = mean(escolari))
merge(all,temp,all.x=T,suffixes=c('','.y'),by='idhogar') -> temp1
temp1[is.na(temp1['meaneduc']),'meaneduc'] <- temp1[is.na(temp1['meaneduc']),'meaneduc.y'] 
all <- temp1[,-grep('meaneduc.y',colnames(temp1))]

#查无空值
sum(is.na(all$meaneduc))

# 3.5 补充SQBmeaned
all[(is.na(all$SQBmeaned)) ,'SQBmeaned'] <- all[(is.na(all['SQBmeaned'])) ,'meaneduc']^2
sum(is.na(all$SQBmeaned))

#4.修正错误值
# 4.1dependency, edjefe, edjefa <- 替换yes/no为 1/0    
table(all$dependency)
table(all$edjefa)
table(all$edjefe)
name <- c('dependency','edjefe','edjefa')
#####################################################################################################
#出大问题
#循环在里面改变不了值，但不用循环可以
#####################################################################################################
if(FALSE){
  for (i in length(name)){
    ifelse(all[,name[i]] == 'yes', all[all[,name[i]] == 'yes',name[i]] <- 1, ifelse(
      all[all[,name[i]] == 'no',name[i]] =='no',all[,name[i]] <-0, all[,name[i]]
    ))
  }
}
all$dependency[all$dependency == 'yes'] <- 1
all$dependency[all$dependency == 'no'] <- 0
all$edjefe[all$edjefe == 'no'] <- 0
all$edjefe[all$edjefe == 'yes'] <- 1
all$edjefa[all$edjefa == 'no'] <- 0
all$edjefa[all$edjefa == 'yes'] <- 1

#4.2 屋主Target
#同家庭target不同 
#先不管
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


#5.导出
#删掉了dis,male,rez_esc-missing,moblie
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

#起来试试这个
datexpr2=as.data.frame(lapply(out_data[,!names(out_data)%in%charact],as.numeric))

out_data[,!names(out_data)%in%charact] <- datexpr2
summary(datexpr2)

#这里要修改数据类型但是怎么都修改不了（已修复）
#lapply(out_data[,!names(out_data)%in%charact], as.numeric)
write.csv(out_data,file = 'output_cleaned.csv', row.names = F, col.names = T)

#feature engineering--------------------------------------------------------------------------

#1  edjefe edjefa 当家长 flag “headmale
out_data$headmale <- 1
out_data[out_data$edjefa==0,]$headmale <- 0

#2  成人总教育年数/总人数
out_data$aldeduhousemean <- out_data$meaneduc * out_data$hogar_adul / out_data$hhsize

#3  mobilephone 删掉
out_data<-out_data%>%select(-c("mobilephone"))

# 13 新增暂住人数
out_data$hhsize_diff <- out_data$tamviv-out_data$hhsize

#4-6  家庭平均年龄 “agemeanfamily”  家庭男平均年龄 “agemeanmale”  家庭女平均年龄 “agemeanfemale”
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