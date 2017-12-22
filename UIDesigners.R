library(mongolite)
library(ggplot2)
library(jiebaRD)
library(jiebaR)

URI = 'mongodb://127.0.0.1:27017'

clean_data <- mongo('zcool', url = URI, collection = 'clean_data')

# 一线二线城市变量
UIDesigners <- clean_data$find('{"job": "UI设计师"}')
UIShangHai <- clean_data$find('{"job": "UI设计师", "livenow.0": "上海"}')
UIBeijing <- clean_data$find('{"job": "UI设计师", "livenow.0": "北京"}')
UITianjin <- clean_data$find('{"job": "UI设计师", "livenow.0": "天津"}')
UIGuangzhou <- clean_data$find('{"job": "UI设计师", "livenow.1": "广州"}')
UIShenzhen <- clean_data$find('{"job": "UI设计师", "livenow.1": "深圳"}')
UICityNo1 <- clean_data$find('{"livenow.1":{"$in": ["上海市", "北京市","天津市", "广州", "深圳"]} , "job": "UI设计师"}')
UICityNo2 <- clean_data$find('{"livenow.1":{"$in": ["杭州", "南京","济南", "重庆", "青岛", "大连", "宁波", "厦门"]} , "job": "UI设计师"}')

# 根据工作种类和城市名称筛选表
JobCityFromMongo <- function(job, city_name){
  clean_data$find(sprintf('{"job": "%s", "livenow.1": "%s"}', job, city_name))
}

male_ana <- ggplot(UIDesigners, aes(x=UIDesigners$male)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label=..count..), vjust=-1, hjust=0.5) +
  labs(x="性别", y="数值", title="UI设计师性别分布表") +
  theme(text = element_text(family = 'Wawati SC', size = 10), plot.title = element_text(hjust = 0.5))

weighted.mean(UICityNo1$stastic$hot)
weighted.mean(UICityNo1$stastic$fans)
weighted.mean(UICityNo1$stastic$work_total)

weighted.mean(UICityNo2$stastic$hot)
weighted.mean(UICityNo2$stastic$fans)
weighted.mean(UICityNo2$stastic$work_total)

ggplot(UIDesigners, aes(x=UIDesigners$stastic$fans, y=UIDesigners$stastic$work_total)) + 
  geom_point() +
  geom_text(aes(label=ifelse(stastic$fans>30000, as.character(username), "")), hjust=0, vjust=0) +
  geom_smooth(method = lm) +
  labs(x="粉丝数", y='人气值', title="设计师粉丝数和人气值") + 
  theme(text = element_text(family = 'Hei', size = 10), plot.title = element_text(hjust = 0.5))

# 分组计算一线城市的加权平均值
ShanghaiPercent <- nrow(UIShangHai)/nrow(UICityNo1)
BeijingPercent <- nrow(UIBeijing)/nrow(UICityNo1)
TianjinPercent <- nrow(UITianjin)/nrow(UICityNo1)
GuangzhouPercent <- nrow(UIGuangzhou)/nrow(UICityNo1)
ShenzhenPercent <- nrow(UIShenzhen)/nrow(UICityNo1)

ShanghaiHotMean <- mean(UIShangHai$stastic$hot)
BeijingHotMean <- mean(UIBeijing$stastic$hot)
TianjinHotMean <- mean(UITianjin$stastic$hot)
GuangzhouHotMean <- mean(UIGuangzhou$stastic$hot)
ShenzhenHotMean <- mean(UIShenzhen$stastic$hot)

weighted.mean(c(ShanghaiHotMean, BeijingHotMean, TianjinHotMean, GuangzhouHotMean, ShenzhenHotMean), 
              c(ShanghaiPercent, BeijingPercent, TianjinPercent, GuangzhouPercent, ShenzhenPercent))

# 分组计算二线城市加权平均值
UIHangzhou  <- JobCityFromMongo('UI设计师', '杭州')
UINanjing   <- JobCityFromMongo('UI设计师', '南京')
UIJinan     <- JobCityFromMongo('UI设计师', '济南')
UIChongqing <- JobCityFromMongo('UI设计师', '重庆市')
UIQingdao   <- JobCityFromMongo('UI设计师', '青岛')
UIDalian    <- JobCityFromMongo('UI设计师', '大连')
UINingbo    <- JobCityFromMongo('UI设计师', '宁波')
UIXiamen    <- JobCityFromMongo('UI设计师', '厦门')

GroupWeightedMean <- function(...){
  args_list <- list(...);
  args_len <- length(args_list);
  verctor_percent <- c();
  verctor_weightmean <- c();
  for(i in 1:args_len){
    percent_city <- nrow(args_list[[i]])/nrow(UICityNo2);
    verctor_percent[i] <- percent_city;
    weightmean_city <- mean(args_list[[i]]$stastic$hot);
    verctor_weightmean[i] <- weightmean_city;
    
  }
  return(weighted.mean(verctor_weightmean, verctor_percent));
}

GroupWeightedMean(UIHangzhou, UINanjing, UIJinan, UIChongqing, UIQingdao, UIDalian, UINingbo, UIXiamen)


# 
ggplot(UIDesigners, aes(x=UIDesigners$stastic$work_total, y=UIDesigners$stastic$hot)) + 
  geom_point() +
  geom_text(aes(label=ifelse(stastic$fans>30000, as.character(username), "")), hjust=0, vjust=0) +
  geom_smooth(method = lm) + 
  xlim(0, 100) +
  ylim(0, 100000) +
  labs(x="作品数", y='人气值', title="UI设计师作品和人气值关系") + 
  theme(text = element_text(family = 'Hei', size = 10), plot.title = element_text(hjust = 0.5))


# 院校和人气值的关系
subset(UIDesigners, stastic$hot>1000000 & educated != NaN, select = c('educated', 'username'))


wk = worker()
wk["我觉得楼栋数据相关的工作我也可以，哈哈哈哈"]






