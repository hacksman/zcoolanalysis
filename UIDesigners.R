library(mongolite)
library(ggplot2)

URI = 'mongodb://127.0.0.1:27017'

clean_data <- mongo('zcool', url = URI, collection = 'clean_data')

UIDesigners <- clean_data$find('{"job": "UI设计师"}')
UIShangHai <- clean_data$find('{"job": "UI设计师", "livenow.0": "上海"}')
UIBeijing <- clean_data$find('{"job": "UI设计师", "livenow.0": "北京"}')
UITianjin <- clean_data$find('{"job": "UI设计师", "livenow.0": "天津"}')
UIGuangzhou <- clean_data$find('{"job": "UI设计师", "livenow.1": "广州"}')
UIShenzhen <- clean_data$find('{"job": "UI设计师", "livenow.1": "深圳"}')
UICityNo1 <- clean_data$find('{"livenow.1":{"$in": ["上海市", "北京市","天津市", "广州", "深圳"]} , "job": "UI设计师"}')
UICityNo2 <- clean_data$find('{"livenow.1":{"$in": ["杭州", "南京","济南", "重庆", "青岛", "大连", "宁波", "厦门"]} , "job": "UI设计师"}')

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

ggplot(UIDesigners, aes(x=UIDesigners$stastic$fans, y=UIDesigners$stastic$hot)) + 
  geom_point() +
  geom_smooth(method = lm) +
  labs(x="粉丝数", y='人气值', title="设计师粉丝数和人气值") + 
  theme(text = element_text(family = 'Wawati SC', size = 10), plot.title = element_text(hjust = 0.5))


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

print(ShanghaiHotSum * ShanghaiPercent)
print(BeijingHotSum * BeijingPercent)

print(c(ShanghaiHotSum, BeijingHotSum, ShanghaiPercent, BeijingPercent))

# 分组计算二线城市加权平均值
subset(UICityNo2, livenow==c('江苏', '南京'), select = c("job"))

print(class(UICityNo2$livenow))


UInanjing <- UICityNo2[UICityNo2$livenow == c('江苏', '南京'), ]













