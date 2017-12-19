library(mongolite)
library(ggplot2)

URI = 'mongodb://127.0.0.1:27017'

clean_data <- mongo('zcool', url = URI, collection = 'clean_data')

UIDesigners <- clean_data$find('{"job": "UI设计师"}')
UIShangHai <- clean_data$find('{"job": "UI设计师", "livenow.0": "上海"}')

male_ana <- ggplot(UIDesigners, aes(x=UIDesigners$male)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label=..count..), vjust=-1, hjust=0.5) +
  labs(x="性别", y="数值", title="UI设计师性别分布表") +
  theme(text = element_text(family = 'w', size = 10), plot.title = element_text(hjust = 0.5))





