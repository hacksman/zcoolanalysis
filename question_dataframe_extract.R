

# 原始数据生成是示例
test_list_shenzhen <- c('广东', '深圳')
test_list_dongguan <- c('广东', '东莞')

test_city <- list(test_list_shenzhen, test_list_shenzhen, test_list_dongguan)
test_demo <- data.frame(name=c('Jack', 'Tom', 'John'), age=c(22, 23, 22))
test_demo$livenow <- I(test_city)



# 如何根据livenow当中的条件进行提取元素，比如我想提取livenow为广东深圳的两条数据

# 我根据网上的找来的subset函数进行提取元素，但是subset对于==固定的值会比较好用，貌似不能够提取list元素
subset(test_demo, livenow==c('广东', '深圳'), select = c('name', 'age', 'livenow'))

# 我后来尝试使用livenow内部的元素
test_aaa <- subset(test_demo, livenow[[2]]=='深圳', select = c('name', 'age', 'livenow'))

# 默认提取了一条，我又尝试提取age=22的数值，进行尝试，发现没有问题
subset(test_demo, age==22, select = c('name', 'age', 'livenow'))


