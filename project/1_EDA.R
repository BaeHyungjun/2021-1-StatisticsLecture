# 1_EDA


# package loading

library(dlookr) # diagnose
library(tidyverse) # dplyr, ggplot
library(GGally) # ggpairs
library(gridExtra) # grid.arrange


# 데이터 불러오기

card_dataset = read_csv('./open/train.csv')
data = card_dataset

test_dataset = read_csv('./open/test.csv')

# 데이터 전처리

data %>% str()

data %>% diagnose()

prepare_data = function(df, type = 'train'){
    
    ### 종속변수 형태 변환, type : train or test
    if (type == 'train'){
        df = df %>% mutate(credit = as.factor(credit))
    } else if (type == 'test'){
        df = df %>% mutate(credit = '999')
    }
    
    ### 질적 변수 전처리
    # gender : as.factor
    # car : as.factor
    # reality : as.factor
    # income_type : as.factor
    # edu_type : as.factor, ordered factor
    # family_type : as.factor
    # house_type : as.factor
    # FLAG_MOBIL : 모든 데이터가 1이기 때문에 삭제
    # work_phone : as.factor
    # phone : as.factor
    # email : as.factor
    # occyp_type : 1/3이 결측값이라 삭제
    df = df %>% mutate(gender = as.factor(gender),
                       car = as.factor(car),
                       reality = as.factor(reality),
                       income_type = as.factor(income_type),
                       edu_type = ordered(edu_type,
                                          levels = c('Academic degree',
                                                     'Higher education',
                                                     'Incomplete higher',
                                                     'Secondary / secondary special',
                                                     'Lower secondary')),
                       family_type = as.factor(family_type),
                       house_type = as.factor(house_type),
                       work_phone = as.factor(work_phone),
                       phone = as.factor(phone),
                       email = as.factor(email)) %>% 
                select(-occyp_type, -FLAG_MOBIL)
    
    ### 양적 변수 전처리
    # child_num : 0, 1, 2, 3~4, 5, more_than_6으로 범주화하여 child_num_category 변수로 대체
    # DAYS_BIRTH : 절댓값을 365로 나눈 몫으로 age변수를 만들어 대체
    # DAYS_EMPLOYED : 양수인 값은 0으로, (음수인 값은 절댓값을 365로 나눈 몫) + 1으로 career_year 변수를 만들어 대체
    # family_size : child_num과 비슷한 경향을 가지지만 child_num은 범주화를 했기 때문에 연속형으로 남겨두겠음
    # parent_num : 가정 내 부모의 수, family_size - child_num으로 계산, 0 이하의 수(자식이 있지만 혼자 사는 경우)는 0으로 변경
    # begin_month : 절댓값 함수를 이용해 양수로 변경
    df = df %>% mutate(child_num_category = ordered(ifelse(child_num == 0, 'zero',
                                                   ifelse(child_num == 1, 'one',
                                                          ifelse(child_num == 2, 'two',
                                                                 ifelse(child_num %in% c(3, 4), 'three_or_four',
                                                                        ifelse(child_num == 5, 'five', 'more_than_6'))))),
                                                   levels = c('zero',
                                                              'one',
                                                              'two',
                                                              'three_or_four',
                                                              'five',
                                                              'more_than_6')),
                       age = abs(DAYS_BIRTH) %/% 365,
                       career_year = ifelse(DAYS_EMPLOYED >= 0, 0, (abs(DAYS_EMPLOYED) %/% 365) + 1),
                       parent_num = ifelse(family_size - child_num > 0, family_size - child_num, 0),
                       begin_month = abs(begin_month)) %>% 
                select(-child_num, -DAYS_BIRTH, -DAYS_EMPLOYED)
    
    return(df)
}

pre_data = prepare_data(data, type = 'train')
pre_test_data = prepare_data(test_dataset, type = 'test')

pre_data %>% diagnose()

# 전처리된 데이터로 검정 및 시각화하여 전처리가 잘 됐는지 확인
# 독립변수 vs 종속변수로 시각화하여 전처리 방향이 맞는지 확인
# 독립변수 + 독립변수 vs 종속변수로 시각화하여 상호작용이 존재하는지 확인

one_variable_chisq_test = function(df, var1, var2){
    
    ### 빈도표, 카이제곱검정 결과 출력
    result_table = df %>% select(var1, var2) %>% table()
    result_prop_table = result_table %>% apply(1, function(x){x / sum(x)}) %>% t()
    result_test = result_table %>% chisq.test()

    return(list(table = result_table, 
                prop_table = result_prop_table,
                test = result_test))
}

barplot_factor_variable = function(df, var1, var2, text = TRUE){
    
    ### 두 범주형 변수의 빈도수 막대그래프 출력
    graph = df %>% 
        select(var1, var2) %>% 
        group_by_all() %>% 
        summarise(count = n()) %>% 
        mutate(prop = paste0(100*round(count / sum(count), 2), '%')) %>% 
        ggplot(aes_string(x = var1, y = 'count', fill = var2)) + 
        geom_bar(stat = 'identity', position = position_dodge()) + 
        geom_text(aes(label = prop), position = position_dodge(0.9), vjust = -0.25, size = 3.5) + 
        ggtitle(paste0('Barplot of ', var1, ' and ', var2)) +
        theme(plot.title = element_text(size = 15, hjust = 0.5))
        
    if (text == TRUE){
        graph = graph +
            geom_text(aes(label = count), position = position_dodge(0.9), vjust = 1.2, size = 4)
    }
    
    return(graph)
}

# # 독립변수 중 질적변수와 종속변수의 관계 시각화
# 
# pre_data %>% one_variable_chisq_test('car', 'credit') # 유의함
# pre_data %>% barplot_factor_variable('car', 'credit')
# 
# pre_data %>% one_variable_chisq_test('reality', 'credit') # 유의함
# pre_data %>% barplot_factor_variable('reality', 'credit')
# 
# pre_data %>% one_variable_chisq_test('income_type', 'credit') # 유의함
# pre_data %>% barplot_factor_variable('income_type', 'credit', text = FALSE)
# 
# pre_data %>% one_variable_chisq_test('house_type', 'credit') # 유의함
# pre_data %>% barplot_factor_variable('house_type', 'credit', text = FALSE)
# 
# pre_data %>% one_variable_chisq_test('child_num_category', 'credit') # 유의함
# pre_data %>% barplot_factor_variable('child_num_category', 'credit', text = FALSE)
# 
# 
# # 독립변수 중 연속형 변수와 종속변수의 관계 시각화
# 
# pre_data %>% 
#     ggplot(aes(x = income_total, fill = credit)) + 
#     geom_density(alpha = 0.3)
# 
# pre_data %>% 
#     ggplot(aes(x = credit, y = income_total)) + 
#     geom_violin(alpha = 0.3)
# 
# pre_data %>% 
#     ggplot(aes(x = credit, y = family_size)) + 
#     geom_violin(alpha = 0.3)
# 
# pre_data %>% 
#     ggplot(aes(x = credit, y = begin_month)) + 
#     geom_violin()
# 
# pre_data %>% 
#     ggplot(aes(x = begin_month, fill = credit)) + 
#     geom_density(alpha = 0.3)
# 
# pre_data %>% 
#     ggplot(aes(x = age, fill = credit)) + 
#     geom_density(alpha = 0.3)
# 
# pre_data %>% 
#     ggplot(aes(x = career_year, fill = credit)) + 
#     geom_density(alpha = 0.3)
# 
# pre_data %>% 
#     ggplot(aes(x = credit, y = career_year)) + 
#     geom_violin()
# 
# pre_data %>% select(parent_num, credit) %>% table()
# 
# pre_data %>% one_variable_chisq_test('parent_num', 'credit') # 유의함


## 시각화 한 것 중에 6개정도만 뽑아보자

pre_data %>% one_variable_chisq_test('income_type', 'credit') # 유의함
pre_data %>% barplot_factor_variable('income_type', 'credit', text = FALSE)
# 소득분류(자영업자, 연금수급자, 공무원, 학생, 직장인)에 따라 신용도의 분포가 다르다

pre_data %>% one_variable_chisq_test('edu_type', 'credit') # 유의하지 않음
pre_data %>% barplot_factor_variable('edu_type', 'credit', text = FALSE)
# 교육수준에 따라 신용도의 분포가 다르지 않다

pre_data %>% one_variable_chisq_test('house_type', 'credit') # 유의함
pre_data %>% barplot_factor_variable('house_type', 'credit', text = FALSE)
# 주거 조건에 따라 신용도의 분포가 다르다

pre_data %>% one_variable_chisq_test('child_num_category', 'credit') # 유의함
pre_data %>% barplot_factor_variable('child_num_category', 'credit', text = FALSE)
# 자녀 수의 범주화 변수에 따라 신용도의 분포가 다르다
# 특히 자녀 수가 5명 이상일 때 분포가 크게 바뀐다

pre_data %>% 
    ggplot(aes(x = income_total, fill = credit)) + 
    geom_density(alpha = 0.2) + 
    ggtitle("Density plot of income_total by credit") + 
    theme(plot.title = element_text(size = 15, hjust = 0.5))
# 신용도별 연간 소득 분포를 그려봤을 때 시각적으로 분포의 차이가 느껴지지 않는다

pre_data %>% 
    group_by(credit) %>% 
    summarise(mean = mean(income_total),
              median = median(income_total),
              min = min(income_total),
              max = max(income_total))
# 신용도별 연간 소득의 대표값을 확인했을 때 max 말고는 크게 차이나는 값이 없다

plot_begin_month1 = pre_data %>% 
    ggplot(aes(x = credit, y = begin_month)) + 
    geom_violin() + 
    ggtitle("Violin plot of begin_month by credit") + 
    theme(plot.title = element_text(size = 15, hjust = 0.5))

plot_begin_month2 = pre_data %>% 
    ggplot(aes(x = begin_month, fill = credit)) + 
    geom_density(alpha = 0.3) +
    ggtitle("Density plot of begin_month by credit") + 
    theme(plot.title = element_text(size = 15, hjust = 0.5))

grid.arrange(plot_begin_month1, plot_begin_month2, ncol = 1)
# 신용도별 카드 발급 월 수의 분포에 시각적으로 차이가 있음을 확인할 수 있다.
# 신용도 1의 경우 다른 집단에 비해 아래가 두껍고 위가 얇음
# 신용도 2의 경우 다른 집단에 비해 아래가 매우 얇음

pre_data %>% select(-index) %>% select_if(is.numeric) %>% ggpairs()

# chisq.test를 통해 통계적으로 유의미한 차이가 있다고 하더라도 그룹의 비율을 확인하면 크게 차이나지 않는 경우도 있다.
# 단일 변수만 가지고 신용도에 따른 분포 차이를 확인하기 힘들다고 판단하여 변수 간 상호작용이 존재하는지 확인해보려고 한다.


# save.image(file='environment.RData')

