# 개인 프로젝트 - 데이콘 신용카드 사용자 연체 예측 AI 경진대회

https://www.dacon.io/competitions/official/235713/overview/description/

## 제출 결과

- xgboost_1.csv : logloss : 0.7611119983	

- xgboost_resampled_2.csv : logloss : 0.7960758665

## 데이터 변수 설명

● 독립변수

1. index

2. gender: 성별

3. car: 차량 소유 여부

4. reality: 부동산 소유 여부

5. child_num: 자녀 수

6. income_total: 연간 소득

7. income_type: 소득 분류

['Commercial associate', 'Working', 'State servant', 'Pensioner', 'Student']

8. edu_type: 교육 수준

['Higher education' ,'Secondary / secondary special', 'Incomplete higher', 'Lower secondary', 'Academic degree']

9. family_type: 결혼 여부

['Married', 'Civil marriage', 'Separated', 'Single / not married', 'Widow']

10. house_type: 생활 방식

['Municipal apartment', 'House / apartment', 'With parents', 'Co-op apartment', 'Rented apartment', 'Office apartment']

11. DAYS_BIRTH: 출생일

데이터 수집 당시 (0)부터 역으로 셈, 즉, -1은 데이터 수집일 하루 전에 태어났음을 의미

12. DAYS_EMPLOYED: 업무 시작일

데이터 수집 당시 (0)부터 역으로 셈, 즉, -1은 데이터 수집일 하루 전부터 일을 시작함을 의미, 양수 값은 고용되지 않은 상태를 의미함

13. FLAG_MOBIL: 핸드폰 소유 여부

14. work_phone: 업무용 전화 소유 여부

15. phone: 가정용 전화 소유 여부

16. email: 이메일 소유 여부

17. occyp_type: 직업 유형
													
18. family_size: 가족 규모

19. begin_month: 신용카드 발급 월
 
데이터 수집 당시 (0)부터 역으로 셈, 즉, -1은 데이터 수집일 한 달 전에 신용카드를 발급함을 의미

● 종속변수

20. credit: 사용자의 신용카드 대금 연체를 기준의 신용도	=> 낮을 수록 높은 신용의 신용카드 사용자를 의미함
