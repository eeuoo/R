# 1. mpg데이터에서 연도별 배기량에 따른 통합연비를 꺽은선으로 그리시오. (단, 2008년은 굵은 선으로 표현하시오)

d2 = mpg %>% group_by(year, displ) %>% 
  summarise(cty = mean(cty), hwy = mean(hwy))

ggplot(d2, aes(displ)) +
  geom_line(aes(y=cty, group= year, color = factor(year), size = factor(year))) +
  geom_line(aes(y=hwy, group= year, color = factor(year), size = factor(year))) +
  scale_size_manual("",breaks = c('1999', '2008'),
                    values = c(0.5, 1) ) +
  scale_color_manual("", breaks = c('1999', '2008'),
                     values = c('red', 'blue') ) +
  xlab('배기량') +
  xlim(1, 7) +
  scale_y_continuous("연비", limits = c(5, 45)) +
  labs(title = '연도별 통합 연비', subtitle = '(굵은 선은 2008년)')

d2

ggplot(d2, aes(displ, group = year, color = factor(year))) +
  geom_line(aes(y=cty)) +
  geom_line(aes(y=hwy)) 

ggplot() +
  geom_line(data=d2 %>% filter(year == 2008), aes(displ, cty, color = '2008 cty'), size = 1) +
  geom_line(data=d2 %>% filter(year == 2008), aes(displ, hwy, color = '2008 hwy'), size = 1) +
  geom_line(data=d2 %>% filter(year == 1999), aes(displ, cty, color = '1999 cty')) +
  geom_line(data=d2 %>% filter(year == 1999), aes(displ, hwy, color = '1999 hwy')) +
  xlab('배기량') +
  labs(colour = "")+
  xlim(1, 7) +
  scale_y_continuous("연비", limits = c(5, 45)) +
  labs(title = '연도별 통합 연비', subtitle = '(굵은 선은 2008년)')




# 2. data(성적.csv) 데이터에서 국어 성적이 80점 이상인 학생들의 수를 성비가 보이도록 학급별로 막대그래프를 그리시오.
ggplot(data %>% filter( kor >= 80) , aes(cls)) +
  geom_bar(aes(fill=gen), width = 0.5) +
  scale_fill_discrete(name = "성별") +      
  labs(x = '학급', y = '학생수', title = '국어 우수 학생', subtitle = '(80점 이상)')



# 3. 국어 성적이 95점 이상인 학생들의 점수별 밀도그래프를 그리시오.
ggplot(data %>% filter( kor >= 95 ), aes(kor)) +
  geom_density(aes(fill=factor(cls)),size = 0.2, alpha = 0.5)



# 4. midwest데이터에서 전체인구와 아시아계 인구의 관계를 알아보기 위한 그래프를 그리시오. (단, 전체인구는 50만명 이하, 아시아계인구는 1만명 이하만 표시되게)


