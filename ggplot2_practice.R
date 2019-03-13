library('ggplot2')
library('dplyr')
theme_set(theme_gray(base_family="AppleGothic"))
par(family = "AppleGothic")

# point ######
ggplot() + geom_point(data=smdt, aes(x=stuno, y=Korean))

ggplot() + geom_point(data=smdt, aes(x=stuno, y=Korean),
                      color='blue', size = 5)
ggplot() + geom_point(data=smdt, aes(x=stuno, y=Korean, color=stuno),
                       size = 5)
ggplot() + geom_point(data=smdt, aes(x=stuno, y=Korean, size=stuno),
                      color='blue')

d = data %>% filter(stuno >= 30000)

ggplot(d, aes(cls, kor)) + geom_point(aes(color=cls, size=kor),
                                      alpha=0.3)
ggplot(d %>% filter(kor > 90), aes(cls, kor)) +
  geom_point(aes(color=cls, size=kor), 
             alpha=0.3)


# line #####
d2 = mpg %>% group_by(manufacturer, displ) %>% 
  summarise(m1 = mean(cty), m2 = mean(hwy))

ggplot(d2, aes(x =  displ)) +
  geom_line(aes(y=m1, color = 'cty')) +
  geom_line(aes(y=m2, color = 'hwy'), size = 1) +
  scale_color_manual("", breaks = c('cty', 'hwy'),
                     values = c('red', 'blue')) +
  xlab('x축') +
  xlim(1, 8) +
  scale_y_continuous("y축", limits = c(5, 45)) +
  labs(title = '타이틀', subtitle = '서브 타이틀')


# histogram  #####
ggplot(mpg, aes(displ)) + 
  geom_histogram(aes(fill = class),
                 binwidth = 0.5,
                 col = 'dark blue',
                 size = 0.3) +
  labs(title = 'Title', subtitle = 'Sub Title')


# bar #####
ggplot(mpg, aes(manufacturer)) +
  geom_bar(aes(fill = class),
           width = 0.5,
           col = 'darkgreen',
           size = 0.3) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.6)) +
  scale_fill_discrete(name = 'class') +
  labs(title = 'Title', subtitle = 'Sub Title')


# denstiy #####
ggplot(mpg, aes(cty)) +
  geom_density(aes(fill=factor(cyl)), size = 0.2, alpha=0.8) +
  labs(title = "밀도 그래프", subtitle = '실린더수에 따른 시내 연비의 밀도 그래프',
       catpion = 'Source : ggplot2::mpg',
       x = '도시연비',
       fill = '실린더수')

ggplot(data, aes(kor)) +
  geom_density(aes(fill=factor(cls)),size = 0.2, alpha = 0.5)


# gridExtra ####
install.packages('gridExtra')
library('gridExtra')

g3 = ggplot(mpg, aes(manufacturer)) +
  geom_bar(aes(fill=class),
           width = 0.7) +
  theme(axis.text.x = element_text(angle=45,
                                   vjust=0.6)) +
  labs(title = 'Title', subtitle = 'Sub Title')


g4 = ggplot(mpg, aes(cty)) +
  geom_density(aes(fill=factor(cyl)), alpha=0.8) +
  labs(title="밀도그래프", subtitle = "실린더수에 따른 시내연비의 밀도그래프",
       caption="Source: ggplot2::mpg",
       x = "도시 연비",
       fill = "실린더수")

grid.arrange(g4, g3, ncol = 1) 
grid.arrange(g4, g3, ncol = 2)
grid.arrange(g4, grid.arrange(g4, g3, ncol = 2), ncol = 1) 


