# install.packages("stringi")
theme_set(theme_gray(base_family="AppleGothic"))
par(family = "AppleGothic")

# persp #####
x = 1:5 ; y = 1:3 ;
z = outer(x,y, function(x,y) {x+y})

x = seq(-10, 10, length = 30) 
y = seq(-5, 5, length = 30)
x = y

f = function(x,y){
  r = sqrt(x^2 + y^2)
  return(10*sin(r)/r)
}

z = outer(x, y, f)

persp(x, y, z, theta = 45, phi = 30, expand = 0.5, col='lightblue',
      ltheta = 100, shade =1, ticktype = 'simple',
      xlab = 'X', ylab = 'Y', zlab = 'Sinc(r)')

# tibble #####
library(tibble)
chodata = rownames_to_column(USArrests, var = 'state')
chodata$state = tolower(chodata$state)

# 단계 구분도 ######
library(ggplot2)
ggiraphExtra::ggChoropleth()
# install.packages("ggiraphExtra")
# install.packages("maps")
library(ggiraphExtra)
# install.packages('mapproj')

head(USArrests)
str(USArrests)
rownames(USArrests)

library(dplyr)
library(tibble)
chodata = rownames_to_column(USArrests, var = 'state')
chodata$state = tolower(chodata$state)

chodata = data.frame(state = tolower(rownames(USArrests)), USArrests)
head(chodata)

usmap = map_data('state')
usmap
head(usmap)

ggChoropleth(data=chodata, 
             aes(fill=Murder, map_id=state),
             map = usmap,
             title = '..',
             reverse = F,
             interactive = T)

# geom_map() ######
ggplot(chodata, aes(map_id = state)) +
  geom_map(aes(fill = Murder), map = usmap) +
  expand_limits(x = usmap$long, y = usmap$lat) +
  labs(title ='USA Murder', fill = '살인')

ggplot(chodata, aes(map_id = state)) +
  geom_map(aes(fill = Murder), map = usmap) +
  expand_limits(x = usmap$long, y = usmap$lat) +
  labs(title ='USA Murder', fill = '살인') +
  scale_fill_gradient2('살인', low = 'green', high = 'red')


# customize ####
tooltips = paste0(
  sprintf("<p><strong>%s</strong></p>", as.character(chodata$state)),
  '<table>',
  '  <tr>',
  '    <td>인구(만)</td>',
  sprintf('<td>%.0f</td>', chodata$UrbanPop * 10),
  '  </tr>',
  '  <tr>',
  '    <td>살인</td>',
  sprintf('<td>%.0f</td>', chodata$Murder),
  '  </tr>',
  '  <tr>',
  '    <td>폭력</td>',
  sprintf('<td>%.0f</td>', chodata$Assault),
  '  </tr>',
  '</table>' )

onclick = sprintf("alert(\"%s\")", as.character(chodata$state))


library(ggiraph)
ggplot(chodata, aes(data = Murder, map_id = state)) +
  geom_map_interactive(
    aes(fill = Murder,
        data_id = state,
        tooltip = tooltips,
        onclick = onclick),
    map = usmap) +
  expand_limits(x = usmap$long, y = usmap$lat) +
  scale_fill_gradient2('살인', low = 'red', high = 'blue', mid = 'green') +
  labs(title = 'USA Murder') -> gg_map

ggiraph(code = print(gg_map))
girafe(ggobj = gg_map)


# 필요한 패키지 및 데이터 #####
lib_p = function() {
  library(kormaps2014)
  library(ggiraph)
  library(ggplot2)
  library(ggiraphExtra)
  library(dplyr)
  theme_set(theme_gray(base_family="AppleGothic"))
  par(family = "AppleGothic")
}

lib_p()


# 우리나라 ####
# install.packages('devtools')
# devtools::install_github("cardiomoon/kormaps2014")
library(kormaps2014)

kormaps2014::korpop1
kdata = korpop1
# save(kdata, file = 'data/kdata.rda')

head(kdata)
kdata = kdata %>% rename(pop = 총인구_명)
kdata = kdata %>% rename(area = 행정구역별_읍면동)


ggChoropleth(data=kdata,
             aes(fill = pop, map_id = code, tooltip = area),
             map = kormap1,
             interactive = T)

ggplot(kdata, aes(data = pop, map_id = code)) +
  geom_map( aes(fill = pop), map = kormap1) + 
  expand_limits(x = kormap1$long, y = kormap1$lat) +
  scale_fill_gradient2('인구', low='darkblue') +
  xlab('경도') + ylab('위도') + 
  labs(title="시도별 인구")


head(kormap1$long)
head(kormap1$lat)
head(kormap1)


# ploty #####
# install.packages('plotly')
library('plotly')

ggplot(data, aes(eng,kor)) +
  geom_point(aes(color = eng, size = kor), alpha = 0.3) -> t

ggplotly(t)


ggplot() +
  geom_line(data=d2 %>% filter(year == 2008), aes(displ, cty, color = '2008 cty'), size = 1) +
  geom_line(data=d2 %>% filter(year == 2008), aes(displ, hwy, color = '2008 hwy'), size = 1) +
  geom_line(data=d2 %>% filter(year == 1999), aes(displ, cty, color = '1999 cty')) +
  geom_line(data=d2 %>% filter(year == 1999), aes(displ, hwy, color = '1999 hwy')) +
  xlab('배기량') +
  labs(colour = "")+
  xlim(1, 7) +
  scale_y_continuous("연비", limits = c(5, 50)) +
  labs(title = '연도별 통합 연비', subtitle = '(굵은 선은 2008년)') -> t

ggplot(data %>% filter( kor >= 80) , aes(cls)) +
  geom_bar(aes(fill=gen), width = 0.5) +
  scale_fill_discrete(name = "성별") +      
  labs(x = '학급', y = '학생수', title = '국어 우수 학생', subtitle = '(80점 이상)') -> t

last_plot()

# time series ####
# install.packages('dygraphs')
library(dygraphs)
library(xts)

head(economics)
ue = xts(economics$unemploy, order.by = economics$date)
dygraph(ue)

dygraph(ue) %>% dyRangeSelector()

psave = xts(economics$psavert, order.by = economics$date)
dygraph(cbind(ue, psave))
ue2 = xts(economics$unemploy/ 1000, order.by = economics$date)
pu = cbind(ue2, psave)
colnames(pu) = c('unemploy','saverate')
dygraph(pu) %>% dyRangeSelector()
