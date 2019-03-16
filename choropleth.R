install.packages("stringi")
theme_set(theme_gray(base_family="AppleGothic"))
par(family = "AppleGothic")

# 단계 구분도 ######
library(ggplot2)
ggiraphExtra::ggChoropleth()
install.packages("ggiraphExtra")
install.packages("maps")
library(ggiraphExtra)
install.packages('mapproj')

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


# 우리나라 ####
install.packages('devtools')
devtools::install_github("cardiomoon/kormaps2014")
library(kormaps2014)

kormaps2014::korpop1
kdata = korpop1
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



