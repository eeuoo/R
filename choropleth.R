
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
