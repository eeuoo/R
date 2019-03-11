library('ggplot2')
smdt = data.frame(stuno = 1:5,
                  Korean = sample(60:100, 5),
                  English = sample((5:10)*10,5),
                  Math = sample(50:100, 5))

# mac에서 한글 깨질 때
theme_set(theme_gray(base_family="AppleGothic"))
par(family = "AppleGothic")
??theme_set


plot(x = smdt$stuno, y = smdt$Korean,
     col = "violetred",
     cex = 1,
     las = 1,
     type = 'b',    # p, l, b, c, o, s
     xlim = c(0,5.5),
     ylim = c(50, 100),
     pch = 23,
     xlab = '학번',
     ylab = '국어',
     main = '그래프 타이틀')

?pch 
colors()

# 그래프 중첩 & 범례 #####

xl = c(0, 5.5)
yl = c(0, 100)

plot(x =  smdt$stuno, y = smdt$Korean,
     col = "violet", 
     cex =  2,
     pch = 8,
     type = 'b',
     xlim = xl,
     ylim = yl,
     xlab = '학번',
     ylab = '국어, 수학',
     main = '우리반 국어/수학 성적')

par(new = T)

plot(x = smdt$stuno, y = smdt$Math,
     col = 'springgreen3',
     cex = 2,
     pch = 21,
     type = 'b',
     xlim = xl,
     ylim = yl,
     xlab = ' ',
     ylab = ' ')

legend('bottomright',
       legend = c('국어', '수학'),
       pch = c(8, 21),
       col = c('violet', 'springgreen'),
       bg = 'grey88')


# 막대 그래프 ####

t = data %>% filter( eng > 90 ) %>% select('cls', 'gen') %>% table

barplot(t,
        beside = T, 
        horiz = T,
        border = 'darkblue',
        density = 50,
        las = 1,
        angle = 15 + 10*1:2,
        xlab = '학급별 성별',
        ylab = '영어',
        legend = rownames(t),
        col = cm.colors(4))

# rainbow(n, s = 1, v = 1, start = 0, end = max(1, n - 1)/n, alpha = 1)
# heat.colors(n, alpha = 1)
# terrain.colors(n, alpha = 1)
# topo.colors(n, alpha = 1)
# cm.colors(n, alpha = 1)


# boxplot ####
boxplot(kor~cls, data=data, col = 'lightcyan')
boxplot(kor~gen, data=data, col = 'grey88')


# histogram ####
hist(data$kor, col='ivory', labels=T, breaks = 10)


# curve ####
curve(sin, -2*pi, 3*pi,
      xname = 'x',
      xlab = 'TT',
      n = 200,
      type = 'p',
      xlim = c(-10, 10),
      ylim = c(-1, 1))


# pie #####
d = data %>% filter(kor > 90) %>% select('cls')

table(d)

pie(table(d), border = "darkblue")
pie(table(d), clockwise = T, col=c('red', 'purple', 'green', 'cyan'))




