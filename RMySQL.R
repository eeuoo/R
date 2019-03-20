# install.packages('RMySQL')
library(RMySQL)

# 필수적으로 실행해야 할 것---------->
drv = dbDriver("MySQL")
conn = dbConnect(drv, host='127.0.0.1', port=3306, dbname='doodb', user='doo', password='11')
dbSendQuery(conn, 'set character set utf8')   # set utf-8
# <--------- 필수적으로 실행해야 할 것


dbListTables(conn)  

dbGetQuery(conn, "select * from Blogger")

rsdf = dbGetQuery(conn, "select * from Blogger limit 5")

dbGetQuery(conn, "update Song set title='선물1' where songno = '30514366'")

dbDisconnect(conn)
# dbUnloadDriver(drv)

tryCatch( {
  dbBegin(conn)
  dbGetQuery(conn, "update MelonTop set title='벌써 12시' where singer = '청하'")
  dbCommit(conn)
},
error = function(e) { 
  dbRollback(conn)
  print(paste("Error!!", e)) 
},
warning = function(w) {
  print(paste("Warining!!", w))
},
finally = {} )



# try this ####

# 1. 멜론 탑 100 곡들의 장르와 랭킹간의 관계를 산점도로 작도하시오.

dhn = dbDriver("MySQL")
conn2 = dbConnect(dhn, host='35.243.112.23', port=3306, dbname='melondb', user='root', password='eileen')
dbSendQuery(conn2, 'set character set utf8') 

dbListTables(conn2)  

head(dbGetQuery(conn2, "select * from MS_Song"))

dbDisconnect(conn2)

# 순위 전체
dhn_meltop = dbGetQuery(conn2, "select sr.rank, ms.title, ms.genre from Song_Rank sr inner join MS_Song ms on sr.song_no = ms.song_no where sr.rankdt = '20190125'")

ggplot() +
  geom_point(data=dhn_meltop,
             aes(x=genre, y=rank, color = genre), alpha = 0.5) +
  xlab("장르") +
  ylab("순위") +
  labs(title = "장르와 순위의 상관관계", subtitle = '멜론 top 100 ')

# 순위 평균값
dhn_meltop2 = dbGetQuery(conn2, "select avg(rank) rank, ms.genre from Song_Rank sr inner join MS_Song ms on sr.song_no = ms.song_no 
           where sr.rankdt = '20190125'
           group by ms.genre")

ggplot(dhn_meltop2, aes(x=genre, y=rank)) +
  geom_point(color = 'darkblue', size = 2) 



# 2. 멜론 탑 100 곡들의 순위와 좋아요의 관계를 산점도로 작도하시오.
meltop = dbGetQuery(conn, "select rank, title, singer, likecnt from MelonTop")
head(meltop)

ggplot() +
  geom_point(data=meltop,
             aes(x=rank, y=likecnt),
             color='blue', size = 2, alpha = 0.5) +
  xlab("순위 (1위~100위)") +
  ylab('좋아요 수') +
  labs(title = "순위와 좋아요의 상관관계", subtitle = '멜론 top 100 ')
  
dbDisconnect(conn)


