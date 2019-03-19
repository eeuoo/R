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
