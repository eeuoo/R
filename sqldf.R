# install.packages('sqldf')
library(sqldf)

data %>% filter(kor > 50)
sqldf("select stuno, kor from data where kor > 50 limit 10")

rs = sqldf("select * from data where stuno = '20439'")
class(rs)

sqldf("select kor, count(*) cnt from data where kor < 30 group by kor order by 2")

sqldf("select cls, (case when cls = '국' then '우리반' else '남의반' end) aa,
      count(*) cnt from data where kor > 50 group by cls") 

sqldf("select cls, avg(kor) from data group by cls")

data2 = sqldf("select stuno, avg(kor) avgkor from data group by stuno limit 5")

sqldf("select * from data inner join data2 on data.stuno = data2.stuno")

sqldf("select b.stuno, a.kor from data a inner join data2 b on a.stuno = b.stuno")



