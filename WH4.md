Facebook粉絲團分析（分析專頁：柯文哲）
================

分析台北市長柯文哲粉絲專頁之每日發文數、likes數、comments數與shares數，資料分析區間為2016/01/02至2016/04/11

讀取柯文哲粉絲團資料
--------------------

``` r
if (!require('Rfacebook')){
  install.packages("Rfacebook")
  library(Rfacebook)
}
```

``` r
token <- 'CAACEdEose0cBAIwnaC3bHPPJCnPFlMUiuvPecbJ7wmRXZAvzi2WnnTAb93wiGoq4PbidRYbgPxmYa2tZBUoTnN9XXBqRHI5J3mKZAGTcDNkJwJHzL239fm68Mqy5FCR3vww2E1Uq5uF8vgMvTnE94VJrIfglvCowQYrk0Op4SgkGZA7toEIDKI3Dd8hXZAjoGiohdx5FFqwZDZD'
totalPage<-NULL
lastDate<-Sys.Date()
DateVectorStr<-as.character(seq(as.Date("2016-01-01"),lastDate,by="5 days"))
for(i in 1:(length(DateVectorStr)-1)){
  tempPage<-getPage("DoctorKoWJ",token,
                    since = DateVectorStr[i],until = DateVectorStr[i+1])
  totalPage<-rbind(totalPage,tempPage)
}
```

    ## 2 posts 10 posts 3 posts 2 posts 4 posts 4 posts 3 posts 4 posts 2 posts 1 posts 1 posts 3 posts 3 posts 2 posts 3 posts 2 posts 4 posts 2 posts 1 posts 1 posts

``` r
nrow(totalPage)
```

    ## [1] 57

2016/01/02至2016/04/11 柯文哲粉絲團一共有57篇文章

每日發文數分析
--------------

說明: 分析柯文哲粉絲團每天的發文數，利用weekdays()讓每個日期都再給予星期幾，再用aggregate來分組計算發文數放入PostCount，最後以發文數高至低顯示出來。

``` r
totalPage$datetime <- as.POSIXct(totalPage$created_time,
                                 format =  "%Y-%m-%dT%H:%M:%S+0000",
                                 tz = "GMT")
totalPage$dateTPE <- format(totalPage$datetime, "%Y-%m-%d", 
                            tz = "Asia/Taipei")
totalPage$weekdays <-weekdays(as.Date(totalPage$dateTPE))
PostCount<-aggregate(id~dateTPE+weekdays,totalPage,length)  
library(knitr)
kable(head(PostCount[order(PostCount$id,decreasing = T),]))
```

|     | dateTPE    | weekdays |   id|
|-----|:-----------|:---------|----:|
| 26  | 2016-01-09 | 星期六   |    4|
| 10  | 2016-03-22 | 星期二   |    2|
| 18  | 2016-01-08 | 星期五   |    2|
| 30  | 2016-02-06 | 星期六   |    2|
| 34  | 2016-01-10 | 星期日   |    2|
| 1   | 2016-01-25 | 星期一   |    1|

討論:2016/01/9（週六）的發文數最多，其原因是因為這天柯市長挑戰『一日北高，雙城挑戰』，所以以影片和發文來分享他的騎乘過程。

每日likes數分析
---------------

說明:以aggregate來分組將likes數以每天發文數來做平均計算，再用kable與head來由高至低取前6名排序出來。

``` r
totalPage$weekdays <-weekdays(as.Date(totalPage$dateTPE))
LikesCount<-aggregate(likes_count~dateTPE+weekdays,totalPage,mean)
library(knitr)
kable(head(LikesCount[order(LikesCount$likes_count,decreasing = T),]))
```

|     | dateTPE    | weekdays |  likes\_count|
|-----|:-----------|:---------|-------------:|
| 27  | 2016-01-16 | 星期六   |      329086.0|
| 38  | 2016-02-28 | 星期日   |      228907.0|
| 34  | 2016-01-10 | 星期日   |      223664.5|
| 42  | 2016-01-14 | 星期四   |      187451.0|
| 32  | 2016-02-27 | 星期六   |      180924.0|
| 5   | 2016-03-28 | 星期一   |      139091.0|

討論:結果顯示2016/01/16這天的likes數最多，因為台灣出現第一任女總統，柯文哲表示未來展望ˋ；而2016/02/28居次，原因是這天以挑戰一日雙塔來紀念228，以自身為受難家屬用以「寬容如海、成就臺灣」與大家共勉；2016/01/10這天排第三名，主要原因是柯市長挑戰一日雙城完後的發文獲得許多的關注。

每日comments數分析
------------------

說明:以aggregate來分組將comments數以每天發文數來做平均計算，再用kable與head來由高至低取前6名排序出來。

``` r
CommentsCount<-aggregate(comments_count~dateTPE+weekdays,totalPage,mean)
library(knitr)
kable(head(CommentsCount[order(CommentsCount$comments_count,decreasing = T),]))
```

|     | dateTPE    | weekdays |  comments\_count|
|-----|:-----------|:---------|----------------:|
| 34  | 2016-01-10 | 星期日   |          5981.50|
| 26  | 2016-01-09 | 星期六   |          5153.25|
| 5   | 2016-03-28 | 星期一   |          5111.00|
| 38  | 2016-02-28 | 星期日   |          3565.00|
| 32  | 2016-02-27 | 星期六   |          3268.00|
| 42  | 2016-01-14 | 星期四   |          2848.00|

討論:2016/01/10這天comments數最多，原因也是市長再挑戰完雙塔後的發文獲得許多網友的回應；2016/01/09居次也因為是挑戰雙塔當天的發文所帶來的影響;2016/03/28這天發生隨機殺人案震驚全台灣，市長發言，引發許多人認同。其人母親所說「從根本、從家庭、從教育來讓這樣子的人消失在社會上」獲得許多人認同。

每日shares數分析
----------------

說明:以aggregate來分組將shares數以每天發文數來做平均計算，再用kable與head來由高至低取前6名排序出來。

``` r
SharesCount<-aggregate(shares_count~dateTPE+weekdays,totalPage,mean)
library(knitr)
kable(head(SharesCount[order(SharesCount$shares_count,decreasing = T),]))
```

|     | dateTPE    | weekdays |  shares\_count|
|-----|:-----------|:---------|--------------:|
| 42  | 2016-01-14 | 星期四   |        34774.0|
| 34  | 2016-01-10 | 星期日   |        16406.5|
| 38  | 2016-02-28 | 星期日   |        11234.0|
| 23  | 2016-03-04 | 星期五   |         5333.0|
| 5   | 2016-03-28 | 星期一   |         4964.0|
| 27  | 2016-01-16 | 星期六   |         4897.0|

討論:2016/01/14這天shares最多，因為市長發了一篇關於「夢想」看法的文，以自身為56歲的阿伯挑戰雙塔成功來勉勵國人做自己的主人,展現意志力追求夢想獲得許多人的認同與分享;2016/01/10居次，原因是成功完成雙塔後的發文引來許多網友的分享與鼓勵；第三是2016/02/28這天的發文，雖然市長沒有出席重要的紀念會，但市長今天特別的以過人的毅力達成一日雙塔來紀念228，並在發文中表達身為受難家屬的感觸獲得許多網友的分享。
