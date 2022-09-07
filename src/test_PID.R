#PID制御の実装
library(ggplot2)

#テスト用
#i <- 1
#t <- 500

PID <- function(goal,t=100){
  #入力量
  M = 1.0
  M1 = 0.0
  #偏差
  e = 0.0
  e1 = 0.0
  e2 = 0.0
  Kp = 0.1
  Ki = 0.05
  Kd = 0.06
  x <- rep(0,t+1)
  y <- rep(0,t+1)
  for(i in 1:t){
    M1 = M
    e2 = e1
    e1 = e
    #入力量と出力量の関係を表現する場合には、この部分を書き変える。
    e = goal - y[i]
    M = M1 + Kp * (e-e1) + Ki*e + Kd *((e-e1) -(e1 - e2))
    x[i+1] <- i
    y[i+1] <- M 
  }
  return(list(x,y))
}
goal = 100
time = 200

out <- PID(goal,time)

temp <- unlist(out[2])
temp2 <- rep(100,time+1)

#plot
#plot(temp,type='l',ylim = c(0,120))
#lines(temp2,col='red')

#ggplot2でplot
temp <- data.frame(
  value = temp,
  time = c(0:time))
plotdata <- ggplot(temp,aes(x = time, y = value))
plotdata <- plotdata + geom_line()
plotdata <- plotdata + geom_hline(yintercept=goal,color = "red",linetype="dashed")
print(plotdata)
