remove(list = ls())
er=0
win=0
k=0

for(i in 1:18)
{
file_name=paste0("D:/DATA1/",i+2001)
file_name=paste0(file_name,".txt")
temp=read.table(file_name)
##order(temp$[,1])
for(j in 1:30)
{
  k=k+1
win[k]=temp[j,2]
er[k]=temp[j,16]
}
##print(temp[,4])
##cat("\n")

}
cor.test(er,win)
summary(lm(win~er))
qqnorm(er,col="blue",ylab="er",pch=0)
qqline(er,col="red")

plot(er,win)
abline(lm(win~er),col="red")
legend("bottomleft",cex=0.7,legend=c("win=-0.09565er+145.762",
                                     expression('r'^2~"=0.4339")))
data1=data.frame(x=er,y=win)
res=residuals(lm(win~er),data=data1)
plot(er,res)

qqnorm(res,col="blue",ylab="res",pch=0)
qqline(res,col="red")

require(car)
ncvTest(lm(win~er))
