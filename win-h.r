remove(list = ls())
h=0
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
h[k]=temp[j,14]
}
##print(temp[,4])
##cat("\n")

}
cor.test(h,win)
summary(lm(win~h))
qqnorm(h,col="blue",ylab="h",pch=0)
qqline(h,col="red")

plot(h,win)
abline(lm(win~h),col="red")
legend("bottomleft",cex=0.7,legend=c("win=-0.071668h+183.646",
                                     expression('r'^2~"=0.3064")))
data1=data.frame(x=h,y=win)
res=residuals(lm(win~h),data=data1)
plot(h,res)

qqnorm(res,col="blue",ylab="res",pch=0)
qqline(res,col="red")

require(car)
ncvTest(lm(win~h))
