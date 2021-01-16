remove(list = ls())
era=0
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
era[k]=temp[j,4]
}
##print(temp[,4])
##cat("\n")

}
cor.test(era,win)
summary(lm(win~era))
qqnorm(era,col="blue",ylab="era",pch=0)
qqline(era,col="red")

plot(era,win)
abline(lm(win~era),col="red")
legend("bottomleft",cex=0.7,legend=c("win=-20.95era+47.34",
                                     expression('r'^2~"=0.4493")))
data1=data.frame(x=era,y=win)
res=residuals(lm(win~era),data=data1)
plot(era,res)

qqnorm(res,col="blue",ylab="res",pch=0)
qqline(res,col="red")

require(car)
ncvTest(lm(win~era))
