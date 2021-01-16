remove(list = ls())
whip=0
win=0
name=0
k=0

for(i in 1:18)
{
file_name=paste0("D:/DATA/",i+2001)
file_name=paste0(file_name,".txt")
file_name1=paste0("D:/DATA1/",i+2001)
file_name1=paste0(file_name1,".txt")
temp=read.table(file_name)
temp1=read.table(file_name1)
##order(temp$[,1])
for(j in 1:30)
{
  k=k+1
win[k]=temp1[j,2]
name[k]=temp1[j,1]

for(l in 1:30)
{
  if(temp[l,1]==name[k]){whip[k]=temp[j,10]}
}
}
##print(temp[,4])
##cat("\n")

}
cor.test(whip,win)
summary(lm(win~whip))
qqnorm(whip,col="blue",ylab="whip",pch=0)
qqline(whip,col="red")

plot(whip,win)
abline(lm(win~whip),col="red")
legend("bottomleft",cex=0.7,legend=c("win=-66.837whip+171.102",
                                     expression('r'^2~"=0.2732")))
data1=data.frame(x=whip,y=win)
res=residuals(lm(win~whip),data=data1)
plot(whip,res)

qqnorm(res,col="blue",ylab="res",pch=0)
qqline(res,col="red")

require(car)
ncvTest(lm(win~whip))

