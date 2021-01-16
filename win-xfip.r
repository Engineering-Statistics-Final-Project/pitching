remove(list = ls())
xfip=0
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
  if(temp[l,1]==name[k]){xfip[k]=temp[j,19]}
}
}
##print(temp[,4])
##cat("\n")

}
cor.test(xfip,win)
summary(lm(win~xfip))
qqnorm(xfip,col="blue",ylab="xfip",pch=0)
qqline(xfip,col="red")

plot(xfip,win)
abline(lm(win~xfip),col="red")
legend("bottomleft",cex=0.7,legend=c("win=-13.71xfip+29.41",
                                     expression('r'^2~"=0.2576")))
data1=data.frame(xfip,y=win)
res=residuals(lm(win~xfip),data=data1)
plot(xfip,res)

qqnorm(res,col="blue",ylab="res",pch=0)
qqline(res,col="red")

require(car)
ncvTest(lm(win~xfip))
