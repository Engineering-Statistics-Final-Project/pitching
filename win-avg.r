remove(list = ls())
avg=0
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
  if(temp[l,1]==name[k]){avg[k]=temp[j,9]}
}
}
##print(temp[,4])
##cat("\n")

}
cor.test(avg,win)
summary(lm(win~avg))
qqnorm(avg,col="blue",ylab="avg",pch=0)
qqline(avg,col="red")

plot(avg,win)
abline(lm(win~avg),col="red")

legend("bottomleft",cex=0.7,legend=c("win=-428.04avg+189.863",
                            expression('r'^2~"=0.2275")))

