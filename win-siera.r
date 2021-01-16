remove(list = ls())
siera=0
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
  if(temp[l,1]==name[k]){siera[k]=temp[j,20]}
}
}
##print(temp[,4])
##cat("\n")

}
cor.test(siera,win)
summary(lm(win~siera))
qqnorm(siera,col="blue",ylab="siera",pch=0)
qqline(siera,col="red")

plot(siera,win)
abline(lm(win~siera),col="red")
legend("bottomleft",cex=0.7,legend=c("win=-16.383siera+148.818",
                                     expression('r'^2~"=0.2324")))

