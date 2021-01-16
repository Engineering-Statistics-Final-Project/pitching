remove(list = ls())
so=0
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
so[k]=temp[j,23]
}
##print(temp[,4])
##cat("\n")

}
cor.test(so,win)
summary(lm(win~so))
qqnorm(so,col="blue",ylab="so",pch=0)
qqline(so,col="red")

plot(so,win)
abline(lm(win~so),col="red")
legend("bottomright",cex=0.7,legend=c("win=0.0233+53.473",
                                     expression('r'^2~"=0.09935")))


