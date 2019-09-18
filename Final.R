#Randu

randu=function(x0,n)
{
x=rep(0,n+1)
x[1]=x0
for(i in 1:n+1)
{
if(i==1)
x[i]=x0
else
x[i]=(65539*x[i-1])%%(2^31)
}
return(x[-1])
}
boxplot(randu(x0,n))
#ICG
icg=function(a0,a1,m,x0,n)
{
x=rep(0,n+1)
x[1]=x0
for(i in 1:n+1)
{
if(i==1)
x[i]=x0
else
{
if(x[i-1]==0)
x[i]=a0 %%m
else
{
xi=(x[i-1]^(m-2))%%m
x[i]=(a0+(a1*x[i-1]))%%m
}}}
return(x)
}
a0=3
a1=1
m=7
x0=1
n=500
boxplot(icg(a0,a1,m,x0,n))
x<-randu(x0,n)
mean(x)
sd(x)
ks.unif.test(x)

chisq.test(x) 

hist(randu(x0,n),main="Randu",xlab="x[i]",border="blue",col="green")

hist(icg(a0,a1,m,x0,n),main="ICG")
a0=0.13
a1=0.075
m=5
x0=3

x0=1080
n=500

