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
n=100
r=icg(a0,a1,m,x0,n)
print(r)
t = matrix(r/(2^31), ncol = 3, byrow = T)
t = as.data.frame(t)
plot(t[,1], 9*t[,1]-6*t[,2]+t[,3], pch = 16, col = "blue", xlab = "",
     ylab = " ", main = "test plot")

