b=function(x,lambda)
{
  y=array(0)
  for(i in 1:length(x))
  {
  y[i]=ifelse(lambda==0,log(x[i]),(x[i]^lambda - 1)/lambda)
  }
  return(y)
}

sgn=function(x){
  y=array(0)
  for(i in 1:length(x))
  {
    y[i]=ifelse(x[i]>0,1,ifelse(x[i]==0,0,-1))
  }
  return(y)
}
sgn(-10:10)

l=function(x,miu,sigma2,lambda)
{

  n=length(x)
   -(n*log(2*pi)/2) -(n*log(sigma2)/2) - 
    (sum((b(x,lambda) - miu)^2)/(2*sigma2)) + 
    (lambda - 1)*sum(sgn(x)*(log(abs(x)+1)))
  
}
x=as.vector(B$Calories)
lik=miu.hat=sigma.hat=array(0)
lambda=seq(-10,10,.01)
for(i in 1:length(lambda))
{
miu.hat[i]=mean(b(x,lambda[i]))
sigma.hat[i] = mean((b(x,lambda[i])-miu.hat[i])^2)
lik[i] = l(x,miu.hat[i],sigma.hat[i],lambda[i])
}

ggplot(data=NULL,aes(x=lambda,y=lik))+
  geom_point()+geom_line()+
  labs(title = 'Scatterplot of Sale Price of House \n vs Area of the property(in sqft)',x=
         'Lot area',y='Sale Price of House(in dollars)')
lambda[which.max(lik)]

plot(as.data.frame(((jitter(B$Calories))^1.88 -1)/1.88),title='d',row=1,col=1)
 

library(LambertW)
test_normality(
  Gaussianize(jitter(x),type = 'h',method = 'IGMM', return.tau.mat = T)$input)
