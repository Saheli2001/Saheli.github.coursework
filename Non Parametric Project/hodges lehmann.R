rm(list=ls())
set.seed(seed=2023)
library(ggplot2)
library(gridExtra)
library(ExtDist)
library(reshape2)

#Defining Mann Whitney Function-----------------------

mann_whitney = function(x,y,n,m){     #Defining a function
  count=0
  for(i in 1:n)
  {
    for(j in 1:m)
    {
      if((x[i]>y[j]))
      {count=count+1}
    }
  }
  count
}


R=1000     
#hodges lehmann
theta=5
alpha=2
n=100
m=100
u=theta_hodges=bias_hodges=mse_hodges=bias_mle=mse_mle=theta_mle=array(0)

hodges_estimator_cauchy = function(alpha,theta,n,m)
{dij=matrix(0,nrow=n,ncol=m)
for(i in 1:300)
{
  x_data=rcauchy(n,alpha,1)
  y_data=rcauchy(m,(alpha+theta),1)
  for(j in 1:n)
  {
    
    dij[j,]=y_data-x_data[j]
  }
  d=sort(as.vector(dij))
  theta_hodges[i]=median(d)
}
return(theta_hodges)
}
hodges_estimator_cauchy(2,5,n,m)
lik_h_0=array(0)
mle_cauchy=function(alpha,theta,n,m)
{
  for(i in 1:300)
  {
  x=rcauchy(n,alpha,1)
  y=rcauchy(m,alpha+theta,1)
  likelihood_0=function(theta){
    log(prod(1/(1+(x-alpha)^2))*prod(1/(1+(y-alpha-theta)^2)))
  }
  lik_h_0[i]=optim(0,likelihood_0,method = "CG",control=list(fnscale=-1))$par
  }
  return(lik_h_0)
}

hodges_estimator_logis = function(alpha,theta,n,m)
{dij=matrix(0,nrow=n,ncol=m)
for(i in 1:300)
{
  x_data=rlogis(n,alpha,1)
  y_data=rlogis(m,(alpha+theta),1)
  for(j in 1:n)
  {
    
    dij[j,]=y_data-x_data[j]
  }
  d=sort(as.vector(dij))
  theta_hodges[i]=median(d)
}
return(theta_hodges)
}

mle_logis=function(alpha,theta,n,m)
{
  
  for(i in 1:300)
  {
  x=rlogis(n,alpha,1)
  y=rlogis(m,alpha+theta,1)
  likelihood_0=function(theta){
    log(prod(exp(-x+alpha)/(1+exp(-x+alpha))^2)*prod(exp(-x+alpha+theta)/(1+exp(-x+alpha+theta))^2))
  }
  lik_h_0[i]=optim(4,likelihood_0,method = "CG",control=list(fnscale=-1))$par
  }
  return(lik_h_0)
}



#bias and variances different theta
bias_mle_cauchy=bias_mle_logis=bias_mle_laplace=bias_h_cauchy=bias_h_logis=bias_h_laplace=array(0)
theta_seq=seq(0,2,.1)

for(j in 1:length(theta_seq))
{

bias_mle_cauchy[j]=sum((mle_cauchy(2,theta_seq[j],n,m)-theta_seq[j])^2)/300
bias_mle_logis[j]=sum((mle_logis(2,theta_seq[j],n,m)-theta_seq[j])^2)/300


bias_h_cauchy[j]=sum((hodges_estimator_cauchy(2,theta_seq[j],n,m)-theta_seq[j])^2)/300
bias_h_logis[j]=sum((hodges_estimator_logis(2,theta_seq[j],n,m)-theta_seq[j])^2)/300

}

 bias.data=melt(data.frame(theta_seq,bias_mle_cauchy,bias_h_cauchy),
                id='theta_seq')
 ggplot(data=bias.data,aes(x=theta_seq,y=value,colour=variable))+
   geom_line()+
   labs(title = 'Comparison betwee MSEs of Hodges Lehmann Estimator and MLE (Cauchy(0,1))',x='Theta',y="MSE")+
   theme(plot.title =
           element_text(size=
                          16,
                        hjust=.5,face='bold'),
         plot.subtitle = element_text(
           size=14,hjust=.5,face='italic'
         ),
         legend.title = element_text(hjust=.5),
         axis.title = element_text(face='bold'),
         axis.text=element_text(face='bold'))+theme_light()+
   scale_color_manual(name="Estimator",
                      labels=c("MLE","Hodges Lehmann"),
                      values=c("red","green"))



bias.data2=melt(data.frame(theta_seq,bias_mle_logis,bias_h_logis),
                id='theta_seq')
ggplot(data=bias.data2,aes(x=theta_seq,y=value,colour=variable))+
  geom_line()+
  labs(title = 'Comparison betwee MSEs of Hodges Lehmann Estimator and MLE (Logistic(0,1))',x='Theta',y="MSE")+
  theme(plot.title =
          element_text(size=
                         16,
                       hjust=.5,face='bold'),
        plot.subtitle = element_text(
          size=14,hjust=.5,face='italic'
        ),
        legend.title = element_text(hjust=.5),
        axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))+theme_light()+
  scale_color_manual(name="Estimator",
                     labels=c("MLE","Hodges Lehmann"),
                     values=c("red","green"))


#bias and variance different n

n=1:30
m=1:30
for(i in 1:length(n))
{
  
  bias_mle_cauchy[i]=mean(mle_cauchy(2,5,n[i],m[i]))
  bias_mle_logis[i]=mean(mle_logis(2,5,n[i],m[i]))
  
  
  bias_h_cauchy[i]=mean(hodges_estimator_cauchy(2,5,n[i],m[i]))
  bias_h_logis[i]=mean(hodges_estimator_logis(2,5,n[i],m[i]))
}
N=n+m
c.data=melt(data.frame(N,bias_mle_cauchy,bias_h_cauchy),
               id='N')
ggplot(data=c.data,aes(x=N,y=value,colour=variable))+
  geom_line()+
  labs(title = 'Consistency of Hodges Lehmann Estimator and MLE (Cauchy(0,1))',x='N=n+m',y="Value of Theta")+
  theme(plot.title =
          element_text(size=
                         16,
                       hjust=.5,face='bold'),
        plot.subtitle = element_text(
          size=14,hjust=.5,face='italic'
        ),
        legend.title = element_text(hjust=.5),
        axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))+theme_light()+
  scale_color_manual(name="Estimator",
                     labels=c("MLE","Hodges Lehmann"),
                     values=c("red","green"))


c.data2=melt(data.frame(N,bias_mle_logis[1:109],bias_h_logis[1:109]),
                id='k')
ggplot(data=c.data2,aes(x=N,y=value,colour=variable))+
  geom_line()+
  labs(title = 'Consistency of Hodges Lehmann Estimator and MLE (Logistic(0,1))',x='N=n+m',y="value of Theta")+
  theme(plot.title =
          element_text(size=
                         16,
                       hjust=.5,face='bold'),
        plot.subtitle = element_text(
          size=14,hjust=.5,face='italic'
        ),
        legend.title = element_text(hjust=.5),
        axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))+theme_light()+
  scale_color_manual(name="Estimator",
                     labels=c("MLE","Hodges Lehmann"),
                     values=c("red","green"))
