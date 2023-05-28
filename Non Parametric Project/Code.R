rm(list=ls())
set.seed(seed=2023)
library(ggplot2)
library(gridExtra)
library(ExtDist)
library(reshape2)

#Defining Mann Whitney function -------------------------
mann_whitney = function(x,y,n,m){     
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


R=1000                 #Replication number
n=c(1,3,10,25)       #Sample size of x
m=c(2,5,8,35)       #Sample size of y


#Distribution-free under H0 for continuous dist.-------------------------
#Normal Distribution--------------------------------------
test_stat.normal=test_stat_cauchy=test_stat.logis=
  test_stat.laplace=test_stat.bin=test_stat.poi=
  test_stat.geom=test_stat.hgeom=matrix(0,nrow=R,ncol=length(n)) 
for(i in 1:length(n))
{
  for (k in 1:R)
  {
    x_normal=rnorm(n[i],0,1)
    y_normal=rnorm(m[i],0,1)
    test_stat.normal[k,i]=mann_whitney(x_normal,y_normal,n[i],m[i])
    x_cauchy = rcauchy(n[i],0,1)
    y_cauchy = rcauchy(m[i],0,1)
    test_stat_cauchy[k,i] = mann_whitney(x_cauchy,y_cauchy,n[i],m[i])
    x_logis=rlogis(n[i],0,1)
    y_logis=rlogis(m[i],0,1)
    test_stat.logis[k,i]=mann_whitney(x_logis,y_logis,n[i],m[i])
    x_laplace=rLaplace(n[i],0,1)
    y_laplace=rLaplace(m[i],0,1)
    test_stat.laplace[k,i]=mann_whitney(x_laplace,y_laplace,n[i],m[i])
    
    x_bin=rbinom(n[i],10,.5)
    y_bin=rbinom(m[i],10,.5)
    test_stat.bin[k,i]=mann_whitney(x_bin,y_bin,n[i],m[i])
    x_poi=rpois(n[i],10)
    y_poi=rpois(m[i],10)
    test_stat.poi[k,i]=mann_whitney(x_poi,y_poi,n[i],m[i])
    x_geom=rgeom(n[i],.7)
    y_geom=rgeom(m[i],.7)
    test_stat.geom[k,i]=mann_whitney(x_geom,y_geom,n[i],m[i])
    x_hgeom=rhyper(n[i],10,20,3)
    y_hgeom=rhyper(m[i],10,20,3)
    test_stat.hgeom[k,i]=mann_whitney(x_hgeom,y_hgeom,n[i],m[i])
    
    
  }
}

#Normal
p1=ggplot(data=NULL,aes(x=test_stat.normal[,1]))+
  geom_bar(width=0.01,col="skyblue")+
  labs(title = 'Normal',x='Test Statistic')+
  theme(plot.title =element_text(size=16,hjust=.5,face='bold'),
        plot.subtitle = element_text(
          size=14,hjust=.5,face='italic'),
        legend.title = element_text(hjust=.5), axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))+theme_light()

p2=ggplot(data=NULL,aes(x=test_stat.normal[,2]))+
  geom_bar(width=0.1,col="skyblue")+
  labs(title = 'Normal',x='Test Statistic')+
  theme(plot.title =element_text(size=16,hjust=.5,face='bold'),
        plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
        legend.title = element_text(hjust=.5),axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))+theme_light()

p3=ggplot(data=NULL,aes(x=test_stat.normal[,3]))+
  geom_bar(width=0.3,col="skyblue")+
  labs(title = 'Normal',x='Test Statistic')+
  theme(plot.title =element_text(size=16,hjust=.5,face='bold'),
        plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
        legend.title = element_text(hjust=.5),axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))+theme_light()

p4=ggplot(data=NULL,aes(x=test_stat.normal[,4]))+
  geom_bar(col="skyblue")+
  labs(title = 'Normal',x='Test Statistic')+theme(plot.title =
                                                    element_text(size=16,hjust=.5,face='bold'),plot.subtitle = element_text(
                                                      size=14,hjust=.5,face='italic'),legend.title = element_text(hjust=.5),
                                                  axis.title = element_text(face='bold'),axis.text=element_text(face='bold'))+
  theme_light()

#Cauchy Distribution--------------------------

r1=ggplot(data=NULL,aes(x=test_stat_cauchy[,1]))+
  geom_bar(width = 0.01,col="skyblue")+
  labs(title = 'Cauchy',x='Test Statistic')+
  theme(plot.title =
          element_text(size=
                         16,
                       hjust=.5,face='bold'),
        plot.subtitle = element_text(
          size=14,hjust=.5,face='italic'
        ),
        legend.title = element_text(hjust=.5),
        axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))+theme_light()

r2=ggplot(data=NULL,aes(x=test_stat_cauchy[,2]))+
  geom_bar(width=0.1,col="skyblue")+
  labs(title = 'Cauchy',x='Test Statistic')+
  theme(plot.title =element_text(size=16,hjust=.5,face='bold'),
        plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
        legend.title = element_text(hjust=.5),
        axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))+theme_light()

r3=ggplot(data=NULL,aes(x=test_stat_cauchy[,3]))+
  geom_bar(width=0.3,col="skyblue")+
  labs(title = 'Cauchy',x='Test Statistic')+theme(plot.title =element_text(size=16,
                                                                           hjust=.5,face='bold'),plot.subtitle = element_text(
                                                                             size=14,hjust=.5,face='italic'),legend.title = element_text(hjust=.5),
                                                  axis.title = element_text(face='bold'),axis.text=element_text(face='bold'))+
  theme_light()

r4=ggplot(data=NULL,aes(x=test_stat_cauchy[,4]))+
  geom_bar(col="skyblue")+
  labs(title = 'Cauchy',x='Test Statistic')+
  theme(plot.title =element_text(size=16,hjust=.5,face='bold'),
        plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
        legend.title = element_text(hjust=.5),
        axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))+theme_light()


#Logistic Distribution------------------------------------
q1=ggplot(data=NULL,aes(x=test_stat.logis[,1]))+
  geom_bar(width = 0.01,col="skyblue")+
  labs(title = 'Logistic',x='Test Statistic')+
  theme(plot.title =element_text(size=16,hjust=.5,face='bold'),
        plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
        legend.title = element_text(hjust=.5),axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))+theme_light()

q2=ggplot(data=NULL,aes(x=test_stat.logis[,2]))+
  geom_bar(width=0.1,col="skyblue")+
  labs(title = 'Logistic',x='Test Statistic')+
  theme(plot.title =element_text(size=16,hjust=.5,face='bold'),
        plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
        legend.title = element_text(hjust=.5),axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))+theme_light()


q3=ggplot(data=NULL,aes(x=test_stat.logis[,3]))+
  geom_bar(width=0.3,col="skyblue")+
  labs(title = 'Logistic',x='Test Statistic')+theme(plot.title =element_text(size=16,
                                                                             hjust=.5,face='bold'),plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
                                                    legend.title = element_text(hjust=.5),axis.title = element_text(face='bold'),
                                                    axis.text=element_text(face='bold'))+theme_light()


q4=ggplot(data=NULL,aes(x=test_stat.logis[,4]))+
  geom_bar(col="skyblue")+
  labs(title = 'Logistic',x='Test Statistic')+theme(plot.title =element_text(size=16,
                                                                             hjust=.5,face='bold'),plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
                                                    legend.title = element_text(hjust=.5),axis.title = element_text(face='bold'),
                                                    axis.text=element_text(face='bold'))+theme_light()



#Double Exponential Distribution---------------------
s1=ggplot(data=NULL,aes(x=test_stat.laplace[,1]))+
  geom_bar(width = 0.01,col="skyblue")+
  labs(title = 'Laplace',x='Test Statistic')+theme(plot.title =element_text(size=16,
                                                                            hjust=.5,face='bold'),plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
                                                   legend.title = element_text(hjust=.5),axis.title = element_text(face='bold'),
                                                   axis.text=element_text(face='bold'))+theme_light()


s2=ggplot(data=NULL,aes(x=test_stat.laplace[,2]))+
  geom_bar(width=0.1,col="skyblue")+
  labs(title = 'Laplace',x='Test Statistic')+
  theme(plot.title =element_text(size=16,hjust=.5,face='bold'),
        plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
        legend.title = element_text(hjust=.5),axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))+theme_light()


s3=ggplot(data=NULL,aes(x=test_stat.laplace[,3]))+
  geom_bar(width=0.3,col="skyblue")+
  labs(title = 'Laplace',x='Test Statistic')+theme(plot.title =element_text(size=16,
                                                                            hjust=.5,face='bold'),plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
                                                   legend.title = element_text(hjust=.5),axis.title = element_text(face='bold'),
                                                   axis.text=element_text(face='bold'))+theme_light()


s4=ggplot(data=NULL,aes(x=test_stat.laplace[,4]))+
  geom_bar(col="skyblue")+
  labs(title = 'Laplace',x='Test Statistic')+theme(plot.title =element_text(size=16,
                                                                            hjust=.5,face='bold'),plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
                                                   legend.title = element_text(hjust=.5),axis.title = element_text(face='bold'),
                                                   axis.text=element_text(face='bold'))+theme_light()

grid.arrange(p1,r1,q1,s1,nrow=2,ncol=2,top="Column Diagram of the Test Statistic under H0 when n=1,m=2(Distribution Function is Continuous)")
grid.arrange(p2,r2,q2,s2,nrow=2,ncol=2,top="Column Diagram of the Test Statistic under H0 when n=3,m=5(Distribution Function is Continuous)")
grid.arrange(p3,r3,q3,s3,nrow=2,ncol=2,top="Column Diagram of the Test Statistic under H0 when n=10,m=8(Distribution Function is Continuous)")
grid.arrange(p4,r4,q4,s4,nrow=2,ncol=2,top="Column Diagram of the Test Statistic under H0 when n=25,m=35(Distribution Function is Continuous)")

# Robustness of Mann Whitney Statistic-----------------------
n_out_small = 6
m_out_small = 6
t_stat_out=array(0)
mann_whit_norm=mann_whit_exp=mann_whit_logis=mann_whit_cauchy=array(0)
for(i in 1:R){
  x_out.norm = c(rnorm(n_out_small),runif(2,100,200))
  y_out.norm = c(rnorm(m_out_small),runif(5,100,200))
  mann_whit_norm[i] = mann_whitney(x_out.norm,y_out.norm,n_out_small+2,m_out_small+5)
  x_out.exp = c(rexp(n_out_small),runif(2,10,20))
  y_out.exp = c(rexp(m_out_small),runif(5,10,20))
  mann_whit_exp[i] = mann_whitney(x_out.exp,y_out.exp,n_out_small+2,m_out_small+5)
  x_out.logis = c(rlogis(n_out_small),runif(2,10,20))
  y_out.logis = c(rlogis(m_out_small),runif(5,10,20))
  mann_whit_logis[i] = mann_whitney(x_out.logis,y_out.logis,n_out_small+2,m_out_small+5)
  x_out.cauchy = c(rcauchy(n_out_small),runif(2,10,20))
  y_out.cauchy = c(rcauchy(m_out_small),runif(5,10,20))
  mann_whit_cauchy[i] = mann_whitney(x_out.cauchy,y_out.cauchy,n_out_small+2,m_out_small+5)
  
  
}

o1=ggplot(data=NULL,aes(x=mann_whit_norm,y=..density..))+
  geom_histogram(bins=10,fill='skyblue',col='black')+
  labs(title = 'Normal',x='Test Statistic')+
  theme(plot.title =element_text(size=16,hjust=.5,face='bold'),
        plot.subtitle = element_text(
          size=14,hjust=.5,face='italic'),
        legend.title = element_text(hjust=.5), axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))+theme_light()+
  geom_density(color='black')

o2=ggplot(data=NULL,aes(x=mann_whit_exp,y=..density..))+
  geom_histogram(bins=10,fill='skyblue',col='black')+
  labs(title = 'Exponential',x='Test Statistic')+
  theme(plot.title =element_text(size=16,hjust=.5,face='bold'),
        plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
        legend.title = element_text(hjust=.5),axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))+theme_light()+
  geom_density(color='black')

o3=ggplot(data=NULL,aes(x=mann_whit_logis,y=..density..))+
  geom_histogram(bins=10,fill='skyblue',col='black')+
  labs(title = 'Logistic',x='Test Statistic')+
  theme(plot.title =element_text(size=16,hjust=.5,face='bold'),
        plot.subtitle = element_text(
          size=14,hjust=.5,face='italic'),
        legend.title = element_text(hjust=.5), axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))+theme_light()+
  geom_density(color='black')

o4=ggplot(data=NULL,aes(x=mann_whit_cauchy,y=..density..))+
  geom_histogram(bins=10,fill='skyblue',col='black')+
  labs(title = 'Cauchy',x='Test Statistic')+
  theme(plot.title =element_text(size=16,hjust=.5,face='bold'),
        plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
        legend.title = element_text(hjust=.5),axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))+theme_light()+
  geom_density(color='black')

gridExtra::grid.arrange(o1,o2,o3,o4,ncol=2,nrow=2,top='Distribution under H0 when outliers are present in the dataset')

# Asymptotic Distribution of Test Statistic when the Distribution Functions are Continuous:

## Under H~0~:
theta = 0
n_large = c(1,3,10,25)
m_large = c(2,5,8,35)
test_stat_null=z_null=test_stat_null_d=z_null_d=z_null_p=test_stat_null_p=matrix(0,nrow=R,ncol=length(n_large))
mean_null=mean_null_d=var_null=var_null_d=mean_null_p=var_null_p=array(0)


for(j in 1:length(n_large))
{
  for(i in 1:R)
  {
    x = rnorm(n_large[j],theta,1)
    y = rnorm(m_large[j],theta,1)
    
    test_stat_null[i,j] = mann_whitney(x,y,n_large[j],m_large[j])
    mean_null[j] = n_large[j]*m_large[j]/2
    var_null[j] = n_large[j]*m_large[j]*(n_large[j]+m_large[j]+1)/12
    
    z_null[i,j] = (test_stat_null[i,j] - mean_null[j])/sqrt(var_null[j])
    
    x_b = rbinom(n_large[j],5,0.3)
    y_b = rbinom(m_large[j],5,0.3)
    
    test_stat_null_d[i,j] = mann_whitney(x_b,y_b,n_large[j],m_large[j])
    mean_null_d[j] = n_large[j]*m_large[j]/2
    var_null_d[j] = n_large[j]*m_large[j]*(n_large[j]+m_large[j]+1)/12
    
    z_null_d[i,j] = (test_stat_null_d[i,j] - mean_null_d[j])/sqrt(var_null_d[j])
    
    x_p = rpois(n_large[j],3)
    y_p = rpois(m_large[j],3)
    
    test_stat_null_p[i,j] = mann_whitney(x_p,y_p,n_large[j],m_large[j])
    mean_null_p[j] = n_large[j]*m_large[j]/2
    var_null_p[j] = n_large[j]*m_large[j]*(n_large[j]+m_large[j]+1)/12
    
    z_null_p[i,j] = (test_stat_null_p[i,j] - mean_null_p[j])/sqrt(var_null_p[j])
  }
}
qq1=ggplot(NULL,aes(sample=z_null[,1]))+stat_qq()+stat_qq_line()+labs(title="n=1,m=2",
                                                                      x="Theoritical N(0,1) Quantiles",y="Observed Quantiles")
qq2=ggplot(NULL,aes(sample=z_null[,2]))+stat_qq()+stat_qq_line()+labs(title="n=3,m=5",
                                                                      x="Theoritical N(0,1) Quantiles",y="Observed Quantiles")
qq3=ggplot(NULL,aes(sample=z_null[,3]))+stat_qq()+stat_qq_line()+labs(title="n=10,m=8",
                                                                      x="Theoritical N(0,1) Quantiles",y="Observed Quantiles")
qq4=ggplot(NULL,aes(sample=z_null[,4]))+stat_qq()+stat_qq_line()+labs(title="n=25,m=35",
                                                                      x="Theoritical N(0,1) Quantiles",y="Observed Quantiles")

grid.arrange(qq1,qq2,qq3,qq4,nrow=2,ncol=2,top="QQplot of Mann Whitney Statistic under H0 (Distribution Function is Continuous)")

## Under H1:
theta.alt=seq(1.5,6,length.out=4)
n=c(1,3,10,25)
m=c(2,5,8,35)

test_stat_alt=z_alt=matrix(0,nrow=R,ncol=length(theta.alt))
mean_alt=var_alt=array(0)

for(j in 1:length(theta.alt))
{
  for(i in 1:R)
  {
    x=rnorm(n[1],0,1)
    y=rnorm(m[1],theta.alt[j],1)
    test_stat_alt[i,j]=mann_whitney(x,y,n[1],m[1]) 
    
  }
  mean_alt[j]=mean(test_stat_alt[,j])
  var_alt[j]=var(test_stat_alt[,j])
}
z_alt=(test_stat_alt - mean_alt)/sqrt(var(var_alt))

a1.1=ggplot(data=NULL)+
  geom_histogram(mapping=aes(x=z_alt[,1],y=..density..),bins=50,fill='green',col='black')+
  labs(x='Standardised Test Statistic',title="theta=1.5")+theme(plot.title =element_text(size=16,
                                                                                         hjust=.5,face='bold'),plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
                                                                legend.title = element_text(hjust=.5),axis.title = element_text(face='bold'),
                                                                axis.text=element_text(face='bold'))+theme_light()+geom_density(color='black')

a1.2=ggplot(data=NULL,aes(x=z_alt[,2],y=..density..))+
  geom_histogram(bins=50,fill='green',col='black')+
  labs(x='Standardised Test Statistic',title="theta=3.0")+theme(plot.title =element_text(size=6,
                                                                                         hjust=.5,face='bold'),plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
                                                                legend.title = element_text(hjust=.5),axis.title = element_text(face='bold'),
                                                                axis.text=element_text(face='bold'))+theme_light()+geom_density(color='black')

a1.3=ggplot(data=NULL,aes(x=z_alt[,3],y=..density..))+
  geom_histogram(bins=50,fill='green',col='black')+
  labs(x='Standardised Test Statistic',title="theta=4.5")+theme(plot.title =element_text(size=16,
                                                                                         hjust=.5,face='bold'),plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
                                                                legend.title = element_text(hjust=.5),axis.title = element_text(face='bold'),
                                                                axis.text=element_text(face='bold'))+theme_light()+geom_density(color='black')

a1.4=ggplot(data=NULL,aes(x=z_alt[,4],y=..density..))+
  geom_histogram(bins=50,fill='green',col='black')+
  labs(x='Standardised Test Statistic',title="theta=6.0")+theme(plot.title =element_text(size=16,
                                                                                         hjust=.5,face='bold'),plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
                                                                legend.title = element_text(hjust=.5), axis.title = element_text(face='bold'),
                                                                axis.text=element_text(face='bold'))+theme_light()+ geom_density(color='black')

grid.arrange(a1.1,a1.2,a1.3,a1.4,nrow=2,ncol=2,top="Histogram of the Test Statistic under H1 when n=1,m=2 (Distribution Function is Continuous)")

for(j in 1:length(theta.alt))
{
  for(i in 1:R)
  {
    x=rnorm(n[2],0,1)
    y=rnorm(m[2],theta.alt[j],1)
    test_stat_alt[i,j]=mann_whitney(x,y,n[2],m[2]) 
    
  }
  mean_alt[j]=mean(test_stat_alt[,j])
  var_alt[j]=var(test_stat_alt[,j])
}
z_alt=(test_stat_alt - mean_alt)/sqrt(var(var_alt))

a2.1=ggplot(data=NULL,aes(x=z_alt[,1],y=..density..))+
  geom_histogram(bins=50,fill='green',col='black')+
  labs(x='Standardised Test Statistic',title="theta=1.5")+theme(plot.title =element_text(size=16,
                                                                                         hjust=.5,face='bold'),plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
                                                                legend.title = element_text(hjust=.5),axis.title = element_text(face='bold'),
                                                                axis.text=element_text(face='bold'))+theme_light()+geom_density(color='black')

a2.2=ggplot(data=NULL,aes(x=z_alt[,2],y=..density..))+
  geom_histogram(bins=50,fill='green',col='black')+
  labs(x='Standardised Test Statistic',title="theta=3.0")+theme(plot.title =element_text(size=16,
                                                                                         hjust=.5,face='bold'),plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
                                                                legend.title = element_text(hjust=.5),axis.title = element_text(face='bold'),
                                                                axis.text=element_text(face='bold'))+theme_light() + geom_density(color='black')

a2.3=ggplot(data=NULL,aes(x=z_alt[,3],y=..density..))+
  geom_histogram(bins=50,fill='green',col='black')+
  labs(x='Standardised Test Statistic',title="theta=4.5")+theme(plot.title =element_text(size=16,
                                                                                         hjust=.5,face='bold'),plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
                                                                legend.title = element_text(hjust=.5),axis.title = element_text(face='bold'),
                                                                axis.text=element_text(face='bold'))+theme_light()+geom_density(color='black')


a2.4=ggplot(data=NULL,aes(x=z_alt[,4],y=..density..))+
  geom_histogram(bins=50,fill='green',col='black',title="theta=6.0")+
  labs(x='Standardised Test Statistic',title="theta=6.0")+theme(plot.title =element_text(size=16,
                                                                                         hjust=.5,face='bold'),plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
                                                                legend.title = element_text(hjust=.5),axis.title = element_text(face='bold'),
                                                                axis.text=element_text(face='bold'))+theme_light()+geom_density(color='black')

grid.arrange(a2.1,a2.2,a2.3,a2.4,nrow=2,ncol=2,top="Histogram of the Test Statistic under H1 when n=3,m=5(Distribution Function is Continuous)")

for(j in 1:length(theta.alt))
{
  for(i in 1:R)
  {
    x=rnorm(n[3],0,1)
    y=rnorm(m[3],theta.alt[j],1)
    test_stat_alt[i,j]=mann_whitney(x,y,n[3],m[3]) 
    
  }
  mean_alt[j]=mean(test_stat_alt[,j])
  var_alt[j]=var(test_stat_alt[,j])
}
z_alt=(test_stat_alt - mean_alt)/sqrt(var(var_alt))

a3.1=ggplot(data=NULL,aes(x=z_alt[,1],y=..density..))+
  geom_histogram(bins=50,fill='green',col='black')+
  labs(x='Standardised Test Statistic',title="theta=1.5")+theme(plot.title =element_text(size=16,
                                                                                         hjust=.5,face='bold'),plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
                                                                legend.title = element_text(hjust=.5),axis.title = element_text(face='bold'),
                                                                axis.text=element_text(face='bold'))+theme_light()+geom_density(color='black')

a3.2=ggplot(data=NULL,aes(x=z_alt[,2],y=..density..))+
  geom_histogram(bins=50,fill='green',col='black')+
  labs(x='Standardised Test Statistic',title="theta=3.0")+
  theme(plot.title =element_text(size=16,hjust=.5,face='bold'),
        plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
        legend.title = element_text(hjust=.5),axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))+theme_light()+geom_density(color='black')

a3.3=ggplot(data=NULL,aes(x=z_alt[,3],y=..density..))+
  geom_histogram(bins=50,fill='green',col='black')+
  labs(x='Standardised Test Statistic',title="theta=4.5")+
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
  geom_density(color='black')

a3.4=ggplot(data=NULL,aes(x=z_alt[,4],y=..density..))+
  geom_histogram(bins=50,fill='green',col='black')+
  labs(x='Standardised Test Statistic',title="theta=6.0")+
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
  geom_density(color='black')

grid.arrange(a3.1,a3.2,a3.3,a3.4,nrow=2,ncol=2,top="Histogram of the Test Statistic under H1 when n=10,m=8(Distribution Function is Continuous)")

for(j in 1:length(theta.alt))
{
  for(i in 1:R)
  {
    x=rnorm(n[4],0,1)
    y=rnorm(m[4],theta.alt[j],1)
    test_stat_alt[i,j]=mann_whitney(x,y,n[4],m[4]) 
    
  }
  mean_alt[j]=mean(test_stat_alt[,j])
  var_alt[j]=var(test_stat_alt[,j])
}
z_alt=(test_stat_alt - mean_alt)/sqrt(var(var_alt))

a4.1=ggplot(data=NULL,aes(x=z_alt[,1],y=..density..))+
  geom_histogram(bins=50,fill='green',col='black')+
  labs(x='Standardised Test Statistic',title="theta=1.5")+
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
  geom_density(color='black')



a4.2=ggplot(data=NULL,aes(x=z_alt[,2],y=..density..))+
  geom_histogram(bins=50,fill='green',col='black')+
  labs(x='Standardised Test Statistic',title="theta=3.0")+
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
  geom_density(color='black')


a4.3=ggplot(data=NULL,aes(x=z_alt[,3],y=..density..))+
  geom_histogram(bins=50,fill='green',col='black')+
  labs(x='Standardised Test Statistic',title="theta=4.5")+
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
  geom_density(color='black')


a4.4=ggplot(data=NULL,aes(x=z_alt[,4],y=..density..))+
  geom_histogram(bins=50,fill='green',col='black')+
  labs(x='Standardised Test Statistic',title="theta=6.0")+
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
  geom_density(color='black')

grid.arrange(a4.1,a4.2,a4.3,a4.4,nrow=2,ncol=2,top="Histogram of the Test Statistic under H1 when n=25,m=35(Distribution Function is Continuous)")

## Checking for Distribution free Of Mann Whitney Statistic when the Distribution Function is not Continuous:

#Binomial
a1=ggplot(data=NULL,aes(x=test_stat.bin[,1]))+
  geom_bar(width=0.01,col="skyblue")+
  labs(title = 'Binomal(10,0.5)',x='Test Statistic')+
  theme(plot.title =element_text(size=16,hjust=.5,face='bold'),
        plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
        legend.title = element_text(hjust=.5),
        axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))+theme_light()

a2=ggplot(data=NULL,aes(x=test_stat.bin[,2]))+
  geom_bar(width=0.1,col="skyblue")+
  labs(title = 'Binomal',x='Test Statistic')+
  theme(plot.title =element_text(size=16,hjust=.5,face='bold'),
        plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
        legend.title = element_text(hjust=.5),axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))+theme_light()

a3=ggplot(data=NULL,aes(x=test_stat.bin[,3]))+
  geom_bar(width=0.3,col="skyblue")+
  labs(title = 'Binomial',x='Test Statistic')+
  theme(plot.title =element_text(size=16,hjust=.5,face='bold'),
        plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
        legend.title = element_text(hjust=.5),axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))+theme_light()

a4=ggplot(data=NULL,aes(x=test_stat.bin[,4]))+
  geom_bar(col="skyblue")+
  labs(title = 'Binomial',x='Test Statistic')+
  theme(plot.title =element_text(size= 16,hjust=.5,face='bold'),
        plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
        legend.title = element_text(hjust=.5),axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))+theme_light()

#Poisson
b1=ggplot(data=NULL,aes(x=test_stat.poi[,1]))+
  geom_bar(width=0.01,col="skyblue")+
  labs(title = 'Poisson(10)',x='Test Statistic')+
  theme(plot.title =element_text(size=16,hjust=.5,face='bold'),
        plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
        legend.title = element_text(hjust=.5),axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))+theme_light()

b2=ggplot(data=NULL,aes(x=test_stat.poi[,2]))+
  geom_bar(width=0.1,col="skyblue")+
  labs(title = 'Poisson',x='Test Statistic')+
  theme(plot.title =element_text(size=16,hjust=.5,face='bold'),
        plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
        legend.title = element_text(hjust=.5),axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))+theme_light()

b3=ggplot(data=NULL,aes(x=test_stat.poi[,3]))+
  geom_bar(width=0.3,col="skyblue")+
  labs(title = 'Poisson',x='Test Statistic')+
  theme(plot.title =element_text(size=16,hjust=.5,face='bold'),
        plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
        legend.title = element_text(hjust=.5),axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))+theme_light()

b4=ggplot(data=NULL,aes(x=test_stat.poi[,4]))+
  geom_bar(col="skyblue")+
  labs(title = 'Poisson',x='Test Statistic')+
  theme(plot.title =element_text(size=16,hjust=.5,face='bold'),
        plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
        legend.title = element_text(hjust=.5),axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))+theme_light()

#Geometric
c1=ggplot(data=NULL,aes(x=test_stat.geom[,1]))+
  geom_bar(width=0.01,col="skyblue")+
  labs(title = 'Geometric(0.7)',x='Test Statistic')+
  theme(plot.title =element_text(size=16,hjust=.5,face='bold'),
        plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
        legend.title = element_text(hjust=.5),axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))+theme_light()

c2=ggplot(data=NULL,aes(x=test_stat.geom[,2]))+
  geom_bar(width=0.1,col="skyblue")+
  labs(title = 'Geometric',x='Test Statistic')+
  theme(plot.title =element_text(size=16,hjust=.5,face='bold'),
        plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
        legend.title = element_text(hjust=.5),axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))+theme_light()
c3=ggplot(data=NULL,aes(x=test_stat.geom[,3]))+
  geom_bar(width=0.3,col="skyblue")+
  labs(title = 'Geometric',x='Test Statistic')+
  theme(plot.title =element_text(size=16,hjust=.5,face='bold'),
        plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
        legend.title = element_text(hjust=.5),axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))+theme_light()

c4=ggplot(data=NULL,aes(x=test_stat.geom[,4]))+
  geom_bar(col="skyblue")+
  labs(title = 'Geometric',x='Test Statistic')+
  theme(plot.title =element_text(size=16,hjust=.5,face='bold'),
        plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
        legend.title = element_text(hjust=.5),axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))+theme_light()

#Hypergeometric
d1=ggplot(data=NULL,aes(x=test_stat.hgeom[,1]))+
  geom_bar(width=0.01,col="skyblue")+
  labs(title = 'Hypergeometric(10,20,0.3)',x='Test Statistic')+
  theme(plot.title =element_text(size=16,hjust=.5,face='bold'),
        plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
        legend.title = element_text(hjust=.5),axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))+theme_light()

d2=ggplot(data=NULL,aes(x=test_stat.hgeom[,2]))+
  geom_bar(width=0.1,col="skyblue")+
  labs(title = 'Hypereometric',x='Test Statistic')+
  theme(plot.title =element_text(size=16,hjust=.5,face='bold'),
        plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
        legend.title = element_text(hjust=.5),axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))+theme_light()

d3=ggplot(data=NULL,aes(x=test_stat.hgeom[,3]))+
  geom_bar(width=0.3,col="skyblue")+
  labs(title = 'Hypergeometric',x='Test Statistic')+theme(plot.title = element_text(size=16,hjust=.5,face='bold'),
                                                          plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
                                                          legend.title = element_text(hjust=.5),axis.title = element_text(face='bold'),
                                                          axis.text=element_text(face='bold'))+theme_light()


d4=ggplot(data=NULL,aes(x=test_stat.hgeom[,4]))+
  geom_bar(col="skyblue")+
  labs(title ='Hypergeometric',x='Test Statistic')+theme(plot.title =element_text(size=16,
                                                                                  hjust=.5,face='bold'),plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
                                                         legend.title = element_text(hjust=.5),axis.title = element_text(face='bold'),
                                                         axis.text=element_text(face='bold'))+theme_light()

grid.arrange(a1,b1,c1,d1,ncol=2,nrow=2,top="Column Diagram of the Test Statistic Under H0 when n=1,m=2 (Distribution Function is not continuous)")
grid.arrange(a2,b2,c2,d2,ncol=2,nrow=2,top="Column Diagram of the Test Statistic under H0  when n=3,m=5(Distribution Function is not continuous)")
grid.arrange(a3,b3,c3,d3,ncol=2,nrow=2,top="Column Diagram of the Test Statistic under H0 when n=10,m=8(Distribution Function is not continuous)")
grid.arrange(a4,b4,c4,d4,ncol=2,nrow=2,top="Column Diagram of the Test Statistic under H0 when n=25,m=35(Distribution Function is not continuous)")

## Asymptotic Distribution of Test Statistic when the Distribution Functions are not continuous:
## Under H~0~:

### The observations are coming from Bin(5,0.3):
#Binomial----------

k1=ggplot(data=NULL,aes(x=z_null_d[,1],y=..density..))+
  geom_histogram(bins=15,fill='skyblue',col='black')+
  labs(title = 'n=1,m=2',x='Standardized Test Statistic')+
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
  geom_density(color='black')+
  scale_x_continuous(limits = c(-7,2.5))+
  scale_y_continuous(limits = c(0,0.65))

k2=ggplot(data=NULL,aes(x=z_null_d[,2],y=..density..))+
  geom_histogram(bins=50,fill='skyblue',col='black')+
  labs(title = 'n=3,m=5',x='Standardized Test Statistic')+
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
  geom_density(color='black')+
  scale_x_continuous(limits = c(-7,2.5))+
  scale_y_continuous(limits = c(0,0.65))

k3=ggplot(data=NULL,aes(x=z_null_d[,3],y=..density..))+
  geom_histogram(bins=50,fill='skyblue',col='black')+
  labs(title = 'n=10,m=8',x='Standardized Test Statistic')+
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
  geom_density(color='black')+
  scale_x_continuous(limits = c(-7,2.5))+
  scale_y_continuous(limits = c(0,0.65))

k4=ggplot(data=NULL,aes(x=z_null_d[,4],y=..density..))+
  geom_histogram(bins=50,fill='skyblue',col='black')+
  labs(title = 'n=25,m=35',x='Standardized Test Statistic')+
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
  geom_density(color='black')+
  scale_x_continuous(limits = c(-7,2.5))+
  scale_y_continuous(limits = c(0,0.65))

grid.arrange(k1,k2,k3,k4,nrow=2,ncol=2,top="Histogram of the Test Statistic under H0 When Underlying Distribution is Binomial Distribution (5,0.3)")

## Under H~1~:
### The observations are coming from Bin(5,p):
#asymptotic under H1 for discrete

p.alt=seq(0.4,0.83,length.out=4)
length(p.alt)
n=c(1,3,10,25)
m=c(2,5,8,35)

test_stat_alt=z_alt=matrix(0,nrow=R,ncol=length(p.alt))
mean_alt=var_alt=array(0)

for(j in 1:length(p.alt))
{
  for(i in 1:R)
  {
    x=rbinom(n[1],5,0.3)
    y=rbinom(m[1],5,p.alt[j])
    test_stat_alt[i,j]=mann_whitney(x,y,n[1],m[1]) 
    
  }
  mean_alt[j]=mean(test_stat_alt[,j])
  var_alt[j]=var(test_stat_alt[,j])
}
z_alt=(test_stat_alt - mean_alt)/sqrt(var(var_alt))

a1.1=ggplot(data=NULL)+
  geom_histogram(mapping=aes(x=z_alt[,1],y=..density..),bins=50,fill='green',col='black')+
  labs(x='Standardised Test Statistic',title="theta=0.4")+theme(plot.title =element_text(size=16,
                                                                                         hjust=.5,face='bold'),plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
                                                                legend.title = element_text(hjust=.5),axis.title = element_text(face='bold'),
                                                                axis.text=element_text(face='bold'))+theme_light()+geom_density(color='black')

a1.2=ggplot(data=NULL,aes(x=z_alt[,2],y=..density..))+
  geom_histogram(bins=50,fill='green',col='black')+
  labs(x='Standardised Test Statistic',title="theta=0.54")+theme(plot.title =element_text(size=6,
                                                                                          hjust=.5,face='bold'),plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
                                                                 legend.title = element_text(hjust=.5),axis.title = element_text(face='bold'),
                                                                 axis.text=element_text(face='bold'))+theme_light()+geom_density(color='black')

a1.3=ggplot(data=NULL,aes(x=z_alt[,3],y=..density..))+
  geom_histogram(bins=50,fill='green',col='black')+
  labs(x='Standardised Test Statistic',title="theta=0.68")+theme(plot.title =element_text(size=16,
                                                                                          hjust=.5,face='bold'),plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
                                                                 legend.title = element_text(hjust=.5),axis.title = element_text(face='bold'),
                                                                 axis.text=element_text(face='bold'))+theme_light()+geom_density(color='black')

a1.4=ggplot(data=NULL,aes(x=z_alt[,4],y=..density..))+
  geom_histogram(bins=50,fill='green',col='black')+
  labs(x='Standardised Test Statistic',title="theta=0.83")+theme(plot.title =element_text(size=16,
                                                                                          hjust=.5,face='bold'),plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
                                                                 legend.title = element_text(hjust=.5), axis.title = element_text(face='bold'),
                                                                 axis.text=element_text(face='bold'))+theme_light()+ geom_density(color='black')

grid.arrange(a1.1,a1.2,a1.3,a1.4,nrow=2,ncol=2,top="Histogram of the Test Statistic under H1 when n=1,m=2 (Distribution Function is not continuous)")
for(j in 1:length(p.alt))
{
  for(i in 1:R)
  {
    x=rbinom(n[2],5,0.3)
    y=rnorm(m[2],5,p.alt[i])
    test_stat_alt[i,j]=mann_whitney(x,y,n[2],m[2]) 
    
  }
  mean_alt[j]=mean(test_stat_alt[,j])
  var_alt[j]=var(test_stat_alt[,j])
}
z_alt=(test_stat_alt - mean_alt)/sqrt(var(var_alt))

a2.1=ggplot(data=NULL,aes(x=z_alt[,1],y=..density..))+
  geom_histogram(bins=50,fill='green',col='black')+
  labs(x='Standardised Test Statistic',title="theta=0.4")+theme(plot.title =element_text(size=16,
                                                                                         hjust=.5,face='bold'),plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
                                                                legend.title = element_text(hjust=.5),axis.title = element_text(face='bold'),
                                                                axis.text=element_text(face='bold'))+theme_light()+geom_density(color='black')

a2.2=ggplot(data=NULL,aes(x=z_alt[,2],y=..density..))+
  geom_histogram(bins=50,fill='green',col='black')+
  labs(x='Standardised Test Statistic',title="theta=0.54")+theme(plot.title =element_text(size=16,
                                                                                          hjust=.5,face='bold'),plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
                                                                 legend.title = element_text(hjust=.5),axis.title = element_text(face='bold'),
                                                                 axis.text=element_text(face='bold'))+theme_light() + geom_density(color='black')

a2.3=ggplot(data=NULL,aes(x=z_alt[,3],y=..density..))+
  geom_histogram(bins=50,fill='green',col='black')+
  labs(x='Standardised Test Statistic',title="theta=0.68")+theme(plot.title =element_text(size=16,
                                                                                          hjust=.5,face='bold'),plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
                                                                 legend.title = element_text(hjust=.5),axis.title = element_text(face='bold'),
                                                                 axis.text=element_text(face='bold'))+theme_light()+geom_density(color='black')


a2.4=ggplot(data=NULL,aes(x=z_alt[,4],y=..density..))+
  geom_histogram(bins=50,fill='green',col='black',title="theta=6.0")+
  labs(x='Standardised Test Statistic',title="theta=0.83")+theme(plot.title =element_text(size=16,
                                                                                          hjust=.5,face='bold'),plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
                                                                 legend.title = element_text(hjust=.5),axis.title = element_text(face='bold'),
                                                                 axis.text=element_text(face='bold'))+theme_light()+geom_density(color='black')

grid.arrange(a2.1,a2.2,a2.3,a2.4,nrow=2,ncol=2,top="Histogram of the Test Statistic under H1 when n=3,m=5(Distribution Function is not continuous)")

for(j in 1:length(p.alt))
{
  for(i in 1:R)
  {
    x=rbinom(n[3],5,0.3)
    y=rbinom(m[3],5,p.alt[j])
    test_stat_alt[i,j]=mann_whitney(x,y,n[3],m[3]) 
    
  }
  mean_alt[j]=mean(test_stat_alt[,j])
  var_alt[j]=var(test_stat_alt[,j])
}
z_alt=(test_stat_alt - mean_alt)/sqrt(var(var_alt))

a3.1=ggplot(data=NULL,aes(x=z_alt[,1],y=..density..))+
  geom_histogram(bins=50,fill='green',col='black')+
  labs(x='Standardised Test Statistic',title="theta=0.4")+theme(plot.title =element_text(size=16,
                                                                                         hjust=.5,face='bold'),plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
                                                                legend.title = element_text(hjust=.5),axis.title = element_text(face='bold'),
                                                                axis.text=element_text(face='bold'))+theme_light()+geom_density(color='black')

a3.2=ggplot(data=NULL,aes(x=z_alt[,2],y=..density..))+
  geom_histogram(bins=50,fill='green',col='black')+
  labs(x='Standardised Test Statistic',title="theta=0.54")+
  theme(plot.title =element_text(size=16,hjust=.5,face='bold'),
        plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
        legend.title = element_text(hjust=.5),axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))+theme_light()+geom_density(color='black')

a3.3=ggplot(data=NULL,aes(x=z_alt[,3],y=..density..))+
  geom_histogram(bins=50,fill='green',col='black')+
  labs(x='Standardised Test Statistic',title="theta=0.68")+
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
  geom_density(color='black')

a3.4=ggplot(data=NULL,aes(x=z_alt[,4],y=..density..))+
  geom_histogram(bins=50,fill='green',col='black')+
  labs(x='Standardised Test Statistic',title="theta=0.83")+
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
  geom_density(color='black')

grid.arrange(a3.1,a3.2,a3.3,a3.4,nrow=2,ncol=2,top="Histogram of the Test Statistic under H1 when n=10,m=8(Distribution Function is not continuous)")
for(j in 1:length(p.alt))
{
  for(i in 1:R)
  {
    x=rbinom(n[4],5,0.3)
    y=rbinom(m[4],5,p.alt[j])
    test_stat_alt[i,j]=mann_whitney(x,y,n[4],m[4]) 
    
  }
  mean_alt[j]=mean(test_stat_alt[,j])
  var_alt[j]=var(test_stat_alt[,j])
}
z_alt=(test_stat_alt - mean_alt)/sqrt(var(var_alt))

a4.1=ggplot(data=NULL,aes(x=z_alt[,1],y=..density..))+
  geom_histogram(bins=50,fill='green',col='black')+
  labs(x='Standardised Test Statistic',title="theta=0.4")+
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
  geom_density(color='black')



a4.2=ggplot(data=NULL,aes(x=z_alt[,2],y=..density..))+
  geom_histogram(bins=50,fill='green',col='black')+
  labs(x='Standardised Test Statistic',title="theta=0.54")+
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
  geom_density(color='black')


a4.3=ggplot(data=NULL,aes(x=z_alt[,3],y=..density..))+
  geom_histogram(bins=50,fill='green',col='black')+
  labs(x='Standardised Test Statistic',title="theta=0.68")+
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
  geom_density(color='black')


a4.4=ggplot(data=NULL,aes(x=z_alt[,4],y=..density..))+
  geom_histogram(bins=50,fill='green',col='black')+
  labs(x='Standardised Test Statistic',title="theta=0.83")+
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
  geom_density(color='black')

grid.arrange(a4.1,a4.2,a4.3,a4.4,nrow=2,ncol=2,top="Histogram of the Test Statistic under H1 when n=25,m=35(Distribution Function is not continuous)")

## Simulated size in case of small sample size(n=5, m=8)
n.test=5
m.test=8
theta = 0
alpha = c(.001,.01,.05,.1)
critval=c(0,4,8,10)

test_stat_null_norm=test_stat_null_cauchy=matrix(0,nrow=R,ncol=length(critval))
size_n=size_c=array(0)

for(j in 1:length(alpha))
{
  
  for(i in 1:R)
  {
    x = rnorm(n.test,theta,1)
    y = rnorm(m.test,theta,1)
    x_cauchy = rcauchy(n.test,theta)
    y_cauchy = rcauchy(m.test,theta)
    
    test_stat_null_norm[i,j] = mann_whitney(x,y,n.test,m.test)
    test_stat_null_cauchy[i,j] = mann_whitney(x_cauchy,y_cauchy,n.test,m.test)
    
    
  }
  size_n[j]=length(which(test_stat_null_norm[,j]<=critval[j]))/R
  size_c[j]=length(which(test_stat_null_cauchy[,j]<=critval[j]))/R
}

V=matrix(0,nrow=4,ncol=3)
V[,2]=size_n
V[,3]=size_c
V[,1]=alpha
colnames(V)=c('level of significance  ',"size(Normal)  ","size(Cauchy)")
V=as.data.frame(V)
V

## Simulated size in case of large sample size(n=100, m=200)
n.test=100
m.test=200
theta = 0
alpha = c(.001,.01,.05,.1)
critval=qnorm(alpha,0,1,lower.tail=T)

test_stat_null=matrix(0,nrow=R,ncol=length(critval))
size=array(0)

for(j in 1:length(alpha))
{
  
  for(i in 1:R)
  {
    x = rnorm(n.test,theta,1)
    y = rnorm(m.test,theta,1)
    
    test_stat_null[i,j] = (mann_whitney(x,y,n.test,m.test)-(n.test*m.test/2))/sqrt(n.test*m.test*(n.test+m.test+1)/12)
    
    
  }
  size[j]=length(which(test_stat_null[,j]<=critval[j]))/R
}
V=matrix(0,nrow=4,ncol=2)
V[,2]=size
V[,1]=alpha
colnames(V)=c('level of significance  ',"size  ")
V=as.data.frame(V)
V

## Simulated power curve in case of small sample size(n=5, m=8)
theta.alt.test=seq(0,7,by=0.1)
l=length(theta.alt.test)
n.test=5
m.test=8
alpha=c(.001,.01,.05,.1)
c_alpha=c(0,4,8,10)
count=array(0)
power1=power2=power3=power4=array(0)
test_stat_alt_p=matrix(0,nrow=R,ncol=l)

for(j in 1:l)
{
  
  for(i in 1:R)
  {
    x=rnorm(n.test,0,1)
    y=rnorm(m.test,theta.alt.test[j],1)
    test_stat_alt_p[i,j]=mann_whitney(x,y,n.test,m.test)
  }
  power1[j]=length(which(test_stat_alt_p[,j]<=0))/R
  power2[j]=length(which(test_stat_alt_p[,j]<=4))/R
  power3[j]=length(which(test_stat_alt_p[,j]<=8))/R
  power4[j]=length(which(test_stat_alt_p[,j]<=10))/R
}

power.data=melt(data.frame(theta.alt.test,power1,power2,power3,power4),
                id='theta.alt.test')
ggplot(data=power.data,aes(x=theta.alt.test,y=value,colour=variable))+
  geom_line()+
  labs(title = 'Power Curve for Mann Whitney for n=5,m=8',x='Theta',y="Power")+
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
  scale_color_manual(name="Value of alpha",
                     labels=c(".001",".01",".05",".1"),
                     values=c("red","green","blue",'maroon'))

## Simulated power curve in case of large sample size(n=18, m=20)
n.test=18
m.test=20
alpha=c(.001,.01,.05,.1)
c_alpha=c(75,100,123,135)
power1=power2=power3=power4=array(0)
test_stat_alt_p=matrix(0,nrow=R,ncol=l)


for(j in 1:l)
{
  
  for(i in 1:R)
  {
    x=rnorm(n.test,0,1)
    y=rnorm(m.test,theta.alt.test[j],1)
    test_stat_alt_p[i,j]=mann_whitney(x,y,n.test,m.test)
  }
  power1[j]=length(which(test_stat_alt_p[,j]<=c_alpha[1]))/R
  power2[j]=length(which(test_stat_alt_p[,j]<=c_alpha[2]))/R
  power3[j]=length(which(test_stat_alt_p[,j]<=c_alpha[3]))/R
  power4[j]=length(which(test_stat_alt_p[,j]<=c_alpha[4]))/R
}

power.data=melt(data.frame(theta.alt.test,power1,power2,power3,power4),
                id='theta.alt.test')
ggplot(data=power.data,aes(x=theta.alt.test,y=value,colour=variable))+
  geom_line()+
  labs(title = 'Power Curve for Mann Whitney for n=18,m=20',x='Theta',y="Power")+
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
  scale_color_manual(name="Value of alpha",
                     labels=c(".001",".01",".05",".1"),
                     values=c("red","green","blue",'maroon'))

## Asymptotic Consistency
n.test=3:20
m.test=3:20
alpha=0.05
c_alpha=c(0,1,4,7,11,15,21,27,34,42,51,61,72,83,95,109,123,138)


theta=c(0.1,2,4)
power1=power2=power3=array(0)
test_stat_alt_p1=test_stat_alt_p2=test_stat_alt_p3=matrix(0,nrow=R,ncol=length(n.test))

for(j in 1:length(n.test))
{
  
  for(i in 1:R)
  {
    x=rnorm(n.test[j],0,1)
    y1=rnorm(m.test[j],theta[1],1)
    y2=rnorm(m.test[j],theta[2],1)
    y3=rnorm(m.test[j],theta[3],1)
    test_stat_alt_p1[i,j]=mann_whitney(x,y1,n.test[j],m.test[j])
    test_stat_alt_p2[i,j]=mann_whitney(x,y2,n.test[j],m.test[j])
    test_stat_alt_p3[i,j]=mann_whitney(x,y3,n.test[j],m.test[j])
  }
  power1[j]=length(which(test_stat_alt_p1[,j]<=c_alpha[j]))/R
  power2[j]=length(which(test_stat_alt_p2[,j]<=c_alpha[j]))/R
  power3[j]=length(which(test_stat_alt_p3[,j]<=c_alpha[j]))/R
}

power.data=melt(data.frame(n.test,power1,power2,power3),
                id='n.test')
ggplot(data=power.data,aes(x=n.test,y=value,colour=variable))+
  geom_line()+
  labs(title = 'Convergence of Power for different values of theta',x='N=m+n',y="Power")+
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
  scale_color_manual(name="Value of Theta",
                     labels=c(".1",'2','4'),
                     values=c("red","green","blue"))

## PARAMETRIC COUNTERPART:
n=5
m=7
theta1=seq(0,0.5,by=0.01)
theta2=seq(0,8,by=0.5)
l1=length(theta1)
l2=length(theta2)
alpha=0.05
t.test.norm.1=t.test.cauchy.1=t.test.logis.1=mann.norm.1=mann.cauchy.1=mann.logis.1=matrix(0,nrow=R,ncol=l1)
power1.1=power2.1=power3.1=power4.1=power5.1=power6.1=array(0)
t.test.norm.2=t.test.cauchy.2=t.test.logis.2=mann.norm.2=mann.cauchy.2=mann.logis.2=matrix(0,nrow=R,ncol=l2)
power1.2=power2.2=power3.2=power4.2=power5.2=power6.2=array(0)
crit_logis=function()
{
  x.logis=rlogis(n,0,1)
  y.logis=rlogis(m,0,1)
  t.test(x.logis,y.logis,alternative="less",var.equal=T)$statistic
}
cut_off_logis=quantile(replicate(10000,crit_logis()),0.05)

crit_cauchy=function()
{
  x.cauchy=rcauchy(n,0,1)
  y.cauchy=rcauchy(m,0,1)
  t.test(x.cauchy,y.cauchy,alternative="less",var.equal=T)$statistic
}
cut_off_cauchy=quantile(replicate(10000,crit_cauchy()),0.05)



for(j in 1:l1)
{
  for (k in 1:R)
  {
    x.norm=rnorm(n,0,1)
    y.norm=rnorm(m,theta1[j],1)
    x.cauchy=rcauchy(n,0,1)
    y.cauchy=rcauchy(m,theta1[j],1)
    x.logis=rlogis(n,0,1)
    y.logis=rlogis(m,theta1[j],1)
    t.test.norm.1[k,j]=t.test(x.norm,y.norm,alternative="less",var.equal=T)$statistic
    t.test.cauchy.1[k,j]=t.test(x.cauchy,y.cauchy,alternative="less",var.equal=T)$statistic
    t.test.logis.1[k,j]=t.test(x.logis,y.logis,alternative="less",var.equal=T)$statistic
    mann.norm.1[k,j]=mann_whitney(x.norm,y.norm,n,m)
    mann.cauchy.1[k,j]=mann_whitney(x.cauchy,y.cauchy,n,m)
    mann.logis.1[k,j]=mann_whitney(x.logis,y.logis,n,m)
  }
  power1.1[j]=mean(t.test.norm.1[,j]<qt(0.05,10))
  power2.1[j]=mean(t.test.cauchy.1[,j]<cut_off_cauchy)
  power3.1[j]=mean(t.test.logis.1[,j]<cut_off_logis)
  power4.1[j]=mean(mann.norm.1[,j]<=6)
  power5.1[j]=mean(mann.cauchy.1[,j]<=7)
  power6.1[j]=mean(mann.logis.1[,j]<=7)
  
}
distribution_likelihood_normal=function(mu1=0,mu2=0)
{
  x=rnorm(n,mu1,1)
  y=rnorm(m,mu2,1)
  likelihood_0=function(alpha){
    log(prod(dnorm(x,alpha,1))*prod(dnorm(y,alpha,1)))
  }
  lik_h_0=optim(mean(c(x,y)),likelihood_0,method = "L-BFGS-B",lower=0,upper=1,control=list(fnscale=-1))
  likelihood_1=function(beta){
    log(prod(dnorm(x,beta[1],1))*prod(dnorm(x,beta[1]+beta[2],1)))
  }
  lik_h_1=optim(c(mean(x),mean(y)-mean(x)),likelihood_1,method = "L-BFGS-B",lower=0,control=list(fnscale=-1))
  lik_stat=exp(lik_h_0$value-lik_h_1$value)
  return(ifelse(lik_stat<1,lik_stat,NA))
}
#distribution_likelihood_normal()
population_pts1=replicate(1000,distribution_likelihood_normal())
cut_off=quantile(na.omit(population_pts1),probs=0.05)
power_normal=function(theta)
{
  mean(replicate(1000,(distribution_likelihood_normal(0,theta)<cut_off)) ,na.rm=TRUE)
}
power7.1=unlist(lapply(theta1, power_normal))
distribution_likelihood_cauchy=function(mu1=0,mu2=0)
{
  x=rcauchy(n,mu1,1)
  y=rcauchy(m,mu2,1)
  likelihood_0=function(alpha){
    log(prod(1/(1+(x-alpha)^2))*prod(1/(1+(y-alpha)^2)))
  }
  lik_h_0=optim(median(c(x,y)),likelihood_0,method = "L-BFGS-B",lower=0,upper=1,control=list(fnscale=-1))
  likelihood_1=function(beta){
    log(prod(1/(1+(x-beta[1])^2))*prod(1/(1+(y-beta[1]-beta[2])^2)))
  }
  lik_h_1=optim(c(median(x),median(y)-median(x)),likelihood_1,method = "CG",control=list(fnscale=-1))
  lik_stat=exp(lik_h_0$value-lik_h_1$value)
  return(ifelse(lik_stat<1,lik_stat,NA))
}
#distribution_likelihood_cauchy()
population_pts1=replicate(1000,distribution_likelihood_cauchy())
cut_off=quantile(na.omit(population_pts1),probs=0.05)
power_cauchy=function(theta)
{
  mean(replicate(1000,(distribution_likelihood_cauchy(0,theta)<cut_off)) ,na.rm=TRUE)
}
power8.1=unlist(lapply(theta1, power_cauchy))

test.data = melt(data.frame(theta1,power1.1,power4.1,power7.1),
                 id='theta1')
a.1.1=ggplot(data=test.data,aes(x=theta1,y=value,colour=variable))+
  geom_line()+
  labs(title = 'Power Curve for normal using Mann Whitney statistic,t and LRT statistic',x='theta',y="Power")+
  theme(plot.title =
          element_text(size=
                         16,
                       hjust=.5,face='bold'),
        plot.subtitle = element_text(
          size=14,hjust=.5,face='italic'),
        legend.title = element_text(hjust=.5),
        axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))+theme_light()+
  scale_color_manual(name='Power Curve',
                     labels=c('t-test','Mann Whitney','LRT'),
                     values=c('red','blue','green'))

a.2.1=ggplot(data=NULL,aes(x=theta1))+
  geom_line(aes(y=power2.1),col="red")+
  geom_line(aes(y=power5.1),col="blue")+
  geom_line(aes(y=power8.1),col="green")+
  labs(title = 'Power Curve for Cauchy using Mann Whitney statistic,t and LRT statistic',x='theta',y="Power")+
  theme(plot.title =
          element_text(size=
                         16,
                       hjust=.5,face='bold'),
        plot.subtitle = element_text(
          size=14,hjust=.5,face='italic'
        ),
        legend.title = element_text(hjust=.5),
        axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))+theme_light()
a.3.1=ggplot(data=NULL,aes(x=theta1))+
  geom_line(aes(y=power3.1),col="red")+  
  geom_line(aes(y=power6.1),col="blue")+
  labs(title = 'Power Curve for Logistic using Mann Whitney statistic and t statistic',x='theta',y="Power")+
  theme(plot.title =
          element_text(size=
                         16,
                       hjust=.5,face='bold'),
        plot.subtitle = element_text(
          size=14,hjust=.5,face='italic'
        ),
        legend.title = element_text(hjust=.5),
        axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))+theme_light()

gridExtra::grid.arrange(a.1.1,a.2.1,a.3.1,nrow=3,top="Comparison of Power for Normal, Cauchy and Logistic distribution in parametric and non parametric set up for smaller values of the location parameter")

for(j in 1:l2)
{
  for (k in 1:R)
  {
    x.norm=rnorm(n,0,1)
    y.norm=rnorm(m,theta2[j],1)
    x.cauchy=rcauchy(n,0,1)
    y.cauchy=rcauchy(m,theta2[j],1)
    x.logis=rlogis(n,0,1)
    y.logis=rlogis(m,theta2[j],1)
    t.test.norm.2[k,j]=t.test(x.norm,y.norm,alternative="less",var.equal=T)$statistic
    t.test.cauchy.2[k,j]=t.test(x.cauchy,y.cauchy,alternative="less",var.equal=T)$statistic
    t.test.logis.2[k,j]=t.test(x.logis,y.logis,alternative="less",var.equal=T)$statistic
    mann.norm.2[k,j]=mann_whitney(x.norm,y.norm,n,m)
    mann.cauchy.2[k,j]=mann_whitney(x.cauchy,y.cauchy,n,m)
    mann.logis.2[k,j]=mann_whitney(x.logis,y.logis,n,m)
  }
  power1.2[j]=mean(t.test.norm.2[,j]<qt(0.05,10))
  power2.2[j]=mean(t.test.cauchy.2[,j]<cut_off_cauchy)
  power3.2[j]=mean(t.test.logis.2[,j]<cut_off_logis)
  power4.2[j]=mean(mann.norm.2[,j]<=6)
  power5.2[j]=mean(mann.cauchy.2[,j]<=7)
  power6.2[j]=mean(mann.logis.2[,j]<=7)
  
}
distribution_likelihood_normal=function(mu1=0,mu2=0)
{
  x=rnorm(n,mu1,1)
  y=rnorm(m,mu2,1)
  likelihood_0=function(alpha){
    log(prod(dnorm(x,alpha,1))*prod(dnorm(y,alpha,1)))
  }
  lik_h_0=optim(mean(c(x,y)),likelihood_0,method = "L-BFGS-B",lower=0,upper=1,control=list(fnscale=-1))
  likelihood_1=function(beta){
    log(prod(dnorm(x,beta[1],1))*prod(dnorm(x,beta[1]+beta[2],1)))
  }
  lik_h_1=optim(c(mean(x),mean(y)-mean(x)),likelihood_1,method = "L-BFGS-B",lower=0,control=list(fnscale=-1))
  lik_stat=exp(lik_h_0$value-lik_h_1$value)
  return(ifelse(lik_stat<1,lik_stat,NA))
}
#distribution_likelihood_normal()
population_pts1=replicate(1000,distribution_likelihood_normal())
cut_off=quantile(na.omit(population_pts1),probs=0.05)
power_normal=function(theta)
{
  mean(replicate(1000,(distribution_likelihood_normal(0,theta)<cut_off)) ,na.rm=TRUE)
}
power7.2=unlist(lapply(theta2, power_normal))
distribution_likelihood_cauchy=function(mu1=0,mu2=0)
{
  x=rcauchy(n,mu1,1)
  y=rcauchy(m,mu2,1)
  likelihood_0=function(alpha){
    log(prod(1/(1+(x-alpha)^2))*prod(1/(1+(y-alpha)^2)))
  }
  lik_h_0=optim(median(c(x,y)),likelihood_0,method = "L-BFGS-B",lower=0,upper=1,control=list(fnscale=-1))
  likelihood_1=function(beta){
    log(prod(1/(1+(x-beta[1])^2))*prod(1/(1+(y-beta[1]-beta[2])^2)))
  }
  lik_h_1=optim(c(median(x),median(y)-median(x)),likelihood_1,method = "CG",control=list(fnscale=-1))
  lik_stat=exp(lik_h_0$value-lik_h_1$value)
  return(ifelse(lik_stat<1,lik_stat,NA))
}
#distribution_likelihood_cauchy()
population_pts1=replicate(1000,distribution_likelihood_cauchy())
cut_off=quantile(na.omit(population_pts1),probs=0.05)
power_cauchy=function(theta)
{
  mean(replicate(1000,(distribution_likelihood_cauchy(0,theta)<cut_off)) ,na.rm=TRUE)
}
power8.2=unlist(lapply(theta2, power_cauchy))

test.data2 = melt(data.frame(theta2,power1.2,power4.2,power7.2),
                  id='theta2')
a.1.2=ggplot(data=test.data2,aes(x=theta2,y=value,
                                 colour=variable))+
  geom_line()+
  labs(title = 'Power Curve for normal using Mann Whitney statistic,t and LRT statistic',x='theta',y="Power")+
  theme(plot.title =
          element_text(size=
                         16,
                       hjust=.5,face='bold'),
        plot.subtitle = element_text(
          size=14,hjust=.5,face='italic'),
        legend.title = element_text(hjust=.5),
        axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))+
  theme_light()+
  scale_color_manual(name='Power Curve',
                     labels=c('t-test','Mann Whitney','LRT'),
                     values=c('red','blue','green'))
a.2.2=ggplot(data=NULL,aes(x=theta2))+
  geom_line(aes(y=power2.2),col="red")+
  geom_line(aes(y=power5.2),col="blue")+
  geom_line(aes(y=power8.2),col="green")+
  labs(title = 'Power Curve for Cauchy using Mann Whitney statistic,t and LRT statistic',x='theta',y="Power")+
  theme(plot.title =
          element_text(size=
                         16,
                       hjust=.5,face='bold'),
        plot.subtitle = element_text(
          size=14,hjust=.5,face='italic'
        ),
        legend.title = element_text(hjust=.5),
        axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))+theme_light()
a.3.2=ggplot(data=NULL,aes(x=theta2))+
  geom_line(aes(y=power3.2),col="red")+  
  geom_line(aes(y=power6.2),col="blue")+
  labs(title = 'Power Curve for Logistic using Mann Whitney statistic and t statistic',x='theta',y="Power")+
  theme(plot.title =
          element_text(size=
                         16,
                       hjust=.5,face='bold'),
        plot.subtitle = element_text(
          size=14,hjust=.5,face='italic'
        ),
        legend.title = element_text(hjust=.5),
        axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))+theme_light()

gridExtra::grid.arrange(a.1.2,a.2.2,a.3.2,nrow=3,top="Comparison of Power for Normal, Cauchy and Logistic distribution in parametric and non parametric set up for high values of the location parameter compared to 0")

## Robustness: t Test and Test based on Mann Whitney U statistic
theta.alt.test=seq(0,30,.5)
t_stat_out.m=man_out=matrix(0,nrow=R,ncol=length(theta.alt.test))
power_t=power_m=array(0)
for(j in 1:length(theta.alt.test))
{
  
  for(i in 1:R)
  {
    x_out.norm = c(rnorm(n_out_small,0,1),rnorm(2,10,20))
    y_out.norm = c(rnorm(m_out_small,theta.alt.test[j],1),runif(2,10,20))
    man_out[i,j] = mann_whitney(x_out.norm,y_out.norm,n_out_small+2,m_out_small+1)
    t_stat_out.m[i,j] = (mean(y_out.norm) - mean(x_out.norm))/sqrt(((n_out_small+1)*var(x_out.norm) + (m_out_small+1)*var(y_out.norm))/(n_out_small+m_out_small+2))
    
  }
  
  power_t[j]=length(which(t_stat_out.m[,j]>=qt(0.95,14)))/R
  power_m[j]=length(which(man_out[,j]<=15))/R
}

power.data=melt(data.frame(theta.alt.test,power_t,power_m),
                id='theta.alt.test')

ggplot(data=power.data,aes(x=theta.alt.test,y=value,colour=variable))+
  geom_line()+
  labs(title = 'Comparison between Power Curve for t-test and Test using Man-Whitney U statistic',x='Theta',y="Power")+
  theme(plot.title =element_text(size=16,hjust=.5,face='bold'),
        plot.subtitle = element_text(size=14,hjust=.5,face='italic'),
        legend.title = element_text(hjust=.5),
        axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))+theme_light()+
  scale_color_manual(name="Test Statistic",
                     labels=c('t-Statistic','Man-Whitney'),
                     values=c("red","green"))

## Two Sample Estimation: Hodges Lehmann Estimator
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
mse1= ggplot(data=bias.data,aes(x=theta_seq,y=value,colour=variable))+
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
mse2=ggplot(data=bias.data2,aes(x=theta_seq,y=value,colour=variable))+
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
grid.arrange(mse1,mse2,nrow=2)

set.seed(1234)
bias_mle_cauchy=bias_mle_logis=bias_h_cauchy=bias_h_logis=array(0)
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
con1=ggplot(data=c.data,aes(x=N,y=value,colour=variable))+
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


c.data2=melt(data.frame(N,bias_mle_logis,bias_h_logis),
             id='N')
con2=ggplot(data=c.data2,aes(x=N,y=value,colour=variable))+
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
grid.arrange(con1,con2,nrow=2)
