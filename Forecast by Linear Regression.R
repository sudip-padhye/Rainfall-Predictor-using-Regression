forecast_by_linear_regression<-function(dataset)
{
  m<-nrow(dataset)
  month<-integer()
  jan_forecast<-double(200)
  feb_forecast<-double(200)
  mar_forecast<-double(200)
  apr_forecast<-double(200)
  may_forecast<-double(200)
  june_forecast<-double(200)
  july_forecast<-double(200)
  aug_forecast<-double(200)
  sept_forecast<-double(200)
  oct_forecast<-double(200)
  nov_forecast<-double(200)
  dec_forecast<-double(200)
  months_theta<-matrix(0,nrow=2,ncol=12)
  alpha<-0.01
  month<-1
  
  while(month<=12)
  {
    theta<-matrix(0,nrow = 2,ncol=1)
    theta<-as.matrix(theta)
    prev_theta<-matrix(0.0,nrow = 2,ncol=1)
    prev_theta<-as.matrix(prev_theta)
    y<-matrix(0,nrow=m,ncol=1)
    
    for(i in seq_len(m))
      y[i,1]<-dataset[i,month+1]
    
    y<-as.matrix(y)
    ones_mat<-matrix(1,nrow=m,ncol=1)
    x<-matrix(0,nrow=m,ncol=1)
    for(i in seq_len(m))
      x[i,1]<-dataset[i,1]
    
    x<-cbind(ones_mat,x)
    x<-as.matrix(x)
    
    is_nan<-FALSE
    
    while(!is_nan)
    {
      is_nan<-FALSE
      prev_theta<-theta
      h<-x%*%theta
      nr<-nrow(x)
      nc<-ncol(x)
      x_trans<-matrix(0,nrow=nc,ncol=nr)
      
      for(j in seq_len(nr))
      {
        for(k in seq_len(nc))
        {
          x_trans[k,j]<-x[j,k]
        }
      }
      
      theta=theta-((alpha/m)*x_trans%*%(h-y))
      
      for(i in seq_len(nrow(theta)))
      {
        if((is.nan(theta[i,1])||is.infinite(theta[i,1]))&&is_nan==FALSE)
          is_nan<-TRUE
      }
      
    }
    
    months_theta[1,month]<-prev_theta[1,1]
    months_theta[2,month]<-prev_theta[2,1]
    month<-month+1
  }
  next_year<-dataset[1,1]+m
  jan_forecast<-months_theta[1,1]+months_theta[2,1]*next_year
  feb_forecast<-months_theta[1,2]+months_theta[2,2]*next_year
  mar_forecast<-months_theta[1,3]+months_theta[2,3]*next_year
  apr_forecast<-months_theta[1,4]+months_theta[2,4]*next_year
  may_forecast<-months_theta[1,5]+months_theta[2,5]*next_year
  june_forecast<-months_theta[1,6]+months_theta[2,6]*next_year
  july_forecast<-months_theta[1,7]+months_theta[2,7]*next_year
  aug_forecast<-months_theta[1,8]+months_theta[2,8]*next_year
  sept_forecast<-months_theta[1,9]+months_theta[2,9]*next_year
  oct_forecast<-months_theta[1,10]+months_theta[2,10]*next_year
  nov_forecast<-months_theta[1,11]+months_theta[2,11]*next_year
  dec_forecast<-months_theta[1,12]+months_theta[2,12]*next_year
  
  display_forecast_result(next_year,jan_forecast,feb_forecast,mar_forecast,apr_forecast,may_forecast,june_forecast,july_forecast,aug_forecast,sept_forecast,oct_forecast,nov_forecast,dec_forecast)
  
  return()
}