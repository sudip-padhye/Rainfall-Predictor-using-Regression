access_data<-function(dataset)
{
  nc<-ncol(dataset)
  cat("Different columns in the given dataset\n")
  col_names<-colnames(dataset)       ##extract column names
  for(i in seq_len(nc)){
    cat(col_names[i],"\n")
  }
  
  nr<-nrow(dataset)
  
  min_data<-numeric()
  max_data<-numeric()
  mean_data<-numeric()
  median_data<-numeric()
  quartile_data<-matrix(0,nrow=3,ncol=nc-1)
  
  i<-2
  sum<-0
  max<-0
  min<-0
  while(i<=nc)
  {
    for(j in seq_len(nr))
    {
      if(j==1)
      {
        max<-dataset[j,i]
        min<-dataset[j,i]
      }
      else{
        if(max<dataset[j,i])
          max<-dataset[j,i]
        if(min>dataset[j,i])
          min<-dataset[j,i]
      }
      sum<-sum+dataset[j,i]
    }
    mean_data[i-1]<-sum/nr
    min_data[i-1]<-min
    max_data[i-1]<-max
    i<-i+1
    sum<-0
  }
  
  col_data<-numeric()
  
  i<-2
  while(i<=nc)
  {
    col_data<-c()
    for(j in seq_len(nr))
    {
      col_data<-c(col_data,dataset[j,i]) 
    }
    col_data<-sort(col_data,decreasing = FALSE)
    if(nr%%2!=0)
    {
      median_data[i-1]<-col_data[ceiling(nr/2)]
      quartile_data[1,i-1]<-col_data[ceiling(nr/4)]
      quartile_data[2,i-1]<-median_data[i-1]
      quartile_data[3,i-1]<-col_data[ceiling(3*nr/4)]
    }
    else
    {
      median_data[i-1]<-(col_data[nr/2]+col_data[(nr/2)+1])/2
      quartile_data[1,i-1]<-col_data[ceiling(nr/4)]
      quartile_data[2,i-1]<-median_data[i-1]
      quartile_data[3,i-1]<-col_data[ceiling(3*nr/4)]
    }
    i<-i+1
  }
  
  cat("Descriptive Analysis\n")
  
  display_descriptive_analysis(nc,col_names,min_data,max_data,mean_data,median_data,quartile_data)
  
  regression_forecast(dataset)
  return
}