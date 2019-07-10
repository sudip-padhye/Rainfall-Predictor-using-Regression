display_descriptive_analysis<-function(nc,col_names,min_data,max_data,mean_data,median_data,quartile_data)
{
  for(i in seq_len(nc-1))        # min,max,mean,median,quartiles
  {
    cat("\n\n")
    cat(col_names[i+1],"\n")
    cat("-----------------------------------------------------------------------------------------\n")
    
    cat("Min : ",min_data[i],"\n")
    cat("Max : ",max_data[i],"\n")
    cat("Mean : ",mean_data[i],"\n")
    cat("Median : ",median_data[i],"\n")
    cat("Quartile : ")
    cat("First Quartile (Q1)=",quartile_data[1,i],"\n")
    cat("Second Quartile (Q2)=",quartile_data[2,i],"\n")
    cat("Third Quartile (Q3)=",quartile_data[3,i],"\n")
  }
  
  cat("-----------------------------------------------------------------------------------------\n")
  cat("\n\nDescriptive Analysis performed...\n")
  cat("Forecasting by Regression...\n")

  return()
}