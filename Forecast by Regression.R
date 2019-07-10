regression_forecast<-function(dataset)
{
  monthly_dataset<-subset.matrix(dataset,select=JAN:DEC)                              # Monthly Rainfall plot
  monthly_data<-matrix(0)
  draw_plot(dataset,monthly_dataset[, -1,drop=TRUE],"Monthly Rainfall Dataset")
  
  quaterly_dataset<-subset.matrix(dataset,select=Jan.Feb:Oct.Dec)                     # Quaterly Rainfall plot
  quaterly_data<-matrix(0)
  draw_plot(dataset,quaterly_dataset[, -1,drop=TRUE],"Quaterly Rainfall Dataset")
  
  annual_dataset<-subset.matrix(dataset,select=ANN)                                   # Annual Rainfall plot
  annual_data<-matrix(0)
  draw_plot(dataset,annual_dataset[, 1,drop=TRUE],"Annual Rainfall Dataset")
  
  cat("-----------------------------------------------------------------------------------------\n")
  cat("Method 1 : Forecasting by Lagrange's Numerical Method\n")
  forecast_by_lagranges(dataset)
  cat("Forecasting done...\n")
  cat("-----------------------------------------------------------------------------------------\n")
  cat("Method 2 : Forecasting by Univariate Linear Regression\n")
  forecast_by_linear_regression(dataset)
  cat("Forecasting done...\n")
  cat("-----------------------------------------------------------------------------------------\n")
  cat("Method 3 : Forecasting by WEKA's Time Series Forecast Package\n")
  
  summary<-FALSE
  eval_summary<-FALSE
  
  done<-FALSE
  while(!done)
  {
    summary<-readline("Whether to display Summary(TRUE|FALSE) : ")
    summary<-as.logical(toupper(summary))
    if(summary==TRUE||summary==FALSE)
    {
      done<-TRUE
    }
    else
    {
      cat("Invalid input\n")
    }
  }
  
  done<-FALSE
  while(!done)
  {
    eval_summary<-readline("Whether to display Evaluation Summary(TRUE|FALSE) : ")
    eval_summary<-as.logical(toupper(eval_summary))
    if(eval_summary==TRUE||eval_summary==FALSE)
    {
      done<-TRUE
    }
    else
    {
      cat("Invalid input\n")
    }
  }
  
  forecast_by_weka_timeseries_func(dataset,summary,eval_summary)   
  # for summary and evaluation summary make function call as forecast_by_weka_timeseries_func(dataset,summary=TRUE,eval_summary=TRUE)
  cat("Forecasting done...\n")
  cat("-----------------------------------------------------------------------------------------\n")
  
  return
}