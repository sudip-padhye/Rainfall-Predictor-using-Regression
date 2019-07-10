display_forecast_result<-function(next_year,jan_forecast,feb_forecast,mar_forecast,apr_forecast,may_forecast,june_forecast,july_forecast,aug_forecast,sept_forecast,oct_forecast,nov_forecast,dec_forecast)
{
  annual_forecast<-numeric()
  jan_to_feb_forecast<-numeric()
  mar_to_may_forecast<-numeric()
  june_to_sept_forecast<-numeric()
  oct_to_dec_forecast<-numeric()
  
  annual_forecast<-jan_forecast+feb_forecast+mar_forecast+apr_forecast+may_forecast+june_forecast
  annual_forecast<-annual_forecast+july_forecast+aug_forecast+sept_forecast+oct_forecast+nov_forecast+dec_forecast
  jan_to_feb_forecast<-jan_forecast+feb_forecast
  mar_to_may_forecast<-mar_forecast+apr_forecast+may_forecast
  june_to_sept_forecast<-june_forecast+july_forecast+aug_forecast+sept_forecast
  oct_to_dec_forecast<-oct_forecast+nov_forecast+dec_forecast
  
  cat("\nPrediction for year ",next_year,"\n")
  cat("-----------------------------------------------------------------------------------------\n")
  cat("Jan : ",jan_forecast," mm\n")
  cat("Feb : ",feb_forecast," mm\n")
  cat("Mar : ",mar_forecast," mm\n")
  cat("Apr : ",apr_forecast," mm\n")
  cat("May : ",may_forecast," mm\n")
  cat("June : ",june_forecast," mm\n")
  cat("July : ",july_forecast," mm\n")
  cat("Aug : ",aug_forecast," mm\n")
  cat("Sept : ",sept_forecast," mm\n")
  cat("Oct : ",oct_forecast," mm\n")
  cat("Nov : ",nov_forecast," mm\n")
  cat("Dec : ",dec_forecast," mm\n")
  cat("Annual : ",annual_forecast," mm\n")
  cat("Jan to Feb : ",jan_to_feb_forecast," mm\n")
  cat("Mar to May : ",mar_to_may_forecast," mm\n")
  cat("June to Sept : ",june_to_sept_forecast," mm\n")
  cat("Oct to Dec : ",oct_to_dec_forecast," mm\n")
  
  return()
}