forecast_by_lagranges<-function(dataset)
{
  next_year<-nrow(dataset)+dataset[1,1]
  
  numerator<-double()
  denominator<-double()
  jan_forecast<-numeric()
  feb_forecast<-numeric()
  mar_forecast<-numeric()
  apr_forecast<-numeric()
  may_forecast<-numeric()
  june_forecast<-numeric()
  july_forecast<-numeric()
  aug_forecast<-numeric()
  sept_forecast<-numeric()
  oct_forecast<-numeric()
  nov_forecast<-numeric()
  dec_forecast<-numeric()
  
  numerator<-1
  denominator<-1
  jan_forecast<-0
  feb_forecast<-0
  mar_forecast<-0
  apr_forecast<-0
  may_forecast<-0
  june_forecast<-0
  july_forecast<-0
  aug_forecast<-0
  sept_forecast<-0
  oct_forecast<-0
  nov_forecast<-0
  dec_forecast<-0
  nr<-nrow(dataset)
  
  for(i in seq_len(nr))
  {
    for(j in seq_len(nr))
    {
      if(i!=j)
      {
        numerator<-numerator*(next_year-dataset[j,1])
        denominator<-denominator*(dataset[i,1]-dataset[j,1])
      }
    }
    if(numerator!=0&&denominator!=0)
    {
      jan_forecast<-jan_forecast + (numerator/denominator)*dataset[i,2]
      feb_forecast<-feb_forecast + (numerator/denominator)*dataset[i,3]
      mar_forecast<-mar_forecast + (numerator/denominator)*dataset[i,4]
      apr_forecast<-apr_forecast + (numerator/denominator)*dataset[i,5]
      may_forecast<-may_forecast + (numerator/denominator)*dataset[i,6]
      june_forecast<-june_forecast + (numerator/denominator)*dataset[i,7]
      july_forecast<-july_forecast + (numerator/denominator)*dataset[i,8]
      aug_forecast<-aug_forecast + (numerator/denominator)*dataset[i,9]
      sept_forecast<-sept_forecast + (numerator/denominator)*dataset[i,10]
      oct_forecast<-oct_forecast + (numerator/denominator)*dataset[i,11]
      nov_forecast<-nov_forecast + (numerator/denominator)*dataset[i,12]
      dec_forecast<-dec_forecast + (numerator/denominator)*dataset[i,13]
    }
    numerator<-1
    denominator<-1
  }
  
  display_forecast_result(next_year,jan_forecast,feb_forecast,mar_forecast,apr_forecast,may_forecast,june_forecast,july_forecast,aug_forecast,sept_forecast,oct_forecast,nov_forecast,dec_forecast)
    
  return()
}