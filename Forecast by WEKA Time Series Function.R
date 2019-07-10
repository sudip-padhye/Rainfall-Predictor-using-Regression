forecast_by_weka_timeseries_func<-function(dataset,summary=FALSE,eval_summary=FALSE)
{
  num_time_units<-readline("Enter number of time units to forecast : ")
  done<-FALSE
  
  while(!done)
  {
    to_store<-readline("Whether to store forecast result into file(TRUE|FALSE) : ")
    to_store<-as.logical(toupper(to_store))
    if(to_store==TRUE||to_store==FALSE)
    {
      done<-TRUE
    }
    else
    {
      cat("Invalid input\n")
    }
  }
  
  JAN<-double(0)
  FEB<-double(0)
  MAR<-double(0)
  APR<-double(0)
  MAY<-double(0)
  JUN<-double(0)
  JUL<-double(0)
  AUG<-double(0)
  SEP<-double(0)
  OCT<-double(0)
  NOV<-double(0)
  DEC<-double(0)
  
  file.create("All India Rainfall Record_copy.csv")
  file.copy(from = "All India Rainfall Record.csv",to = "All India Rainfall Record_copy.csv",overwrite = TRUE)
  dataset_copy<-read.csv("All India Rainfall Record_copy.csv")
  
  for(k in seq_len(num_time_units))
  {
    YEAR<-nrow(dataset_copy)+dataset_copy[1,1]
    JAN<-0
    FEB<-0
    MAR<-0
    APR<-0
    MAY<-0
    JUN<-0
    JUL<-0
    AUG<-0
    SEP<-0
    OCT<-0
    NOV<-0
    DEC<-0
    for(i in 1:12)
    {
      dataset_new<-dataset_subset_creator(i,dataset_copy)
      
      if(i==1)
      {
        dataset_reg<-LinearRegression(JAN ~ .,data=dataset_new)
        step(lm(JAN ~ ., data = dataset_new), trace = 0)
      }
      else if(i==2)
      {
        dataset_reg<-LinearRegression(FEB ~ .,data=dataset_new)
        step(lm(FEB ~ ., data = dataset_new), trace = 0)
      }
      else if(i==3)
      {
        dataset_reg<-LinearRegression(MAR ~ .,data=dataset_new)
        step(lm(MAR ~ ., data = dataset_new), trace = 0)
      }
      else if(i==4)
      {
        dataset_reg<-LinearRegression(APR ~ .,data=dataset_new)
        step(lm(APR ~ ., data = dataset_new), trace = 0)
      }
      else if(i==5)
      {
        dataset_reg<-LinearRegression(MAY ~ .,data=dataset_new)
        step(lm(MAY ~ ., data = dataset_new), trace = 0)
      }
      else if(i==6)
      {
        dataset_reg<-LinearRegression(JUN ~ .,data=dataset_new)
        step(lm(JUN ~ ., data = dataset_new), trace = 0)
      }
      else if(i==7)
      {
        dataset_reg<-LinearRegression(JUL ~ .,data=dataset_new)
        step(lm(JUL ~ ., data = dataset_new), trace = 0)
      }
      else if(i==8)
      {
        dataset_reg<-LinearRegression(AUG ~ .,data=dataset_new)
        step(lm(AUG ~ ., data = dataset_new), trace = 0)
      }
      else if(i==9)
      {
        dataset_reg<-LinearRegression(SEP ~ .,data=dataset_new)
        step(lm(SEP ~ ., data = dataset_new), trace = 0)
      }
      else if(i==10)
      {
        dataset_reg<-LinearRegression(OCT ~ .,data=dataset_new)
        step(lm(OCT ~ ., data = dataset_new), trace = 0)
      }
      else if(i==11)
      {
        dataset_reg<-LinearRegression(NOV ~ .,data=dataset_new)
        step(lm(NOV ~ ., data = dataset_new), trace = 0)
      }
      else
      {
        dataset_reg<-LinearRegression(DEC ~ .,data=dataset_new)
        step(lm(DEC ~ ., data = dataset_new), trace = 0)
      }
      
      capture.output(dataset_reg,file = "Hypothesis_formula.txt")
      text<-read.fwf("Hypothesis_formula.txt", c(0,900), stringsAsFactors=FALSE)
      text<-text[2][1]
      text<-text[,1,drop=TRUE]
      text<-as.character(text)
      text<-na.omit(text)
      text<-as.character(text)
      text<-str_replace_all(text, " ", "")
      text<-str_replace_all(text, "LinearRegressionModel", "")
      text<-paste(text,collapse="")
      text<-stri_replace_all_fixed(text, "+-", "-")
      text<-stri_replace_all_fixed(text, "-+", "-")
      eval(parse(text=text))
      
      cat("-----------------------------------------------------------------------------------------\n")
      cat("The Built model is given as\n")
      print(dataset_reg)
      
      if(summary==TRUE)
      {
        cat("-----------------------------------------------------------------------------------------\n")
        cat("Summary of the model is\n")
        print(summary(dataset_reg))
      }
      
      if(eval_summary==TRUE)
      {  
        cat("-----------------------------------------------------------------------------------------\n")
        cat("Summary of the model on Cross validation\n")
        eval_forecast <- evaluate_Weka_classifier(dataset_reg, numFolds = 10, complexity = FALSE, seed = 1, class = TRUE)
        print(eval_forecast)
        cat("-----------------------------------------------------------------------------------------\n")
      }
    }
    
    dataset_copy<-forecast_data_write_to_file(dataset_copy,YEAR,JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT,NOV,DEC)
    print(dataset_copy)
    if(to_store==TRUE)
    {
      file.remove("All India Rainfall Record_copy.csv")
      file.create("All India Rainfall Record_copy.csv")
      write.csv(dataset_copy,file = "All India Rainfall Record_copy.csv",row.names = FALSE)
    }
    display_forecast_result(YEAR,JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT,NOV,DEC)
  }
  
  if(to_store==FALSE)
  {
    file.remove("All India Rainfall Record_copy.csv")
  }
  
  return()
}