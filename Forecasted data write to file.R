forecast_data_write_to_file<-function(dataset_copy,YEAR,JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT,NOV,DEC)
{
  ANN<-(JAN+FEB+MAR+APR+MAY+JUN+JUL+AUG+SEP+OCT+NOV+DEC)
  combn1<-(JAN+FEB)
  combn2<-(MAR+APR+MAY)
  combn3<-(JUN+JUL+AUG+SEP)
  combn4<-(OCT+NOV+DEC)
  new_row<-c(YEAR,JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT,NOV,DEC,ANN,combn1,combn2,combn3,combn4)
  
  dataset_copy<-rbind.data.frame(dataset_copy,new_row)
  returnValue(dataset_copy)
}