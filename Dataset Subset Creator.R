dataset_subset_creator<-function(i,dataset)
{
  dataset_new<-dataset[i+1][1]
  
  for(j in 1:i)
  {
    dataset_new<-cbind(dataset_new,dataset[j][1])
  }
  
  returnValue(dataset_new)
}