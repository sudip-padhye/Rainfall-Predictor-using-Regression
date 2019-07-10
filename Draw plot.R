draw_plot<-function(dataset,data,caption)
{
  year<-matrix(0)
  year<-dataset[,1,drop=TRUE]
  matplot(year, data, type="l", main=caption)
}