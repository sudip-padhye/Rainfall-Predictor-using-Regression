import_data<-function()
{
  source("Load files.R")
  load_files()
  cat("Loading dataset...\n")
  dataset<-read.csv("All India Rainfall Record.csv")
  cat("Dataset loaded...\n")
  access_data(dataset)
  cat("Finishing predictions and closing dataset...\n")
}