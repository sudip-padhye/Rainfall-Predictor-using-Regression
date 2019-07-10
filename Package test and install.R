pkgTest_and_install <- function(x)    
  #Tests for package name and also is it previously installed. If aleady installed then it skips its installation
{
  if (!require(x,character.only = TRUE))    
    #require ensures that packages are not only installed but are able to be used (i.e. dependencies are met, etc.)
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) 
      stop("Package not found")
  }
}