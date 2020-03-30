if (!require('OCMSExplorer', character.only = TRUE))
{
  install.packages("/gfs/devel/syen/release/OCMSExplorer", 
                   repo = NULL, type = "source", dep=TRUE)
  if(!require('OCMSExplorer', character.only = TRUE)) stop("Package not found")
}

library(OCMSExplorer)
OCMSExplorer::run_app()
