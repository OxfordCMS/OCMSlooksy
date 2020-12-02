if (!require('OCMSlooksy', character.only = TRUE))
{
  install.packages("/gfs/devel/syen/release/OCMSlooksy", 
                   repo = NULL, type = "source", dep=TRUE)
  if(!require('OCMSlooksy', character.only = TRUE)) stop("Package not found")
}

library(OCMSlooksy)
OCMSlooksy::run_app()
