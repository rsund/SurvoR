.muste.del <- function(tiedosto)
  {
  unlink(tiedosto)
  if (file.exists(tiedosto)) { file.remove(tiedosto) }
  }

.muste.dir <- function(komento,odotus=FALSE)
  {
  if (.muste$sysname=="Windows") 
  	{
#  	dircmd <- paste("DIR",gsub("/","\\",komento,fixed=TRUE),"/-c")
    dircmd <- paste("DIR",komento)
  	.muste.system(dircmd,odotus=odotus)
  	}
  else
  	{ 
  	dircmd <- paste("ls -al",komento)
  	.muste.system(dircmd,odotus=odotus)
  	}
  }

.muste.getwd <- function()
    {
    .muste$workdir <- getwd()
    }

.muste.checkfile <- function(tiedosto)
    {
    .muste$filestatus <- as.integer(file.exists(tiedosto))
#print(paste(tiedosto,.muste$filestatus))    
    }

.muste.getfile <- function(file="") # ,keep=0,dest=NULL
	{		
	if(length(grep("^(http|ftp|https)://", file)))
		{
		tmp <- tempfile()
		if (length(grep("https://",file)))
		    {
		    .muste.command(c("Require","RCurl"),force=TRUE)	    	
            if (requireNamespace("RCurl",quietly=TRUE))        
                {
                if(RCurl::url.exists(file))
                    {
                    content <- RCurl::getBinaryURL(file)
                    writeBin(content, con = tmp)
                    }
                }
		    }
		else
		    {
		    download.file(file, tmp, quiet = TRUE, mode = "wb")
		    }
#		if (keep)
#			{
#			if (!is.null(dest)) outfile=dest
#			else outfile=substring(file,regexpr("\\/[^\\/]*$", file)+1)
#			file.copy(tmp,outfile,overwrite=TRUE)
#			file<-outfile
#			}
#		else 
		file <- tmp
		}
	
	.muste$retrievedfile <- file	
	}

.muste.txtconv <- function(infile,outfile,instring=c(""),outstring=c(""),lines=1,MAXSIZE=10000000,perl=FALSE)
	{
    if(length(grep("^(http|ftp|https)://", infile)))
    	{
        tmp <- tempfile()
        download.file(infile, tmp, quiet = TRUE, mode = "wb")
        infile <- tmp
        on.exit(unlink(infile))
        }	
	
	if (!file.exists(infile)) return
	fs <- file.info(infile)$size
	if (fs>MAXSIZE) 
		{ 
		stop("File too large, increase MAXSIZE!\n") 
		return 
		}
	if (lines==1) s<-readLines(infile)
	else s<-readChar(infile,fs)
	for (i in 1:length(instring)) s <- gsub(instring[i],outstring[i],s,perl=perl)		
	sink(outfile)
	cat(s,sep="\n")
	sink()
	}

.muste.svoattributes <- function(rval)
	{
	att <- attributes(rval)
    rval <- as.data.frame(rval, stringsAsFactors=FALSE)
##    class(rval) <- "data.frame"
    newatt <- attributes(rval)
    newatt <- c(newatt, att[!(names(att) %in% names(newatt))])
    return(newatt)
	}

.muste.system <- function(komento,odotus=FALSE)
  {
  if (.muste$sysname=="Windows")
  	{
#  	komento <- gsub("\t","\\",komento,fixed=TRUE) 
  	shell(komento,wait=odotus)
  	}
  else
  	{
  	komento <- gsub("\\","/",komento,fixed=TRUE) 
  	system(komento,wait=odotus)
  	}
  }

.muste.systemopen <- function(komento=".",odotus=FALSE,paran=1)
  {
  if (substr(komento,1,4)=="www.") cat(paste("http://",komento,sep=""))
    
  if (.muste$sysname=="Windows")
  	{
  	if (paran==1) 
  		{
  		komento2 <- gsub("\"","",komento,fixed=TRUE)
  		if (substr(komento2,1,7)=="http://" ||
  		    substr(komento2,1,8)=="https://" ||
  		    substr(komento2,1,6)=="ftp://")
  		    {
  		    komento <- paste("start",komento2,sep=" ")
  		    }
  		else
  			{
 			ismis<-is.na(file.info(komento2)$isdir) 
 			if (ismis) if (substr(komento2,nchar(komento2)-1,nchar(komento2))==":\\") ismis<-as.integer(2)
			if (ismis==0 || ismis==2) if (ismis==2 || file.info(komento2)$isdir)
				{
				komento <- paste("explorer",komento2,sep=" ")
				}
  			}
  		}
#  	cat(komento)		
  	shell(komento,wait=odotus)
  	}
  else
  	{
  	komento <- gsub("\\","/",komento,fixed=TRUE) 
  	
  	if (.muste$sysname=="Darwin") komento <- paste("open",komento,sep=" ")
  	else komento <- paste("xdg-open",komento,sep=" ")

  	system(komento,wait=odotus)
  	}
  }