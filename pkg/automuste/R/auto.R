.onLoad <- function(libname,pkgname) { if (interactive()) automuste() }

Timeout <- function(..., envir=parent.frame(), timeout=3.0, cpu=timeout, elapsed=timeout, onTimeout="error") {

  # Default result value
  res <- invisible();

  setTimeLimit(cpu=cpu, elapsed=elapsed, transient=TRUE);
  on.exit({
    setTimeLimit(cpu=Inf, elapsed=Inf, transient=FALSE);
  });

  tryCatch({
    res <- eval(..., envir=envir);
  }, error = function(ex) {
    msg <- ex$message;
    # Was it a timeout?
    pattern <- gettext("reached elapsed time limit");
    if (regexpr(pattern, msg) != -1L) {
      if (onTimeout == "error") {
        print(ex);
      } 
    } 
  })

  res;
}


testUrl <- function(url="http://www.survo.fi/muste/PACKAGES",
               checktxt="Package: muste")
    {

    http <- as.logical(capabilities(what = "http/ftp"))
    if (http == FALSE) 
        {
        return(999)  # R build has no http capabilities
        }

    connection <- url(url)
    
    elapsedtime <- system.time(
    test <- try(suppressWarnings(
        res <- Timeout(readLines(connection,n=1), timeout=3.0, onTimeout="silent")
    ), silent = TRUE)
    )[3]    
    
    close(connection)

    if (inherits(test, "try-error")) 
        {
        return(999)  # if the readLines from url failed
        } 
    else 
        {
        if (is.character(test)) 
            {
            if (test == checktxt) 
                {
                return(elapsedtime)  # you are able to access internet
                }
            else 
                {
                return(999)   # If access is blocked and/or returns something "wrong"
                }
            }
        else 
            {
            return(999)  # if the requested url does not contain text
            }
        }
    }

checkLoadedPackages <- function(curl)
    {
    aml <- intersect(.packages(),"automuste")
    if (length(aml)>0)
        {
        warning(paste("\nDetaching package: ",aml)) 
        tryCatch(
            {
            detach("package:automuste",unload=TRUE)
            },
            error = function(error)
            { 
            warning("\nError in detaching!")
            }, 
            interrupt = function(inter) { 
            warning("\nError in detaching!")
            }
        )
        }      
    updpkgs <- as.vector(available.packages(contriburl=curl)[,"Package"])           
    intpkgs <- intersect(.packages(),updpkgs)       
    if (length(intpkgs)>0)
        {
        warning(paste("\nPackage",intpkgs,"loaded - update skipped.")) 
        }            
    updpkgs <- setdiff(updpkgs,.packages())
    updpkgs
    }

automuste <- function(kysy=FALSE,curl=contrib.url(repos="http://www.survo.fi"))
    {
    workdir <- getwd()
    packageStartupMessage("Checking internet connection...", appendLF = FALSE) 
    elapsedtime <- testUrl()
    packageStartupMessage("done!")
    if (elapsedtime<3)
        {
        if (elapsedtime>0.5)
            {
            packageStartupMessage(paste("Slow connection:",round(elapsedtime,2),"sec latency!"))  
            }
        packageStartupMessage("Checking new packages...", appendLF = FALSE) 
        newpkgs <- invisible()
        elapsedtime <- system.time(
        Timeout(
        { newpkgs <- new.packages(contriburl=curl,ask=kysy) }
        , timeout=3.0, onTimeout="silent")
        )[3]
        packageStartupMessage(paste("done! (",round(elapsedtime,2),"sec)"))         
        if (length(newpkgs>0))
            {
            packageStartupMessage("Installing new packages...", appendLF = FALSE)
            elapsedtime <- system.time(try(install.packages(newpkgs,contriburl=curl)))[3]
            packageStartupMessage(paste("done! (",round(elapsedtime,2),"sec)"))  
            }
        packageStartupMessage("Checking available packages...", appendLF = FALSE)
        elapsedtime <- system.time(updpkgs <- checkLoadedPackages(curl))[3]
        packageStartupMessage(paste("done! (",round(elapsedtime,2),"sec)"))         
        if (length(updpkgs>0))
            {
            packageStartupMessage("Checking updates for packages...", appendLF = FALSE)
            elapsedtime <- system.time(try(update.packages(contriburl=curl,oldPkgs=updpkgs,ask=kysy)))[3]
            packageStartupMessage(paste("done! (",round(elapsedtime,2),"sec)"))         
            }              
        }
    else
        {
        warning("Unable to access internet - update check skipped.")
        }
    if(length(intersect(ls(all.names=TRUE),".First"))==0)
        {
        tmp <- tempfile()
		cat(".First <- function() require(automuste)",file=tmp,sep="\n",append=TRUE)
		out <- paste(workdir,"/.Rprofile",sep="")
        if (file.access(out)==0)
            {
            file.remove(tmp)
#            warning(".Rprofile already exists, autostart not created!")
            }
        else
            {
            if (!file.rename(tmp,out))
                {
                warning("Could not create .Rprofile for autostart!")
                }
            }
        }
    else
        {
        warning(".First() existing, autostart not created!")
        }        
            
    if (require("muste")) if (interactive()) survo()
    }
    