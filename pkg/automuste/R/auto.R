.onLoad <- function(libname,pkgname) { if (interactive()) automuste() }

automuste <- function(kysy=FALSE,curl="http://www.survo.fi/muste")
    {
    workdir <- getwd()
    newpkgs <- new.packages(contriburl=curl,ask=kysy)
    if (length(newpkgs>0))
        try(install.packages(newpkgs,contriburl=curl))
    updpkgs <- as.vector(available.packages(contriburl=curl)[,"Package"])
    intpkgs <- intersect(.packages(),updpkgs)
    if (length(intpkgs)>0)
        warning(paste("Package(s)",intpkgs,"loaded - skipping update."))
    updpkgs <- setdiff(updpkgs,.packages())
    if (length(updpkgs>0))
        try(update.packages(contriburl=curl,oldPkgs=updpkgs,ask=kysy))
    if(length(intersect(ls(all.names=TRUE),".First"))==0)
        {
        tmp <- tempfile()
		cat(".First <- function() require(automuste)",file=tmp,sep="\n",append=TRUE)
        if (!file.rename(tmp,paste(workdir,"/.Rprofile",sep="")))
            {
            file.remove(tmp)
            warning("Could not create .Rprofile for autostart!")
            }
        }
    if (require("muste")) if (interactive()) muste()
    }