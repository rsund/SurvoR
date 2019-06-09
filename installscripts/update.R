survoupdate <- function(kysy=FALSE,curl=contrib.url(repos="http://www.survo.fi",type="binary"))
    {
    if (!length(curl)) curl <- contrib.url(repos="http://www.survo.fi")
    if (!length(curl)) curl <- ""    
    if (grepl("/src/",curl)) tyyppi <- "source" else tyyppi <- .Platform$pkgType
    newpkgs <- new.packages(contriburl=curl,ask=kysy,type=tyyppi)
    if (length(newpkgs>0)) try(install.packages(newpkgs,contriburl=curl,type=tyyppi))
    updpkgs <- as.vector(available.packages(contriburl=curl,type=tyyppi)[,"Package"])
    intpkgs <- intersect(.packages(),updpkgs)
    if (length(intpkgs)>0) warning(paste("Package(s)",intpkgs,"loaded - skipping update."))
    updpkgs <- setdiff(updpkgs,.packages())
    if (length(updpkgs>0)) try(update.packages(contriburl=curl,oldPkgs=updpkgs,ask=kysy,type=tyyppi))
    if (require("muste")) survo()
    }

survoupdate()
