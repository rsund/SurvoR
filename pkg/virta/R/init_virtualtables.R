# Borrowed from RSqlite.extfuns-package by Seth Falcon

.allows_extensions <- function(db)
{
    v <- dbGetInfo(db)[["loadableExtensions"]]
    isTRUE(v) || (v == "on")
}

.lib_path <- function()
{
    ## this is a bit of a trick, but the NAMESPACE code
    ## puts .packageName in the package environment and this
    ## seems slightly better than hard-coding.
    ##
    ## This also relies on the DLL being loaded even though only SQLite
    ## actually needs to load the library.  It does not appear that
    ## loading it causes any harm and it makes finding the path easy
    ## (don't have to worry about arch issues).
    getLoadedDLLs()[[.packageName]][["path"]]
}

init_virtualtables <- function(db) # RS 7.2.2013 CHA init_extensions <- function(db)
{
    ans <- FALSE
    if (.allows_extensions(db)) {
        res <- dbGetQuery(db, sprintf("SELECT load_extension('%s')",
                                      .lib_path()))
        ans <- all(dim(res) == c(1, 1))
    } else {
        stop("loadable extensions are not enabled for this db connection")
    }
    ans
}
