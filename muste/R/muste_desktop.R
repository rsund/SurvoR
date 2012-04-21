
# Some R functions for Muste Desktop operations by KV 20.6.2011 (24.9.2011) (11.11.11) (28.11.2011)

.muste.desktop.fileinfo.INDEX <- function(filespec)
{
  .muste$tmp.filespec    <- filespec
  .muste$tmp.length      <- nchar(.muste$tmp.filespec)
  .muste$tmp.filespec    <- Sys.glob(.muste$tmp.filespec)
  .muste$tmp.dirname     <- dirname(.muste$tmp.filespec)[1]

# Check whether we have all files or selected ones (e.g. "*.C"):
  .muste$tmp.fileinfo    <- file.info(list.files(.muste$tmp.dirname,full.names=TRUE,recursive=FALSE,include.dirs=TRUE))
  .muste$tmp.nfiles      <- dim(.muste$tmp.fileinfo[.muste$tmp.fileinfo$isdir==FALSE,])[1]
  .muste$tmp.fileinfo    <- file.info(.muste$tmp.filespec)
  .muste$tmp.nthese      <- dim(.muste$tmp.fileinfo[.muste$tmp.fileinfo$isdir==FALSE,])[1]
  .muste$tmp.selected    <- as.integer(.muste$tmp.nfiles != .muste$tmp.nthese)

# Gather file info of 0=dirs and 1=files and form one data frame in that order:
  .muste$tmp.fileinfo1   <- file.info(.muste$tmp.filespec)
  .muste$tmp.fileinfo1   <- .muste$tmp.fileinfo1[.muste$tmp.fileinfo1$isdir==FALSE,]
  .muste$tmp.filename    <- row.names(.muste$tmp.fileinfo1)
  .muste$tmp.dirname     <- dirname(.muste$tmp.filename)[1]
  .muste$tmp.fileinfo0   <- file.info(list.dirs(.muste$tmp.dirname,full.names=TRUE,recursive=FALSE))
  .muste$tmp.fileinfo    <- rbind(.muste$tmp.fileinfo0, .muste$tmp.fileinfo1)

# Pick up the essential information from fileinfo:
  .muste$tmp.filecount   <- dim(.muste$tmp.fileinfo)[1]
  .muste$tmp.filisdir    <- as.integer(.muste$tmp.fileinfo[,"isdir"])
  .muste$tmp.filesize    <- as.integer(.muste$tmp.fileinfo[,"size"])
  .muste$tmp.filetime    <- as.integer(.muste$tmp.fileinfo[,"mtime"])
  .muste$tmp.filename    <- row.names(.muste$tmp.fileinfo)
  .muste$tmp.dirname     <- dirname(.muste$tmp.filename)
  .muste$tmp.basename    <- basename(.muste$tmp.filename)
}

.muste.desktop.fileinfo.INDEX.cleanup <- function()
{
remove(list=grep("^tmp.",ls(.muste),value=TRUE),envir=.muste)
}

.muste.desktop.fileinfo.SEARCH1 <- function(filespec) # non-recursive
{
  .muste$tmp.filespec    <- Sys.glob(filespec)
  .muste$tmp.fileinfo    <- file.info(.muste$tmp.filespec)

# Pick up the essential information from fileinfo:
  .muste$tmp.filecount   <- dim(.muste$tmp.fileinfo)[1]
  .muste$tmp.filisdir    <- as.integer(.muste$tmp.fileinfo[,"isdir"])
  .muste$tmp.filesize    <- as.integer(.muste$tmp.fileinfo[,"size"])
  .muste$tmp.filetime    <- as.integer(.muste$tmp.fileinfo[,"mtime"])
  .muste$tmp.filename    <- row.names(.muste$tmp.fileinfo)
  .muste$tmp.dirname     <- dirname(.muste$tmp.filename)
  .muste$tmp.basename    <- basename(.muste$tmp.filename)
}

.muste.desktop.fileinfo.SEARCH1.cleanup <- function()
{
remove(list=grep("^tmp.",ls(.muste),value=TRUE),envir=.muste)
}

.muste.desktop.fileinfo.SEARCH2 <- function(filespec) # recursive ("SUBDIRS")
{
  .muste$tmp.filespec    <- filespec
  .muste$tmp.dirname     <- dirname(.muste$tmp.filespec)
  .muste$tmp.basename    <- basename(.muste$tmp.filespec)

  .muste$tmp.dirslist    <- list.dirs(.muste$tmp.dirname, full.names=TRUE)
  .muste$tmp.filepath    <- file.path(.muste$tmp.dirslist, .muste$tmp.basename)
  .muste$tmp.filespec    <- Sys.glob(.muste$tmp.filepath)
  .muste$tmp.fileinfo    <- file.info(.muste$tmp.filespec)

# Pick up the essential information from fileinfo:
  .muste$tmp.filecount   <- dim(.muste$tmp.fileinfo)[1]
  .muste$tmp.filisdir    <- as.integer(.muste$tmp.fileinfo[,"isdir"])
  .muste$tmp.filesize    <- as.integer(.muste$tmp.fileinfo[,"size"])
  .muste$tmp.filetime    <- as.integer(.muste$tmp.fileinfo[,"mtime"])
  .muste$tmp.filename    <- row.names(.muste$tmp.fileinfo)
  .muste$tmp.dirname     <- dirname(.muste$tmp.filename)
  .muste$tmp.basename    <- basename(.muste$tmp.filename)
}

.muste.desktop.fileinfo.SEARCH2.cleanup <- function()
{
remove(list=grep("^tmp.",ls(.muste),value=TRUE),envir=.muste)
}

.muste.desktop.fileinfo.DD <- function(filespec)
{
  .muste$tmp.filespec    <- filespec
  .muste$tmp.length      <- nchar(.muste$tmp.filespec)
  .muste$tmp.filespec    <- Sys.glob(.muste$tmp.filespec)

  if (identical(.muste$tmp.filespec, character(0))) { # (path/file not found)
    .muste$tmp.filespec    <- ""
    .muste$tmp.filecount   <- as.integer(0)
    .muste$tmp.selected    <- as.integer(0)
    .muste$tmp.dirname     <- ""
  } else {
    .muste$tmp.dirname     <- dirname(.muste$tmp.filespec)[1]

# Check whether we have all files or selected ones (e.g. "*.C"):
  .muste$tmp.fileinfo    <- file.info(list.files(.muste$tmp.dirname,full.names=TRUE,recursive=FALSE,include.dirs=TRUE))
  .muste$tmp.nfiles      <- dim(.muste$tmp.fileinfo[.muste$tmp.fileinfo$isdir==FALSE,])[1]
  .muste$tmp.fileinfo    <- file.info(.muste$tmp.filespec)
  .muste$tmp.nthese      <- dim(.muste$tmp.fileinfo[.muste$tmp.fileinfo$isdir==FALSE,])[1]
  .muste$tmp.selected    <- as.integer(.muste$tmp.nfiles != .muste$tmp.nthese)

# Gather file info of 0=dirs and 1=files and form one data frame in that order:
  .muste$tmp.fileinfo1   <- file.info(.muste$tmp.filespec)
  .muste$tmp.fileinfo1   <- .muste$tmp.fileinfo1[.muste$tmp.fileinfo1$isdir==FALSE,]
  .muste$tmp.filename    <- row.names(.muste$tmp.fileinfo1)
  .muste$tmp.dirname     <- dirname(.muste$tmp.filename)[1]
  .muste$tmp.fileinfo0   <- file.info(list.dirs(.muste$tmp.dirname,full.names=TRUE,recursive=FALSE))
  .muste$tmp.fileinfo    <- rbind(.muste$tmp.fileinfo0, .muste$tmp.fileinfo1)

# Add the current dir in the beginning of the list (only in DD):
  .muste$tmp.thisdir     <- paste(.muste$tmp.dirname,"/.",sep="")
  .muste$tmp.fileinfo0   <- file.info(.muste$tmp.thisdir)
  .muste$tmp.fileinfo    <- rbind(.muste$tmp.fileinfo0, .muste$tmp.fileinfo)

# Pick up the essential information from fileinfo:
  .muste$tmp.filecount   <- dim(.muste$tmp.fileinfo)[1]
  .muste$tmp.filisdir    <- as.integer(.muste$tmp.fileinfo[,"isdir"])
  .muste$tmp.filesize    <- as.integer(.muste$tmp.fileinfo[,"size"])
  .muste$tmp.filetime    <- as.integer(.muste$tmp.fileinfo[,"mtime"])
  .muste$tmp.filename    <- row.names(.muste$tmp.fileinfo)
  .muste$tmp.dirname     <- dirname(.muste$tmp.filename)
  .muste$tmp.basename    <- basename(.muste$tmp.filename)
  }
}

.muste.desktop.fileinfo.DD.cleanup <- function()
{
remove(list=grep("^tmp.",ls(.muste),value=TRUE),envir=.muste)
}

.muste.desktop.fileinfo.WHERE <- function(filespec)
{
  .muste$tmp.filespec    <- filespec
  .muste$tmp.basename    <- basename(.muste$tmp.filespec)
  .muste$tmp.dirname     <- dirname(Sys.glob(.muste$tmp.filespec)[1])
  .muste$tmp.dirslist    <- list.dirs(.muste$tmp.dirname, full.names=TRUE)
  .muste$tmp.filepath    <- file.path(.muste$tmp.dirslist, .muste$tmp.basename)
  .muste$tmp.filespec    <- Sys.glob(.muste$tmp.filepath)
  .muste$tmp.fileinfo    <- file.info(.muste$tmp.filespec)

# Pick up the essential information from fileinfo:
  .muste$tmp.filecount   <- dim(.muste$tmp.fileinfo)[1]
  .muste$tmp.filisdir    <- as.integer(.muste$tmp.fileinfo[,"isdir"])
  .muste$tmp.filesize    <- as.integer(.muste$tmp.fileinfo[,"size"])
  .muste$tmp.filetime    <- as.integer(.muste$tmp.fileinfo[,"mtime"])
  .muste$tmp.filename    <- row.names(.muste$tmp.fileinfo)
  .muste$tmp.dirname     <- dirname(.muste$tmp.filename)
  .muste$tmp.basename    <- basename(.muste$tmp.filename)
}

.muste.desktop.fileinfo.WHERE.cleanup <- function()
{
remove(list=grep("^tmp.",ls(.muste),value=TRUE),envir=.muste)
}

