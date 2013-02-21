
.virta.checkmuste <- function()
  {
  if (!exists("editor",where=.muste))
    {
      warning("Muste editor should be running while using virta!")
      muste()
    }
  }
  

virta.sql <- function(query="",destfile="",database=":memory:",encod="UTF-8")
{
if (!nchar(query)) return

.virta.checkmuste()
db <- dbConnect(SQLite(), dbname = database)
on.exit(dbDisconnect(db))
init_extensions(db)
init_virtualtables(db)

# need to extract svo-files from query-string and create corresponding virtual tables

quer<-enc2utf8(gsub("muste.","MUSTE_",query,ignore.case=TRUE))
ftab<-unique(grep("MUSTE_",strsplit(quer,"\\,\\s|\\,|\\;|\\s")[[1]],value=TRUE))
if (length(ftab)>0)
	{
	infiles<-muste_ExpandFileName(gsub("MUSTE_","",ftab))
	vtab<-paste("MUSTE__",basename(infiles),1:length(infiles),sep="")
	stab<-paste("MUSTE_",basename(infiles),1:length(infiles),sep="")
	
	tabl <- dbListTables(db)
	for (i in 1:length(vtab))
	  {
	  quer<-gsub(ftab[i],stab[i],quer,fixed=TRUE)	  
	  if (length(tabl)>0) if (max(tabl==vtab[i])) dbGetQuery(db, paste("drop table", vtab[i]))
	  dbGetQuery(db, paste('create virtual table',vtab[i],'using svo ("',infiles[i],'")'))
	  if (length(tabl)>0) if (max(tabl==stab[i])) dbGetQuery(db, paste("drop table", stab[i]))
	  dbGetQuery(db, paste('create table',stab[i],'as select * from',vtab[i]))	  	  
	  }
	}  
# Just confirm that encoding will be UTF-8 
Encoding(quer)<-encod
quer <- strsplit(quer,";")[[1]]
for (i in 1:length(quer))
    {
    chkquer <- gsub("\\s","",quer[i]) 
    if (nchar(chkquer)>0 && substr(chkquer,1,1)!='#') 
        resu<-dbGetQuery(db,quer[i])
    }
# This should be changed so that data from last query will be fetched in parts to save memory
if (!is.null(resu))
    {
    renc<-which(sapply(resu, is.character))
    for (i in 1:length(renc)) Encoding(resu[[renc[i]]])<-encod
    Encoding(names(resu))<-encod
    }

# Fetched data frame should be saved to survo-file if destfile is given
# FILE SAVE R>resu TO NEW KYS  / NEW only on first call if fetched in parts

#dbDisconnect(db)
tabl <- dbListTables(db)
if (length(ftab)>0) for (i in 1:length(vtab))
  {
  if (length(tabl)>0) if (max(tabl==vtab[i])) dbGetQuery(db, paste("drop table", vtab[i]))
  if (length(tabl)>0) if (max(tabl==stab[i])) dbGetQuery(db, paste("drop table", stab[i]))
  }

if (!is.null(resu) && nchar(destfile)) 
	{
	write.svo(resu,muste_ExpandFileName(destfile)) 
	return 
	}
resu
}


