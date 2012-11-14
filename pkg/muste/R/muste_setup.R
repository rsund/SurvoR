.muste.setup <- function(init=FALSE)
	{
	if (init)
		{
		if(file.access(.muste$homedir,mode=2)==-1) return() 
		viesti <-
		paste("It seems that you're launching Muste for the first time on this computer as the configuration file was not found!\nIs it OK to create file\n",
		      .muste$apufile,"?",sep=" ")
		response <- tclvalue(tkmessageBox(message=viesti,
						icon="question", type="yesno", default="no",title=""))
		if (response == "no") 
			{
#			.muste$apufile <- paste(.muste$mustepath,'/SURVO.APU',sep="")
			return(invisible(response))
			}
		dir.create(paste(.muste$homedir,'/.muste',sep=""),showWarnings = FALSE)
		file.create(.muste$apufile)
		cat("/ Muste auxiliary parameter updates (Configuration file for Muste)",
		file=.muste$apufile,sep="\n",append=TRUE)
		.muste.command(c("Apufile",.muste$apufile))
		}
	}