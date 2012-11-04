.muste.setup <- function(init=FALSE)
	{
	if (init)
		{
		if(file.access(.muste$homedir,mode=2)==-1) return() 
		viesti <-
		paste("Configuration file for Muste is missing!\nIs it OK to create file\n",
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
		.muste.command(c("Apufile",.muste$apufile))
		}
	}