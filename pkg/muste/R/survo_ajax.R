websurvo <- function()
{
# library(Rook)
.muste.command(c("Require","Rook"),force=TRUE)	    	
if (requireNamespace("RCurl",quietly=TRUE)) {

s <- Rook::Rhttpd$new()


makeRandomString <- function(n=1, lenght=12)
{
    randomString <- c(1:n)                  # initialize vector
    for (i in 1:n)
    {
        randomString[i] <- paste(sample(c(0:9, letters, LETTERS),
                                 sample(1:lenght,1), replace=TRUE),
                                 collapse="")
    }
    return(randomString)
}

pla <- 0

ajaxHandler <- function(env) {

    req <- Rook::Request$new(env)

    if(!is.null(req$POST())) {

    disp <- req$POST()[["disp"]]

    if (disp==1)
        {
        .muste$redraw <- as.integer(2)
        }
    else
        {
    cha <- req$POST()[["char"]]
    
    .muste$event.time<-as.integer(.muste$event.time+1)
    .muste$event.type<-as.integer(1)  # KEY_EVENT
    .muste$key.char<-cha
    .muste$key.keysym<-as.integer(0)
    .muste$key.status<-as.integer(1)
        }
       
    invisible(.Call("Muste_Eventloop",.muste$eventloopargs,PACKAGE="muste"))

      
    res <- Rook::Response$new()
    res$header("Content-Type", "text/plain")
    res$write(.muste$ajaxmsg)
    res$finish()
}

}

normHandler <- function(env) {
    req <- Rook::Request$new(env)
    res <- Rook::Response$new()

    res$write('<!DOCTYPE html><html>
      <head><meta charset="UTF-8">
    <style>
      body, html {
        margin: 0px;
        padding: 0px;
        border: 0;
        overflow: hidden; /*  Disable scrollbars */
        display: block;  /* No floating content on sides */        
      }
    </style>
  </head> 
    <body onload=loadDocChar("survo_ajax.txt","disp=1",myFunction)>')

res$write('
<script src="web/survo.js"></script>

<p id="demo"></p>
<p id="status"></p>
<p id="status2"></p>

</body></html>
    ')
    res$finish()
    }

Survo.app <- function() Rook::Builder$new(

    Rook::Static$new(
        urls = c('/web'),
        root = system.file(package="muste")
    ),

Rook::URLMap$new(
        '/survo_ajax.txt' = ajaxHandler,
        '/' = normHandler
#        ,        '/' = Redirect$new('/web/survo.html')        
    )
)

s$add(app = Survo.app(), name = "Survo")
s$add(app = Survo.app(), name = "Survo/")

s$start()
survourl <- s$full_url(2)


.muste$redraw <- as.integer(1)


# Sys.sleep(10)


viewer <- getOption("viewer")
    if (!is.null(viewer))
       viewer(survourl)
    else {  
        s$browse("Survo/")
#        utils::browseURL(survourl)     
     }

}
}