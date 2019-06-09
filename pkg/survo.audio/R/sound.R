snd <- new.env(hash=TRUE, parent=emptyenv())

.onLoad <- function(libname,pkgname)
    { 
    if (interactive()) .loadsounds() 
    }

.loadsounds <- function()
    {
    data(survo.sounds,package="survo.audio",envir=snd)
    snd$trigger <- 0
    snd$playing <- 0
    }

.pauseplay <- function(sample=1)
    {
    pause(snd$playing)
    }

survo.play <- function(sound="default",async=TRUE,timeout=NA)
    {
    if (require("audio"))
        {
        sndindex<-match(sound,names(snd$survo.sounds))
        if (is.na(sndindex))
            {
            if (require(muste))
                {
                path <- muste::muste_ExpandFileName(sound)
                }
            else
                {
                path <- sound
                }
            snd$survo.sounds[["default"]] <- load.wave(path)
            sndindex <- 1
            }
        if (typeof(snd$trigger)=="externalptr")
            {
            if (require(tcltk))
                {
                tcl("after", "cancel", snd$trigger)
                }
            snd$trigger <- 0
            }              
        if (typeof(snd$playing)=="externalptr")
            {
            pause(snd$playing)
            }    
        snd$playing <- audio::play(snd$survo.sounds[[sndindex]])
        if (async)
            {
            if (!is.na(timeout))
                {
                if (require(tcltk))
                    {
                    snd$trigger <- tcl("after", 1000*timeout, .pauseplay)
                    }
                }
            }
        else
            {
            wait(snd$playing,timeout)
            pause(snd$playing)
            }
        }
    }
    
survo.tone <- function (tonefreq=420, duration=0.2, sampfreq=22050)
    {
    n <- round(sampfreq * duration)
    t <- seq(0, duration * 2 * pi, length.out = n)
    sound <- sin(tonefreq * t)
    sound <- sound/max(abs(sound))
    if (require("audio"))
        {
        if (typeof(snd$playing)=="externalptr")
            {
            pause(snd$playing)
            }    
        snd$playing <- audio::play(audio::audioSample(sound, rate = sampfreq, bits = 16, clip = FALSE))
        wait(snd$playing)
        pause(snd$playing)
        }
    }
