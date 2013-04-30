savepar <- par(ask=FALSE)
     dragplot <- function(..., xlim=NULL, ylim=NULL, xaxs="r", yaxs="r") {
      
       plot(..., xlim=xlim, ylim=ylim, xaxs=xaxs, yaxs=yaxs)
         
         
         startx <- NULL
         starty <- NULL
         usr <- NULL
         j<<-1
         
     
         devset <- function()
             if (dev.cur() != eventEnv$which) dev.set(eventEnv$which)
             
         dragmousedown <- function(buttons, x, y) {
             startx <<- x
             starty <<- y
             devset()
             usr <<- par("usr")
             eventEnv$onMouseMove <- dragmousemove
             NULL
         }
         
         dragmousemove <- function(buttons, x, y) {
             devset()
             deltax <- diff(grconvertX(c(startx,x), "ndc", "user"))
             deltay <- diff(grconvertY(c(starty,y), "ndc", "user"))
             plot(..., xlim=usr[1:2]-deltax, xaxs="i",
                       ylim=usr[3:4]-deltay, yaxs="i")
             NULL
         }
         
         mouseup <- function(buttons, x, y) {    
             eventEnv$onMouseMove <- NULL
         }   
             
         keydown <- function(key) {
             if (key == "q") return(invisible(1))
             eventEnv$onMouseMove <- NULL
             if(key=="6"){
               plot(rnorm(10))
               Sys.sleep(0.1)
              }
            if(key=="4"){
              plot(rnorm(1000))
              Sys.sleep(0.1)
            }
              NULL
        }

        
         
         setGraphicsEventHandlers(prompt="Click and drag, hit q to quit",
                          onMouseDown = dragmousedown,
                          onMouseUp = mouseup,
                          onKeybd = keydown)
         eventEnv <- getGraphicsEventEnv()
     }
     
     X11(type = "Xlib")
     dragplot(rnorm(1000), rnorm(1000))
     # This currently only works on the Windows
     # and X11(type = "Xlib") screen devices...
     getGraphicsEvent()
     par(savepar)
     

