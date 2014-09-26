
## quadrant gate




quadrant.gate=function(action, window){
    
    print("Quadrant Gate")
    
    select.channels()
    
    #     # widget gtkDrawingArea() to plot
    #     graphics <- gtkDrawingArea()
    #     
    #     require("cairoDevice")
    #     asCairoDevice(graphics)
    #     
    #     button=gtkButton(label="OK")
    #     
    #     # a window to contain
    #     win <- gtkWindow(show = FALSE)
    #     vbox <- gtkVBox()
    #     vbox$packStart(graphics, expand = TRUE, fill = TRUE, padding = 0)
    #     vbox$packStart(button, expand = FALSE, fill = FALSE, padding = 0)
    #     
    #     win$add(vbox)
    #     win$setDefaultSize(400,400)
    #     win$setTitle(title="quadrantGate")
    #     win$showAll()
    #     
    
    ##cairoDevice::Cairo()
    ## if plot in cairoDevice::Cairo(), the coordinates is completely off
    ## if plot in asCairoDevice(graphics), the coordinates follows X11()
    ## make sure not to load cairoDevice::Cairo(), as this function needs
    ## x11() coordinates, so grconvertX can work, it doesn't work on Cairo() or cairoDevice::Cairo()
    ## or one may simply does not embed, but use a single x11() window
    ## however, X11 need to implement save function to save the graphics
    
    
    ## try not to use embeded cairo
    #options(device="X11")    
    #device.new()
    xWindow=X11()
    
    flowPlot(x=appspace[active.seqFrame],plotParameters=c(appspace[channelX],appspace[channelY]))
    
    #options(device = "X11")
    #dev.new()
    ## define the event handlers
    
    ## define collectors
    #user.x=0
    #user.y=0
    ## use appspace to collect
    
    quadrant=function(buttons,x,y){
        #plot.new()
        #plot(0:1, 0:1, type='n')
        # plot
        
        flowPlot(x=appspace[active.seqFrame],plotParameters=c(appspace[channelX],appspace[channelY]))
        
        user.x=grconvertX(x,from="ndc",to="user")
        user.y=grconvertY(y,from="ndc",to="user")
        
        ## output x,y
        appspace[user.x]=user.x
        appspace[user.y]=user.y
        
        
        abline(v=user.x,col="red")
        abline(h=user.y,col="red")
        
        ## without ok button
        ## use a list to collect all user selections
        ## ouput the second last
        cat("user.x=",appspace[user.x],"\t")
        cat("user.y=",appspace[user.y],"\n")
        NULL
        
    }
    
    keybd <- function(key) {
        cat("Key <", key, ">\n", sep = "")
        #if (key=="ctrl-Q") "Done"
        
        #if (key=="ctrl-S") dev.copy2pdf(file = "table.2.pdf")
        if (key=="ctrl-S") {
            save_PDF(window=xWindow)
            "Done"
        }
        
    }
    # this step turn out not necessary
    #set the event handlers
    #setGraphicsEventHandlers(onMouseDown = quadrant)
    
    ##call getGraphicsEvent() (with no arguments)
    getGraphicsEvent("Click mouse to draw quadrant gate \nClick OK button to finish",
                     onMouseDown=quadrant,
                     onKeybd = keybd
    )
    
    
    
    # try on mouse move
    # getGraphicsEvent("Click mouse to draw quadrant gate \nClick OK button to finish",onMouseMove=quadrant)
    # it worked, but flashing all the time
    # the reasonable doing is if one can add a layer on top of the plot, instead of plotting it all time.
    
    
    #     #quadGate(filterId="myQuadGate3", .gate=c("FSC-H"=100, "SSC-H"=400))
    
    
    # can also use this
    
    #         mousedown = function(buttons, x, y) {
    #             quadrant(buttons,x,y)
    #         }
    #         
    #         getGraphicsEvent("Click mouse to draw quadrant gate \nClick OK button to finish", onMouseDown = mousedown)
    
    
    
    
    #Quit=function(widget, window) window$destroy()
    
    
    #     ok_button_cb=function(action,window){
    #         loc=c(user.x,user.y)
    #  
    #     }
    
    
    # deparse(appspace[channelX])
    # "\"Male.Het.DNAme\""
    # this can be put in one line
    #strsplit(deparse(appspace[channelX]),split="\"")[[1]][2]
    #strsplit(deparse(appspace[channelY]),split="\"")[[1]][2]
    
    # or use another variable
    #x.label=appspace[channelX]
    #y.label=appspace[channelY]
    
    # names(appspace[channelX])=appspace[channelX]
    # names(appspace[channelY])=appspace[channelY]
    
    
    names(appspace[user.x])=appspace[channelX]
    names(appspace[user.y])=appspace[channelY]
    
    
    param=c(appspace[user.x],appspace[user.y])
    quadGate=quadGate(.gate=param,filterId="quadGate")
    quadGate.filter=filter(appspace[active.seqFrame],quadGate)
    
    
    
    ## filter box need to be a list, to store all filter in it.
    ## then how does one distinguish the first quad gate vs the last?
    appspace[filterBox]=quadGate.filter
    
    print(summary(quadGate.filter))
    
    appspace[quadGate.split]=split(appspace[active.seqFrame],quadGate)
    
    child.node.name=sapply(appspace[quadGate.split],function(frames){
        keyword(frames)$GUID  
    })
    
    ## add the veggi name for now
    for (i in 1:length(child.node.name)) keyword(appspace[quadGate.split][[i]])$VEGGI.NAME=child.node.name[i]
    
    
    #sapply(appspace[quadGate.split],keyword,"VEGGI.NAME")
    
    sapply(seq_along(child.node.name),function(i){
        assign(child.node.name[i],value=appspace[quadGate.split][[i]],envir=.AppEnv) })
    
    insert.node(node.name=child.node.name,parent=appspace[active.view],loc="insert")
    
    
    
    
    
    
    #     button=gtkButton(label="OK")
    #     
    #     # a window to contain
    #     win <- gtkWindow(show = FALSE)
    #     vbox <- gtkVBox()
    #     vbox$packStart(graphics, expand = TRUE, fill = TRUE, padding = 0)
    #     vbox$packStart(button, expand = FALSE, fill = FALSE, padding = 0)
    #     
    #     win$add(vbox)
    #     win$setDefaultSize(400,400)
    #     win$showAll()
    #     
    
    ## adjust save_csv to be dynamic to selected.node
    selected.node=selected.node(appspace[active.view])
    appspace[save_csv]=get(selected.node,envir=.AppEnv)
    
}


