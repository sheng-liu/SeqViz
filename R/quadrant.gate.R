
## quadrant gate

quadrant.gate=function(action, window){
    
    print("Quadrant Gate")
    
    select.channels()
    
    ##--------------------------------------------------------------------------
    ## mouse events

    xWindow=X11()
    flowPlot(x=appspace[active.seqFrame],plotParameters=c(appspace[channelX],appspace[channelY]))
    
    ## define the event handlers
    mousedown=function(buttons,x,y){

        flowPlot(x=appspace[active.seqFrame],plotParameters=c(appspace[channelX],appspace[channelY]))
        
        user.x=grconvertX(x,from="ndc",to="user")
        user.y=grconvertY(y,from="ndc",to="user")
        
        ## output x,y
        appspace[user.x]=user.x
        appspace[user.y]=user.y
        
        # draw guid lines
        abline(v=user.x,col="red")
        abline(h=user.y,col="red")
        
        ## if want to implement click outside plot stop sampling
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

    getGraphicsEvent("Click mouse to draw quadrant gate \nClick OK button to finish",
                     onMouseDown=mousedown,
                     onKeybd = keybd
    )

    ##--------------------------------------------------------------------------
    ## data   
    
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
     
    ## adjust save_csv to be dynamic to selected.node
    selected.node=selected.node(appspace[active.view])
    appspace[save_csv]=get(selected.node,envir=.AppEnv)
    
}

##  embed cairoDevice method

#     # widget gtkDrawingArea() to plot
#     graphics <- gtkDrawingArea()
#     require("cairoDevice")
#     asCairoDevice(graphics)
#     button=gtkButton(label="OK")
#     # a window to contain
#     win <- gtkWindow(show = FALSE)
#     vbox <- gtkVBox()
#     vbox$packStart(graphics, expand = TRUE, fill = TRUE, padding = 0)
#     vbox$packStart(button, expand = FALSE, fill = FALSE, padding = 0)
#     win$add(vbox)
#     win$setDefaultSize(400,400)
#     win$setTitle(title="quadrantGate")
#     win$showAll()
