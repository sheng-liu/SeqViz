
## quadrant gate

quadrant.gate=function(action, window){
    
    print("Quadrant Gate")
    
    # update selected.node and active.seqFrame
    # selected.node(appspace[active.view])
    
    select.channels()
    
    ##--------------------------------------------------------------------------
    ## mouse events
    
    #xWindow=X11()
    x11()
    flowPlot(x=appspace[active.seqFrame],
             plotParameters=c(appspace[channelX],appspace[channelY]))
    
    ## define the event handlers
    mousedown=function(buttons,x,y){
        
        if(length(buttons)==2) "Done"
        else {
            flowPlot(x=appspace[active.seqFrame],
                     plotParameters=c(appspace[channelX],appspace[channelY]))
            
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
        
    }
    
    
    getGraphicsEvent(
        "Click mouse to draw quadrant gate \nClick OK button to finish",
        onMouseDown=mousedown
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
    
    # no need to put quadGate.split into appspace, just put them into seqFrame.list
    #appspace[quadGate.split]=split(appspace[active.seqFrame],quadGate)
    quadGate.split=split(appspace[active.seqFrame],quadGate)
    
    ## add the veggi name for now
    child.node.name=sapply(quadGate.split,function(frames){keyword(frames)$GUID})
    
   
#     for (i in 1:length(child.node.name)) 
#         keyword(appspace[quadGate.split][[i]])$VEGGI.NAME=child.node.name[i]
    
    for (i in 1:length(child.node.name)) 
        keyword(quadGate.split[[i]])$VEGGI.NAME=child.node.name[i]
    
    #sapply(appspace[quadGate.split],keyword,"VEGGI.NAME")
    
    # assign all variables to appspace
#     sapply(seq_along(child.node.name),function(i){
#         assign(child.node.name[i],
#                value=appspace[quadGate.split][[i]],envir=.AppEnv) })
    
    # put result seqframes into seqFrame.list
    sapply(seq_along(child.node.name),function(i){
        appspace[seqFrame.list][[child.node.name[i]]]=quadGate.split[[i]]})
    
    
    
    
    insert.node(
        node.name=child.node.name,tree.view=appspace[active.view],method="insert")
    
    
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


## keybord save
#  onKeybd = keybd

#     keybd <- function(key) {
#         cat("Key <", key, ">\n", sep = "")
#         #if (key=="ctrl-Q") "Done"
#         
#         #if (key=="ctrl-S") dev.copy2pdf(file = "table.2.pdf")
#         if (key=="ctrl-S") {
#             Save_PDF(window=xWindow)
#             "Done"
#         }   
#     }