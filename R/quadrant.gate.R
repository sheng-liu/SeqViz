
## quadrant gate

quadrant.gate=function(action, window){
    
    print("Quadrant Gate")
    
    # update selected.node and active.SeqFrame
    # selected.node(appspace[SeqFrame.hierachy.view])
    
    select.channels()
    
    ##--------------------------------------------------------------------------
    ## mouse events
    
   x11(width=4,height=4)
    flowPlot(x=appspace[active.SeqFrame],
             plotParameters=c(appspace[channelX],appspace[channelY]))
    
    
    
    #,colramp=col
    # try xyplot
#         dat=appspace[active.SeqFrame]
#         x=appspace[channelX]
#         y=appspace[channelY]
#         f=sprintf("xyplot(`%s`~`%s`,data =dat,smooth=F,outline=T )",y,x)
#         parse(text = f)    
#         x11(width=4,height=4)
#         plot(eval(parse(text = f)))
#     
    
    
    
    ## define the event handlers
    mousedown=function(buttons,x,y){
        
        if(length(buttons)==2) "Done"
        else {
            flowPlot(x=appspace[active.SeqFrame],
                     plotParameters=c(appspace[channelX],appspace[channelY]))
            
#             dat=appspace[active.SeqFrame]
#             x=appspace[channelX]
#             y=appspace[channelY]
#             f=sprintf("xyplot(`%s`~`%s`,data =dat,smooth=F,outline=T )",y,x)
#             plot.new()   
#             plot(eval(parse(text = f)))
            
            
            user.x=grconvertX(x,from="ndc",to="user")
            user.y=grconvertY(y,from="ndc",to="user")
            
            ## output x,y
            appspace[user.x]=user.x
            appspace[user.y]=user.y
            
            # draw guid lines
            abline(v=user.x,col="red")
            abline(h=user.y,col="red")
            
#             xyplot(mpg ~ disp | factor(gear), data=mtcars,
#                    layout=c(3, 1), aspect=1,
#                    panel=function(...) {
#                        panel.xyplot(...)  # this does not need to be the first
#                        panel.abline(h=29, lty="dashed")
#                        panel.text(470, 29.5, "efficiency criterion",
#                                   adj=c(1, 0), cex=.7)
#                        
#                        
#                    })
#             
#             panel.abline(h=29, lty="dashed")
            
            ## if want to implement click outside plot stop sampling
            ## use a list to collect all user selections
            ## ouput the second last
            
            #cat("user.x=",appspace[user.x],"\t")
            #cat("user.y=",appspace[user.y],"\n")
            NULL
        } 
    }
    
    getGraphicsEvent(
        "\nLeft click to draw gate \nRight click to finish\n\nClick SavePDF to save current gating window\nClose the current gating window before open a new one\n",
        onMouseDown=mousedown
    )
    
    ##--------------------------------------------------------------------------
    ## data   
    
    names(appspace[user.x])=appspace[channelX]
    names(appspace[user.y])=appspace[channelY]
    
    param=c(appspace[user.x],appspace[user.y])
    
    quadGate=quadGate(.gate=param,filterId="quadGate")
    quadGate.filter=filter(appspace[active.SeqFrame],quadGate)
    
    ## filter box need to be a list, to store all filter in it.
    ## then how does one distinguish the first quad gate vs the last?
    appspace[filterBox]=quadGate.filter
    
    print(summary(quadGate.filter))
    cat("\n")
    
    
    
    # no need to put quadGate.split into appspace, just put them into SeqFrame.list
    #appspace[quadGate.split]=split(appspace[active.SeqFrame],quadGate)
    #quadGate.split=split(appspace[active.SeqFrame],quadGate)
    quadGate.split=SeqFrame::split(appspace[active.SeqFrame],quadGate)
    
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
    
    ############################################################################
    # put result seqframes into SeqFrame.list
    sapply(seq_along(child.node.name),function(i){
        appspace[SeqFrame.list][[child.node.name[i]]]=quadGate.split[[i]]})
    ############################################################################
    
    
    insert.node(
        node.name=child.node.name,tree.view=appspace[SeqFrame.hierachy.view],method="insert")
    
    
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