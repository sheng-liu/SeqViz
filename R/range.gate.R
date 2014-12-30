

## range gate
## rectagle gate with 2 dimension

range.gate=function(action, window){
    
    print("rangeGate")
    
    # select.channels(channelY=F)
    # checked.channels.name=appspace[channelX]
    # this worked
    # a better way is to use radio button, as it is easy to see all the channels
    check.channels(multiple=F)
    checked.channels.name=appspace[checked.channels]
    
    
    
    
    ##--------------------------------------------------------------------------
    ## data  for mouse event
    
    ## let's first deal with there is only one channel selected
    
    model=appspace[SeqFrame.hierachy.view]$getModel()
    selected.node=selected.node(appspace[SeqFrame.hierachy.view])
    
    #selected.frame=get(x=selected.node,envir=.AppEnv)
    selected.frame=appspace[active.SeqFrame]
    
    keyword(selected.frame,"GUID")
    dat=selected.frame
    
    channels=paste(checked.channels.name,collapse="`+`")
    
    
    df=data.frame(exprs(dat))
    
    f=sprintf("plot(density(x=df[,'%s']),col='red',main='',xlab='%s')",channels,channels)
    #parse(text=f)
    #eval(parse(text=f))
    
    
    ##--------------------------------------------------------------------------
    ## mouse events   
    
    #xWindow=X11()
    X11(width=4,height=4,type="Xlib")
    plot.new()
    # this plot.new is required as plot(eval(parse(text = f))) 
    # is not recoginized as a plot when put inside mouse event handlers
    
    eval(parse(text=f))
    
    mousedown <- function(buttons, x, y) {
        
        if(length(buttons)==2) "Done"
        else {
            x.cord=grconvertX(x,from="ndc",to="user")
            y.cord=grconvertY(y,from="ndc",to="user")
            
            eval(parse(text=f))
            abline(v=x.cord,col="cornflowerblue")
            
            #cat("Buttons ", paste(buttons, collapse=" "), " at ", x, y, "\n")
            #cat("convert ", paste(buttons, collapse=" "), " at ", x.cord, y.cord, "\n")
            appspace[x.cord.ini]=x.cord
            NULL
        }
    }
    
    mousemove=function(buttons,x,y){
        
        #plot(eval(parse(text = f)))
        eval(parse(text=f))
        x.cord=grconvertX(x,from="ndc",to="user")
        y.cord=grconvertY(y,from="ndc",to="user")
        
        # this draws start, and drag to the end
        abline(v=appspace[x.cord.ini],col="cornflowerblue")
        abline(v=x.cord,col="limegreen")
        # draw a line segment connects the two lines
        segments(x0=appspace[x.cord.ini],y0=y.cord,
                 x1=x.cord,y1=y.cord,col="gray")
        
        appspace[x.cord.end]=x.cord
        
        #cat("moves ", paste(buttons, collapse=" "), " at ", x, y, "\n")
        #cat("convert ", paste(buttons, collapse=" "), " at ", x.cord, y.cord, "\n")
        NULL
        
    }
    
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
    
    getGraphicsEvent(
        "\nLeft click and drag to draw gate \nRight click to finish\n\nClick SavePDF to save current gating window\nClose the current gating window before open a new one\n",
        onMouseDown = mousedown,
        onMouseMove = mousemove
        #                      onKeybd = keybd
    ) 
    
    
    
    
    
    ##--------------------------------------------------------------------------
    ## data 
    
    # use matrix as input, can also use list
    param=cbind(c(appspace[x.cord.ini],appspace[x.cord.end]))
    colnames(param)=channels
    
    rangeGate=rectangleGate(.gate=param,filterID="rangeGate")
    rangeGate.filter=filter(appspace[active.SeqFrame],rangeGate)
    
    ## filter box need to be a list, to store all filter in it.
    ## then how does one distinguish the first quad gate vs the last?
    appspace[filterBox]=rangeGate.filter
    
    print(summary(rangeGate.filter))
    cat("\n")
    
    rangeGate.split=split(appspace[active.SeqFrame],rangeGate)
    child.node.name=sapply(rangeGate.split,function(frames){
        keyword(frames)$GUID  
    })
    
    ## add the veggi name for now
    for (i in 1:length(child.node.name)) keyword(rangeGate.split[[i]])$VEGGI.NAME=child.node.name[i]
    
    # put result seqframes into SeqFrame.list
    sapply(seq_along(child.node.name),function(i){
        appspace[SeqFrame.list][[child.node.name[i]]]=rangeGate.split[[i]]})
    
    # insert child.node.name
    insert.node(
        node.name=child.node.name,tree.view=appspace[SeqFrame.hierachy.view],method="insert")
    
    # update infor 
    PlotPage.info.model <- rGtkDataFrame (exprs(appspace[active.SeqFrame]))
    
    
    
    
    
}


#plot(density(data.frame(exprs(dat))$Male.Het.H3K9me3),col="red",main="",xlab="Male.Het.H3K9me3")


