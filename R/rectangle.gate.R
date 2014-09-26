## rectangle gate is also on mouse move

rectangle.gate=function(action,window){
    
    print("Rectangel Gate")
    
    select.channels()
    
    ## data
    #     model=appspace[active.view]$getModel()
    #     selected.node=selected.node(appspace[active.view])
    #     selected.frame=get(x=selected.node,envir=.AppEnv)
    #     keyword(selected.frame,"GUID")
    #     dat=selected.frame
    
    
    xWindow=X11()
    flowPlot(x=appspace[active.seqFrame],plotParameters=c(appspace[channelX],appspace[channelY]))
    
    mousedown <- function(buttons, x, y) {
        
        x.cord=grconvertX(x,from="ndc",to="user")
        y.cord=grconvertY(y,from="ndc",to="user")
        
        #plot(eval(parse(text = f))) 
        #densityplot(~Male.Het.H3K9me3,data=df,plot.points = FALSE)
        #         eval(parse(text=f))
        #         abline(v=x.cord,col="cornflowerblue")
        
        
        cat("Buttons ", paste(buttons, collapse=" "), " at ", x, y, "\n")
        cat("convert ", paste(buttons, collapse=" "), " at ", x.cord, y.cord, "\n")
        
        appspace[x.cord.ini]=x.cord
        appspace[y.cord.ini]=y.cord
        
        NULL
    }
    
    mousemove=function(buttons,x,y){
        
        #plot(eval(parse(text = f)))
        # eval(parse(text=f))
        flowPlot(x=appspace[active.seqFrame],
                 plotParameters=c(appspace[channelX],appspace[channelY]))
        
        x.cord=grconvertX(x,from="ndc",to="user")
        y.cord=grconvertY(y,from="ndc",to="user")
        
        
        
        rect(xleft=appspace[x.cord.ini],xright=x.cord,
             ybottom=appspace[y.cord.ini],ytop=y.cord,
             border="red")
        #density=10,angle=45)
        
        
        
        appspace[x.cord.end]=x.cord
        appspace[y.cord.end]=y.cord
        
        
        cat("moves ", paste(buttons, collapse=" "), " at ", x, y, "\n")
        cat("convert ", paste(buttons, collapse=" "), " at ", x.cord, y.cord, "\n")
        NULL
        
    }
    
    keybd <- function(key) {
        cat("Key <", key, ">\n", sep = "")
        # if (key=="ctrl-Q") "Done"
        
        #if (key=="ctrl-S") dev.copy2pdf(file = "table.2.pdf")
        if (key=="ctrl-S") {
            save_PDF(window=xWindow)
            "Done"
        }
        
    }
    
    
    getGraphicsEvent("Click and drag to draw rectangeGate",
                     onMouseDown = mousedown,
                     onMouseMove = mousemove,
                     onKeybd = keybd) 
    
    
    
    ## data
    
    ## varibles are rectGate
    
    # use matrix as input, can also use list
    param=cbind(c(appspace[x.cord.ini],appspace[y.cord.ini]),
                c(appspace[x.cord.end],appspace[y.cord.end]))
    
  
    colnames(param)=c(appspace[channelX],appspace[channelY])
    
  
    param
    
    
    rectGate=rectangleGate(.gate=param,filterID="rectangleGate")
    
    rectGate.filter=filter(appspace[active.seqFrame],rectGate)
    
    appspace[filterBox]=rectGate.filter
    
    print(summary(rectGate.filter))
    
    appspace[rectGate.split]=split(appspace[active.seqFrame],rectGate)
    
    child.node.name=sapply(appspace[rectGate.split],function(frames){
        keyword(frames)$GUID  
    })
    
    ## add the veggi name for now
    for (i in 1:length(child.node.name)) keyword(appspace[rectGate.split][[i]])$VEGGI.NAME=child.node.name[i]
    
    sapply(seq_along(child.node.name),function(i){
        assign(child.node.name[i],value=appspace[rectGate.split][[i]],envir=.AppEnv) })
    
    insert.node(node.name=child.node.name,parent=appspace[active.view],loc="insert")
    
    ## adjust save_csv to be dynamic to selected.node
    selected.node=selected.node(appspace[active.view])
    appspace[save_csv]=get(selected.node,envir=.AppEnv)
    
    
    
    
    
    
    
    
    
    
}




