## rectangle gate is also on mouse move

rectangle.gate=function(action,window){
    
    print("Rectangel Gate")
    
    select.channels()
    
    
    ##--------------------------------------------------------------------------
    ## mouse events
    
    #xWindow=X11()
    x11()
    flowPlot(x=appspace[active.seqFrame],
             plotParameters=c(appspace[channelX],appspace[channelY]))
    
    mousedown <- function(buttons, x, y) {
        
        
        
        
        if(length(buttons)==2) "Done"
        else {
            flowPlot(x=appspace[active.seqFrame],
                     plotParameters=c(appspace[channelX],appspace[channelY]))
        x.cord=grconvertX(x,from="ndc",to="user")
        y.cord=grconvertY(y,from="ndc",to="user")
        
        #cat("Buttons ", paste(buttons, collapse=" "), " at ", x, y, "\n")
        #cat("convert ", paste(buttons, collapse=" "), " at ", x.cord, y.cord, "\n")
        
        appspace[x.cord.ini]=x.cord
        appspace[y.cord.ini]=y.cord
        NULL
        }
    }
    
    mousemove=function(buttons,x,y){
        
        flowPlot(x=appspace[active.seqFrame],
                 plotParameters=c(appspace[channelX],appspace[channelY]))
        
        x.cord=grconvertX(x,from="ndc",to="user")
        y.cord=grconvertY(y,from="ndc",to="user")
        
        rect(xleft=appspace[x.cord.ini],xright=x.cord,
             ybottom=appspace[y.cord.ini],ytop=y.cord,
             border="red")
        #density=10,angle=45        
        
        appspace[x.cord.end]=x.cord
        appspace[y.cord.end]=y.cord
        
        cat("moves ", paste(buttons, collapse=" "), " at ", x, y, "\n")
        cat("convert ", paste(buttons, collapse=" "), " at ", x.cord, y.cord, "\n")
        NULL
        
    }
    
#     keybd <- function(key) {
#         cat("Key <", key, ">\n", sep = "")
#         # if (key=="ctrl-Q") "Done"
#         
#         # if (key=="ctrl-S") dev.copy2pdf(file = "table.2.pdf")
#         if (key=="ctrl-S") {
#             Save_PDF(window=xWindow)
#             "Done"
#         }  
#     }
    
    getGraphicsEvent("Click and drag to draw rectangeGate",
                     onMouseDown = mousedown,
                     onMouseMove = mousemove
                     #onKeybd = keybd
                     ) 
    
    ##--------------------------------------------------------------------------
    ## data    
    
    # use matrix as input, can also use list
    param=cbind(c(appspace[x.cord.ini],appspace[y.cord.ini]),
                c(appspace[x.cord.end],appspace[y.cord.end]))
    colnames(param)=c(appspace[channelX],appspace[channelY])
    
    rectGate=rectangleGate(.gate=param,filterID="rectangleGate")
    rectGate.filter=filter(appspace[active.seqFrame],rectGate)
    
    # use a list is better
    appspace[filterBox]=rectGate.filter
    
    print(summary(rectGate.filter))
    
    
    
    appspace[rectGate.split]=split(appspace[active.seqFrame],rectGate)
    
    child.node.name=sapply(appspace[rectGate.split],
                           function(frames){keyword(frames)$GUID})
    
    ## add the veggi name for now
    for (i in 1:length(child.node.name)) 
        keyword(appspace[rectGate.split][[i]])$VEGGI.NAME=child.node.name[i]
    
    sapply(seq_along(child.node.name),
           function(i){
               assign(x=child.node.name[i],value=appspace[rectGate.split][[i]],envir=.AppEnv) })
    
    insert.node(node.name=child.node.name,parent=appspace[active.view],loc="insert")
    
    ## adjust save_csv to be dynamic to selected.node
    selected.node=selected.node(appspace[active.view])
    appspace[save_csv]=get(selected.node,envir=.AppEnv)
    
}




