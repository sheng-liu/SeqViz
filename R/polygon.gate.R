## polygon.gate


polygon.gate=function(action,window) {    
    
    
    print("Polygon gate")
    
    select.channels()
    
    
    ##--------------------------------------------------------------------------
    ## use locator replace mouse events   
    
    ## X11() way of implementation
    
    #xWindow=X11()
    X11(width=4,height=4)
    options(locatorBell = FALSE)
    
    flowPlot(x=appspace[active.seqFrame],
             plotParameters=c(appspace[channelX],appspace[channelY]))
    
    # gate
    location <- locator(n =512, type = "o")
    location.df=data.frame(location$x,location$y)
    
    
    # if use quartz() would have saved this 
    #     keybd <- function(key) {
    #         cat("Key <", key, ">\n", sep = "")
    #         #if (key=="ctrl-Q") "Done"
    #         
    #         #if (key=="ctrl-S") dev.copy2pdf(file = "table.2.pdf")
    #         if (key=="ctrl-S") {
    #             save_PDF(window=xWindow)
    #             "Done"
    #         }   
    #     }
    #     getGraphicsEvent("ctrl-S to save",onKeybd = keybd)

    
    ## maybe use just this to gate, and save the filter results, and let user to plot at later time or any time
    
    ##--------------------------------------------------------------------------
    ## data   
    
    colnames(location.df)[1]=appspace[channelX]
    colnames(location.df)[2]=appspace[channelY]
    
    polygonGate=polygonGate(.gate=location.df,filterId="polygonGate")
    
    polygonGate.filter=filter(appspace[active.seqFrame],polygonGate)
    
    appspace[filterBox]=polygonGate.filter
    
    print(summary(polygonGate.filter))
    
    
    polygonGate.split=split(appspace[active.seqFrame],polygonGate)
    
    
    #x=sapply(polygonGate.split,keyword,"GUID")
    child.node.name=sapply(polygonGate.split,function(frames){
        keyword(frames)$GUID  
    })
    
    
    ## add one keyword/description as fruit.name serve as a nick name to fast catch the gate in the plotting
    for (i in 1:length(child.node.name)) 
        keyword(polygonGate.split[[i]])$VEGGI.NAME=child.node.name[i]
    
    
    # the R way for repeated operation
    # could have use for loop instead
    #     sapply(seq_along(child.node.name),function(i){
    #         assign(child.node.name[i],value=polygonGate.split[[i]],envir=.AppEnv) })
    
    # put result seqframes into seqFrame.list
    sapply(seq_along(child.node.name),function(i){
        appspace[seqFrame.list][[child.node.name[i]]]=polygonGate.split[[i]]})
    
    
    insert.node(
        node.name=child.node.name,tree.view=appspace[active.view],method="insert")
     
}


# assign different values to each list element
#     values=child.node.name
#     lists=polygonGate.split
#     for (i in 1:length(values)) keyword(lists[[i]])$VEGGI.NAME=values[i]

