## polygon.gate


polygon.gate=function(action,window) {    

    
    print("Polygon gate")
    
    select.channels()
 
    
    ##--------------------------------------------------------------------------
    ## use locator replace mouse events   
    
    ## X11() way of implementation
    
    #xWindow=X11()
    X11()
    options(locatorBell = FALSE)
    
    flowPlot(x=appspace[active.seqFrame],plotParameters=c(appspace[channelX],appspace[channelY]))
    
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
#     
    
    ## maybe use just this to gate, and save the filter results, and let user to plot at later time or any time
    
    

    ##--------------------------------------------------------------------------
    ## data   
    
    colnames(location.df)[1]=appspace[channelX]
    colnames(location.df)[2]=appspace[channelY]
    
    polygonGate=polygonGate(.gate=location.df,filterId="polygonGate")
    
    polygonGate.filter=filter(appspace[active.seqFrame],polygonGate)
    
    appspace[filterBox]=polygonGate.filter
    
    print(summary(polygonGate.filter))
    

    appspace[polygonGate.split]=split(appspace[active.seqFrame],polygonGate)
    
    
    #x=sapply(appspace[polygonGate.split],keyword,"GUID")
    child.node.name=sapply(appspace[polygonGate.split],function(frames){
      keyword(frames)$GUID  
    })
    

    ## add one keyword/description as fruit.name serve as a nick name to fast catch the gate in the plotting
    for (i in 1:length(child.node.name)) 
        keyword(appspace[polygonGate.split][[i]])$VEGGI.NAME=child.node.name[i]
    
    
    # the R way for repeated operation
    # could have use for loop instead
    sapply(seq_along(child.node.name),function(i){
        assign(child.node.name[i],value=appspace[polygonGate.split][[i]],envir=.AppEnv) })
    
    insert.node(node.name=child.node.name,parent=appspace[active.view],loc="insert")
    
   
    ## adjust save_csv to be dynamic to selected.node
    selected.node=selected.node(appspace[active.view])
    appspace[save_csv]=get(selected.node,envir=.AppEnv)
    
    

}


# assign different values to each list element
#     values=child.node.name
#     lists=appspace[polygonGate.split]
#     for (i in 1:length(values)) keyword(lists[[i]])$VEGGI.NAME=values[i]

##TODO
## make the tree expand
