## scatter.plot

## aka dot.plot


#smoother=F  ## add to preferenc panel

scatter.plot=function(action,window){
    print("scatter plot")
    
    select.channels()
    
    # flowPlot(x=appspace[active.seqFrame],plotParameters=c(appspace[channelX],appspace[channelY]))
    # this doens't use the range parameter so only this works without proper annotation, lucky to use this one first
    
    # this also works, it is only for flowframe, doesn't have color when smooth=F
    # plot(sf,c(colnames(sf)[2],colnames(sf)[3]))
    # plot(sf,c(colnames(sf)[2],colnames(sf)[3]),smooth=F)
    
    
    ##---------------
    ## aim is to display the original with the child subset plot so people can compare
    ## kind of like a stacked 
    ## make the flowSet
    
    model=appspace[active.view]$getModel()
    
    # select node's parent node and form a flowSet
    selected.node=selected.node(appspace[active.view])
    all.child.node=select.all.child.node(appspace[active.view],itself=T)
    parent.node=select.parent.node(appspace[active.view],itself=F)
    
    parent.frames=lapply(parent.node,function(node.name){
        get(x=node.name,envir=.AppEnv)
    })
    
    child.frames=lapply(all.child.node,function(node.name){
        get(x=node.name,envir=.AppEnv)
    })
    
    frames=c(child.frames,parent.frames)
    
    names(frames)=sapply(frames, keyword, "GUID")
    
    ## change name into veggi name, it is GUID is too long to catch fast
    #names(frames)=sapply(frames, keyword, "VEGGI.NAME")
    
    # the plot is ordered by the name, so guid ID makes the order better
    
    fs=as(frames,"flowSet")
    
    
    
    dat=fs
    
    #dat=appspace[active.seqFrame]
   
    x=appspace[channelX]
    y=appspace[channelY]
    f=sprintf("xyplot(`%s`~`%s`,data =dat,smooth=F,outline=T )",y,x)
    parse(text = f)
    #dev.new()
    ## dev.new gives device based on the operation system.
    
    dev.new(width=6,height=4)
    plot(eval(parse(text = f)))
    
    #eval(parse(text = f))  # only this also works in command line
    
    
    # > f
    # [1] "xyplot(`Male.Het.H3K27me3`~`Male.Het.H3K9me3`, data =dat )"
    # > parse(text = f)
    # expression(xyplot(`Male.Het.H3K27me3`~`Male.Het.H3K9me3`, data =dat ))
    
    
    # the working expression
    # make sure type in exactly like this
    #> xyplot(`Male.Het.H3K9me3`~`Male.Het.H3K27me3`, data =dat )
    #> xyplot(`Male.Het.H3K9me3`~`Male.Het.H3K27me3`, data =dat ,smooth=F)
    
    ## TODO
    ## GUID maybe need to change to the corresponding fruit name 
    
    
    
}


