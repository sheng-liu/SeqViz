## scatter.plot

## aka dot.plot


#smoother=F  ## add to preferenc panel

scatter.plot=function(action,window){
    print("scatter plot")
    
    # update selected.node and active.SeqFrame
    # selected.node()
    # there is always an active.SeqFrame created in appspace when data loaded in
    
    select.channels()
    
    # flowPlot(x=appspace[active.SeqFrame],plotParameters=c(appspace[channelX],appspace[channelY]))
    # this doens't use the range parameter so only this works without proper annotation, lucky to use this one first
    
    # this also works, it is only for flowframe, doesn't have color when smooth=F
    # plot(sf,c(colnames(sf)[2],colnames(sf)[3]))
    # plot(sf,c(colnames(sf)[2],colnames(sf)[3]),smooth=F)
    
    
    ##---------------
    ## aim is to display the original with the child subset plot so people can compare
    ## kind of like a stacked 
    ## make the flowSet
    
    # model=appspace[SeqFrame.hierachy.view]$getModel()
    
    # select node's parent node and form a flowSet
    selected.node=selected.node(appspace[SeqFrame.hierachy.view])
    all.child.node=select.all.child.node(appspace[SeqFrame.hierachy.view],itself=T)
    parent.node=select.parent.node(appspace[SeqFrame.hierachy.view],itself=F)
    
    #     parent.frames=lapply(parent.node,function(node.name){
    #         # get(x=node.name,envir=.AppEnv)
    # })
    
    
    parent.frames=lapply(parent.node,function(parent.node.name){
        SeqFrame.list=appspace[SeqFrame.list]
        parent.SeqFrame=SeqFrame.list[
            names(SeqFrame.list)==parent.node.name][[parent.node.name]]
        return(parent.SeqFrame)
    })
    
    
    
    #     child.frames=lapply(all.child.node,function(node.name){
    #         get(x=node.name,envir=.AppEnv)
    #     })
    
    child.frames=lapply(all.child.node,function(child.node.name){
        SeqFrame.list=appspace[SeqFrame.list]
        child.SeqFrame=SeqFrame.list[
            names(SeqFrame.list)==child.node.name][[child.node.name]]
        return(child.SeqFrame)
    })  
    
    
    
    frames=c(child.frames,parent.frames)
    
    names(frames)=sapply(frames, keyword, "GUID")
    
    # keyword(object, keyword)
    # passing in two variables to sapply
    # sapply(frames, keyword)
    
    ## change name into veggi name, it is GUID is too long to catch fast
    #names(frames)=sapply(frames, keyword, "VEGGI.NAME")
    
    # the plot is ordered by the name, so guid ID makes the order better
    
    fs=as(frames,"flowSet")
    
    
    
    dat=fs
    
    #dat=appspace[active.SeqFrame]
    
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


