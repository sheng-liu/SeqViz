## density.plot
## aka histogram

## use checkbox for multiple channel in one plot
## realize matt's gatting statistics on all channel

density.plot=function(action, window){
    library(lattice)
    #Error in plot(eval(parse(text = f))) : 
    #  error in evaluating the argument 'x' in selecting a method for function 'plot': Error in .densityplot.flowSet(x, data, ...) : 
    #  could not find function "lattice.getOption"
    # put it anywhere in namespace won't work
    
    print("densityplot")
    
    ## form data
    # lets first implement the flowViz version, as it is easy to add up checked channels
    # make it easier for now, always plot the parent, latter can add a button to switch
    # make the flowSet
    model=appspace[active.view]$getModel()
    
    #     # select node's parent node and form a flowSet
    #     selected.node=selected.node(appspace[active.view])
    #     parent.node=select.ajacent.node(appspace[active.view],up=T)
    #     
    #     # data=list(appspace[selected.node],appspace[parent.node])
    #     child=get(x=selected.node,envir=.AppEnv)
    #     parent=get(x=parent.node,envir=.AppEnv)
    #     
    #     frames=list(child,parent)
    #     names(frames)=sapply(frames, keyword, "GUID")
    #     fs=as(frames,"flowSet")
    
    
    ## try use select.all.child.node to construct fs
    selected.node=selected.node(appspace[active.view])
    all.child.node=select.all.child.node(appspace[active.view])
    parent.node=select.parent.node(appspace[active.view])
    
    # data=list(appspace[selected.node],appspace[parent.node])
    #parent.frame=get(x=parent.node,envir=.AppEnv)
    
#     parent.frames=lapply(parent.node,function(node.name){
#         get(x=node.name,envir=.AppEnv)
#     })
#     
#     child.frames=lapply(all.child.node,function(node.name){
#         get(x=node.name,envir=.AppEnv)
#     })
#     
    
    parent.frames=lapply(parent.node,function(parent.node.name){
        SeqFrame.list=appspace[SeqFrame.list]
        parent.SeqFrame=SeqFrame.list[
            names(SeqFrame.list)==parent.node.name][[parent.node.name]]
        return(parent.SeqFrame)
    })
    
    child.frames=lapply(all.child.node,function(child.node.name){
        SeqFrame.list=appspace[SeqFrame.list]
        child.SeqFrame=SeqFrame.list[
            names(SeqFrame.list)==child.node.name][[child.node.name]]
        return(child.SeqFrame)
    })  
    
    frames=c(child.frames,parent.frames)
    
    #frames=child.frames
    #frames$parent=parent.frame
    
    names(frames)=sapply(frames, keyword, "GUID")
    #names(frames)=sapply(frames, keyword, "VEGGI.NAME")
    
    fs=as(frames,"flowSet")
    
    
    
    # checkbox for selection of channels
    # defaul select all
    check.channels()
    
    
    # plot    
    checked.channels.name=appspace[checked.channels]
    channels=paste(checked.channels.name,collapse="`+`")
    
    #selected.node(appspace[active.view])
    
    # dat=appspace[active.SeqFrame]
    dat=fs
    
    f=sprintf("densityplot(~`%s`,data =dat,margin=T)",channels)
    parse(text = f)
    
    ## make a windows version and mac version
    ## on mac , always use quartz, windows use windows
    ## use x11 only when there is no other choice, such as getGraphicEvents
    ## options(device="quartz") no need to set option, just plot 
    ## quartz(width=6,height=4)
    #options(device="quartz")  ## options makes the code uniform
    dev.new(width=6,height=4)
    # dev.new gives the device according to the operation system
    plot(eval(parse(text = f)))
    
    
}


# densityplot(~`Male.Het.H3K9me3`,data =fs)
# densityplot(~`Male.Het.H3K9me3`,data =dat,filter=curv1Filter("Male.Het.H3K9me3"))



#     this formula also work, 

#     f=sprintf("~`%s`",channels)
#     fm=as.formula(formula)
#     densityplot(x=fm,data=dat,margin=T)


