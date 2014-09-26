## polygon.gate


polygon.gate=function(action,window) {    

    
    print("Polygon gate")
    
    select.channels()
  
    
    ##################
    # try change the model of the view when one clicked on the subnodes
    # always change model to the corresponding selected nodes name 
    # this means the base/root node needs to be assoicated with its node name too, instead of csv file name, or make the file name the model name # did this
    # now catch the variable with the selected name
    
    
    
    #              selection=appspace[active.view]$getSelection()
    #             # not really need active.view, there is only one view
    #             
    #              iter=gtkTreeSelectionGetSelected(selection)
    #         
    #              model=gtkTreeViewGetModel(appspace[active.view])
    #              
    #         
    #              
    #              selected.node.name=model$get(iter=iter$iter,column=0)[[1]]
    #mode=gtkTreeModel
    
    #          print(selected.node.name)
    
    ## which to plot, ie. the active.seqFrame is determined by select.channels
    
    
    
    ## no, you only need to change the sf for the plot, sf is not event he model
    
    ## always plot the active flowFrame, there is nothing to do with model and view, which is only to get names showing which to  plot
    
    #active.seqFrame=## selected node name
    #appspace[active.seqFrame]=selected.node.name
    
    # appspace[active.seqFrame]=get(x=selected.node.name,envir=.AppEnv)
    
    
    #     assign(x=selected.node.name,value=model,envir=.AppEnv)
    #     
    #     appspace[active.seqFrame]=appspace[selected.node.name]
    
    
    # connect the selected.node.name to the underneath seqFrame
    # use their name
    
    
    ##########################
    #appspace[active.model]=gtkTreeViewGetModel(selection)
    
    ## there is only one tree view, but there is multiple dataset seqFrame serve as node to insert and for plot
    ## tree view is only for display and interaction with user
    
    ## there is multiple view, each action creates one or more views, which has its own data store model
    
    ## when add nodes, is the new view automatically created, or has to set every time an action applies to a view
    
    # I think it has to be set again
    
    
    #flowPlot(x=appspace[sf],plotParameters=c(appspace[channelX],appspace[channelY]))
    options(device = "quartz")
    dev.new()
    quartz()
    flowPlot(x=appspace[active.seqFrame],plotParameters=c(appspace[channelX],appspace[channelY]))
    
    # gate
    location <- locator(n =512, type = "o")
    location.df=data.frame(location$x,location$y)
    
    colnames(location.df)[1]=appspace[channelX]
    colnames(location.df)[2]=appspace[channelY]
    
    polygonGate=polygonGate(.gate=location.df,filterId="polygonGate")
    
    #polygonGate.filter=filter(appspace[sf],polygonGate)
    polygonGate.filter=filter(appspace[active.seqFrame],polygonGate)
    
    appspace[filterBox]=polygonGate.filter
    
    print(summary(polygonGate.filter))
    
    #appspace[polygonGate.subset]=Subset(appspace[sf],polygonGate)
    
    appspace[polygonGate.split]=split(appspace[active.seqFrame],polygonGate)
    # add new data as nodes to the exiting parent node
    
    #child.node.name=paste(veggie(),names(appspace[polygonGate.split]),sep="_")
    
    ## for now, use GUID is good for plotting with sequence
    
    
    #x=sapply(appspace[polygonGate.split],keyword,"GUID")
    child.node.name=sapply(appspace[polygonGate.split],function(frames){
      keyword(frames)$GUID  
    })
    
    # print(child.node.name)
    # try change the model of the view when one clicked on the subnodes
    # always change model to the corresponding selected nodes name 
    
    
    ## add multiple child nodes support
    
    # child.node.name=paste(value,"polygonGate.subset",sep="_")
    #assign(child.node.name,value=appspace[polygonGate.subset],envir=.AppEnv)
    # make the single output results into a list then this will also work for single output gate
    
    
    ## add one keyword/description as fruit.name serve as a nick name to fast catch the gate in the plotting
    
  
    
    # assign different values to each list element
    
#     values=child.node.name
#     lists=appspace[polygonGate.split]
#     for (i in 1:length(values)) keyword(lists[[i]])$VEGGI.NAME=values[i]
         
    for (i in 1:length(child.node.name)) keyword(appspace[polygonGate.split][[i]])$VEGGI.NAME=child.node.name[i]
    
    
    
  
    
    # the R way for repeated operation
    # could have use for loop instead
    sapply(seq_along(child.node.name),function(i){
        assign(child.node.name[i],value=appspace[polygonGate.split][[i]],envir=.AppEnv) })
    
    insert.node(node.name=child.node.name,parent=appspace[active.view],loc="insert")
    # this parent view may need to be dynamic if support multiple view
    
    # return selected node name
    #     selection=appspace[active.view]$getSelection()
    #     iter=gtkTreeSelectionGetSelected(selection)
    #     model=gtkTreeViewGetModel(appspace[active.view])
    #     selected.node.name=model$get(iter=iter$iter,column=0)[[1]]
    
    
    # save only the active seqFrame
    #appspace[save_csv]=appspace[selected.node.name]
    
    #print(selected.node.name)
    
    ## adjust save_csv to be dynamic to selected.node
    selected.node=selected.node(appspace[active.view])
    appspace[save_csv]=get(selected.node,envir=.AppEnv)
    
    #appspace[save_csv]=appspace[child.node.name]
    

}


##TODO
## make the tree expand
