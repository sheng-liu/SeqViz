## drag and drop

## between annotation and data files

## drag call backs

drag <- function(widget, context, sel, info, time, data){    
    print("drag source")
    # get the name from the widget
    name=selected.node(widget,setActive=F)
    # print(name)
    # set the name to middle data transfer widget sel [gtkSelectionData]
    sel$setText(name)
}   

## drop call back

drop <- function(widget, context, x, y, sel, info, time){
    print("drop-target")
    
    row.text=sel$getText()
    dragged.node.name=rawToChar(row.text)
    print(dragged.node.name)
    
    dest.row=gtkTreeViewGetDestRowAtPos(widget, drag.x=x, drag.y=y)
    
    model=widget$getModel()
    iter=model$getIter(dest.row$path)
    dropped.node.name=model$get(iter=iter$iter,column=0)[[1]]
    print(dropped.node.name)
    
    #     cat("gtkTreePathGetIndices\n")
    #     print(gtkTreePathGetIndices(dest.row$path))
    #     
    #     cat("gtkTreePathGetDepth\n")
    #     print(gtkTreePathGetDepth(dest.row$path))
    
    dnd=list(dragged.node.name=dragged.node.name,
             dropped.node.name=dropped.node.name)
    print(dnd)
    return(dnd)  
} 


drop.and.count=function(widget, context, x, y, sel, info, time){
    node.name=drop(widget, context, x, y, sel, info, time)
    
    count.method=selected.method()
    #count.method=appspace[count.method]
    
    ## retrieve bamFile from dropped.node.name
    # it needs to be a bamFile.list in appspace
    # get its full name from its basename
    bamFile.basename=node.name$dropped.node.name
    bamFile=appspace[bamFile.list][[bamFile.basename]]
    
    
    ## retrieve annotation from dragged.node.name
    annotationFile.basename=node.name$dragged.node.name
    ## same here, needs annotationFile.list in appspace
    annotationFile=appspace[annotationFile.list][[annotationFile.basename]]
    
    
    sd=SeqData(bamFile=bamFile,annotationFile=annotationFile)
    
    switch(count.method,
           "read counts"={sd=countReads(sd)},
           "read coverage"={sd=viewCoverage(sd)},
           "base percentage"={
               baseReport=bamFile
               annotationFile=annotationFile
               sd=summarizeBases(baseReport=bamFile,
                                 annotationFile=annotationFile)})
    return(sd)
}





