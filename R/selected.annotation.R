
## select row, output its annotation GRanges



# get selected.node.name
# selection=tree.view$getSelection()
# iter=gtkTreeSelectionGetSelected(selection)
# model=tree.view$getModel()
# selected.node.name=model$get(iter=iter$iter,column=0)[[1]]
# cat("selected.node.name is: ",selected.node.name,"\n")


selected.annotation=function(tree.view,path,col,browser=T){
    
    # get the selected node's id
    # selected.node(appspace[active.table.view],setActive=F) # also work
    # selected.node(tree.view,setActive=F)
   
    selection=tree.view$getSelection()
    iter=gtkTreeSelectionGetSelected(selection)
    model=tree.view$getModel()
    
    # id column is the last column although it shows up in the first
    # use this to find id column # length=dim(model)[2]
    # would be nice if one can find id column by its name
    cln=colnames(model)
    id.col=which(cln=="id")-1
    id=model$getValue(iter=iter$iter,column=id.col)$value
    #get(iter=iter$iter,column=id.col)[[1]]
    
    anno=annotation(appspace[active.SeqFrame])
    
    selected.anno=anno[which(anno$id==id),]
    cat("selected annotation\n")
    print(selected.anno)

    
    #browserView(session,
    #        GRangesForUCSCGenome("mm10", "chr2", IRanges(20000, 50000)))
    if(browser==T){
        session <- browserSession()
        browserView(session,
                    GRangesForUCSCGenome(genome="mm10",
                                         chrom=selected.anno$chr, 
                                         ranges=IRanges(selected.anno$start,
                                                        selected.anno$end)))
    }
    
    
}