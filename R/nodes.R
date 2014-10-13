## node operations

##------------------------------------------------------------------------------
## insert.node

## insert node.name into a tree.view
## can insert one or multiple at a time
## method "append" insert node(s) to bottom of the list, 
## else insert into selected node
## node.name [character], tree.view [gtkTreeView]
##' @export insert.node
insert.node=function(node.name,tree.view,method=c("insert","append")){
    
    # set parent.iter
    parent.iter=if (method=="append") NULL else 
        tree.view$getSelection()$getSelected()$iter 
    
    # add new row(s), correspond to parent.iter
    for (i in 1:length(node.name)) {
        
        # add new row depend at the iter location
        iter=tree.view$getModel()$append(parent.iter)$iter 
        
        # insert node.name
        print(node.name[i])
        tree.view$getModel()$setValue(iter=iter,column=0,value=node.name[i])
        
        # select on (last) inserted node
        path=tree.view$getModel()$getPath(iter)
        tree.view$setCursor(path,focus.column=NULL)
        
    }
    
    # expend row when insert
    if(!is.null(parent.iter)){
        path.parent=tree.view$getModel()$getPath(parent.iter)
        tree.view$expandRow(path.parent,open.all=T)
    }    
    
    # output a signal
    cat(method," node(s) ",node.name," to ",deparse(substitute(tree.view)),"\n")
    return(tree.view)
    
}

# test
# insert.node(node.name=c("abc","bcd"),tree.view=appspace[active.view],method="append")
# insert.node(node.name=c("abc","bcd"),tree.view=appspace[active.view],method="insert")

#parent$getModel()$iterNext(iter$iter) 

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## selected.node
## return selected node name, and 
## change the active.SeqFrame to selected node
## try this function interactively with the GUI, quite fun
##' @export selected.node
selected.node=function(tree.view){
    
    # get selected.node.name
    selection=tree.view$getSelection()
    iter=gtkTreeSelectionGetSelected(selection)
    model=tree.view$getModel()
    selected.node.name=model$get(iter=iter$iter,column=0)[[1]]
    cat("selected.node.name is: ",selected.node.name,"\n")
    
    # appspace is viewed as public transaction space for all functions
    # change the active.SeqFrame to selected node
    SeqFrame.list=appspace[SeqFrame.list]
    #active.SeqFrame=SeqFrame.list[[names(SeqFrame.list)==selected.node.name]]
    active.SeqFrame=SeqFrame.list[
        names(SeqFrame.list)==selected.node.name][[selected.node.name]]
    
    appspace[active.SeqFrame]=active.SeqFrame
    
    cat("appspace[active.SeqFrame] set to: ",selected.node.name,"\n")
    return(selected.node.name)
}

# may change the table.view also to this selelcted.node
# put it in another function so it is easy to manipulate, more flexibility
# update.table(df=exprs(appspace[active.SeqFrame]),table.view=appspace[active.table.view])
# this need to be connected to click on the rows of the view via gSignal connect

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## select.parent.node
## output selected.node's parent node
## if no parent, output itself
##' @export select.parent.node
select.parent.node=function(tree.view,itself=F){
    
    # get selected node, same as selected.node() function
    selection=tree.view$getSelection()
    selected_iter=selection$getSelected()
    model=tree.view$getModel()
    selected.node.name=model$get(iter=selected_iter$iter,column=0)[[1]]
    
    # get parent.node, this only goes one layer up
    parent_iter=model$iterParent(child=selected_iter$iter)

    # use a list to store parent and itself
    parent.node.list=list()

    # whether it has parent
    has.parent=parent_iter$retval
    if (has.parent){
        parent.node.list[[1]]=
            model$getValue(iter=parent_iter$iter,column=0)$value
            # model$get(iter=parent_iter$iter,column=0)[[1]] # also work
    }
    # whether to add itself
    if(itself==T){
        parent.node.list$iteself=selected.node.name
    }
    return(parent.node.list) 
}

# You have to be careful with ifelse(); it will return an object with the same length and attributes as the test clause. We can see how this might be a problem via ifelse(FALSE, NA, 1:10), which returns 1, not 1:10. If coef(result)[[1]] is more complex that the test, then your suggestion will fail. It might be syntactically simpler, but it isn't necessarily the right tool here. 


## only select parent node, single node
## if there is, output, if there isn't (root), output nothing
## to make it work first, output itself
## so root will be output twice, both by select.all.Child, and select.parent

## output selected.node's parent node
## if no parent, output itself

# it is always lenthg[1] unless you pass in multiple iter
# model$get(iter=parent_iter$iter,column=0)[[1]]  


## code could be simpler using ifelse, however it is very tricky sometimes doeson't work, this time it works

#     parent.node.name=ifelse(test=parent_iter$retval,
#                             yes=model$get(iter=parent_iter$iter,column=0)[[1]],
#                             no=selected.node.name)

#     if (!identical(parent.node.name,selected.node.name)) 
#         parent.node.name.list=list(parent.node.name,selected.node.name) else
#         parent.node.name.list=list(selected.node.name) # if selected root node

# this will work two, syntax simpler
#     parent.node.name.list=
#         if (!identical(parent.node.name,selected.node.name)) 
#             list(parent.node.name,selected.node.name) else 
#                 list(selected.node.name)



## this time the ifelse has problem again
## parent.node.name.list only have the first value when it is yes
#     parent.node.name.list=ifelse(
#         test=!identical(parent.node.name,selected.node.name),
#         yes=list(parent.node.name,selected.node.name),
#         no=list(selected.node.name)
#         )



## TODO:
## when select parent nodes, output all child nodes if has any
## if no child, only ouput itself
## this should be the selct child


##------------------------------------------------------------------------------
## return all child node of selected node

# detect if it has child node
# if has, output all child node names
# if selected node no child, return NULL
# itself=T, return itself in the list

# gtkTreeModelIterHasChild
# gtkTreeModelIterNChildren(object, iter = NULL)

# this only goes one layer down
##' @export select.all.child.node
select.all.child.node=function(tree.view,itself=T){
    
    # get selected node, same as selected.node() function
    model=tree.view$getModel()
    selection=tree.view$getSelection()
    selected_iter=selection$getSelected()
    selected.node.name=model$get(iter=selected_iter$iter,column=0)[[1]]
    
    # use a list to collect child node names
    child.node.list=list()
    # when selected.node have child
    has.child=model$iterHasChild(iter=selected_iter$iter)
    if (has.child){
        n=model$iterNChildren(selected_iter$iter)    # total number of child
        for (i in 1:n){
            # get Nth child's iter
            child_iter=model$iterNthChild(parent=selected_iter$iter,i-1)$iter
            child.node.name=model$getValue(iter=child_iter,column=0)$value
            # model$get(iter=child_iter,column=0)[[1]] # works too
            child.node.list[i]=child.node.name
        }
    }
    ## whether to add itself
    if(itself==T) child.node.list$itself=selected.node.name
    
    return(child.node.list)      
}

# child_iter=model$iterNthChild(parent=selected_iter$iter,3)$iter
# model$get(iter=child_iter,column=0)[[1]]  # a list
# model$getValue(iter=child_iter,column=0)$value  # a character


##------------------------------------------------------------------------------

## delete.node
## using key press event to delete a gtkTeeView node

## connects gtkTreeStore

## gtkTreeStoreRemove(object, iter)
# view is gtkTreeView
# event is key-press-event
##' @export delete.node
delete.node=function(view,event){
    key <- event[["keyval"]]
    if (key == GDK_Delete){
        
        print(GDK_Delete)
        # get selected iter
        selection=view$getSelection()
        iter=gtkTreeSelectionGetSelected(selection)
        
        model=view$getModel()
        
        # a helper for knowing the function
        selected.node.name=model$get(iter=iter$iter,column=0)[[1]]
        
        # remove the corresponding nodes in the appspace
        #rm(list=selected.node.name,envir=.AppEnv)
        #cat(selected.node.name,"deleted\n")
        
        # remove the corresponding nodes from SeqFrame.list
        appspace[SeqFrame.list][selected.node.name]=NULL 
        
        
        # redo function can work somewhere here
#         if (undo==T){
#             insert.node()
#             insert.node=function(node.name=selected.node.name,parent=appspace[active.view],loc="insert")
#         }
        
        
        # remove row
        gtkTreeStoreRemove(object=model,iter=iter$iter)
    }
    
}


# > x
# [1] "Demo.csv (H3K9me3-H3K27me3-)"
# # none of these works
# rm(get(x,envir=.AppEnv),envir=.AppEnv)
# get(x,envir=.AppEnv)
# rm(x,envir=.AppEnv)
# rm(basename(x),envir=.AppEnv)
# rm(as.symbol(x),envir=.AppEnv)
# 
# # This does the work
# rm(list=x,envir=.AppEnv)



# be careful with ifelse
# > parent.iter1=ifelse(F,NULL,view$getSelection()$getSelected()$iter)
# Error in rep(no, length.out = length(ans)) : 
#   attempt to replicate an object of type 'externalptr'
# > x=ifelse(2==3,2,3)
# > x
# [1] 3





##------------------------------------------------------------------------------

select.ajacent.node=function(tree.view,up=T){
    # two collector
    parent.node.name=character(0)
    child.node.name=character(0)
    
    # get selected node
    # same as selected.node() function
    selection=tree.view$getSelection()
    selected_iter=selection$getSelected()
    model=tree.view$getModel()
    selected.node.name=model$get(iter=selected_iter$iter,column=0)[[1]]
    
    # get parent.node
    parent_iter=model$iterParent(child=selected_iter$iter)
    #print(parent_iter)
    
    # when there is parent node parent_iter$retval==T
    if (parent_iter$retval==T){
        parent.node.name=model$get(iter=parent_iter$iter,column=0)[[1]]
    }
    
    # get child.node
    child_iter=model$iterChildren(parent=selected_iter$iter)
    #print(child_iter)
    
    # when there is child node parent_iter$retval==T
    if (child_iter$retval==T){
        child.node.name=model$get(iter=child_iter$iter,column=0)[[1]]
    }
    
    if (up==T&(length(parent.node.name)>0)) 
        return(parent.node.name) 
    else if (length(child.node.name)>0) 
        return (child.node.name) 
    else print("No ajacent node find")
    
}


# determine active.SeqFrame with selection by the user
# once the mouse clicked on, the active.view and active.model has to be reset to the selected one
# return selected node name


##########################
#appspace[active.model]=gtkTreeViewGetModel(selection)

## there is only one tree view, but there is multiple dataset SeqFrame serve as node to insert and for plot
## tree view is only for display and interaction with user

## there is multiple view, each action creates one or more views, which has its own data store model

## when add nodes, is the new view automatically created, or has to set every time an action applies to a view

# I think it has to be set again


## determine the active seqSet

## holds some variable/parameters/container in appspace when started, change the state of the parameters when actions are taken

## activeSeqSet

#active.SeqFrame=appspace()

#appspace[activeSeqFrame]=NULL


## just set the selected one to be active, no need for multiple workflow, people can open multiple windows to do that, each window, one workflow


# try change the model of the view when one clicked on the subnodes
# always change model to the corresponding selected nodes name 


#active.SeqFrame=



{
    # gtkTreeModel for the combo boxes
    
    #channelsModel=rGtkDataFrame(x=)
    
    #     channelsXcombo=
    #     channelsYcombo=
    #     
    #     
    #     channels <- colnames(active.SeqFrame())
    #     
    #     
    #     
    #     
    #     
    #     
    #     if(channelsDialog$run()==GtkResponseType["ok"]) print("chanel selected") 
    # channels$destroy()
}
# 
#x=PlotPage.data.view$getSelection()
#gtkTreeSelectionGetSelected(x)

## everywhere need a function to referen the selected name to underneath SeqFrame
# return selected node name
# selection=appspace[active.view]$getSelection()
# iter=gtkTreeSelectionGetSelected(selection)
# model=gtkTreeViewGetModel(appspace[active.view])
# selected.node.name=model$get(iter=iter$iter,column=0)[[1]]

