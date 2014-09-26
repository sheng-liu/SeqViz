


## selectChannel
##------------------------------------------------------------------------------

# function 
# when clicked the button, pop up chanel selection menu for user to choose which channel and combinations they want to plot

# can have a default value as in FlowJo, user can change to the one they want on the plot

# first implement the selction-subsetting-plot theme

## in the drawing area, add a combo box for user to select the chanel they want, this is the flowjo style. which is easier for user, saved a lot of clicking

## gtkMessageDialog does this most simple way
## 

## beter than flowjo, allows for a single sample, multiple graphs with different channels
## flowjo, one sample, one graph, change channle within that graph
## for complicated issues, and in total two samples, allow more detailed comparison to find pattern




# selectChannel
# subsetData
# plotData

#selectChannel=function(action,window){
# Biobase have a function called "selctChannles", so avoid been masked

#select.channels=function(window){

select.channels=function(channelY=T){
    channelsDialog=gtkMessageDialog(
        #parent=window,
        
        # somehow this cause
        #gSignalConnect(obj=polygon.gate.button, signal="clicked", f=polygon.polygonGate)
        #         Error in checkPtrType(parent, "GtkWindow", nullOk = T) : 
        #             argument "window" is missing, with no default
        
        # this way it doens't need window
        parent=NULL,
        flags="destroy-with-parent",
        type="info",
        buttons="ok",
        label="Select channels")
    
    # active.seqFrame=appspace[sf]
    # determine active.seqFrame with selection by the user
    
    
    selected.node(appspace[active.view])
    active.seqFrame=appspace[active.seqFrame]
    
    #     selected.node=selected.node.name(appspace[active.view])
    #     active.seqFrame=get(selected.node,envir=.AppEnv)
    #     appspace[active.seqFrame]=active.seqFrame
    
    # active.seqFrame=appspace[as.symbol(selected.node.name)]
    # as.name  
    # Error in x[seq_len(n)] : object of type 'symbol' is not subsettable
    
    
    
    channelXcombo=gtkComboBoxNewText()
    sapply(colnames(active.seqFrame), channelXcombo$appendText)
    #channelXcombo$appendText(text=colnames(active.seqFrame()))  
    
    
    channelYcombo=gtkComboBoxNewText()
    sapply(colnames(active.seqFrame), channelYcombo$appendText)
    #channelYcombo$appendText(text=colnames(active.seqFrame()))    
    
    
    X.label=gtkLabel(str="X:")
    Y.label=gtkLabel(str="Y:")
    # put two gtkLabel and two combo into dialog's vbox
    
    hbox=gtkHBox()
    hbox$packStart(child=X.label)
    hbox$packStart(child=channelXcombo)
    
    if (channelY==T){
        hbox$packStart(child=Y.label)
        hbox$packStart(child=channelYcombo)
    }
    channelsDialog[["vbox"]]$add(hbox)
    
    if(channelY==T){
        if(channelsDialog$run()==GtkResponseType["ok"]){
            appspace[channelX]=channelXcombo$getActiveText()
            appspace[channelY]=channelYcombo$getActiveText()
            # print(appspace[channelX])
            # print(appspace[channelY])
            cat("selected channelX",appspace[channelX],"\n")
            cat("selected channelY",appspace[channelY],"\n")
            
            channelsDialog$destroy()
        }
    }else{
        if(channelsDialog$run()==GtkResponseType["ok"]){
            appspace[channelX]=channelXcombo$getActiveText()
            #appspace[channelY]=channelYcombo$getActiveText()
            # print(appspace[channelX])
            # print(appspace[channelY])
            cat("selected channelX",appspace[channelX],"\n")
            #cat("selected channelY",appspace[channelY],"\n")
            
            channelsDialog$destroy()
        }
    }
    
    
    
    
}



## determine the active seqSet

## holds some variable/parameters/container in appspace when started, change the state of the parameters when actions are taken

## activeSeqSet

#active.SeqFrame=appspace()

#appspace[activeSeqFrame]=NULL


## just set the selected one to be active, no need for multiple workflow, people can open multiple windows to do that, each window, one workflow



#active.seqFrame=



{
    # gtkTreeModel for the combo boxes
    
    #channelsModel=rGtkDataFrame(x=)
    
    #     channelsXcombo=
    #     channelsYcombo=
    #     
    #     
    #     channels <- colnames(active.seqFrame())
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

## everywhere need a function to referen the selected name to underneath seqFrame
# return selected node name
# selection=appspace[active.view]$getSelection()
# iter=gtkTreeSelectionGetSelected(selection)
# model=gtkTreeViewGetModel(appspace[active.view])
# selected.node.name=model$get(iter=iter$iter,column=0)[[1]]



# determine active.seqFrame with selection by the user
# once the mouse clicked on, the active.view and active.model has to be reset to the selected one
# return selected node name


# return selected node name, and 
# change the active.seqFrame to selected node
selected.node=function(tree.view){
    selection=tree.view$getSelection()
    iter=gtkTreeSelectionGetSelected(selection)
    model=tree.view$getModel()
    #model=appspace[active.model]
    #model=getModel(appspace[active.view])
    selected.node.name=model$get(iter=iter$iter,column=0)[[1]]
    
    
    # change the active.seqFrame to selected node
    active.seqFrame=get(selected.node.name,envir=.AppEnv)
    appspace[active.seqFrame]=active.seqFrame
    #appspace[active.seqFrame]=appspace[`selected.node.name`]
    
    cat("active.seqFrame set to: ",selected.node.name,"\n")
    return(selected.node.name)
}

# try this function interactively with the GUI, quite fun


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


## output selected.node's parent node
## if no parent, output itself

select.parent.node=function(tree.view,itself=F){
    
    # get selected node
    # same as selected.node() function
    selection=tree.view$getSelection()
    selected_iter=selection$getSelected()
    model=tree.view$getModel()
    selected.node.name=model$get(iter=selected_iter$iter,column=0)[[1]]
    
    # can be simplified, no need selected_iter
    # selected.node.name=selected.node(tree.view)
    
    # get parent.node
    parent_iter=model$iterParent(child=selected_iter$iter)
    #print(parent_iter)
    
    #     parent.node.name.list=list()
    # when there is parent node parent_iter$retval==T
    #         if (parent_iter$retval==T) {
    #             parent.node.name$parent=model$get(iter=parent_iter$iter,column=0)[[1]]
    #             parent.node.name$itself=selected.node.name
    #         } else parent.node.name=selected.node.name
    
    parent.node.name.list=list()
    if (parent_iter$retval==T) {
        parent.node.name.list[[1]]=model$get(iter=parent_iter$iter,column=0)[[1]]
        if (itself==T) parent.node.name.list$itself=selected.node.name
    }else {
        parent.node.name.list
        if(itself==T) parent.node.name.list$itself=selected.node.name
        
    }
    
    
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
    
    
    return(parent.node.name.list) 
}

# You have to be careful with ifelse(); it will return an object with the same length and attributes as the test clause. We can see how this might be a problem via ifelse(FALSE, NA, 1:10), which returns 1, not 1:10. If coef(result)[[1]] is more complex that the test, then your suggestion will fail. It might be syntactically simpler, but it isn't necessarily the right tool here. 


## only select parent node, single node
## if there is, output, if there isn't (root), output nothing
## to make it work first, output itself
## so root will be output twice, both by select.all.Child, and select.parent

## output selected.node's parent node
## if no parent, output itself







## TODO:
## when select parent nodes, output all child nodes if has any
## if no child, only ouput itself
## this should be the selct child

select.all.child.node=function(tree.view,itself=T){
    # two collector
    parent.node.name=character(0)
    child.node.name=character(0)
    
    # get selected node
    # same as selected.node() function
    selection=tree.view$getSelection()
    selected_iter=selection$getSelected()
    model=tree.view$getModel()
    selected.node.name=model$get(iter=selected_iter$iter,column=0)[[1]]
    
    # detect if it has child node
    # if has, output all child node names
    #gtkTreeModelIterHasChild
    #gtkTreeModelIterNChildren(object, iter = NULL)
    
    #child_iter_name=list()
    if (model$iterHasChild(iter=selected_iter$iter)==T){
        #gtkTreeModelIterHasChild(model,selected_iter$iter)
        n=model$iterNChildren()
        
        all_child_iter=list()
        for (i in seq(from=0,to=n,by=1)){
            #gtkTreeModelIterNthChild(object, parent = NULL, n)
            
            child_iter=model$iterNthChild(parent=selected_iter$iter,i)$iter
            all_child_iter=c(all_child_iter,child_iter)
            #all_child_iter[i]=child_iter
        }
        
        # will this vectorization work?
        ## i=0:n
        #child_iter=model$iterNthChild(parent=selected_iter,i)
        
        
        
        
        child_iter_name=lapply(all_child_iter,function(x)
            model$get(iter=x,column=0)[[1]])
        ## add itself before other nodes, for display
        if(itself==T) child_iter_name$itself=selected.node.name
        
    }else{
        child_iter_name=list(selected.node.name)
    }
    
    return(child_iter_name)    
    
    # or directly run as if no children, iterNthChild will returns false    
    
    
    
}


## check.channels, can choose multiple selecton or single
## this program is cool, simple and powerful, try it out, it is fun
check.channels=function(multiple=T){
    
    ## present the dialog
    checkChannelsDialog=gtkMessageDialog(
        #parent=window,
        # this way it doens't need window
        parent=NULL,
        flags="destroy-with-parent",
        type="info",
        buttons="ok",
        label="Check channels")
    
    # create checkButtons according to the channels of the seqFrame
    # need to name the variable differently to access them
    active.seqFrame=appspace[active.seqFrame]
    
    #channels.list=list()
    channels.list=vector("list",length(colnames(active.seqFrame)))
    #do.call(lapply(channels.list),)
    names(channels.list)=colnames(active.seqFrame)
    
    # multiple==T, use checkButton
    if (multiple==T){
        for (i in 1:length(colnames(active.seqFrame))) {
            channels.list[[i]]=gtkCheckButton(label=colnames(active.seqFrame)[i])
        }
        
    }else{ 
        # multiple==F, use radio button
        for (i in 1:length(colnames(active.seqFrame))) {
            channels.list[[i]]=gtkRadioButton(label=colnames(active.seqFrame)[i])
        }
        
        # put the channels into one group ( the first buttons group)
        # gtkRadioButtonSetGroup(object=channels.list[[2]],group=g) 
        g=gtkRadioButtonGetGroup(channels.list[[1]])

        channels.tobe.grouped=channels.list[-1]
        lapply(channels.tobe.grouped,function(channel){
            gtkRadioButtonSetGroup(object=channel,group=g)
        }) 
    }
    
   
    vbox=gtkVBox()
    
    # could use for loop instead
    sapply(seq_along(colnames(active.seqFrame)),
           function(i){vbox$packStart(child=channels.list[[i]])})
    
    checkChannelsDialog[["vbox"]]$add(vbox)
    
    ## define action
    # the toggle is cool, but not used here
    # instead using property "active"
    if(checkChannelsDialog$run()==GtkResponseType["ok"]){
        
        # if checkButton's "active, return their name to a vector
        check.list=lapply(channels.list,function(x) x["active"])
        #channels.list[[2]]["active"]
        
        
        checked=unlist(check.list)
        checked.channels.name=names(checked)[checked==T]
        
        appspace[checked.channels]=checked.channels.name
        cat("checked channels:",appspace[checked.channels],"\n")
        checkChannelsDialog$destroy()
    }
    
}

#  working code for using toggle
# connect actions
# make channels.name synched to the checked names
# for all checked buttons, return their name to a vecter
#     appspace[checked.channels]=c()
#     sapply(seq_along((channels.list)),function(i){
#         
#         gSignalConnect(channels.list[[i]], "toggled", function(button) {
#             appspace[checked.channels][i]=names(channels.list[i])
#             message("selected  ",appspace[checked.channels][i],"  for densityplot")
#         }) 
#     })

# this is good for toggle
#checked.channels.name=na.omit(appspace[checked.channels])


# list 2 data.frame (matrix)
# x=do.call(cbind, check.list)
# list 2 vectoer
# unlist(check.list)