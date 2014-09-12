


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

select.channels=function(window){
    channelsDialog=gtkMessageDialog(
        parent=window,
        flags="destroy-with-parent",
        type="info",
        buttons="ok",
        label="Select channels")
    
    # active.seqFrame=appspace[sf]
    # determine active.seqFrame with selection by the user

    
    
    
    selected.node=selected.node.name(appspace[active.view])
    active.seqFrame=get(selected.node,envir=.AppEnv)
    appspace[active.seqFrame]=active.seqFrame
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
    
    hbox$packStart(child=Y.label)
    hbox$packStart(child=channelYcombo)
    
    channelsDialog[["vbox"]]$add(hbox)
    
    
    if(channelsDialog$run()==GtkResponseType["ok"]){
    appspace[channelX]=channelXcombo$getActiveText()
    appspace[channelY]=channelYcombo$getActiveText()
    print(appspace[channelX])
    print(appspace[channelY])
    channelsDialog$destroy()
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


selected.node.name=function(tree.view){
    selection=tree.view$getSelection()
    iter=gtkTreeSelectionGetSelected(selection)
    model=tree.view$getModel()
    #model=appspace[active.model]
    #model=getModel(appspace[active.view])
    selected.node.name=model$get(iter=iter$iter,column=0)[[1]]
    return(selected.node.name)
}

# try this function interactively with the GUI, quite fun





