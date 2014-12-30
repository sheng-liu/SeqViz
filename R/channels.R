

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

##' @export select.channels
select.channels=function(channelY=T,channelZ=F){
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
    
    
    # determine active.SeqFrame with selection by the user
    selected.node(appspace[SeqFrame.hierachy.view])
    active.SeqFrame=appspace[active.SeqFrame]
    
    #     selected.node=selected.node.name(appspace[SeqFrame.hierachy.view])
    #     active.SeqFrame=get(selected.node,envir=.AppEnv)
    #     appspace[active.SeqFrame]=active.SeqFrame
    
    # active.SeqFrame=appspace[as.symbol(selected.node.name)]
    # as.name  
    # Error in x[seq_len(n)] : object of type 'symbol' is not subsettable
    
    
    
    channelXcombo=gtkComboBoxNewText()
    sapply(colnames(active.SeqFrame), channelXcombo$appendText)
    #channelXcombo$appendText(text=colnames(active.SeqFrame()))  
    
    
    channelYcombo=gtkComboBoxNewText()
    sapply(colnames(active.SeqFrame), channelYcombo$appendText)
    #channelYcombo$appendText(text=colnames(active.SeqFrame()))    
    
    channelZcombo=gtkComboBoxNewText()
    sapply(colnames(active.SeqFrame), channelZcombo$appendText)
    
    X.label=gtkLabel(str="X:")
    Y.label=gtkLabel(str="Y:")
    Z.label=gtkLabel(str="color:")
    
    # put two gtkLabel and two combo into dialog's vbox
    # now it is three
    
    hbox=gtkHBox()
    hbox$packStart(child=X.label)
    hbox$packStart(child=channelXcombo)
    
    if (channelY==T){
        hbox$packStart(child=Y.label)
        hbox$packStart(child=channelYcombo)
    }
    
    if (channelZ==T){
        hbox$packStart(child=Z.label)
        hbox$packStart(child=channelZcombo)
    }
    
    channelsDialog[["vbox"]]$add(hbox)
    
    # assuming z is needed when Y is always used
    if(channelY==T){
        if(channelsDialog$run()==GtkResponseType["ok"]){
            appspace[channelX]=channelXcombo$getActiveText()
            appspace[channelY]=channelYcombo$getActiveText()
            # print(appspace[channelX])
            # print(appspace[channelY])
            cat("selected channelX",appspace[channelX],"\n")
            cat("selected channelY",appspace[channelY],"\n")
            if(channelZ==T){
                appspace[channelZ]=channelZcombo$getActiveText()
                cat("selected channelZ",appspace[channelZ],"\n")
            }
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
    
#     if(channelZ==T){
#         if(channelsDialog$run()==GtkResponseType["ok"]){
#             appspace[channelX]=channelXcombo$getActiveText()
#             appspace[channelY]=channelYcombo$getActiveText()
#             appspace[channelZ]=channelYcombo$getActiveText()
#             # print(appspace[channelX])
#             # print(appspace[channelY])
#             cat("selected channelX",appspace[channelX],"\n")
#             cat("selected channelY",appspace[channelY],"\n")
#             cat("selected channelZ",appspace[channelZ],"\n")
#             #channelsDialog$destroy()
#         }
#     }else{
#         if(channelsDialog$run()==GtkResponseType["ok"]){
#             appspace[channelX]=channelXcombo$getActiveText()
#             appspace[channelY]=channelYcombo$getActiveText()
#             # print(appspace[channelX])
#             # print(appspace[channelY])
#             cat("selected channelX",appspace[channelX],"\n")
#             cat("selected channelY",appspace[channelY],"\n")
#             
#             #channelsDialog$destroy()
#         }
#     }
    
    #channelsDialog$destroy()  
}




## check.channels, can choose multiple selecton or single
## this program is cool, simple and powerful, try it out, it is fun
##' @export check.channels
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
    
    # create checkButtons according to the channels of the SeqFrame
    # need to name the variable differently to access them
    active.SeqFrame=appspace[active.SeqFrame]
    
    #channels.list=list()
    channels.list=vector("list",length(colnames(active.SeqFrame)))
    #do.call(lapply(channels.list),)
    names(channels.list)=colnames(active.SeqFrame)
    
    # multiple==T, use checkButton
    if (multiple==T){
        for (i in 1:length(colnames(active.SeqFrame))) {
            channels.list[[i]]=gtkCheckButton(label=colnames(active.SeqFrame)[i])
        }
        
    }else{ 
        # multiple==F, use radio button
        for (i in 1:length(colnames(active.SeqFrame))) {
            channels.list[[i]]=gtkRadioButton(label=colnames(active.SeqFrame)[i])
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
    sapply(seq_along(colnames(active.SeqFrame)),
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