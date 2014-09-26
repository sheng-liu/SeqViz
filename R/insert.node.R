
## insert.node

## insert name of the data into an existing or new gtkTreeStore

## pass in name of the data and parent (gtkTreeStore or NULL) insert that data name into parent gtkTreeStore

insert.node=function(node.name,parent,loc="last"){
    # node.name "character"
    # parent "gtkTreeView"
    # loc="last" insert to bottom of the list, else insert to selected row
    
    # set parent.iter
    parent.iter=if (loc=="last") NULL else parent$getSelection()$getSelected()$iter 
    # add new row depend at the iter location
    #iter=parent$getModel()$append(parent.iter)$iter
    
    # allow insert of multiple nodes
    # nodes pass in as vector
   
    for (i in 1:length(node.name)) {
        
        # add new row depend at the iter location
        iter=parent$getModel()$append(parent.iter)$iter  
        
    # insert node.name
        print(node.name[i])
    parent$getModel()$setValue(iter=iter,column=0,value=node.name[i])
    #parent$getModel()$iterNext(iter$iter)
    
    }
    
    return(parent)
 
}


# > parent.iter1=ifelse(F,NULL,view$getSelection()$getSelected()$iter)
# Error in rep(no, length.out = length(ans)) : 
#   attempt to replicate an object of type 'externalptr'
# > x=ifelse(2==3,2,3)
# > x
# [1] 3



## alow insert of multiple child nodes
# test=c("test1","test2")
# data.frame(test)
# 
# insert.node(node.name=c("test1","test2"),parent=appspace[active.view],loc="insert")
# insert.node(node.name=test,parent=appspace[active.view],loc="insert")
# 
# 
# len=length(test)
# 
# for (i in 1:len) insert.node(node.name=test[i],parent=appspace[active.view],loc="insert")
