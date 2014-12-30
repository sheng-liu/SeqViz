## populate.data
## populate data in a gtkTreeView

## double click view item, update table



#update.table.view=function(action, window){

# Error in (function (action, window)  : 
#               unused argument (<pointer: 0x7ffd21e0a180>)

# tree.view row-activated passes in three pointer, gtkTreeView, gtkTreePath, and GtkTreeViewColmn, plus a gpointer to userdata


##' @export update.table.view
update.table.view=function(tree.view,path,col){
    #selected.node(appspace[SeqFrame.hierachy.view])
    selected.node(tree.view)
    #print(col)
    
    df=data.frame(exprs(appspace[active.SeqFrame]))
    
    # one may solely use three variable passed in to get the selcted node 
    # and retrieve data
    # use tree.view to retrive model   model=gtkGetModel(tree.view)
    # use path to row.number  
    # use row.number to retriew the selected node name
    # use selected.node.name to retrive data
    
    table.view=appspace[SeqFrame.table.view]
    update.table(df,table.view)
}

update.table=function(df,table.view){
        
        # remove all columns
        lapply(gtkTreeViewGetColumns(table.view),function(x){
            gtkTreeViewRemoveColumn(table.view,x)})
        
        gtkDF=rGtkDataFrame(df)
        table.view$setModel(gtkDF) 
        
        # populate
        mapply(table.view$insertColumnWithAttributes,
               position=0,   ## -1 append, 0, 1 fill
               title=colnames(gtkDF),
               cell=list(gtkCellRendererText()),text=seq_len(ncol(gtkDF))-1)

    return(table.view)
}


# populate.data=function(df,parent.data.view){
#     gtkDF=rGtkDataFrame(df)
#     #appspace[PlotPage.info.model]=gtkDF
#     #appspace[active.model]=gtkDF
#     
#     #parent.data.view=gtkTreeView(appspace[PlotPage.info.model])
#     
#     parent.data.view=gtkTreeView(gtkDF)
#     
#     #parent.data.column=gtkTreeViewColumn()
#     #cell_renderer<-gtkCellRendererText()
#     #parent.data.column$packStart(cell_renderer)
#     #parent.data.column$addAttribute(cell_renderer,"text",0)
#     
#     mapply(parent.data.view$insertColumnWithAttributes,
#            position=-1,
#            title=colnames(gtkDF),
#            cell=list(gtkCellRendererText()),text=seq_len(ncol(gtkDF))-1
#     )
#     
#     return(parent.data.view)
#     
# }


# 
# ## TODO replaceMethod=c(,) test
# 
# 
# update.table=function(df,table.view,populate=T){
#     # two ways of updating table.view
#     # replaceModel seems work better
#     
#     # populate==T, it will replace the whole and populate it (i.e. column number changed to the df)
#     ## if false, it will not populate, only replace 
#     
#     
#     # replace model to update the table.view
#     gtkDF=rGtkDataFrame(df)
#     table.view$setModel(gtkDF)
#     
#     
#     # replace data.frame to update the table.view
#     # works when table.view has Model/data.frame
#     # gtkDF=table.view$getModel()
#     # gtkDF$setFrame(frame=df)
#     
#     if(populate==T){
#         # reset table.view to avoid append
#         #table.view=gtkTreeView()
#         #table.view$setModel(gtkDF)
#         
#         # populate if start of as empty
#         mapply(table.view$insertColumnWithAttributes,
#                position=0,   ## -1 append, 0, 1 fill
#                title=colnames(gtkDF),
#                cell=list(gtkCellRendererText()),text=seq_len(ncol(gtkDF))-1
#         )
#         
#     }
#     return(table.view)
# }
# 
# ## this function is very cool, it is connection of outside with inside, just like insert.node
# ## all these is enabled by only one connection
# ## share the view
# #appspace[SeqFrame.hierachy.view]=SeqFrame.hierachy.view
# #appspace[SeqFrame.table.view]=SeqFrame.table.view
# 
# ## somehow, these wiget SeqFrame.hierachy.view, SeqFrame.table.view is constantly updating the views in the program SeqFrame.hierachy.view and SeqFrame.table.view. the "=" sign behaves different as normal variable here
# 
# ## one shold always populate, no matter there is column or not,
# ## basically is making a new one and fit it in the window
# update.table=function(df,table.view,append=F){
#     # two ways of updating table.view
#     # replaceModel seems work better
#     
#     # populate==T, it will replace the whole and populate it (i.e. column number changed to the df)
#     ## if false, it will not populate, only replace 
#     
#     if(append==F){
#         
#         # reset table.view to avoid append
#         #table.view=gtkTreeView()
#         # this create a new gtkTreeView which has nothing to do with the current one wants to modify
#         
#         # remove all columns
#         lapply(gtkTreeViewGetColumns(table.view),function(x){
#             gtkTreeViewRemoveColumn(table.view,x)})
#         
#         gtkDF=rGtkDataFrame(df)
#         table.view$setModel(gtkDF) 
#         
#         # populate
#         mapply(table.view$insertColumnWithAttributes,
#                position=0,   ## -1 append, 0, 1 fill
#                title=colnames(gtkDF),
#                cell=list(gtkCellRendererText()),text=seq_len(ncol(gtkDF))-1
#         )
#         
#     }else{
#         gtkDF=rGtkDataFrame(df)
#         #table.view$setModel(gtkDF) 
#         mapply(table.view$insertColumnWithAttributes,
#                position=-1,   ## -1 append, 0, 1 fill
#                title=colnames(gtkDF),
#                cell=list(gtkCellRendererText()),text=seq_len(ncol(gtkDF))-1)
#         
#     }
#     
#     return(table.view)
# }
# 
# # above append is not working out, if setModel, it replace, all, you only want to append without replace if append is T
# 
# # for now not deal with append simply replace
# 














# 
# 
# 
# 
# columns=gtkTreeViewGetColumns(view)
# 
# 
# 
# view$removeColumn(view$getColumns)
# 
# 
# # remove all columns
# 
# lapply(gtkTreeViewGetColumns(view),function(x){
#     gtkTreeViewRemoveColumn(view,x)})
# 



## populate SeqFrame.table.view
## change model does not change anything, until using mapply populated the view
## so the operation is on view, and it is after fill in the column it will take effect


#         appspace[PlotPage.table.model]=rGtkDataFrame(exprs(sf))
#         appspace[SeqFrame.table.view]<-gtkTreeView(appspace[PlotPage.table.model])
#         
#         mapply(appspace[SeqFrame.table.view]$insertColumnWithAttributes,
#                position=-1,
#                title=colnames(appspace[PlotPage.table.model]),
#                cell=list(gtkCellRendererText()),text=seq_len(ncol(appspace[PlotPage.table.model]))-1
#         )
# no way to change it through view, can display new easily        

## populate SeqFrame.table.view
## simply change its underlining data frame through rGtkDataFrameSetFrame

# SeqFrame.table.view$setFrame(rGtkDataFrame(exprs(sf)))

# replace frame
#rGtkDataFrameSetFrame(appspace[PlotPage.table.model],frame=exprs(sf))
#rGtkDataFrameSetFrame(PlotPage.table.model,frame=exprs(sf))

# replace model
#     df=exprs(sf)
#     gtkDF=rGtkDataFrame(df)
#SeqFrame.table.view$setModel(gtkDF)
#     appspace[SeqFrame.table.view]$setModel(gtkDF)
#     appspace[SeqFrame.table.view]$setModel(gtkDF)
