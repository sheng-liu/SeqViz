
# SeqViz UImanager

################################################################################

## -----------------------------------------------------------------------------
## SeqViz Roxygen help
##' @name SeqViz
##' @aliases SeqViz
##' @title SeqViz-graphical interface for epigenomic data exploration
##' @rdname SeqViz
##' @docType methods
##' @description SeqViz is a graphical interface for visulization of epigenomic
##'   data.
##' @usage
##' SeqViz()
##  @param bamFie Full path to bam file (optional).
##  @param annotationFile Full path to annotation file (optional).
##
##' @return a GTK+ graphical interface for simple epigenomic data computation
##'   and visulization
##' @details SeqViz provide a graphical interface for epigenomic data 
##'  visulization. It is implemented with GTK+ through rGTK+ package, its 
##'  underlying computation is implemented by SeqData package for computations
##'  on bam file and SeqFrame package for interfacing tables to flowCore. SeqViz
##'  then utilize flowCore and flowViz for data subsetting and visulization.
##'  
##'  Organization of the three packages:
##'  
##'  -Calculations from a bam file with SeqData
##'  
##'  -Interface tables to flowCore with SeqFrame
##'  
##'  -Graphical interface for visualization and gating with SeqViz
##'  
##'  Each package has its own help manual, use ?(packageName) to see package 
##'  specific help (e.g. ?SeqData).
##'  
##'  SeqFrame is isolated as an separate package as it is functionally
##'  independent with SeqData for basic computation or SeqViz for pure graphical
##'  interface. It can interface a variaty of table like data into SeqFrame (see
##'  SeqFrame help for details), the graphical interface currently support
##'  tables in csv format.
##'  
##'  For usage of SeqViz for basic functions, see a 15 min video tutorial 
##'  (http://youtu.be/Zsv4LGTgdA0).

##' @seealso See SeqFrame for interfacing more different type of data, and
##' SeqData for more functions that is not implemented in the graphical
##' interface.


##' @import RGtk2
##' @import SeqData
##' @import SeqFrame

##' @import flowCore
##' @import flowViz
##' @import flowStats

library(SeqData)
library(SeqFrame)
library(RGtk2)
library(flowCore)
library(flowViz)
library(Biobase)
library(flowStats)
library(lattice) 
library(ggplot2)


##' @export SeqViz
SeqViz=function(){
    
    ## -------------------------------------------------------------------------
    ## UImanager 
    
    uimanager = gtkUIManager()
    # define main window
    main_window <- gtkWindow(show = FALSE)
    # change title (property)
    main_window["title"] <- "SeqViz"
    # setDefultSize ()
    main_window$setDefaultSize(800, 600)
    
    
    ##==========================================================================
    ## Implementing callbacks (actions)
    ##==========================================================================
    
    
    ## -------------------------------------------------------------------------
    ## file group
    ## Open_Bam
    
    Open_Bam= function(widget, window) {
        dialog<-gtkFileChooserDialog(
            title="Choose a Bam file",
            parent=window,
            action="open",
            # the names of variables are mapped above this line
            button1_label="gtk-cancel", button1_response=GtkResponseType["cancel"], 
            button2_label="gtk-open", button2_response=GtkResponseType["accept"]) 
        
        if(dialog$run()==GtkResponseType["accept"]){
            
            # get file name, no further reading functions, as it is too big
            file=dialog$getFilename()
            data.name=basename(file)
            
            ## put bamFile into bamFile.list
            model=appspace[bamFile.hierachy.view]$getModel()
            length.tree.view=model$iterNChildren()
            if(length.tree.view==0){
                appspace[bamFile.list]=list()
            }
            # set the name of the bamFile.list
            appspace[bamFile.list][data.name]=data.name 
            # fill in the content
            appspace[bamFile.list][[data.name]]=file
            # set active.SeqFrame to inserted bamFile
            cat("active.bamFile set to inserted bamFile ",data.name,"\n")
            appspace[active.bamFile]=file
            
            # insert.node
            insert.node(node.name=data.name,
                        tree.view=bamFile.hierachy.view,
                        method="append")
            
            
        }
        dialog$destroy()
    }    
    
    ## -------------------------------------------------------------------------
    ## Open_Annotation
    Open_Annotation=function(widget, window) {
        dialog<-gtkFileChooserDialog(
            title="Choose a CSV file",
            parent=window,
            action="open",
            # the names of variable are mapped above this line
            button1_label="gtk-cancel", button1_response=GtkResponseType["cancel"], 
            button2_label="gtk-open", button2_response=GtkResponseType["accept"]) 
        
        if(dialog$run()==GtkResponseType["accept"]){
            
            file=dialog$getFilename()
            #appspace[annotationFile]=file
            data.name=basename(file)
            
            ## put annotationFile into annotationFile.list
            model=appspace[annotationFile.hierachy.view]$getModel()
            length.tree.view=model$iterNChildren()
            if(length.tree.view==0){
                appspace[annotationFile.list]=list()
            }
            
            
            # set the name of the annotationFile.list
            appspace[annotationFile.list][data.name]=data.name 
            # fill in the content
            appspace[annotationFile.list][[data.name]]=file
            # set active.SeqFrame to inserted annotationFile
            cat("active.annotationFile set to inserted annotationFile",data.name,"\n")
            appspace[active.annotationFile]=file
            
            # insert.node
            insert.node(node.name=data.name,
                        tree.view=annotationFile.hierachy.view,
                        method="append")
        }
        dialog$destroy()
        
    }
    
    ## -------------------------------------------------------------------------
    ## Open_CSV
    Open_CSV=function(widget, window) {
        dialog=gtkFileChooserDialog(
            title="Choose a CSV file",
            parent=window,
            action="open",
            # button1_label="gtk-cancel", button1_response=GtkResponseType["cancel"], 
            button2_label="gtk-open", button2_response=GtkResponseType["accept"]) 
        
        if(dialog$run()==GtkResponseType["accept"]){
            # get file name
            file=dialog$getFilename()
            # read in and convert file into SeqFrame
            sf=SeqFrame(file=file)
            
            # add an veggi name to sf for easy finding it when plotting
            # veggi name is not used, it is optional for future functions
            data.name=basename(file)
            keyword(sf)$VEGGI.NAME=data.name
            
            # if there is no SeqFrame.list in appspace, create one, else append
            # here using tree.view.length to determine
            # can also determined by existence of SeqFrame.list in appspace
            # exists("SeqFrame.list",envir=.AppEnv)
            
            model=appspace[SeqFrame.hierachy.view]$getModel()
            length.tree.view=model$iterNChildren()
            if(length.tree.view==0){
                appspace[SeqFrame.list]=list()
            }
            
            # set the name of the SeqFrame.list
            appspace[SeqFrame.list][data.name]=data.name 
            # fill in the content
            appspace[SeqFrame.list][[data.name]]=sf
            
            # active.SeqFrame is decided here when new data inserted
            # later it is specified by select.node
            # set active.SeqFrame to the newly loaded SeqFrame
            
            # set active.SeqFrame to inserted SeqFrame
            cat("active.SeqFrame set to inserted SeqFrame ",data.name,"\n")
            appspace[active.SeqFrame]=sf
            
            # althernatively can
            # use selected node to set the active SeqFrame
            # selected.node(appspace[SeqFrame.hierachy.view])
            
            insert.node(node.name=data.name,
                        tree.view=SeqFrame.hierachy.view,
                        method="append")
            
            
            # update table.view to newly imported node
            update.table(df=exprs(sf),table.view=appspace[SeqFrame.table.view])
        }
        
        #print(is.environment(.GlobalEnv))
        #print(environment())
        #print(parent.env(environment()))
        
        dialog$destroy()
        
        ## consider a way to do the calculation after the file selection
        gtkNotebookSetCurrentPage(notebook,page.num=1)
    }
    
    
    ## -------------------------------------------------------------------------
    ## Save_CSV   
    Save_CSV=function(widget, window) {
        dialog<-gtkFileChooserDialog(
            title="Enter a name for the file",
            parent=window,
            action="save", 
            #button1_label="gtk-cancel", button1_response=GtkResponseType["cancel"], 
            button2_label="gtk-save", button2_response=GtkResponseType["accept"])
        
        if(dialog$run()==GtkResponseType["accept"]){
            
            # update selected.node
            selected.node(appspace[SeqFrame.hierachy.view])
            # save only active.seqFram
            sf.table=SeqFrame.table(appspace[active.SeqFrame])
            file.name=paste(dialog$getFilename(),".csv",sep="")
            cat("\nCSV file saved to ",file.name,"\n")
            write.csv(file=file.name,sf.table,row.names=F)
            
            dialog$destroy()
        }
        
    }
    
    ## -------------------------------------------------------------------------
    ## Save_PDF
    Save_PDF=function(widget, window) {
        dialog<-gtkFileChooserDialog(
            title="Enter a name for the file",
            parent=window,
            action="save", 
            #button1_label="gtk-cancel", button1_response=GtkResponseType["cancel"], 
            button2_label="gtk-save", button2_response=GtkResponseType["accept"]
        )
        
        if (dialog$run()==GtkResponseType["accept"]) {
            
            file.name=paste(dialog$getFilename(),".pdf",sep="")
            cat("\nPDF file saved to ",file.name,"\n")
            dev.copy2pdf(file = file.name)
            dialog$destroy()
        } 
    }
    
    ## -------------------------------------------------------------------------
    ## Quit
    Quit=function(widget, window) window$destroy()
    
    
    ## -------------------------------------------------------------------------
    ## Demo group
    ## Demo_CSV
    Demo_CSV=function(widget,window) {
        # code copy from Open_CSV(), see logic there
        file=system.file("extdata", "Demo.csv",package = "SeqViz")
        sf=file2sf(file)
        data.name=basename(file)
        keyword(sf)$VEGGI.NAME=data.name
        
        # if there is no SeqFrame.list in appspace, create one, else append
        if(!exists("SeqFrame.list",envir=.AppEnv)){
            appspace[SeqFrame.list]=list()
        }
        
        # set the name of the SeqFrame.list
        appspace[SeqFrame.list][data.name]=data.name 
        # fill in the content
        appspace[SeqFrame.list][[data.name]]=sf
        
        # set active.SeqFrame to inserted SeqFrame
        cat("active.SeqFrame set to inserted SeqFrame ",data.name,"\n")
        appspace[active.SeqFrame]=sf
        
        # insert node
        insert.node(node.name=data.name,
                    tree.view=SeqFrame.hierachy.view,
                    method="append")
        
        # update table.view to newly imported node
        update.table(df=exprs(sf),table.view=appspace[SeqFrame.table.view])
        
        gtkNotebookSetCurrentPage(notebook,page.num=1)
    }
    
    
    ## -------------------------------------------------------------------------
    ## Demo_Bam
    Demo_Bam=function(widget,window) {
        # code copy from Open_CSV(), see logic there
        file=system.file("extdata", "ChrY.bam",package = "SeqData")
        data.name=basename(file)
        
        ## put bamFile into bamFile.list
        model=appspace[bamFile.hierachy.view]$getModel()
        length.tree.view=model$iterNChildren()
        if(length.tree.view==0){
            appspace[bamFile.list]=list()}
        
        
        # set the name of the bamFile.list
        appspace[bamFile.list][data.name]=data.name 
        # fill in the content
        appspace[bamFile.list][[data.name]]=file
        # set active.SeqFrame to inserted bamFile
        cat("active.bamFile set to inserted bamFile ",data.name,"\n")
        appspace[active.bamFile]=file
        
        # insert.node
        insert.node(node.name=data.name,
                    tree.view=bamFile.hierachy.view,
                    method="append")
    }    
    
    ## -------------------------------------------------------------------------
    ## Demo_Annotation
    Demo_Annotation=function(widget,window) {
        
        file=system.file("extdata","refGene.csv",package ="SeqData")
        data.name=basename(file)
        
        ## put annotationFile into annotationFile.list
        model=appspace[annotationFile.hierachy.view]$getModel()
        length.tree.view=model$iterNChildren()
        if(length.tree.view==0){
            appspace[annotationFile.list]=list()
        }
        
        
        # set the name of the annotationFile.list
        appspace[annotationFile.list][data.name]=data.name 
        # fill in the content
        appspace[annotationFile.list][[data.name]]=file
        # set active.SeqFrame to inserted annotationFile
        cat("active.annotationFile set to inserted annotationFile ",data.name,"\n")
        appspace[active.annotationFile]=file
        
        # insert.node
        insert.node(node.name=data.name,
                    tree.view=annotationFile.hierachy.view,
                    method="append")
        
    }
    ## -------------------------------------------------------------------------
    ## Demo_BaseCall
    Demo_BaseCall=function(widget,window) {
        
        file=system.file("extdata","chrY_mC.csv",package="SeqData")
        data.name=basename(file)
        
        ## put bamFile into bamFile.list
        model=appspace[bamFile.hierachy.view]$getModel()
        length.tree.view=model$iterNChildren()
        if(length.tree.view==0){
            appspace[bamFile.list]=list()}
        
        # set the name of the bamFile.list
        appspace[bamFile.list][data.name]=data.name 
        # fill in the content
        appspace[bamFile.list][[data.name]]=file
        # set active.SeqFrame to inserted bamFile
        cat("active.bamFile set to inserted bamFile ",data.name,"\n")
        appspace[active.bamFile]=file
        
        # insert.node
        insert.node(node.name=data.name,
                    tree.view=bamFile.hierachy.view,
                    method="append")
    }    
    
    
    ##==========================================================================
    ## defining the actions (action groups)
    ##==========================================================================
    
    
    ##--------------------------------------------------------------------------
    ## fileActionGroup
    fileActionGroup=gtkActionGroup(name="fileActionGroup")
    
    fileActionEntries=list(  ## name, ID, label, accelerator, tooltip, callback
        
        file = list("File",NULL,"File",NULL,NULL,NULL),
        
        open_bam = list("Open_Bam", "gtk-open", "Open_Bam", 
                        NULL, "Open Bam File", Open_Bam),
        
        open_anno = list("Open_Annotation", "gtk-open", "Open_Annotation", 
                         NULL, "Open Annotation File", Open_Annotation),
        
        open_csv = list("Open_CSV", "gtk-open", "Open_CSV", 
                        NULL, "Open CSV File", Open_CSV),
        
        save_csv = list("Save_CSV", "gtk-save", "Save_CSV", 
                        "<alt>S", "Save CSV File", Save_CSV),
        
        save_pdf = list("Save_PDF", "gtk-save", "Save_PDF", 
                        "<ctrl>S", "Save PDF File", Save_PDF),
        
        quit = list("Quit", "gtk-quit", "_Quit", "<ctrl>Q", "Quit", Quit)
        
    )
    
    fileActionGroup$addActions(entries=fileActionEntries,user.data=main_window)
    
    
    ##--------------------------------------------------------------------------
    ## helpActionGroup
    helpActionGroup <- gtkActionGroup(name="helpActionGroup")
    helpActionEntries <- list(  # name,ID,label,accelerator,tooltip,callback
        help = list("Help",NULL,"Help",NULL,NULL,NULL),
        vignette = list("Vignette", "gtk-help", "Vignette", NULL, "Vignette", NULL),
        about = list("About", "gtk-about", "About", NULL, "About SeqViz", 
                     NULL)
    )
    helpActionGroup$addActions(entries=helpActionEntries,user.data=main_window)
    
    ##--------------------------------------------------------------------------
    ## demo action group
    demoActionGroup <- gtkActionGroup(name="demoActionGroup")
    demoActionEntries <- list(  # name,ID,label,accelerator,tooltip,callback
        
        demoBam=list("Demo_Bam","gtk-file","Demo_Bam",NULL,
                     "Load Demo Bam Data",Demo_Bam),
        demoBaseCall=list("Demo_BaseCall","gtk-file","Demo_BaseCall",NULL,
                          "Load Demo BaseCall Data",Demo_BaseCall),
        demoAnnotation=list("Demo_Annotation","gtk-file","Demo_Annotation",NULL,
                            "Load Demo Annotation Data",Demo_Annotation),
        demoCSV=list("Demo_CSV","gtk-file","Demo_CSV",NULL,
                     "Load Demo CSV Data",Demo_CSV)
    )
    demoActionGroup$addActions(entries=demoActionEntries,user.data=main_window)
    
    
    ##--------------------------------------------------------------------------
    ## insert the action group into the UI manager
    uimanager$insertActionGroup(fileActionGroup,0)
    uimanager$insertActionGroup(helpActionGroup,0)
    uimanager$insertActionGroup(demoActionGroup,0)
    
    
    
    ##==========================================================================
    ## Defining UI layout
    ##==========================================================================
    ## SeqViz-menu.xml 
    ## defines layout (what icon to put into) of tool bar and menu bar
    xml=system.file("etc", "SeqViz-menu.xml",package = "SeqViz")
    
    id =  uimanager$addUiFromFile(xml)
    
    accelgroup <- uimanager$getAccelGroup()
    main_window$addAccelGroup(accelgroup)
    
    ## -------------------------------------------------------------------------
    ## Creating widgets for the actions
    menubar <- uimanager$getWidget("/menubar")
    toolbar <- uimanager$getWidget("/toolbar")
    statusbar <- gtkStatusbar()
    
    ## -------------------------------------------------------------------------
    ## Integrating the components
    vbox <- gtkVBox()
    
    main_window$add(widget=vbox)
    
    vbox$packStart(child=menubar, expand=FALSE, fill=FALSE, padding=0)
    vbox$packStart(child=toolbar, expand=FALSE, fill= FALSE, padding=0)
    
    # Creates a new GtkNotebook widget with no pages.
    notebook=gtkNotebook(show=TRUE)
    notebook$setTabPos(pos="top")
    
    DataPage=gtkHPanedNew()
    DataPage$setPosition(position=393)
    PlotPage=gtkHPanedNew()
    PlotPage$setPosition(position=393)
    
    # code to determine the position
    # > gtkPanedGetPosition(DataPage)
    # [1] 398
    # > gtkPanedGetPosition(PlotPage)
    # [1] 393
    
    # append pages
    notebook$appendPage(child=DataPage,tab.label=gtkLabel("Data"))
    notebook$appendPage(child=PlotPage,tab.label=gtkLabel("Plot"))
    
    notebook$childSetProperty(child=DataPage,"tab-expand",T)
    notebook["homogeneous"]=TRUE
    
    # adjust the packing of the child of gtknotebook
    vbox$packStart(child=notebook, expand=TRUE, fill=TRUE, padding=0)
    vbox$packStart(child=statusbar, expand=FALSE, fill=FALSE, padding=0)
    
    
    ## -------------------------------------------------------------------------
    ## PlotPage
    ## -------------------------------------------------------------------------
    
    PlotPage.leftPane=gtkVBox()
    PlotPage.rightPane=gtkVBox()
    PlotPage$pack1(PlotPage.leftPane,resize=T,shrink=T)
    PlotPage$pack2(PlotPage.rightPane,resize=T,shrink=T)
    
    ## -------------------------------------------------------------------------
    ## PlotPage.leftPane - Gates
    ## -------------------------------------------------------------------------
    ## create tree model for data selection
    # define gtkTreeStore model with one column stores "character"
    PlotPage.data.model=gtkTreeStore("gchararray")
    
    # setup gtkTreeView to display gtkTreeStore model
    SeqFrame.hierachy.view=gtkTreeView()
    
    ## somehow this single line is able to make the view dynamic seems wiget is
    ## constantly lisenting to varialbes? all the changes in gate is made to
    ## appspace[SeqFrame.hierachy.view] and it is able to insert node into this
    ## view in this SeqViz function environment
    
    ## share the view
    appspace[SeqFrame.hierachy.view]=SeqFrame.hierachy.view 
    
    
    # insert column one by one
    # position= -1 append, 0 fill
    SeqFrame.hierachy.view$insertColumnWithAttributes(position=0,   
                                                      title="",
                                                      cell=gtkCellRendererText(),
                                                      text=1-1)  
    
    # add model to view
    SeqFrame.hierachy.view$setModel(PlotPage.data.model)
    
    
    ## -------------------------------------------------------------------------
    ## Gate buttons
    gate.button.table=gtkTable(rows=3, columns=3, homogeneous=T)
    
    polygon.gate.button=gtkButton(label="Polygon")
    rectangle.gate.button=gtkButton(label="Rectangle")
    quadrant.gate.button=gtkButton(label="Quadrant")
    range.gate.button=gtkButton(label="Range")
    norm2.gate.button=gtkButton(label="Norm2")
    
    gate.label=gtkLabel("Gate")
    gate.button.table$attach(
        gate.label,left.attach=1,2, top.attach=0,1)
    
    gate.button.table$attach(
        quadrant.gate.button, left.attach = 0,1, top.attach = 1,2)
    
    gate.button.table$attach(
        rectangle.gate.button, left.attach = 1,2, top.attach = 1,2)
    
    gate.button.table$attach(
        polygon.gate.button, left.attach = 2,3, top.attach = 1,2)
    
    gate.button.table$attach(
        range.gate.button, left.attach = 0,1, top.attach = 2,3)
    
    # PlotPage.leftPane$packStart(child=gate.label,expand=F,fill=F,padding=0)
    PlotPage.leftPane$packStart(child=SeqFrame.hierachy.view,expand=T,fill=T,padding=0)
    PlotPage.leftPane$packEnd(child=gate.button.table,expand=F,fill=F,padding=0)
    
    
    ## -------------------------------------------------------------------------
    ## PlotPage.rightPane - Plots
    ## -------------------------------------------------------------------------
    ## contains a table of buttons and an gtkTreeView 
    
    ## -------------------------------------------------------------------------
    ## table
    
    # SeqFrame.table.view need to made public to able to be dynamic display
    # this one line doesn't work
    # appspace[SeqFrame.table.view]=gtkTreeView()
    
    # this one adds to the main window
    SeqFrame.table.view=gtkTreeView()
    # this one duplicate into appspace (and all objects are self updating)
    appspace[SeqFrame.table.view]=SeqFrame.table.view
    
    # construct gtkTreeViewColumn
    PlotPage.info.scrolled.window<-gtkScrolledWindow()
    PlotPage.info.scrolled.window$add(SeqFrame.table.view)
    
    ## Graph buttons
    plot.button.table=gtkTable(rows=2, columns=3, homogeneous=T)
    
    countour.plot.button=gtkButton(label="Countour")
    scatter.plot.button=gtkButton(label="Scatter")
    density.plot.button=gtkButton(label="Density")
    histogram.plot.button=gtkButton(label="Histogram")
    DDplus.plot.button=gtkButton(label="2D+")
    
    plot.info.label=gtkLabel("Information")
    
    # plot.button.table$attach(plot.info.label,left.attach=0,1, right.attach=2,3, bottom.attach=2,3)
    
    plot.button.table$attach(scatter.plot.button, left.attach = 0,1, top.attach = 0,1)
    plot.button.table$attach(density.plot.button, left.attach = 1,2, top.attach = 0,1)
    plot.button.table$attach(DDplus.plot.button, left.attach = 2,3, top.attach = 0,1)
    #plot.button.table$attach(histogram.plot.button, left.attach = 0,1, top.attach = 1,2)
    #plot.button.table$attach(, left.attach = 1,2, top.attach = 2,3)
    
    PlotPage.rightPane$packStart(child=plot.button.table,expand=F,fill=F,padding=0)
    PlotPage.rightPane$packStart(child=plot.info.label,expand=F,fill=F,padding=0)
    
    PlotPage.rightPane$packEnd(child=PlotPage.info.scrolled.window,expand=T,fill=T,padding=0)
    
    ## -------------------------------------------------------------------------
    ## DataPage
    ## -------------------------------------------------------------------------
    
    DataPage.leftPane=gtkVBox()
    DataPage.rightPane=gtkVBox()
    DataPage$pack1(DataPage.leftPane,resize=T,shrink=T)
    DataPage$pack2(DataPage.rightPane,resize=T,shrink=T)
    
    ## -------------------------------------------------------------------------
    ## DataPage.leftPane - Data
    ## -------------------------------------------------------------------------
    # define gtkTreeStore model with one column stores "character"
    DataPage.data.model=gtkTreeStore("gchararray")
    
    # setup gtkTreeView to display gtkTreeStore model
    bamFile.hierachy.view=gtkTreeView()
    
    # insert column one by one
    # position=-1 append, 0 fill
    bamFile.hierachy.view$insertColumnWithAttributes(position=0,   
                                                     #title="Sequencing Data",
                                                     title="",
                                                     cell=gtkCellRendererText(),
                                                     text=1-1)  
    
    # add model to view
    bamFile.hierachy.view$setModel(DataPage.data.model)
    appspace[bamFile.hierachy.view]=bamFile.hierachy.view
    ## Annotation buttons and button tables removed for future use
    
    # pack DataPage.leftPane
    DataPage.leftPane$packStart(child=bamFile.hierachy.view,expand=T,fill=T,padding=0)
    
    
    ## -------------------------------------------------------------------------
    ## Annotation buttons
    ## structural, future use
    #     annotation.button.table=gtkTable(rows=3, columns=3, homogeneous=T)
    #     
    #     gene=gtkButton(label="")
    #     promoter=gtkButton(label="")
    #     retro=gtkButton(label="")
    #     repeats=gtkButton(label="")
    #     bins=gtkButton(label="")
    #     user=gtkButton(label="")
    #     
    #     annotation.label=gtkLabel("")
    #     annotation.button.table$attach(annotation.label,left.attach=0,1, right.attach=2,3, top.attach=0,1)
    #     annotation.button.table$attach(gene, left.attach = 0,1, top.attach = 1,2)
    #     annotation.button.table$attach(promoter, left.attach = 1,2, top.attach = 1,2)
    #     annotation.button.table$attach(retro, left.attach = 2,3, top.attach = 1,2)
    #     annotation.button.table$attach(repeats, left.attach = 0,1, top.attach = 2,3)
    #     annotation.button.table$attach(bins, left.attach = 1,2, top.attach = 2,3)
    #     annotation.button.table$attach(user, left.attach = 2,3, top.attach = 2,3)
    #     
    #     DataPage.leftPane$packEnd(child=annotation.button.table,expand=F,fill=F,padding=0)
    
    
    ## -------------------------------------------------------------------------
    ## DataPage.rightPane - Annotations
    ## -------------------------------------------------------------------------
    ## contains a table of buttons and an gtkTreeView 
    
    # gtkTreeView for the Info, construct using rGtkDataFrame, can also define
    # it as treeStore, future have child nodes
    
    # define gtkTreeStore model with one column stores "character"
    DataPage.anno.model=gtkTreeStore("gchararray")
    # setup gtkTreeView to display gtkTreeStore model
    annotationFile.hierachy.view=gtkTreeView()
    # insert column one by one
    # -1 append, 0 fill
    annotationFile.hierachy.view$insertColumnWithAttributes(position=0,   
                                                            title="",
                                                            cell=gtkCellRendererText(),
                                                            text=1-1)  
    
    # add model to view
    annotationFile.hierachy.view$setModel(DataPage.anno.model)
    
    ## for simplicity for now, annotation view does not show internal annotaion
    ## yet, instead annotation is printed out in the console when clicked
    
    appspace[annotationFile.hierachy.view]=annotationFile.hierachy.view
    
    DataPage.anno.scrolled.window<-gtkScrolledWindow()
    DataPage.anno.scrolled.window$add(annotationFile.hierachy.view)
    
    
    ## -------------------------------------------------------------------------
    ## function buttons
    function.button.table=gtkTable(rows=3, columns=3, homogeneous=T)
    
    #data.summary.button=gtkButton(label="dataSummary")
    #get.measure.button=gtkButton(label="getMeasure")
    #findEnriched=gtkButton(label="findEnrichedRegion")
    
    ## functions 
    count.method.label=gtkLabel("Drag&Drop annotation")
    
    ## future use
    #     ## TOD: use a frame surrounding the button and the radio button 
    #     function.button.table$attach(
    #         function.label,left.attach=0,1,right.attach=2,3, top.attach=0,1)
    #     function.button.table$attach(
    #         data.summary.button, left.attach = 0,1, top.attach = 1,2)
    #     function.button.table$attach(
    #         get.measure.button, left.attach = 1,2, top.attach = 1,2)
    #     function.button.table$attach(
    #         findEnriched, left.attach = 2,3, top.attach = 1,2)
    #     function.button.table$attach(
    #         histogram.plot.button, left.attach = 0,1, top.attach = 1,2)
    
    #     ## add a spinner for fun 
    #     spinner <- gtkSpinner()
    #     spinner["visible"]=F
    #     spinner["active"]=F
    #    appspace[spinner]=spinner
    # function.button.table$attach(spinner, left.attach = 2,3, top.attach = 2,3) 
    
    ## -------------------------------------------------------------------------
    ## radio buttons for count method    
    count.method=c("read counts","read coverage","base percentage")
    
    names(count.method)=count.method
    mode(count.method)="list"
    
    # use radio button
    for (i in 1:length(count.method)) {
        count.method[[i]]=gtkRadioButton(label=count.method[i])
    }
    # put the buttons into one group (the first buttons group)
    g=gtkRadioButtonGetGroup(count.method[[1]])
    
    buttons.tobe.grouped=count.method[-1]
    lapply(buttons.tobe.grouped,function(button){
        gtkRadioButtonSetGroup(object=button,group=g)
    }) 
    
    ## put radio button's selection into appspace
    appspace[count.method]=count.method
    
    
    # put radio button in the function button table
    function.button.table$attach(
        count.method[[1]], left.attach = 0,1, top.attach = 1,2)
    function.button.table$attach(
        count.method[[2]], left.attach = 1,2, top.attach = 1,2)
    function.button.table$attach(
        count.method[[3]], left.attach = 2,3, top.attach = 1,2)
    
    # pack DataPage.rightPane
    DataPage.rightPane$packStart(
        child=function.button.table,expand=F,fill=F,padding=0)
    DataPage.rightPane$packStart(
        child=count.method.label,expand=F,fill=F,padding=0)
    DataPage.rightPane$packEnd(
        child=DataPage.anno.scrolled.window,expand=T,fill=T,padding=0)
    
    # show all widgets
    main_window$showAll()
    
    ##==========================================================================
    ## connect actions
    ##==========================================================================
    
    
    ## -------------------------------------------------------------------------
    ## drag and drops
    # dnd setup
    TARGET <- c(string = 0)
    targetentries <-
        list(
            c(target="STRING", flags=0, info=TARGET[["string"]]),
            c(target="text/plain", flags=0, info=TARGET[["string"]]) 
        )
    
    # gtkDragSourceSet, this won't allow row-wise drag-drop, it is widget wise,
    # works between two views
    gtkDragSourceSet(object=annotationFile.hierachy.view, 
                     start.button.mask="button1-mask", 
                     targets=targetentries, 
                     actions="copy")
    gtkDragDestSet(obj=bamFile.hierachy.view, 
                   flags="all", 
                   targets=targetentries, 
                   actions="copy")
    
    # test highlight rows, as it is not easy to locate which row the drop is on,
    # some times gives (unknown enum) errors if it is in the middle of two row.
    gtkTreeViewEnableModelDragDest(obj=bamFile.hierachy.view, 
                                   targets=targetentries, 
                                   actions="copy")
    ## -------------------------------------------------------------------------
    ## connect signals
    
    # connect drag and drop 
    gSignalConnect(obj=annotationFile.hierachy.view, signal="drag-data-get", f=drag)
    gSignalConnect(obj=bamFile.hierachy.view, signal="drag-data-received", f=drop.and.count)
    
    
    # connect buttons
    gSignalConnect(obj=polygon.gate.button, signal="clicked", f=polygon.gate)
    gSignalConnect(obj=scatter.plot.button, signal="clicked", f=scatter.plot)    
    gSignalConnect(obj=density.plot.button, signal="clicked", f=density.plot) 
    
    gSignalConnect(obj=DDplus.plot.button, signal="clicked", f=DDplus.plot)
    
    gSignalConnect(obj=quadrant.gate.button, signal="clicked", f=quadrant.gate)   
    gSignalConnect(obj=range.gate.button, signal="clicked", f=range.gate)    
    gSignalConnect(obj=rectangle.gate.button, signal="clicked", f=rectangle.gate)
    
    
    # connect key-press-event [delete] with view
    gSignalConnect(
        obj=appspace[SeqFrame.hierachy.view], signal="key-press-event", f=delete.node)
    gSignalConnect(
        obj=appspace[bamFile.hierachy.view], signal="key-press-event", f=delete.node)
    gSignalConnect(
        obj=appspace[annotationFile.hierachy.view], signal="key-press-event", f=delete.node)
    
    ## connect radio button count method
    gSignalConnect(obj=SeqFrame.hierachy.view,signal="row-activated",f=update.table.view)
    
    # selected.annotation
    gSignalConnect(obj=SeqFrame.table.view,signal="row-activated",f=selected.annotation)
    
    
    
}


