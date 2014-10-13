
# SeqViz UImanager

## -----------------------------------------------------------------------------
## SeqViz 


## TODO
## add import UCSC tracks
## clickable to UCSC browser

# for better display on mac for now
# Rstudio graphics not clear
options(device = "quartz")


##' @import RGtk2
##' @import SeqData
##' @import SeqFrame

## these may not be needed as SeqFrame loaded them

##' @import flowCore
##' @import flowViz
##' @import Biobase
##' @import flowStats



library(SeqData)
library(SeqFrame)
library(RGtk2)
library(flowCore)
library(flowViz)
library(Biobase)
library(flowStats)
library(lattice) 
# though flowViz load it, somehow it is not loaded when load the package



##' @export SeqViz
SeqViz=function(){
    
    # initialize active.SeqFrame
    #file=system.file("extdata", "Initialize.csv",package = "SeqViz")
    #sf=file2sf(file)
    
    #data.name=basename(file)
    #keyword(sf)$VEGGI.NAME=data.name
    #assign(x=data.name,value=sf,envir=.AppEnv)
    
    #appspace[active.SeqFrame]=sf      
    
    #df=read.csv(file=file,header=T,as.is=T)
    #appspace[PlotPage.table.model]=rGtkDataFrame(df)
    #PlotPage.table.model <- rGtkDataFrame(df)
    #PlotPage.table.view=gtkTreeView()
    ## -------------------------------------------------------------------------
    ## UImanager 
    
    uimanager = gtkUIManager()
    
    main_window <- gtkWindow(show = FALSE)
    # change title (property)
    main_window["title"] <- "SeqViz"
    # setDefultSize ()
    main_window$setDefaultSize(800, 600)
    
    
    ## -------------------------------------------------------------------------
    ## Implementing callbacks
    
    
    ## file group
    ## Open_Bam
    
    Open_Bam= function(widget, window) {
        dialog<-gtkFileChooserDialog(
            title="Choose a Bam file",
            parent=window,
            action="open",
            # the names of variable are mapped above this line
            button1_label="gtk-cancel", button1_response=GtkResponseType["cancel"], 
            button2_label="gtk-open", button2_response=GtkResponseType["accept"]) 
        
        if(dialog$run()==GtkResponseType["accept"]){
            
            # get file name, no further reading functions, as it is too big
            file=dialog$getFilename()
            appspace[bamFile]=file
            
            data.name=basename(file)
            
            
            insert.node(node.name=data.name,
                        tree.view=DataPage.node.view,
                        method="append")
            
            
        }
        dialog$destroy()
    }    
    
    
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
            appspace[annotationFile]=file

            data.name=basename(file)

            #data=getannotation(annotationFile=file)
            #anno=annotation(data)
            #appspace[annotation]=anno
        }
        dialog$destroy()
        
    }
    
    
    Open_CSV=function(widget, window) {
        dialog=gtkFileChooserDialog(
            title="Choose a CSV file",
            parent=window,
            action="open",
            # the names of variable are mapped above this line
            #button1_label="gtk-cancel", button1_response=GtkResponseType["cancel"], 
            button2_label="gtk-open", button2_response=GtkResponseType["accept"]) 
        
        if(dialog$run()==GtkResponseType["accept"]){
            
            ## the current implementation only works for one dataset, 
            ## if new data are selected, it will overwrite the old one
            ## need a list to store if want to support multiple files
            
            # assign data.frame to appspace for passing of variables
            # no need to put the process, only sf is used for plotting
            file=dialog$getFilename()
            
            # sf=file2sf(file)
            sf=SeqFrame(file=file)
            
            # add an veggi name to sf for easy finding it when plotting
            data.name=basename(file)
            keyword(sf)$VEGGI.NAME=data.name
            
            # if it only have one node, create a list, 
            # else add into the existing list
            
            #             on startup, (only) 4 views in appspace
            #             csv.tree.view 
            #             table.view
            #             
            #             bam.tree.view
            #             text.view
            
            
            # if there is no SeqFrame.list in appspace, create one, else append
            # here using tree.view.length to determine
            # can also by existence of SeqFrame.list in appspace
            # exists("SeqFrame.list",envir=.AppEnv)
            
            model=appspace[active.view]$getModel()
            length.tree.view=model$iterNChildren()
            if(length.tree.view==0){
                appspace[SeqFrame.list]=list()
            }
            
            # set the name of the SeqFrame.list
            appspace[SeqFrame.list][data.name]=data.name 
            # fill in the content
            appspace[SeqFrame.list][[data.name]]=sf
            
            # set active.SeqFrame to inserted SeqFrame
            cat("active.SeqFrame set to inserted SeqFrame ",data.name,"\n")
            appspace[active.SeqFrame]=sf
              
            # active.SeqFrame is decided here when new data inserted
            # later it is specified by select.node
            # set active.SeqFrame to the newly loaded SeqFrame
            
            # althernatively can
            # use selected node to set the active SeqFrame
            # selected.node(appspace[active.view])
            
            

                        
            insert.node(node.name=data.name,
                        tree.view=PlotPage.node.view,
                        method="append")
            
            
            # update table.view to newly imported node
            update.table(df=exprs(sf),table.view=appspace[active.table.view])
        }
        
        #print(is.environment(.GlobalEnv))
        #print(environment())
        #print(parent.env(environment()))
        dialog$destroy()
        
        ## consider a way to do the calculation after the file selection
        gtkNotebookSetCurrentPage(notebook,page.num=1)
    }
    
    
    
    Save_CSV=function(widget, window) {
        dialog<-gtkFileChooserDialog(
            title="Enter a name for the file",
            parent=window,
            action="save", 
            #button1_label="gtk-cancel", button1_response=GtkResponseType["cancel"], 
            button2_label="gtk-save", button2_response=GtkResponseType["accept"])
        
        if(dialog$run()==GtkResponseType["accept"]){
            file.name=paste(dialog$getFilename(),".csv",sep="")
            cat("CSV file saved to ",file.name,"\n")
            # save only active.seqFram
            write.csv(file=file.name,exprs(appspace[active.SeqFrame]),row.names=F)
            dialog$destroy()
        }
        
    }
    
    # note this only saves the current active window which is show at the top of
    # the x11 window
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
            cat("PDF file saved to ",file.name,"\n")
            dev.copy2pdf(file = file.name)
            dialog$destroy()
            
        } #else if(dialog$run()==GtkResponseType["cancel"])
        #dialog$destroy()
        # need to click twice to close, same as the close button
        
        
    }
    
    
    
    
    
    
    
    ## open_bigWig
    ## it would be nice one can simply input UCSC track location, then look at it globally
    ## add a button conversion between UCSC 
    
    
    
    
    #save_file
    
    Quit=function(widget, window) window$destroy()
    
    
    
    # Demo
    Demo=function(widget,window) {
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
                    tree.view=PlotPage.node.view,
                    method="append")
        
        # update table.view to newly imported node
        update.table(df=exprs(sf),table.view=appspace[active.table.view])
        
        gtkNotebookSetCurrentPage(notebook,page.num=1)
    }
    
    
    
    ## tool group
    
    UnDo=function(action,...) 
        statusbar$push(statusbar$getContextId("message"), 
                       action$getName())
    
    Redo=function(action,...) 
        statusbar$push(statusbar$getContextId("message"), 
                       action$getName())
    
    Preference=function(action,...) 
        statusbar$push(statusbar$getContextId("message"), 
                       action$getName())
    
    DataSummary=function(action,...) 
        statusbar$push(statusbar$getContextId("message"), 
                       action$getName())
    
    
    GetMeasure=function(action,...) 
        statusbar$push(statusbar$getContextId("message"), 
                       action$getName())
    
    
    
    
    
    
    
    
    PolygonGate=polygon.gate
    
    RectangleGate=function(action,...) 
        statusbar$push(statusbar$getContextId("message"), 
                       action$getName())
    
    CountourPlot=function(action,...) 
        statusbar$push(statusbar$getContextId("message"), 
                       action$getName())
    
    HistogramPlot=function(action,...) 
        statusbar$push(statusbar$getContextId("message"), 
                       action$getName())
    
    ## -----------------------------------------------------------------------------
    ## defining the actions
    
    # xml defines what icon to put into tool bar and menu bar
    
    
    
    # fileActionGroup
    fileActionGroup=gtkActionGroup(name="fileActionGroup")
    
    fileActionEntries=list(  ## name, ID, label, accelerator, tooltip, callback
        
        file = list("File",NULL,"File",NULL,NULL,NULL),
        
        open_bam = list("Open_Bam", "gtk-open", "Open_Bam", NULL, "Open Bam File", Open_Bam),
        open_anno = list("Open_Annotation", "gtk-open", "Open_Annotation", NULL, "Open Annotation File", Open_Annotation),
        open_csv = list("Open_CSV", "gtk-open", "Open_CSV", NULL, "Open CSV File", Open_CSV),
        save_csv = list("Save_CSV", "gtk-save", "Save_CSV", "<alt>S", "Save CSV File", Save_CSV),
        save_pdf = list("Save_PDF", "gtk-save", "Save_PDF", "<ctrl>S", "Save PDF File", Save_PDF),
        quit = list("Quit", "gtk-quit", "_Quit", "<ctrl>Q", "Quit", Quit),
        
        edit = list("Edit", NULL, "Edit", NULL, NULL, NULL),
        undo = list("Undo", "gtk-undo", "_Undo", "<ctrl>Z",  "Undo change", UnDo),
        redo = list("Redo", "gtk-redo", "_Redo", "<ctrl>U", "Redo change", Redo)
    )
    
    fileActionGroup$addActions(entries=fileActionEntries,user.data=main_window)
    
    # toolActionGroup
    toolActionGroup=gtkActionGroup(name="toolActionGroup")
    
    toolActionEntries=list(  # name,ID,label,accelerator,tooltip,callback
        
        tool=list("Tools",NULL,"Tools",NULL,NULL,NULL),
        preference=list("Preference",NULL,"Preference",NULL,"Set Computation Preference",Preference),
        
        data_summary=list("DataSummary",NULL,"DataSummary",NULL,"Sequencing Data Summary",DataSummary),
        get_measure=list("GetMeasure",NULL,"GetMeasure",NULL,"Sequencing Data Summary",DataSummary),
        
        gate = list("Gate", NULL, "Gate", NULL, NULL, NULL),
        polygon_gate=list("PolygonGate",NULL,"PolygonGate",NULL,"Polygon Gate",PolygonGate),
        rectangle_gate=list("RectangleGate",NULL,"RectangleGate",NULL,"Rectangle Gate",RectangleGate),
        
        plot = list("Plot", NULL, "Plot", NULL, NULL, NULL),
        countour_plot=list("CountourPlot",NULL,"CountourPlot",NULL,"Countour Plot",CountourPlot),
        histogram_plot=list("HistogramPlot",NULL,"HistogramPlot",NULL,"Histogram Plot",HistogramPlot)
        
    )
    
    toolActionGroup$addActions(entries=toolActionEntries,user.data=main_window)
    
    
    helpActionGroup <- gtkActionGroup(name="helpActionGroup")
    helpActionEntries <- list(  # name,ID,label,accelerator,tooltip,callback
        demo=list("Demo","","Demo",NULL,"Load Demo Data",Demo),
        help = list("Help", "", "Help", NULL, "Open Help Doc", NULL),
        about = list("About", "gtk-about", "About", NULL, "About SeqViz", 
                     NULL)
    )
    #helpActionGroup$addActions(helpActionEntries)
    helpActionGroup$addActions(entries=helpActionEntries,user.data=main_window)
    
    
    
    #insert the action group into the UI manager
    uimanager$insertActionGroup(fileActionGroup,0)
    uimanager$insertActionGroup(toolActionGroup,0)
    uimanager$insertActionGroup(helpActionGroup,0)
    
    ## -------------------------------------------------------------------------
    ## Defining UI layout
    
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
    
    # > gtkPanedGetPosition(DataPage)
    # [1] 398
    # > gtkPanedGetPosition(PlotPage)
    # [1] 393
    
    # append pages
    notebook$appendPage(child=DataPage,tab.label=gtkLabel("Data"))
    notebook$appendPage(child=PlotPage,tab.label=gtkLabel("Plot"))
    
    #notebook$setTabLabelPacking(child=DataPage,expand=T,fill=T,pack.type="Top")
    
    # setTabLabelPacking is about to deprecate
    # use tab-expand and tab-fill instead
    # notebook$tab-expand(child=DataPage)
    # notebook$DataPage$tab-expand=T
    
    
    # > DataPage["tab-expand"]
    # Error in gObjectGet(obj, c(value, ...)) : Invalid property tab-expand!
    
    # notebook$getTabDetachable(DataPage) this is operation on child
    # tab-fill and tab expand is child property
    
    ## gtkContainerChildSetProperty(object, child, property.name, value)
    
    
    notebook$childSetProperty(child=DataPage,"tab-expand",T)
    notebook["homogeneous"]=TRUE
    
    vbox$packStart(child=notebook, expand=TRUE, fill=TRUE, padding=0)
    vbox$packStart(child=statusbar, expand=FALSE, fill=FALSE, padding=0)
    
    
    
    # how to adjust the packing of the child of gtknotebook
    # DataPage["tab-expand"]=T
    # DataPage$propInfo()
    # notebook$propInfo()
    # notebook$packStart(DataPage,expand=T,fill=TRUE, padding=0)
    
    
    ## -----------------------------------------------------------------------------
    ## Plot pane
    
    # PlotPage.leftPane
    PlotPage.leftPane=gtkVBox()
    PlotPage.rightPane=gtkVBox()
    PlotPage$pack1(PlotPage.leftPane,resize=T,shrink=T)
    PlotPage$pack2(PlotPage.rightPane,resize=T,shrink=T)
    
    
    ## Plot pane - Data
    
    # plot.data.file=gtkFileChooserNew()
    # gate=gtkButton(label="Gate")
    
    # plot.data.label=gtkLabel("Plot Data")
    
    
    
    ## create tree model for data selection
    
    # define gtkTreeStore model with one column stores "character"
    PlotPage.data.model=gtkTreeStore("gchararray")
    
    # fill in data to PlotPage.data.model
    
    # setup gtkTreeView to display gtkTreeStore model
    PlotPage.node.view=gtkTreeView()
    
    ## share the view
    appspace[active.view]=PlotPage.node.view
    #appspace[active.table.view]=PlotPage.table.view
    
    #### somehow this single line is able to make the view dynamic
    ## seems wiget is constantly lisenting to varialbes?
    ## all the changes in gate is made to appspace[active.view]
    ## and it is able to insert node into this view in this SeqViz function environment
    
    
    
    # insert column one by one
    PlotPage.node.view$insertColumnWithAttributes(position=0,   # -1 append, 0 fill
                                                  title="Plot Data",
                                                  cell=gtkCellRendererText(),
                                                  text=1-1)  
    
    # add model to view
    PlotPage.node.view$setModel(PlotPage.data.model)
    
    
    
    
    
    
    gate.label=gtkLabel("Gate")
    
    ## Gate buttons
    
    gate.button.table=gtkTable(rows=3, columns=3, homogeneous=T)
    
    polygon.gate.button=gtkButton(label="Polygon")
    rectangle.gate.button=gtkButton(label="Rectangle")
    quadrant.gate.button=gtkButton(label="Quadrant")
    range.gate.button=gtkButton(label="Range")
    norm2.gate.button=gtkButton(label="Norm2")
    
    
    gate.button.table$attach(
        gate.label,left.attach=0,1, right.attach=2,3, top.attach=0,1)
    
    gate.button.table$attach(
        quadrant.gate.button, left.attach = 0,1, top.attach = 1,2)
    
    gate.button.table$attach(
        rectangle.gate.button, left.attach = 1,2, top.attach = 1,2)
    
    gate.button.table$attach(
        polygon.gate.button, left.attach = 2,3, top.attach = 1,2)
    
    gate.button.table$attach(
        range.gate.button, left.attach = 0,1, top.attach = 2,3)
    
    
    #gate.button.table$attach(norm2.gate.button, left.attach = 1,2, top.attach = 2,3)
    
    
    
    # PlotPage.leftPane$packStart(child=plot.data.label,expand=F,fill=F,padding=0)
    
    PlotPage.leftPane$packEnd(child=gate.button.table,expand=F,fill=F,padding=0)
    # PlotPage.leftPane$packEnd(child=gate.label,expand=F,fill=F,padding=0)
    
    PlotPage.leftPane$packStart(child=PlotPage.node.view,expand=T,fill=T,padding=0)
    
    ## PlotPage.rightPane
    
    ## contains a table of buttons and an gtkTreeView 
    
    ##------------------------------------------------------------------------------
    ## table
    
    #file=system.file("extdata", "Demo.csv",package = "SeqViz")
    #sf=file2sf(file)
    # load("~/DoScience/DoScience/Projects/Clover/Dev/Data/2014-08-29/Male.Het.ff.rda")
    #load("~/DoScience/DoScience/Projects/Clover/Dev/Data/2014-08-29/Male.Het.fs.rda")
    
    # data(Cars93 , package="MASS")
    
    # gtkTreeView for the Info
    # construct rGtkDataFrame
    #PlotPage.table.model <- rGtkDataFrame (exprs(Male.Het.ff))
    
    #appspace[PlotPage.table.model]=rGtkDataFrame(df)
    
    #PlotPage.table.model <- rGtkDataFrame (exprs(appspace[active.SeqFrame]))
    
    # Displaying data as a list or table
    #PlotPage.table.view<-gtkTreeView(appspace[PlotPage.table.model])
    
    
    # PlotPage.table.view need to made public to able to be dynamic display
    #PlotPage.table.view=appspace[PlotPage.table.view]
    PlotPage.table.view=gtkTreeView()
    
    appspace[PlotPage.table.view]=PlotPage.table.view
    
    appspace[active.table.view]=PlotPage.table.view
    # construct gtkTreeViewColumn
    #PlotPage.info.column<-gtkTreeViewColumn()
    
    # PlotPage.info.column$setTitle("Manufacturer")
    
    #cell_renderer<-gtkCellRendererText()
    #PlotPage.info.column$packStart(cell_renderer)
    #PlotPage.info.column$addAttribute(cell_renderer,"text",0)
    
    # insert gtkTreeViewColumn to the first position of the column
    # PlotPage.table.view$insertColumn(PlotPage.info.column,0)
    
    # no need to initilize
    #         mapply(PlotPage.table.view$insertColumnWithAttributes,
    #                position=-1,
    #                title=colnames(appspace[PlotPage.table.model]),
    #                cell=list(gtkCellRendererText()),text=seq_len(ncol(appspace[PlotPage.table.model]))-1
    #         )
    
    
    #     mapply(appspace[PlotPage.table.view]$insertColumnWithAttributes,
    #            position=-1,
    #            title=colnames(appspace[PlotPage.table.model]),
    #            cell=list(gtkCellRendererText()),text=seq_len(ncol(appspace[PlotPage.table.model]))-1
    #     )
    
    PlotPage.info.scrolled.window<-gtkScrolledWindow()
    #PlotPage.info.scrolled.window$add(appspace[PlotPage.table.view])
    PlotPage.info.scrolled.window$add(PlotPage.table.view)
    
    ## Graph buttons
    
    plot.button.table=gtkTable(rows=2, columns=3, homogeneous=T)
    
    countour.plot.button=gtkButton(label="Countour")
    scatter.plot.button=gtkButton(label="Scatter")
    density.plot.button=gtkButton(label="Density")
    histogram.plot.button=gtkButton(label="Histogram")
    
    plot.info.label=gtkLabel("Information")
    
    # plot.button.table$attach(plot.info.label,left.attach=0,1, right.attach=2,3, bottom.attach=2,3)
    
    plot.button.table$attach(scatter.plot.button, left.attach = 0,1, top.attach = 0,1)
    plot.button.table$attach(density.plot.button, left.attach = 1,2, top.attach = 0,1)
    #plot.button.table$attach(countour.plot.button, left.attach = 2,3, top.attach = 0,1)
    #plot.button.table$attach(histogram.plot.button, left.attach = 0,1, top.attach = 1,2)
    #plot.button.table$attach(, left.attach = 1,2, top.attach = 2,3)
    
    PlotPage.rightPane$packStart(child=plot.button.table,expand=F,fill=F,padding=0)
    PlotPage.rightPane$packStart(child=plot.info.label,expand=F,fill=F,padding=0)
    
    PlotPage.rightPane$packEnd(child=PlotPage.info.scrolled.window,expand=T,fill=T,padding=0)
    
    ## -----------------------------------------------------------------------------
    ## Data page
    
    ## same pattern as PlotPage, this also reflects the analysis similarity
    
    # DataPage.leftPane
    DataPage.leftPane=gtkVBox()
    DataPage.rightPane=gtkVBox()
    DataPage$pack1(DataPage.leftPane,resize=T,shrink=T)
    DataPage$pack2(DataPage.rightPane,resize=T,shrink=T)
    
    
    # define gtkTreeStore model with one column stores "character"
    DataPage.node.model=gtkTreeStore("gchararray")
    
    # setup gtkTreeView to display gtkTreeStore model
    DataPage.node.view=gtkTreeView()
    
    # insert column one by one
    DataPage.node.view$insertColumnWithAttributes(position=0,   # -1 append, 0 fill
                                                  title="Sequencing Data",
                                                  cell=gtkCellRendererText(),
                                                  text=1-1)  
    
    # add model to view
    DataPage.node.view$setModel(DataPage.node.model)
    
    appspace[active.data.view]=DataPage.node.view
    
    annotation.label=gtkLabel("Annotation_mm9")
    
    ## Annotation buttons
    
    annotation.button.table=gtkTable(rows=3, columns=3, homogeneous=T)
    
    gene=gtkButton(label="Gene")
    promoter=gtkButton(label="Promoter")
    retro=gtkButton(label="Retrotransposons")
    repeats=gtkButton(label="SimpleRepeats")
    bins=gtkButton(label="Bins")
    user=gtkButton(label="User")
    
    annotation.button.table$attach(annotation.label,left.attach=0,1, right.attach=2,3, top.attach=0,1)
    
    annotation.button.table$attach(gene, left.attach = 0,1, top.attach = 1,2)
    annotation.button.table$attach(promoter, left.attach = 1,2, top.attach = 1,2)
    annotation.button.table$attach(retro, left.attach = 2,3, top.attach = 1,2)
    annotation.button.table$attach(repeats, left.attach = 0,1, top.attach = 2,3)
    annotation.button.table$attach(bins, left.attach = 1,2, top.attach = 2,3)
    annotation.button.table$attach(user, left.attach = 2,3, top.attach = 2,3)
    
    
    # PlotPage.leftPane$packStart(child=plot.data.label,expand=F,fill=F,padding=0)
    
    DataPage.leftPane$packEnd(child=annotation.button.table,expand=F,fill=F,padding=0)
    # PlotPage.leftPane$packEnd(child=gate.label,expand=F,fill=F,padding=0)
    
    DataPage.leftPane$packStart(child=DataPage.node.view,expand=T,fill=T,padding=0)
    
    ##---------------------------------------------------------------------------
    ## DataPage.rightPane
    
    ## contains a table of buttons and an gtkTreeView 
    
    # gtkTreeView for the Info
    # construct rGtkDataFrame
    #   DataPage.info.model <- rGtkDataFrame (exprs(sf))
    #DataPage.info.model$setFrame(Cars93[1:5 , 1:5])
    
    
    # Displaying data as a list or table
    #    DataPage.info.view<-gtkTreeView(DataPage.info.model)
    
    # construct gtkTreeViewColumn
    #    DataPage.info.column<-gtkTreeViewColumn()
    
    # DataPage.info.column$setTitle("Manufacturer")
    
    #    cell_renderer<-gtkCellRendererText()
    #    DataPage.info.column$packStart(cell_renderer)
    #    DataPage.info.column$addAttribute(cell_renderer,"text",0)
    
    # insert gtkTreeViewColumn to the first position of the column
    # DataPage.info.view$insertColumn(DataPage.info.column,0)
    
    #     mapply(DataPage.info.view$insertColumnWithAttributes,
    #            position=-1,
    #            title=colnames(DataPage.info.model),
    #            cell=list(gtkCellRendererText()),text=seq_len(ncol(DataPage.info.model))-1)
    #     
    DataPage.info.view=gtkTreeView()    
    
    DataPage.info.scrolled.window<-gtkScrolledWindow()
    DataPage.info.scrolled.window$add(DataPage.info.view)
    
    
    ## Graph buttons
    
    function.button.table=gtkTable(rows=3, columns=3, homogeneous=T)
    
    data.summary.button=gtkButton(label="dataSummary")
    get.measure.button=gtkButton(label="getMeasure")
    findEnriched=gtkButton(label="findEnrichedRegion")
    
    ## function 
    function.info.label=gtkLabel("Information")
    
    #function.button.table$attach(function.label,left.attach=0,1, right.attach=2,3, top.attach=0,1)
    function.button.table$attach(data.summary.button, left.attach = 0,1, top.attach = 1,2)
    function.button.table$attach(get.measure.button, left.attach = 1,2, top.attach = 1,2)
    function.button.table$attach(findEnriched, left.attach = 2,3, top.attach = 1,2)
    
    #function.button.table$attach(histogram.plot.button, left.attach = 0,1, top.attach = 1,2)
    # add a spinner for fun
    # also shows progress 
#     spinner <- gtkSpinner()
#     spinner["visible"]=F
#     spinner["active"]=F
    
#    appspace[spinner]=spinner
#    function.button.table$attach(spinner, left.attach = 2,3, top.attach = 2,3)
    
    
    
    DataPage.rightPane$packStart(child=function.button.table,expand=F,fill=F,padding=0)
    DataPage.rightPane$packStart(child=function.info.label,expand=F,fill=F,padding=0)
    DataPage.rightPane$packEnd(child=DataPage.info.scrolled.window,expand=T,fill=T,padding=0)
    
    main_window$showAll()
    
    # connect buttons
    gSignalConnect(obj=polygon.gate.button, signal="clicked", f=polygon.gate)
    gSignalConnect(obj=scatter.plot.button, signal="clicked", f=scatter.plot)    
    gSignalConnect(obj=density.plot.button, signal="clicked", f=density.plot)    
    gSignalConnect(obj=quadrant.gate.button, signal="clicked", f=quadrant.gate)   
    gSignalConnect(obj=range.gate.button, signal="clicked", f=range.gate)    
    gSignalConnect(obj=rectangle.gate.button, signal="clicked", f=rectangle.gate)
    
    
    # connect key-press-event with view
    # gSignalConnect(PlotPage.node.view, "key-press-event", delete.node) 
    gSignalConnect(obj=appspace[active.view], signal="key-press-event", f=delete.node)
    gSignalConnect(obj=get.measure.button, signal="clicked", f=get.measure)
    gSignalConnect(obj=data.summary.button, signal="clicked", f=data.summary)
    
    
    
    
}

## nomenclature
## tree.view  gtkTreeView is named as node.view and table.view for better discriminition




# clear variables in the appspace
# rm(list=ls(.AppEnv),envir=.AppEnv)

# this assigns a variable called  "data.name" to .AppEnv
# appspace[data.name]=appspace[ ]
