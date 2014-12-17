## Callbacks for the DataPage

## getMeasure
## countReads (RNAseq)
## getReadCoverage (ChIPseq)
## percentMethylation (BisulfiteSeq)

get.measure=function(widget,window){
    
    # select assay type - method for measurement
    # may need some code for detection of the data type first (bam, bigwig, bed, csv)
    
    # let's first just deal with bam, reads file
    #spinner$start()
    select.measureType()
    
    sd=SeqData(bamFile=appspace[bamFile],annotationFile=appspace[annotationFile])
    
    switch(appspace[measureType],
           "getReadCoverage"={viewCoverage(sd)},
           "countReads"={countReads(sd)},
           "cytosineMethylation"={}
           )
    
    
    
    if (appspace[measureType]=="getReadCoverage"){
        
        #spinner$start() 

        
        sd=SeqData(bamFile=appspace[bamFile],annotationFile=appspace[annotationFile])
        #sd=getReadCoverage(sd)
        
        sd=viewCoverage(sd)
        
        #spinner$stop()
    
        
    }
       
}



select.measureType=function(){
    
    measuresDialog=gtkMessageDialog(
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
        label="Select measure type")
    
    measureType=c("countReads","getReadCoverage","getCytosineMethylation")
    
    #measureType.list=vector("list",length(measureType))
    #names(measureType)
    
    measureTypeCombo=gtkComboBoxNewText()
    sapply(measureType,measureTypeCombo$appendText)
    
    hbox=gtkHBox()
    hbox$packStart(child=measureTypeCombo)
    
    measuresDialog[["vbox"]]$add(hbox)
    
    if(measuresDialog$run()==GtkResponseType["ok"]){
        
        appspace[measureType]=measureTypeCombo$getActiveText()
        
        cat("selected measure type ",appspace[measureType],"\n")
        
        measuresDialog$destroy()
        
    }
}

## seems a radio box is more suited here, so when to use a combobox and when to use a radio box

# first get it runing, then think about the style



