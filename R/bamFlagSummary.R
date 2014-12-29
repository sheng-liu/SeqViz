
# bamFlagSummary
# add a progress bar to this function
library(SeqData)

data.summary=function(widget,window){
    
    #spinner$start()
    #appspace[spinner]["visible"]=T
    #appspace[spinner]["active"]=T
    print("quickBamFlageSummary")
    bamFile=appspace[bamFile]
    quickBamFlagSummary(bamFile, main.groups.only=TRUE)
    
    #spinner$stop()
#     appspace[spinner]["active"]=F
#     appspace[spinner]["visible"]=F
    
}






