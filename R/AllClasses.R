# add a new slot called featureAnnotation


library(RGtk2)
library(flowCore)
library(flowViz)
library(Biobase)   
# for Class AnnotatedDataFrame and its assoicated functions
# its function selectChannels marks one of my function
# it is importaed for flowCore, so presumably it would have imported
# 
# Error in (function (widget, window)  : 
# could not find function "AnnotatedDataFrame"

## -----------------------------------------------------------------------------
## Class seqFrame

setClass(Class="seqFrame",
         representation(featureAnnotation="character"),
         contains="flowFrame") 

# getClass("seqFrame")

# guid <- function(len=10){
#     ltrs <- c(LETTERS,letters)
#     paste(c(sample(ltrs,1),sample(c(ltrs,0:9),len-1,replace=TRUE)),collapse="")
# }




veggie=function(){
    veg=c("avocado","melon","olive","pepper","pumpkin","vanilla","tomato","squash","broccoli","chickpea","bean","celery")
    

    name=paste(sample(veg,1),sample(0:9,1),sample(0:9,1),sep="")
    return(name)
}




#     num=c(sample(0:9,3,replace=T))
#     num=as.integer(sample(0:9,3,replace=T))
#     sample(0:9,3)


# guid <- function(len=10){
#     ltrs <- letters
#     paste(sample(c(letters,0:9),replace=TRUE),collapse="")
# }