# add a new slot called featureAnnotation

# for better display on mac for now
# Rstudio graphics not clear
options(device = "quartz")



library(RGtk2)
library(flowCore)
library(flowViz)
library(Biobase)
library(flowStats)
library(lattice) 
# though flowViz load it, somehow it is not loaded when load the package


# for Class AnnotatedDataFrame and its assoicated functions
# its function selectChannels marks one of my function
# it is importaed for flowCore, so presumably it would have imported
# 
# Error in (function (widget, window)  : 
# could not find function "AnnotatedDataFrame"

## -----------------------------------------------------------------------------
## Class seqFrame

setClass(Class="seqFrame",
         representation(featureAnnotation="data.frame"),
         contains="flowFrame") 

# getClass("seqFrame")

# guid <- function(len=10){
#     ltrs <- c(LETTERS,letters)
#     paste(c(sample(ltrs,1),sample(c(ltrs,0:9),len-1,replace=TRUE)),collapse="")
# }


# > getClass("flowFrame")
# Class "flowFrame" [package "flowCore"]
# Slots:
# Name:               exprs         parameters        description
# Class:       NcdfOrMatrix AnnotatedDataFrame               list


# setClass(Class="seqFrame",
#          contains="flowFrame",
#          representation(featureAnnotation="data.frame")) 
# change the sequence won't change the sequence slot printed out


# setClass(Class="seqFrame",
#          contains="flowFrame",
#          representation(annotation="data.frame")) 

# data.frame is better in this case, as it is easier to manipulate and display, althought they are essentially the same. 
# when construct, user has to pass in the annotation they want to look at, it may not be the same as the parameters were calculated, such as promoter for histone coverage, while exon counts for the same gene's expression. as long as they are corresponding, they are good. 

# > getClass("seqFrame")
# Class "seqFrame" [in ".GlobalEnv"]
# Slots: 
# Name:     annotation      exprs           parameters              description
# Class:    data.frame      NcdfOrMatrix    AnnotatedDataFrame      list                        
# Extends: "flowFrame"

# since I haven't decide whether and what to do with annotation, just use what's working now featureAnnotation



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