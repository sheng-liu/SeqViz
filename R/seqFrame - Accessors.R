# seqFrame - Accessors
# 
# 
###############################################################################

## generics

##'@exportMethod annotation<-
setGeneric(
    name="annotation<-",
    def=function(obj,value){
        standardGeneric("annotation<-")
    })

##'@exportMethod annotation
setGeneric(
    name="annotation",
    def=function(obj){
        standardGeneric("annotation")
    })



## methods

setReplaceMethod(
    f="annotation",
    signature="seqFrame",
    definition=function(obj,value){
        initialize(obj,annotation=value)    
    })

setMethod(
    f="annotation",
    signature="seqFrame",
    definition=function(obj){
        obj@annotation
    })


