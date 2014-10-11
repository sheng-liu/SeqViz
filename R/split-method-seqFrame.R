## split method on seqFrame

## dispatch split on seqFrame
## what it does is split as flowFrame then add annotation and reconstruct

# the splited seqFrame remain seqFrame instead of flowFrame (even though the print method shows it as flowFrame)
## its annotation also gets inherited but not splited
## @export split.seqFrame
# setMethod(
#     f="split",
#     signature=c(x="seqFrame",f="logicalFilterResult"),
#     definition=function(x,f,drop=FALSE,...){
#         
#         # split
#         list=flowCore::split(x,f)
#         # list=split(x,f)
#         
#         # merge exprs with its own annotation
#         sf.list=lapply(list,function(frame){
#             df=merge(exprs(frame),annotation(frame),by="id")
#             cat("Construct seqFrame\n")
#             sf=df2sf(df,keyword(frame))
#         })
#         
#         return(sf.list)
#     })

# Error during wrapup: evaluation nested too deeply: infinite recursion / options(expressions=)?
# Error in loadMethod(function (x, f, drop = FALSE, ...)  : 
# error in evaluating the argument 'method' in selecting a method for function 'loadMethod': evaluation nested too deeply: infinite recursion / options(expressions=)?

# S4 method cause infiite loop, as uses split inside of the split 
# use S3 method instead, TODO, define a S4 split disptach on seqFrame

# Error: segfault from C stack overflow


# split.seqFrame=function(x,f,drop=FALSE,...){
#     # split
#     list=flowCore::split(x,f)
#     #class(x)="flowFrame"
#     #list=split(x,f)
#     # merge exprs with its own annotation
#     sf.list=lapply(list,function(frame){
#         df=merge(exprs(frame),annotation(frame),by="id")
#         cat("Construct seqFrame\n")
#         sf=df2sf(df,keyword(frame))
#     })
#     return(sf.list)
# }



# it issue a note, then it is fine. 

# Note: method with signature ‘flowFrame#logicalFilterResult’ chosen for function ‘split’,
# target signature ‘seqFrame#logicalFilterResult’.
# "seqFrame#ANY" would also be valid
# 
# Function: split (package base)
# x="ANY", f="ANY"
# x="ANY", f="Vector"
# x="flowFrame", f="ANY"
# x="flowFrame", f="character"
# x="flowFrame", f="factor"
# x="flowFrame", f="filter"
# x="flowFrame", f="filterSet"
# x="flowFrame", f="logicalFilterResult"
# x="flowFrame", f="manyFilterResult"
# x="flowFrame", f="multipleFilterResult"
# x="flowFrame", f="numeric"
# x="flowSet", f="ANY"
# x="flowSet", f="character"
# x="flowSet", f="factor"
# x="flowSet", f="filter"
# x="flowSet", f="filterResult"
# x="flowSet", f="filterResultList"
# x="flowSet", f="list"
# x="flowSet", f="numeric"
# x="list", f="Vector"
# x="ncdfFlowList", f="character"
# x="ncdfFlowList", f="factor"
# x="ncdfFlowSet", f="character"
# x="ncdfFlowSet", f="factor"
# x="ncdfFlowSet", f="filter"
# x="ncdfFlowSet", f="filterResultList"
# x="ncdfFlowSet", f="list"
# x="RangedData", f="ANY"

# x="seqFrame", f="ANY"
# x="seqFrame", f="logicalFilterResult"
# (inherited from: x="flowFrame", f="logicalFilterResult")
# x="seqFrame", f="missing"
# (inherited from: x="seqFrame", f="ANY")

# x="SummarizedExperiment", f="ANY"
# x="UCSCData", f="ANY"
# x="UCSCData", f="Vector"
# x="Vector", f="ANY"
# x="Vector", f="Vector"

## try purely flowCore then convert to seqFrame
##' @export split
setMethod(
    f="split",
    signature=c(x="seqFrame",f="logicalFilterResult"),
    definition=function(x,f,drop=FALSE,...){
        
        annotation=annotation(x)
        class(x)="flowFrame"
        # split as flowFrame
        list=flowCore::split(x,f)

        # change flowFrame to seqFrame
#         sf.list=lapply(list,function(frame){
#             flowFrame2seqFrame(frame,annotation)
#         })
        
        # merge exprs with its own annotation
        sf.list=lapply(list,function(frame){
            df=merge(exprs(frame),annotation,by="id")
            cat("Construct seqFrame\n")
            sf=df2sf(df,keyword(frame))
        })
        
        return(sf.list)
    })

