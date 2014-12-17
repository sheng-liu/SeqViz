## helpers


# return which count method is currently selected through radio button
##' @export selected.method
selected.method=function(widget,window){
    # if checkButton's "active, return their name to a vector
    check.list=lapply(appspace[count.method.list],function(x) x["active"])
    #count.method.list[[2]]["active"]
    checked=unlist(check.list)
    count.method.name=names(checked)[checked==T]
    print(count.method.name)
    return(count.method.name)
}

## the best way is to respond to signal when selection changed
## so far didn't see which is the signal


# sample
# if the file has more than 50k rows, sample 50k

##' @export sample.df
sample.df=function(df,size){
    id=dim(df)[1]
    id.sp=sample(id,size,replace=F)
    df=df[id.sp,]
    return(df)
}

