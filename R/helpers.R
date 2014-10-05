## helpers

## df2sf

df2sf=function(df){
    #df=read.csv(dialog$getFilename())  # as.is=T,header=T
    sf.exprs=as.matrix(df)
    
    # construction of AnnotatedDataFrame 
    # must contain this five "name, desc, range, minRange and maxRange" 
    # to be able to plot right
    name=colnames(df)
    desc=colnames(df)  # add description for each channel
    range=ceiling(apply(df,2,max))
    minRange=rep(0,dim(df)[2])
    maxRange=ceiling(apply(df,2,max))
    anno.df=data.frame(name,desc,range,minRange,maxRange,
                       stringsAsFactors = FALSE)
    anno.adf=AnnotatedDataFrame(data=anno.df)
    
    #data.name=basename(dialog$getFilename())
    
    sf=new("seqFrame",
                     exprs=sf.exprs,
                     parameters=anno.adf,
                     description=list(),
                     featureAnnotation="")
}


## two important description (keyword)
## access description
# description(sf)$FILENAME=filename
# description(sf)$GUID=filename

# or use keyword method to access description
# keyword(sf)$FILENAME=filename
# keyword(sf)$GUID=filename


# access parameters
# pData(parameters(sf))





file2sf=function(file.path){
    df=read.csv(file=file.path,as.is=T,header=T)
    sf.exprs=as.matrix(df)
    
    # construction of AnnotatedDataFrame 
    # must contain this five "name, desc, range, minRange and maxRange" 
    # to be able to plot right
    name=colnames(df)
    desc=colnames(df)  # add description for each channel
    range=ceiling(apply(df,2,max))
    minRange=rep(0,dim(df)[2])
    maxRange=ceiling(apply(df,2,max))
    anno.df=data.frame(name,desc,range,minRange,maxRange,
                       stringsAsFactors = FALSE)
    anno.adf=AnnotatedDataFrame(data=anno.df)
    
    data.name=basename(file.path)
    
    sf=new("seqFrame",
           exprs=sf.exprs,
           parameters=anno.adf,
           description=list(FILENAME=data.name,GUID=data.name),
           featureAnnotation=data.frame())
    return(sf)
}






