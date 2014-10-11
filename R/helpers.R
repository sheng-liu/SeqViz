## helpers

## df2sf

## one need to select the parameter column to be output
## the annotation column can be done automaticly. 

## "chr","start","end","strand","symbol"
## "readcounts", coverage, percent methylation 


# file="/Users/shengliu/DoScience/DoScience/Projects/Clover/oocytes methylation /DNAme/NGO-GVO-methylation.csv"
# 
# df=read.csv(file=file,as.is=T,header=T)
# keyword=list(FILENAME=data.name,GUID=data.name)
## keyword is a list 
##' @export df2sf
df2sf=function(df,keyword=NULL){
    
    # use df as FILENAME if FILENAME is not supplied
    df.name=deparse(substitute(df))
    # note once df value changed, substitute(df) will produce the new df value instead of its origianl name (promised object)
    
    # add strand if there isn't 
    cln=colnames(df)
    strand= if (length(which(cln=="strand"))!=0) 
        df$strand else rep("*",dim(df)[1]) 
    
    # add ID column
    id=row.names(df)
    df=cbind(df,id)
    cln=colnames(df)
    
    # subset annotation with exprs
    conserved.n=c("chr","start","end","strand","symbol")
    exprs.n=setdiff(cln,conserved.n)
    exprs.n=exprs.n[complete.cases(exprs.n)]
    exprs.mx=sapply(exprs.n,function(x){df[[x]]})  # matrix
    #sf.exprs=as.matrix(exprs.df)
    
    # annotation
    anno.n=c(conserved.n,"id")
    anno.n=anno.n[complete.cases(anno.n)]
    anno.mx=sapply(anno.n,function(x){df[[x]]})
    anno.df=as.data.frame(anno.mx,stringAsFactors=F) # matrix, and it is still factor
    anno.df=transform(anno.df,
                      id=as.integer(as.character(id)),
                      start=as.integer(as.character(start)),
                      end=as.integer(as.character(end)),
                      symbol=as.character(symbol),
                      chr=as.character(chr)
    )
    
    # GRanges form for future use if necessary
    # sf.anno=df2gr(anno.df)  
    
    
    # construction of AnnotatedDataFrame 
    # must contain this five "name, desc, range, minRange and maxRange" 
    # to be able to plot right
    name=colnames(exprs.mx)
    desc=colnames(exprs.mx)  # add description for each channel
    range=ceiling(apply(exprs.mx,2,max))
    minRange=rep(0,dim(exprs.mx)[2])
    maxRange=ceiling(apply(exprs.mx,2,max))
    parameters.adf=data.frame(name,desc,range,minRange,maxRange,
                              stringsAsFactors = FALSE)
    parameters.adf=AnnotatedDataFrame(data=parameters.adf)
    
    
    if(is.null(keyword)) keyword=list(FILENAME=data.name,GUID=data.name)
    
    sf=new("seqFrame",
           exprs=exprs.mx,
           parameters=parameters.adf,
           description=keyword,
           annotation=anno.df)  #sf.anno
    return(sf)
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

##------------------------------------------------------------------------------
##' @export file2sf
file2sf=function(file){
    df=read.csv(file=file,as.is=T,header=T)
    data.name=basename(file)
    keyword=list(FILENAME=data.name,GUID=data.name)
    df2sf(df,keyword)
}


##------------------------------------------------------------------------------
##' @export veggie
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

##------------------------------------------------------------------------------
##' @export seqFrame.table
seqFrame.table=function(sf){
    merge(annotation(sf),exprs(sf),by="id") 
}




