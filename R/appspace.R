




## helpers


## appspace

## a custom assignment function to put variabel from workspace to appspace

## an intuitive assignment wrapper function to put variabel from workspace (.GlobalEnv) to appspace (.AppEnv) for passing of variables between functions, simply assign it to appspace and easily retrive it back.

##' @export .AppEnv
.AppEnv=new.env()


##' @export appspace
appspace=structure(NA,class="appspace")


##' @export "[<-.appspace"
"[<-.appspace"=function(appspace,x,value){
    xname=deparse(substitute(x))
    assign(xname,value,.AppEnv)
    appspace
    
}

##' @export "[.appspace"
"[.appspace"=function(appsapce,x,value){
    xname=deparse(substitute(x))
    get(xname,envir=.AppEnv)
}


# > appspace[test]="new"
# > test
# Error: object 'test' not found
# > appspace[test]
# [1] "new"
# 
# > class(appspace[test])
# [1] "character"






