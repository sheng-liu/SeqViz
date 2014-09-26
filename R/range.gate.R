

## range gate
## rectagle gate with 2 dimension
## drawing a guided line when mouse hover graph is not easy in R

## can use ggobi is not available for R3 yet
## can use slider to position the line, which can do the work
## or simply ask user to input 


range.gate=function(action, window){
    
    print("rangeGate")
    
    
    
    ## check.channels need to be select channels only one dimension
    
    
    # check.channels()
    # this may need to modify into a radio buttons
    
    # select.channels(channelY=F)
    # this worked
    # a better way is to use radio button, as it is easy to see all the channels
    
    check.channels(multiple=F)
    
    
    
    
    
    ## data
    model=appspace[active.view]$getModel()
    selected.node=selected.node(appspace[active.view])
    selected.frame=get(x=selected.node,envir=.AppEnv)
    keyword(selected.frame,"GUID")
    dat=selected.frame
    
    
    ## let's first deal with there is only one channel selected
    checked.channels.name=appspace[checked.channels]
    
    #checked.channels.name=appspace[channelX]
    # this works
    
    channels=paste(checked.channels.name,collapse="`+`")
    
    
    
    ## layout
    
    #     plot(density(data.frame(exprs(dat))$Male.Het.H3K9me3),col="red")
    #     > par("usr")
    #     [1] -0.72945141 14.67859603 -0.04430655  1.15203936
    #     
    #     dev.new()
    #     flowViz::densityplot(~`Male.Het.H3K27me3`,data =dat)
    #     > par("usr")
    #     [1] 0 1 0 1
    #     
    #     densityplot(~`Male.Het.H3K27me3`,data =dat)
    #     > par("usr")
    #     [1] 0 1 0 1
    
    ## somehow flowViz density plot user coordinates is not detectable
    ## however it works well when do flowPlot, so maybe also works xyplot
    ## but not density plot
    
    
    # use base graphics densityplot, as here doesnt need to put three together
    # but multiple at the same time if later need
    
    #plot(density(data.frame(exprs(dat))$Male.Het.H3K9me3,kernel="cosine"),col="red",main="",xlab="Male.Het.H3K9me3")
    
    #plot(density(data.frame(exprs(dat))$Male.Het.H3K9me3),col="red",main="",xlab="Male.Het.H3K9me3")
    
    
    
    df=data.frame(exprs(dat))
    
    
    
    
    
    
    
    
    
    # not working
    #f=sprintf("density(x=df[,'%s']),col='red',main='',xlab='%s'",channels,channels)
    
    
    
    f=sprintf("plot(density(x=df[,'%s']),col='red',main='',xlab='%s')",channels,channels)
    #parse(text=f)
    #eval(parse(text=f))
    
    
    
    
    
    ## lattice
    #f=sprintf("densityplot(df[,'%s'],plot.points = FALSE)",channels)
    #parse(text=f)
    #plot(eval(parse(text=f)))
    
    # this is the lattice version, and it looks exactly as in flowPlate
    ## however its par("usr") is not detectable either
    
    #densityplot(~Male.Het.H3K9me3,data=df,plot.points = FALSE)
    # > par("usr")
    # [1] 0 1 0 1
    
    
    
    #f=sprintf("densityplot(~`%s`,data =dat,margin=F)",channels)
    
    #options(device="X11") 
    #dev.new(width=4,height=3)
    
    
    xWindow=X11()
    plot.new()
    # this plot.new is required as plot(eval(parse(text = f))) 
    # is not recoginized as a plot when put inside mouse event handlers
    
    #plot(eval(parse(text = f)))
    #     > par("usr")
    #     [1] 0 1 0 1
    eval(parse(text=f))
    
    mousedown <- function(buttons, x, y) {
        
        x.cord=grconvertX(x,from="ndc",to="user")
        y.cord=grconvertY(y,from="ndc",to="user")
        
        #plot(eval(parse(text = f))) 
        #densityplot(~Male.Het.H3K9me3,data=df,plot.points = FALSE)
        eval(parse(text=f))
        abline(v=x.cord,col="cornflowerblue")
        
        cat("Buttons ", paste(buttons, collapse=" "), " at ", x, y, "\n")
        cat("convert ", paste(buttons, collapse=" "), " at ", x.cord, y.cord, "\n")
        appspace[x.cord.ini]=x.cord
        NULL
    }
    
    
    
    
    #     mousedown <- function(buttons, x, y) {
    #         if(buttons==0){
    #             x.cord=grconvertX(x,from="ndc",to="user")
    #             y.cord=grconvertY(y,from="ndc",to="user")
    #             
    #             #plot(eval(parse(text = f)))
    #             eval(parse(text=f))
    #             abline(v=x.cord,col="blue")
    #             
    #             cat("Buttons ", paste(buttons, collapse=" "), " at ", x, y, "\n")
    #             cat("convert ", paste(buttons, collapse=" "), " at ", x.cord, y.cord, "\n")
    #             appspace[x.cord.ini]=x.cord
    #             NULL
    #         }else "Done"
    #     }
    
    
    # mousedown <- function(buttons, x, y) {
    #     switch(buttons,
    #            `0`={
    #                x.cord=grconvertX(x,from="ndc",to="user")
    #                y.cord=grconvertY(y,from="ndc",to="user")
    #                
    #                plot(eval(parse(text = f))) 
    #                abline(v=x.cord,col="blue")
    #                
    #                cat("Buttons ", paste(buttons, collapse=" "), " at ", x, y, "\n")
    #                cat("convert ", paste(buttons, collapse=" "), " at ", x.cord, y.cord, "\n")
    #                appspace[x.cord.ini]=x.cord
    #                NULL
    #            },
    #            `2`="Done")
    # }
    
    #     if(buttons==0){
    #         x.cord=grconvertX(x,from="ndc",to="user")
    #         y.cord=grconvertY(y,from="ndc",to="user")
    #         
    #         plot(eval(parse(text = f))) 
    #         abline(v=x.cord,col="blue")
    #         
    #         cat("Buttons ", paste(buttons, collapse=" "), " at ", x, y, "\n")
    #         cat("convert ", paste(buttons, collapse=" "), " at ", x.cord, y.cord, "\n")
    #         appspace[x.cord.ini]=x.cord
    #         NULL
    #     }else "Done"
    # }
    
    
    mousemove=function(buttons,x,y){
        
        #plot(eval(parse(text = f)))
        eval(parse(text=f))
        x.cord=grconvertX(x,from="ndc",to="user")
        y.cord=grconvertY(y,from="ndc",to="user")
        
        # this draws start, and drag to the end
        abline(v=appspace[x.cord.ini],col="cornflowerblue")
        abline(v=x.cord,col="limegreen")
        # draw a line segment connects the two lines
        segments(x0=appspace[x.cord.ini],y0=y.cord,x1=x.cord,y1=y.cord,col="gray")
        
        appspace[x.cord.end]=x.cord
        
        cat("moves ", paste(buttons, collapse=" "), " at ", x, y, "\n")
        cat("convert ", paste(buttons, collapse=" "), " at ", x.cord, y.cord, "\n")
        NULL
        
    }
    
    keybd <- function(key) {
        cat("Key <", key, ">\n", sep = "")
        #if (key=="ctrl-Q") "Done"
        
        #if (key=="ctrl-S") dev.copy2pdf(file = "table.2.pdf")
        if (key=="ctrl-S") {
            save_PDF(window=xWindow)
            "Done"
        }
        
    }
    
    getGraphicsEvent("Click and drag to draw rangeGate",
                     onMouseDown = mousedown,
                     onMouseMove = mousemove,
                     onKeybd = keybd) 
    
    
    
    
    
    # getGraphicsEvent("Click and drag to draw rangeGate",
    #                  onMouseDown = mousedown) 
    #     
    #     grconvertX(appspace[x.cord.ini],from="user",to="nic")
    
    
    ## data
    ## data manipulation for all gate seems a lot similar
    ## maybe can put into a function
    ## workflow maybe dealing with that
    
    # a simpler way to construct this matrix
    # mx=cbind("FSC-H"=c(200,600),"SSC-H"=c(0,400))
    
    # use matrix as input, can also use list
    param=cbind(c(appspace[x.cord.ini],appspace[x.cord.end]))
    colnames(param)=channels
    
    
    #rangeGate=rectangleGate(.gate=param,filterID="rectangleGate")
    rangeGate=rectangleGate(.gate=param,filterID="rangeGate")
    rangeGate.filter=filter(appspace[active.seqFrame],rangeGate)
    
    ## filter box need to be a list, to store all filter in it.
    ## then how does one distinguish the first quad gate vs the last?
    appspace[filterBox]=rangeGate.filter
    
    print(summary(rangeGate.filter))
    
    appspace[rangeGate.split]=split(appspace[active.seqFrame],rangeGate)
    
    child.node.name=sapply(appspace[rangeGate.split],function(frames){
        keyword(frames)$GUID  
    })
    
    ## add the veggi name for now
    for (i in 1:length(child.node.name)) keyword(appspace[rangeGate.split][[i]])$VEGGI.NAME=child.node.name[i]
    
    
    sapply(seq_along(child.node.name),function(i){
        assign(child.node.name[i],value=appspace[rangeGate.split][[i]],envir=.AppEnv) })
    
    insert.node(node.name=child.node.name,parent=appspace[active.view],loc="insert")
    
    ## adjust save_csv to be dynamic to selected.node
    selected.node=selected.node(appspace[active.view])
    appspace[save_csv]=get(selected.node,envir=.AppEnv)
    
    
    
}

## ToDo
# if select one channel, plot 1, if select 3, plot 3, and ask
## need a place to put all the gate so people can combine them
## leave combine gate to next version
## now just do one gate at a time then combine



# can draw a line segment in th middle of this two line
#rect()



