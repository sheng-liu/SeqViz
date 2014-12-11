## 2Dplus
## 2d plot plus coloring with 3rd variable


library(ggplot2)

# p + geom_point(aes(x = Male.Het.H3K9me3, y = Male.KO.H3K9me3, color = Male.Het.H3K27me3), 
#                alpha = 1/2) + theme_classic()
# 
# 
# 
# load("~/DoScience/DoScience/Projects/PGC project/PGCs/Data/3-SeqData/BSseq/Data/2014-03-06/K9K27.gated.sp.rda")
# 
# names(K9K27.gated.sp)
# p = ggplot(K9K27.gated.sp)
# p + geom_point(aes(x = Male.Het.H3K9me3, y = Male.KO.H3K9me3, color = Male.Het.H3K27me3), 
#                alpha = 1/2) + theme_classic()
# 
# 
# xyplot(Male.KO.H3K9me3~Male.Het.H3K9me3,data=K9K27.gated.sp)
# 
# 
# xyplot(Male.KO.H3K9me3~Male.Het.H3K9me3,data=K9K27.gated.sp)


# p+geom_point(aes(x=Male.Het.H3K27me3,y=Male.KO.H3K27me3,color=Male.Het.H3K9me3))+
#     theme_classic()+
#     theme_classic()+scale_color_gradient(low = "black", high = "yellow")+
#     theme(legend.background=element_blank())

DDplus.plot=function(action,window){
    library(ggplot2)
    
    print("2D+ plot")
    
    select.channels(channelZ=T)
    
    x=appspace[channelX]
    y=appspace[channelY]
    z=appspace[channelZ]
    
    
    df=as.data.frame(exprs(appspace[active.SeqFrame]))
    p=ggplot(df)
    p=p+
        theme_classic()+
        scale_color_gradient(low = "black", high = "yellow")+
        theme(legend.background=element_blank())

    f=sprintf("p+geom_point(aes(x=`%s`,y=`%s`,color=`%s`))",x,y,z)
    # parse(text = f)
    
    dev.new(width=6,height=4)
    print(eval(parse(text=f)))

    
}


#     quote(x)
#     x=deparse(substitute(x))
#     y=deparse(substitute(y))
#     z=deparse(substitute(z))
#     
#     x=as.symbol(x)
#     y=as.symbol(y)
#     z=as.symbol(z)
#     
#     x=print(x,quote=F)
#     x=deparse(substitute(appspace[channelX]))
#     y=
# p+geom_point(aes(x="Male.Het.DNAme",y="Male.KO.DNAme",color="Male.KO.DNAme"))
# 
# 
# 
# 
# 
# p+geom_point(aes(x=Male.Het.H3K27me3,y=Male.KO.H3K27me3,color=Male.Het.H3K9me3))+
#     theme_classic()+
#     theme_classic()+scale_color_gradient(low = "black", high = "yellow")+
#     theme(legend.background=element_blank())+
#     theme(legend.position=c(0,1), legend.justification=c(0,0.9))


## add minor ticks
addMinorTicks=function(start,end,by){
    minor.ticks=seq(start,end,by=by)
    major.ticks=seq(start,end,by=by*2)
    blank.ticks=setdiff(minor.ticks,major.ticks)
    
    blank.index=c()
    for (i in 1:length(blank.ticks)){
        blank=which(minor.ticks==blank.ticks[i])
        blank.index=c(blank.index,blank)
    }
    
    minor.ticks.label=replace(minor.ticks,blank.index," ")
    return(minor.ticks.label)
}

# last_plot()+scale_y_continuous(breaks=c(seq(0,100,by=5)),labels=addMinorTicks(0,100,10))



# try use lattice to do the same, but only works on catogorical ones
# it will be multipannel though, kind of like ggplot2 


##FIXME:
## plot with different pannel for catogorical varaibles



# p <- ggplot(mtcars, aes(disp, mpg))
# p + geom_point() + facet_grid(.~gear)
# 
# f=sprintf("p+geom_point(aes(x=`%s`,y=`%s`))+facet_grid(.~`%s`)",x,y,z)
