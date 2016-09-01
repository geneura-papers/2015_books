library(stringr)
library(reshape2)
library(ggplot2)


#Multiplot
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



filenames_all <- list.files(path = "datasets/AllAttributes/",pattern = ".csv",full.names = TRUE)
ldf <- lapply(filenames_all, read.csv, skip=0, header=TRUE, sep=";",blank.lines.skip = TRUE,strip.white = TRUE, colClasses = rep("character",7))
for(i in seq(from=1,to=length(filenames_all),by = 1)){
  ldf[[i]]$Dataset <- sapply(str_split(filenames_all, '/'), '[', 4)[i]
  ldf[[i]]$Attributes <- "All"
}


filenames_sel <- list.files(path = "datasets/ReliefFAttributeEval-FS//",pattern = ".csv",full.names = TRUE)
ldf_s <- lapply(filenames_sel, read.csv, skip=0, header=TRUE, sep=";",blank.lines.skip = TRUE,strip.white = TRUE, colClasses = rep("character",7))
for(i in seq(from=1,to=length(filenames_sel),by = 1)){
  ldf_s[[i]]$Dataset <- sapply(str_split(filenames_sel, '/'), '[', 5)[i]
  ldf_s[[i]]$Attributes <- "ReliefFAttributeEval-FS"
}

m_all <- do.call(rbind,ldf)
m_sel <- do.call(rbind,ldf_s)

m <- rbind(m_all,m_sel)

rm(m_all,m_sel,ldf,ldf_s,filenames_sel,filenames_all,i)

datos <- melt(data = m,id.vars = c("Method","Dataset","Attributes"),measure.vars = c("R2","MAE","RMSE","RAE","RRSE","TIME"),)

datos$Dataset <- str_replace(datos$Dataset,".csv","")

datos$error <- as.numeric(sapply(str_split(datos$value, '_'), '[', 2))
datos$value <- as.numeric(sapply(str_split(datos$value, '_'), '[', 1))

dataset = unique(datos$Dataset)

ggplot(data = subset(datos,Dataset==dataset[1]), aes(x=Attributes,y=value,color=Attributes,shape=Attributes)) + geom_point(stat="identity",size=3) + 
  facet_grid(variable ~ Method,scales = "free_y",) + 
  scale_color_grey(start=0.0,end=0.2) + labs(title=dataset[1],x="",y="") + 
  geom_errorbar(aes(ymin=value-error,ymax=value+error), width=0.2,size=0.25) + theme_bw()  + theme(legend.position="bottom", axis.title.x=element_blank(),
                                                                                                   axis.text.x=element_blank(),
                                                                                                   axis.ticks.x=element_blank())


ggplot(data = subset(datos,Dataset==dataset[1]), aes(x=Attributes,y=value,fill=Attributes,shape=Attributes)) + geom_bar(stat="identity") + 
  facet_grid(variable ~ Method,scales = "free_y",) + 
  scale_fill_grey(start=0.2,end=0.6) + labs(title=dataset[1],x="",y="") + 
  geom_errorbar(aes(ymin=value-error,ymax=value+error), width=0.5,size=0.5) + theme_bw()  + theme(legend.position="bottom", axis.title.x=element_blank(),
                                                                                                   axis.text.x=element_blank(),
                                                                                                   axis.ticks.x=element_blank())

# Publisher > Method > Metric > Attribute Selection

dataset = unique(datos$Dataset)

for(i in dataset){
  print(ggplot(data = subset(datos,Dataset==i), aes(x=Attributes,y=value,fill=Attributes,shape=Attributes)) + geom_bar(stat="identity") + 
    facet_grid(variable ~ Method,scales = "free_y") + 
    scale_fill_grey(start=0.2,end=0.6) + labs(title=i,x="",y="") + 
    geom_errorbar(aes(ymin=value-error,ymax=value+error), width=0.5,size=0.5) + theme_bw()  + theme(legend.position="bottom", axis.title.x=element_blank(),
                                                                                                    axis.text.x=element_blank(),
                                                                                             axis.ticks.x=element_blank()))
  ggsave(file=paste0("img_1_",i,".pdf"),scale=1.2)
  
}

# Attribute Selection > Publisher > Method > Metric

att = unique(datos$Attributes)

for(i in dataset){
  for(j in att){
  
print(ggplot(data = subset(datos,Attributes==j & Dataset==i), aes(x=Method,y=value,color=Attributes,shape=Attributes)) + geom_point(stat="identity",size=2) + 
  facet_grid(variable ~ Dataset,scales = "free_y") + 
  scale_color_grey(start=0.0,end=0.2) + labs(title=paste0(i," - ", j),x="",y="") + 
  geom_errorbar(aes(ymin=value-error,ymax=value+error), width=0.2,size=0.25) + theme_bw()  + theme(legend.position="none"))

  ggsave(file=paste0("img_2_",i,"-",j,".pdf"),scale=1.2)

}
}


p2 <- ggplot(data = subset(datos,Attributes==att[2]), aes(x=Method,y=value,color=Attributes,shape=Attributes)) + geom_point(stat="identity",size=2) + 
  facet_grid(variable ~ Dataset,scales = "free_y") + 
  scale_color_grey(start=0.0,end=0.2) + labs(title=att[2],x="",y="") + 
  geom_errorbar(aes(ymin=value-error,ymax=value+error), width=0.2,size=0.25) + theme_bw()  + theme(legend.position="none")

multiplot(p1,p2,cols=1)
















ggplot(data = datos, aes(x=variable,y=value)) + geom_point() + facet_grid(Attributes ~ Dataset)

ggplot(data = subset(datos,variable=="R2"), aes(x=Attributes,y=value,fill=Attributes)) + geom_bar(stat="identity",position="dodge") + facet_grid(variable ~ Dataset)

ggplot(data = subset(datos), aes(x=Attributes,y=value,fill=Attributes)) + geom_bar(stat="identity",position="dodge") + 
  facet_grid(variable ~ Dataset,scales = "free_y") + theme(legend.position="bottom") + scale_fill_brewer(type="qual",palette = "Set1")


dataset = unique(datos$Dataset)

ggplot(data = subset(datos,Dataset==dataset[1]), aes(x=Attributes,y=value,fill=Attributes)) + geom_bar(stat="identity",position="dodge") + 
  facet_grid(variable ~ Method,scales = "free_y") + theme(legend.position="bottom", axis.title.x=element_blank(),
                                                          axis.text.x=element_blank(),
                                                          axis.ticks.x=element_blank()) + 
  scale_fill_grey(start=0.2,end=0.8) + labs(title=dataset[1],x="",y="")



ggplot(data = subset(datos,Dataset==dataset[1]), aes(x=Attributes,y=value,fill=Attributes)) + geom_point(stat="identity",position="dodge") + 
  facet_grid(variable ~ Method,scales = "free_y") + theme(legend.position="bottom", axis.title.x=element_blank(),
                                                          axis.text.x=element_blank(),
                                                          axis.ticks.x=element_blank()) + 
  scale_fill_grey(start=0.2,end=0.8) + labs(title=dataset[1],x="",y="")



