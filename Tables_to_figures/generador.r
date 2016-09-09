library(stringr)
library(reshape2)
library(ggplot2)

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

filenames_som <- list.files(path = "datasets/SOM-FS//",pattern = ".csv",full.names = TRUE)
ldf_som <- lapply(filenames_som, read.csv, skip=0, header=TRUE, sep=";",blank.lines.skip = TRUE,strip.white = TRUE, colClasses = rep("character",7))
for(i in seq(from=1,to=length(filenames_som),by = 1)){
  ldf_som[[i]]$Dataset <- sapply(str_split(filenames_som, '/'), '[', 5)[i]
  ldf_som[[i]]$Attributes <- "SOM-FS"
}


filenames_cfs <- list.files(path = "datasets/CfsSubsetEval-FS//",pattern = ".csv",full.names = TRUE)
ldf_cfs <- lapply(filenames_cfs, read.csv, skip=0, header=TRUE, sep=";",blank.lines.skip = TRUE,strip.white = TRUE, colClasses = rep("character",7))
for(i in seq(from=1,to=length(filenames_cfs),by = 1)){
  ldf_cfs[[i]]$Dataset <- sapply(str_split(filenames_cfs, '/'), '[', 5)[i]
  ldf_cfs[[i]]$Attributes <- "CfsSubsetEval-FS"
}

m_all <- do.call(rbind,ldf)
m_sel <- do.call(rbind,ldf_s)
m_som <- do.call(rbind,ldf_som)
m_cfs <- do.call(rbind,ldf_cfs)

m <- rbind(m_all,m_sel)
m <- rbind(m,m_som)
m <- rbind(m,m_cfs)

rm(m_all,m_sel,m_som,m_cfs,ldf,ldf_s,ldf_som,ldf_cfs,filenames_sel,filenames_all,filenames_som,filenames_cfs,i)

datos <- melt(data = m,id.vars = c("Method","Dataset","Attributes"),measure.vars = c("R.","MAE","RMSE","RAE","RRSE","TIME"),)

datos$Dataset <- str_replace(datos$Dataset,".csv","")

datos$error <- as.numeric(sapply(str_split(datos$value, '_'), '[', 2))
datos$value <- as.numeric(sapply(str_split(datos$value, '_'), '[', 1))

datos$variable <- as.character(datos$variable)
datos[datos$variable=="R.","variable"] <- "R"
dataset = unique(datos$Dataset)

datos$Dataset <- factor(datos$Dataset,c("PublishingCo-UDL929","PublishingCo-UDLW12","PublishingCo-UDL704","PublishingCo-UDLU11"))

#ggplot(data = subset(datos,Dataset==dataset[1]), aes(x=Attributes,y=value,color=Attributes,shape=Attributes)) + geom_point(stat="identity",size=3) + 
#  facet_grid(variable ~ Method,scales = "free_y",labeller = as_labeller(expression()) + 
#  scale_color_grey(start=0.0,end=0.2) + labs(title=dataset[1],x="",y="") + 
#  geom_errorbar(aes(ymin=value-error,ymax=value+error), width=0.2,size=0.25) + theme_bw()  + theme(legend.position="bottom", axis.title.x=element_blank(),
#                                                                                                   axis.text.x=element_blank(),
#                                                                                                   axis.ticks.x=element_blank())


#ggplot(data = subset(datos,Dataset==dataset[1]), aes(x=Attributes,y=value,fill=Attributes,shape=Attributes)) + geom_bar(stat="identity") + 
#  facet_grid(variable ~ Method,scales = "free_y",) + 
#  scale_fill_grey(start=0.2,end=0.6) + labs(title=dataset[1],x="",y="") + 
#  geom_errorbar(aes(ymin=value-error,ymax=value+error), width=0.5,size=0.5) + theme_bw()  + theme(legend.position="bottom", axis.title.x=element_blank(),
#                                                                                                   axis.text.x=element_blank(),
#                                                                                                   axis.ticks.x=element_blank())

# Publisher > Method > Metric > Attribute Selection

dataset = unique(datos$Dataset)

for(i in dataset){
  print(ggplot(data = subset(datos,Dataset==i), aes(x=Attributes,y=value,fill=Attributes,shape=Attributes)) + geom_point(size=2) + geom_bar(stat="identity") + 
    facet_grid(variable ~ Method,scales = "free_y") + 
    scale_fill_grey(start=0.6,end=0.8) + labs(title=i,x="",y="") + 
    geom_errorbar(aes(ymin=value-error,ymax=value+error), width=0.5,size=0.5) + theme_bw()  + theme(legend.position="bottom", axis.title.x=element_blank(),
                                                                                                    axis.text.x=element_blank(),
                                                                                             axis.ticks.x=element_blank()))
  ggsave(file=paste0("../imgs/datasets_",i,".eps"),scale=2,units = "cm", width = 16, height = 8,dpi = 800)
  ggsave(file=paste0("../imgs/datasets_",i,".png"),scale=2,units = "cm", width = 16, height = 8,dpi = 800)
}

  
# Attribute Selection > Publisher > Method > Metric

att = unique(datos$Attributes)

#for(i in dataset){
  for(j in att){
  
print(ggplot(data = subset(datos,Attributes==j), aes(x=Method,y=value,fill=Method)) + 
  geom_point(stat="identity",size=2)  + geom_bar(stat="identity") + 
  facet_grid(variable ~ Dataset,scales = "free_y") + 
  scale_fill_grey(start=0.2,end=0.6) + labs(title=j,x="",y="") + 
  geom_errorbar(aes(ymin=value-error,ymax=value+error), width=0.2,size=0.25) + theme_bw()  + theme(legend.position="right") +
    scale_y_continuous(limits = c(0, NA))
  )
      

  #ggsave(file=paste0("../imgs/attribute_",j,".png"),scale=2,units = "cm", width = 14, height = 8,dpi = 800)
  #ggsave(file=paste0("../imgs/attribute_",j,".eps"),scale=2,units = "cm", width = 14, height = 8,dpi = 800)

  }

var = unique(datos$variable)

for(i in var){
  
  ggplot(data = subset(datos,variable==i), aes(x=Method,y=value)) + geom_point(stat="identity",size=3) +
    geom_bar(stat="identity",size=3) + 
    facet_grid(Dataset ~ Attributes,scales = "free_y") + 
    scale_fill_grey(start=0.2,end=0.8) + labs(title=i,x="Method",y="Seconds") + 
    geom_errorbar(aes(ymin=value-error,ymax=value+error), width=0.2,size=0.25) + theme_bw()  + theme(legend.position="bottom")
  
  
  ggsave(file=paste0("../imgs/metric_",i,".eps"),scale=2.5,units = "cm", width = 16, height = 8,dpi = 800)
  ggsave(file=paste0("../imgs/metric_",i,".png"),scale=2.5,units = "cm", width = 16, height = 8,dpi = 800)
}


#Figure to replace Table 3
all <- read.csv(file = "datasets/all.csv",skip=0, header=TRUE, sep=";",blank.lines.skip = TRUE,strip.white = TRUE, colClasses = rep("character",7))

all <- melt(data = all,id.vars = c("Method"),measure.vars = c("R.","MAE","RMSE","RAE","RRSE","TIME"))

all$error <- as.numeric(sapply(str_split(all$value, '_'), '[', 2))
all$value <- as.numeric(sapply(str_split(all$value, '_'), '[', 1))

all$variable <- as.character(all$variable)
all[all$variable=="R.","variable"] <- "R"



ggplot(data = subset(all), aes(x=Method,y=value)) + 
  geom_bar(aes(fill=Method),stat="identity",size=3) + geom_point(stat="identity",size=3) +
  facet_grid(variable ~ . ,scales = "free_y") + 
  scale_fill_grey(start=0.2,end=0.6) + labs(title="",x="",y="") + 
  geom_errorbar(aes(ymin=value-error,ymax=value+error), width=0.2,size=0.25) + theme_bw()  + theme(legend.position="right")


ggsave(file=paste0("../imgs/prediction_all_publisher_all_features_Table3.eps"),scale=2.5,units = "cm", width = 16, height = 8,dpi = 800)
ggsave(file=paste0("../imgs/prediction_all_publisher_all_features_Table3.png"),scale=2.5,units = "cm", width = 16, height = 8,dpi = 800)

#Reemplazo tabla 5


filenames_all_p <- list.files(path = "datasets/AllPublishers//",pattern = ".csv",full.names = TRUE)
ldf_p <- lapply(filenames_all_p, read.csv, skip=0, header=TRUE, sep=";",blank.lines.skip = TRUE,strip.white = TRUE, colClasses = rep("character",7))
for(i in seq(from=1,to=length(filenames_all_p),by = 1)){
  ldf_p[[i]]$Attributes <- sapply(str_split(filenames_all_p, '/'), '[', 5)[i]
  ldf_p[[i]]$Dataset <- "All Publisher"
}

m_p <- do.call(rbind,ldf_p)

rm(filenames_all_p)

datos <- melt(data = m_p,id.vars = c("Method","Attributes"),measure.vars = c("R.","MAE","RMSE","RAE","RRSE","TIME"),)

datos$Attributes <- str_replace(datos$Attributes,".csv","")

datos$error <- as.numeric(sapply(str_split(datos$value, '_'), '[', 2))
datos$value <- as.numeric(sapply(str_split(datos$value, '_'), '[', 1))

datos$variable <- as.character(datos$variable)
datos[datos$variable=="R.","variable"] <- "R"

ggplot(data = subset(datos), aes(x=Method,y=value)) + 
  geom_bar(aes(fill=Method),stat="identity",size=3) + geom_point(stat="identity",size=3) +
  facet_grid(variable ~ Attributes ,scales = "free_y") + 
  scale_fill_grey(start=0.2,end=0.6) + labs(title="",x="",y="") + 
  geom_errorbar(aes(ymin=value-error,ymax=value+error), width=0.2,size=0.25) + theme_bw()  + theme(legend.position="right")


ggsave(file=paste0("../imgs/prediction_all_publisher_Table5.eps"),scale=2.5,units = "cm", width = 16, height = 8,dpi = 800)
ggsave(file=paste0("../imgs/prediction_all_publisher_Table5.png"),scale=2.5,units = "cm", width = 16, height = 8,dpi = 800)



#}




# 
# p2 <- ggplot(data = subset(datos,Attributes==att[2]), aes(x=Method,y=value,color=Attributes,shape=Attributes)) + geom_point(stat="identity",size=2) + 
#   facet_grid(variable ~ Dataset,scales = "free_y") + 
#   scale_color_grey(start=0.0,end=0.2) + labs(title=att[2],x="",y="") + 
#   geom_errorbar(aes(ymin=value-error,ymax=value+error), width=0.2,size=0.25) + theme_bw()  + theme(legend.position="none")
# 
# multiplot(p1,p2,cols=1)
# 
# #Time dataset, selection, method
# 
# 
# ggplot(data = subset(datos,variable=="TIME"), aes(x=Method,y=value)) + geom_point(stat="identity",size=3) +
#   geom_bar(stat="identity",size=3) + 
#   facet_grid(Dataset ~ Attributes,scales = "free_y") + 
#   scale_fill_grey(start=0.2,end=0.8) + labs(title="TIME",x="Method",y="Seconds") + 
#   geom_errorbar(aes(ymin=value-error,ymax=value+error), width=0.2,size=0.25) + theme_bw()  + theme(legend.position="bottom")
#   
# 
# ggplot(data = subset(datos,variable=="MAE"), aes(x=Method,y=value)) + geom_bar(stat="identity",size=3) + 
#   facet_grid(Dataset ~ Attributes,scales = "free_y") + 
#   scale_fill_grey(start=0.2,end=0.8) + labs(title="MAE",x="Method",y="Units") + 
#   geom_errorbar(aes(ymin=value-error,ymax=value+error), width=0.2,size=0.25) + theme_bw()  + theme(legend.position="bottom")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ggplot(data = datos, aes(x=variable,y=value)) + geom_point() + facet_grid(Attributes ~ Dataset)
# 
# ggplot(data = subset(datos,variable=="R2"), aes(x=Attributes,y=value,fill=Attributes)) + geom_bar(stat="identity",position="dodge") + facet_grid(variable ~ Dataset)
# 
# ggplot(data = subset(datos), aes(x=Attributes,y=value,fill=Attributes)) + geom_bar(stat="identity",position="dodge") + 
#   facet_grid(variable ~ Dataset,scales = "free_y") + theme(legend.position="bottom") + scale_fill_brewer(type="qual",palette = "Set1")
# 
# 
# dataset = unique(datos$Dataset)
# 
# ggplot(data = subset(datos,Dataset==dataset[1]), aes(x=Attributes,y=value,fill=Attributes)) + geom_bar(stat="identity",position="dodge") + 
#   facet_grid(variable ~ Method,scales = "free_y") + theme(legend.position="bottom", axis.title.x=element_blank(),
#                                                           axis.text.x=element_blank(),
#                                                           axis.ticks.x=element_blank()) + 
#   scale_fill_grey(start=0.2,end=0.8) + labs(title=dataset[1],x="",y="")
# 
# 
# 
# ggplot(data = subset(datos,Dataset==dataset[1]), aes(x=Attributes,y=value,fill=Attributes)) + geom_point(stat="identity",position="dodge") + 
#   facet_grid(variable ~ Method,scales = "free_y") + theme(legend.position="bottom", axis.title.x=element_blank(),
#                                                           axis.text.x=element_blank(),
#                                                           axis.ticks.x=element_blank()) + 
#   scale_fill_grey(start=0.2,end=0.8) + labs(title=dataset[1],x="",y="")
# 
# 
# 

