library(tidyverse)
df <- read_csv("BSAI_specs.csv",na=c("","n/a","NA","UN","0*"))
names(df)
?pivot_longer
dfl <- df %>%  pivot_longer(cols=2:56,names_to="spparea",values_to="value")
dfl <- dfl %>% separate(YrType, c('year', 'type')) %>% 
  separate(spparea, c('area', 'stock'))

df <- read_csv("GOA_specs.csv",na=c("","n/a","NA","UN","0*"))

# get chapter order
dfl <- df %>%  pivot_longer(cols=2:130,names_to="spparea",values_to="value") %>% 
separate(YrType, c('year', 'type'),sep="([|])")  %>%  separate(spparea, c('stock','area'),sep="([|])") %>% 
  mutate(stock=fct_inorder(factor(stock)),
  area=(fct_inorder((area))  ),
  year=as.numeric(year))
dfl$area <- factor(dfl$area, levels=unique(dfl$area)[c(1:10,22,13:19,11,20,23,21,12)])

dfl %>% filter(type=="ABC",stock=="Pollock") %>% group_by(area) %>% summarise(sum(na.rm=TRUE,value))
dfl %>% filter(type=="ABC",stock!="Total",str_detect(area,"Total")|area=="GW" ) %>% group_by(year) %>% summarise(Sum=sum(na.rm=TRUE,value)) %>%  print(n=Inf)
dfl %>% filter(type=="ABC",stock!="Total",str_detect(area,"Total")|area=="GW" ) %>% group_by(year) %>% summarise(Sum=sum(na.rm=TRUE,value)) %>%  print(n=Inf)
dfl %>% filter(type=="ABC",year=="2021",str_detect(area,"Total")|area=="GW",value>0) %>% 
  group_by(stock,area) %>% summarise(sum=sum(na.rm=TRUE,value)) %>%  print(n=Inf)
dfl %>% filter(type=="ABC",year=="2021",value>0) %>% 
  group_by(stock,area) %>% summarise(sum=sum(na.rm=TRUE,value)) %>%  print(n=Inf)

library(ggthemes)
dfl %>% filter(value>0,year>1987,stock!="Total",str_detect(area,"Total")|area=="GW" ) %>% 
  group_by(type,year) %>% summarise(Sum=sum(na.rm=TRUE,value)) %>% print(n=Inf) %>%  
  ggplot(aes(x=year,y=Sum,color=type)) + geom_line(stat="identity",size=2) + theme_few() +
  ylim(c(0,1100000))

unique(dfl$stock)
dfl %>% 
    filter(value>0,year>1989,type=="ABC",stock!="Total",str_detect(area,"Total")|area=="GW" ) %>% 
  mutate(stock=ifelse(
  stock %in% c("Pollock","Pacific cod", "Sablefish","Arrowtooth Flounder" ),as.character(stock),"Other")) %>% 
  group_by(type,year,stock) %>% summarise(ABCs=sum(na.rm=TRUE,value)) %>% 
  ggplot(aes(x=year,y=ABCs,fill=stock)) + geom_area(stat="identity") + theme_few() +
  ylim(c(0,750000)) + ggtitle("Gulf of Alaska ABCs by main stocks") + ylab("ABCs (t)")
