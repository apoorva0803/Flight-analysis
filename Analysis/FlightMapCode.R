library(ggmap)
data.port <- read.csv('airports.csv',F)
data.line <- read.csv('routes.csv',F)
library(stringr)
# Find US airports
portinUS <- str_detect(data.port[,'V4'], "United States")
usport <- data.port[portinUS,]
usport <-usport[usport$V5!='',
                      c('V3','V5','V7','V8','V9')]
names(usport) <- c('city','code','lan','lon','att')
# Find US lines
lineinus <- (data.line[,'V3'] %in% usport$code) & (data.line[,'V5'] %in% usport$code)
usline <- data.line[lineinus,c('V3','V5','V9')]
names(usline) <- c('source','destination','equipment')

#Build a function which convert airport code to longitude and latitude
findposition <- function(code) {
  find <- usport$code==code
  x <- usport[find,'lon']
  y <- usport[find,'lan']
  return(data.frame(x,y))
}

# Convert airport code to longitude and latitude
from <- lapply(as.character(usline$source),findposition)
from <- do.call('rbind',from)
from$group <- 1:dim(from)[1]
names(from) <- c('lon','lan','group')

to <- lapply(as.character(usline$destination),findposition)
to <- do.call('rbind',to)
to$group <-1:dim(to)[1]
names(to) <-c('lon','lan','group')
data.line <- rbind(from,to)
temp<- data.line[data.line$group<100,]
# Use ggmap to get the map of US,apply previous data to the map
ggmap(get_googlemap(center = 'United States', zoom=4,
                    maptype='terrain'),extent='device')+
  geom_point(data=usport,aes(x=lon,y=lan),
             colour = 'darkgreen',alpha=0.5)+
  geom_line(data=data.line,aes(x=lon,y=lan,group=group),
            size=0.1,alpha=0.05,color='darkgreen')

