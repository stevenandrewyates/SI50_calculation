# load the ggplot2 library to make some nice plots 
library(ggplot2) 

# load some data, here we take one example population 3 * 3. 
SIscore <- c(rep.int(0,35),rep.int(2,8),rep.int(3,1),rep.int(4,24),rep.int(5,2),rep.int(7,4),rep.int(9,5),rep.int(10,78)) 

# now increment all values by one to avoid division by 0 later (which doesn't work) 
SIscore <- SIscore + 1 

# now convert these to quantiles, every 0.01 (1%) 

td <- as.data.frame(cbind(seq(0, 1, 0.01),quantile(SIscore,seq(0, 1, 0.01)))) 
colnames(td) <- c("Quantile","SIscore") 

# now plot the data 
pdf("fig.1.pdf",width=5,height=4) 
ggplot(data=td,aes(x=Quantile,y=SIscore-1)) +geom_point(shape=4,colour="limegreen")+theme_bw() + geom_point(shape=4,colour="blue",data=td,aes(x=Quantile,y=SIscore)) + ylab("SI score") +geom_rect(aes(xmax=0.9,xmin=0.6,ymin=1.5,ymax=4.5),fill="white",colour="black") + annotate("text", x = 0.75, y = 2, label = "raw data",colour="limegreen")+ annotate("text", x = 0.75, y = 3, label = "incremented data",colour="blue") +  annotate("text", x = 0.75, y = 4, label = "Key",colour="black") 
dev.off() 

# Now we need to add values to ensure the lower asymptote is correct (it in's necessary in this example). To do this we add 100 values of 1 to the left 

ip <- cbind(seq(-1, 0, 0.01),1) 

colnames(ip) <- c("Quantile","SIscore") 

g <- rbind(ip,td) 

# Now we need to add values to ensure the upper asymptote is correct (it in's necessary in this example). To do this we add 100 values of 11 to the right 
ip <- cbind(seq(1, 2, 0.01),11) 
colnames(ip) <- c("Quantile","SIscore") 

g <- rbind(g,ip) 
# now plot the data 

pdf("fig.2.pdf",width=5,height=4) 
ggplot(data=g[102:202,],aes(x=Quantile,y=SIscore)) +geom_point(shape=4,colour="blue") + theme_bw() + geom_point(shape=4,colour="red",data=g[1:102,],aes(x=Quantile,y=SIscore)) + 
geom_point(shape=4,colour="red",data=g[203:303,],aes(x=Quantile,y=SIscore)) +  ylab("SI score") + 
geom_rect(aes(xmax=1,xmin=2,ymin=1.5,ymax=4.5),fill="white",colour="black") + annotate( "text", x = 1.5, y = 2, label = "added data",colour="red")+ annotate("text", x = 1.5, y = 3, label = "incremented data" , colour="blue") +  annotate("text", x = 1.5, y = 4, label = "Key", colour="black") 
dev.off() 

# now we can calculate the four-factor self starting model, to predict the 50% SI index 
model <- nls(SIscore~SSfpl(Quantile, a, b, c, d),data=g) 

summary(model) 

# now we predict the parts from the model and assign this back to the dataframe (without the fake upper and lower asymtotes) 
td$model <- predict(model)[102:202] 

# next we can calculate the SI50. More technically this is known as the inflection point from the four factor model, or xmid in R 
xmid <- summary(model)$coefficients[3,1] 

# now we need to calculate how good the model is? 
# to do this we extract the residuals sum of squares from the model, with the real data 
RSS.p <- sum(residuals(model)[102:202]^2) 

# next we need to the extract the total sum of squares 
TSS <- sum((SIscore - mean(SIscore))^2) 

# now we can calculate the R2 given the 
R2 <- round(1 - (RSS.p/TSS),2) 

# now we need to format some output for plotting the data 
eq <- substitute(italic(R)^2~"="~r2, list(r2 = R2)) 
lb1 <-     as.character(as.expression(eq)); 
XMID <- round(xmid,2) 
XMID <- paste("SI50 = ",XMID,sep="") 

# now plot the results, voila
pdf("fig.3.pdf",width=5,height=4) 
ggplot(data=td,aes(x=Quantile,y=SIscore-1)) +geom_point(shape=4,colour="limegreen")+theme_bw() +geom_line(aes(x=Quantile,y=model-1)) +geom_vline(xintercept =xmid, linetype = "longdash",colour="red") + 
annotate("text",label=lb1,y=10,x=0.15,parse=T) + 
annotate("text",label=XMID,y=9,x=0.15,colour="red") + 
ylab("SI score") + 
geom_rect(aes(xmax=0.9,xmin=0.6,ymin=0.5,ymax=4.5),fill="white",colour="black") + 
annotate("text", x = 0.75, y = 2, label = "raw data",colour="limegreen")+ 
annotate("text", x = 0.75, y = 3, label = "SI50",colour="red") +  
annotate("text", x = 0.75, y = 1, label = "predicted",colour="black") +  
annotate("text", x = 0.75, y = 4, label = "Key",colour="black") 
dev.off() 