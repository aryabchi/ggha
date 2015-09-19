library(plspm)
library(ggplot2)

#load data sample
df<-read.csv("sample_20140324.csv", row.names=1)
#print(head(df))

#inner model spec
Contribution_opportunities<-c(0,0,0,0)
Development_activity<-c(0,0,0,0)
Owner_status<-c(0,0,0,0)
Contribution_attractiveness<-c(1,1,1,0)

path<-rbind(Contribution_opportunities,Development_activity,Owner_status,Contribution_attractiveness)

colnames(path) <- rownames(path)

print(path)

#blocks of indicators for latent variables
blocks<-list(c("issues","comments"),c("commits","contributors"),c("followers"),c("subscribers","stargazers","forks","pull_requests"))

#reflective  modes
modes<-rep("A",4)

#run analysis
pls<-plspm(df,path,blocks,modes=modes,boot.val=T,br=3000)
print(summary(pls))

#descriptive stats
print(summary(df))

#histograms
names<-colnames(df)
for(i in 1:length(names)){
	fname<-paste(names[i],".png",sep="")
	png(filename = fname , width = 120, height = 80, bg = "transparent")
	par(mar=c(0,0,0,0),yaxt="n",xaxt="n")
	hist(log(df[,names[i]]+1), probability = T, col = "black", border = "white", breaks = 10, xlab = "", ylab="", axes = F, main = NULL)
	dev.off()
}
