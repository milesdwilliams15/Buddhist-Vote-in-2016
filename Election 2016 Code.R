
## After downloading the data from dataverse.harvard.edu open the dataset
cces<-x # rename the data file.

library(dplyr)

library(car)

library(ggplot2)

library(extrafont)

library(weights)

cces$black <- Recode(cces$race, "2=1; else=0")

## Vote 2016 

cces$vote16 <- Recode(cces$CC16_364c, "1=1; 2=2; 3=3; 4=4; else=0")

cces$vote16 <- as.numeric(cces$vote16)

cces <- filter(cces, vote16 >0)

cces$vote16<-Recode(cces$vote16,"1='Donald Trump';
                    2='Hillary Clinton';
                    3='Gary Johnson';
                    4='Jill Stein'",
                    as.factor=TRUE)

## Evangelical

cces$evanbaptist <- Recode(cces$religpew_baptist, "1=1; 5:90=1; else=0")
cces$evanmeth <- Recode(cces$religpew_methodist, "2=1; else=0")
cces$evannd <- Recode(cces$religpew_nondenom, "1:90=1; else=0")
cces$evanluth <- Recode(cces$religpew_lutheran, "2:3=1; else=0")
cces$evanpres <- Recode(cces$religpew_presby, "6=1; else=0")
cces$pente <- Recode(cces$religpew_pentecost, "1:90=1; else=0")
cces$evanchrist <- Recode(cces$religpew_christian, "1=1; 3:4=1; else=0")
cces$evancong <- Recode(cces$religpew_congreg, "2=1; else=0")
cces$evanholy <- Recode(cces$religpew_holiness, "1:90=1; else=0")
cces$evanadvent <- Recode(cces$religpew_advent, "1:90=1; else=0")

evangelical <- filter(cces, evanbaptist == 1 | evanmeth == 1 | evannd == 1 | evanluth == 1 | evanpres == 1 | pente == 1 | evanchrist == 1 | evancong == 1 | evanholy == 1 | evanadvent ==1)
evangelical <- filter(evangelical, black !=1)

cces$evangelical <- cces$evanbaptist + cces$evanmeth + cces$evannd + cces$evanluth + cces$evanpres + cces$pente + cces$evanchrist + cces$evancong + cces$evanholy + cces$evanadvent
cces$evangelical <- Recode(cces$evangelical, "1:4=1; else=0")

## Mainline

cces$mlbaptist <- Recode(cces$religpew_baptist, "2=1; 4=1; else=0")
cces$mlmeth <- Recode(cces$religpew_methodist, "1=1; 90=1; else=0")
cces$mlluth <- Recode(cces$religpew_lutheran, "1=1; 4=1; else=0")
cces$mlpres <- Recode(cces$religpew_presby, "1:5=1; 90=1; else=0")
cces$mlchrist <- Recode(cces$religpew_christian, "2=1; else=0")
cces$mlcong <- Recode(cces$religpew_congreg, "1=1; 3=1; else=0")
cces$mlreform <- Recode(cces$religpew_reformed, "1:90=1; else=0")
cces$episp <- Recode(cces$religpew_episcop, "1:90=1; else=0")

cces$mainline <- cces$mlbaptist + cces$mlmeth + cces$mlluth + cces$mlpres + cces$mlchrist + cces$mlcong + cces$mlreform + cces$episp

## Black Protestant

bprot <- filter(cces, black ==1 & religpew ==1)



## Catholic 
cces$catholic <- Recode(cces$religpew_catholic, "1:90=1; else=0")

## Mormon
cces$mormon <- Recode(cces$religpew_mormon, "1:90=1; else=0")

## Jewish
cces$jewish <- Recode(cces$religpew, "5=1; else=0")

## Muslim 
cces$muslim <- Recode(cces$religpew, "6=1; else=0")

## Buddhist
cces$buddhist <- Recode(cces$religpew, "7=1; else=0")

## Hindus
cces$hindu <- Recode(cces$religpew, "8=1; else=0")

## No Religion 
cces$atheist <- Recode(cces$religpew, "9:10=1; else=0")


## Evangelical
evangelical <- filter(cces, evangelical == 1)
wpct(evangelical$vote16, evangelical$commonweight_post)

candidate <- c("Donald Trump", "Hillary Clinton" , "Gary Johnson" , "Jill Stein")
count <- c(58.5, 36.2, 4.3, 1 )
count <- as.numeric(count)
evan <- cbind(candidate, count)
evan <- as.data.frame(evan)
evan$count <- c(58.5, 36.2, 4.3, 1)
evan$candidate <- factor(evan$candidate, levels=unique(evan$candidate))
evan$tradition <- c("Evangelical")

colors <- c("firebrick1","dodgerblue3", "goldenrod1", "forestgreen")
windows()
ggplot(evan, aes(x= candidate, y = count)) + theme_minimal() + 
  geom_col(fill = colors, colour = "black") +
  scale_y_continuous(labels=scales::percent) +
  ggtitle("Vote Choice Among Evangelicals (w/Weights)",
          subtitle="2016 U.S. Presidential Election") + xlab("") + 
  ylab("") +
  theme(text=element_text(family="serif",size=14)) +
  theme(plot.title = element_text(hjust = 0, size=16,face="bold")) +
  theme(plot.subtitle=element_text(hjust=0, size=14,face="italic")) +
  theme(panel.grid.major.y=element_line(colour="grey50",linetype=3)) +
  theme(panel.grid.minor.y=element_blank()) +
  theme(panel.grid.major.x=element_blank())

## Black Protestant

bprot <- filter(cces, black ==1 & religpew ==1)
wpct(bprot$vote16, bprot$commonweight_post)

candidate <- c("Donald Trump", "Hillary Clinton" , "Gary Johnson" , "Jill Stein")
count <- c(6, 91.5, 1.6, 1 )
count <- as.numeric(count)
bprot <- cbind(candidate, count)
bprot <- as.data.frame(bprot)
bprot$count <- c(6, 91.5, 1.6, 1)
bprot$candidate <- factor(bprot$candidate, levels=unique(bprot$candidate))
bprot$tradition <- c("Black Protestant")

## Mormons 

mormon <- filter(cces, mormon ==1)
wpct(mormon$vote16, mormon$commonweight_post)

candidate <- c("Donald Trump", "Hillary Clinton" , "Gary Johnson" , "Evan McMullin", "Jill Stein")
count <- c(45, 26, 13.6, 12.9, 2.5 )
count <- as.numeric(count)
mormon <- cbind(candidate, count)
mormon <- as.data.frame(mormon)
mormon$count <- c(45, 26, 13.6, 12.9, 2.5 )
mormon$candidate <- factor(mormon$candidate, levels=unique(mormon$candidate))
mormon$tradition <- c("Mormon")

colors <- c("firebrick1","dodgerblue3", "goldenrod1", "darkgrey", "forestgreen")
windows()
ggplot(mormon, aes(x= candidate, y = count)) + geom_col(fill = colors, colour = "black") + 
  ggtitle("Vote Choice Among Mormons") + 
  xlab("Candidate") + ylab("Percent of Votes Cast") +
  theme(text=element_text(size=18, family="KerkisSans")) +
  theme(plot.title = element_text(hjust = 0.5))

## Mainline
mainline <- filter(cces, mainline ==1)
wpct(mainline$vote16, mainline$commonweight_post)

candidate <- c("Donald Trump", "Hillary Clinton" , "Gary Johnson" , "Jill Stein")
count <- c(50, 44.8, 4, 1 )
count <- as.numeric(count)
ml <- cbind(candidate, count)
ml <- as.data.frame(ml)
ml$count <- c(50, 44.8, 4, 1 )
ml$candidate <- factor(ml$candidate, levels=unique(ml$candidate))
ml$tradition <- c("Mainline")

## Jewish
jewish <- filter(cces, jewish ==1)
wpct(jewish$vote16, jewish$commonweight_post)

candidate <- c("Donald Trump", "Hillary Clinton" , "Gary Johnson" , "Jill Stein")
count <- c(26, 70, 2.5, 1 )
count <- as.numeric(count)
jewish <- cbind(candidate, count)
jewish <- as.data.frame(jewish)
jewish$count <- c(26, 70, 2.5, 1  )
jewish$candidate <- factor(jewish$candidate, levels=unique(jewish$candidate))
jewish$tradition <- c("Jewish")


## Catholic
catholic <- filter(cces, catholic ==1)
wpct(catholic$vote16, catholic$commonweight_post)

candidate <- c("Donald Trump", "Hillary Clinton" , "Gary Johnson" , "Jill Stein")
count <- c(45.5, 49.4, 4, 1 )
count <- as.numeric(count)
catholic <- cbind(candidate, count)
catholic <- as.data.frame(catholic)
catholic$count <- c(45.5, 49.4, 4, 1  )
catholic$candidate <- factor(catholic$candidate, levels=unique(catholic$candidate))
catholic$tradition <- c("Catholic")

## Muslim
muslim <- filter(cces, muslim ==1)
wpct(muslim$vote16, muslim$commonweight_post)

candidate <- c("Donald Trump", "Hillary Clinton" , "Gary Johnson" , "Jill Stein")
count <- c(11.3, 82.7, 2.3, 3.5 )
count <- as.numeric(count)
muslim <- cbind(candidate, count)
muslim <- as.data.frame(muslim)
muslim$count <- c(11.3, 82.7, 2.3, 3.5 )
muslim$candidate <- factor(muslim$candidate, levels=unique(muslim$candidate))
muslim$tradition <- c("Muslim")

## Atheist
atheist <- filter(cces, atheist ==1)
wpct(atheist$vote16, atheist$commonweight_post)

candidate <- c("Donald Trump", "Hillary Clinton" , "Gary Johnson" , "Jill Stein")
count <- c(17.6, 71, 6.7, 4.6 )
count <- as.numeric(count)
atheist <- cbind(candidate, count)
atheist <- as.data.frame(atheist)
atheist$count <- c(17.6, 71, 6.7, 4.6 )
atheist$candidate <- factor(atheist$candidate, levels=unique(atheist$candidate))
atheist$tradition <- c("Atheist")

## Buddhist
candidate <- c("Donald Trump", "Hillary Clinton" , "Gary Johnson" , "Jill Stein")
count <- c(24.3, 63.3, 3.1, 9.1 )
count <- as.numeric(count)
buddhist <- cbind(candidate, count)
buddhist <- as.data.frame(buddhist)
buddhist$count <- c(24.3, 63.3, 3.1, 9.1 )
buddhist$candidate <- factor(buddhist$candidate, levels=unique(buddhist$candidate))
buddhist$tradition <- c("Buddhist")

## Hindu
hindu <- filter(cces, hindu ==1)
wpct(hindu$vote16, hindu$commonweight_post)

candidate <- c("Donald Trump", "Hillary Clinton" , "Gary Johnson" , "Jill Stein")
count <- c(17.4, 81.7, .4, .3 )
count <- as.numeric(count)
hindu <- cbind(candidate, count)
hindu <- as.data.frame(hindu)
hindu$count <- c(17.4, 81.7, .4, .3 )
hindu$candidate <- factor(hindu$candidate, levels=unique(hindu$candidate))
hindu$tradition <- c("Hindu")

cces$vote16<-cces$CC16_364c
## All In One Graph
all <- rbind(evan, ml, catholic, mormon, jewish, muslim, atheist, buddhist, hindu, bprot)

all$candidate <- factor(all$candidate, levels=unique(all$candidate))
all$tradition <- factor(all$tradition, levels = c("Evangelical", "Mainline", "Black Protestant", "Mormon", "Catholic", "Jewish", "Muslim", "Hindu", "Buddhist", "Atheist"))

windows()
ggplot(all, aes(tradition, count)) + theme_minimal() + 
  geom_col(aes(fill=candidate), colour = "black", width=.5) +ylab("") + xlab("") +
  theme(legend.position="bottom") +
  ggtitle("2016 Presidential Election",
          subtitle="Voting Percentages by Religious Tradition") +
  theme(plot.title = element_text(hjust = 0,size=16,face="bold")) +
  theme(plot.subtitle = element_text(hjust=0,size=14,face="italic")) +
  theme(text=element_text(size=14, family="serif")) + 
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_manual(values=c("firebrick1","dodgerblue3","goldenrod1","forestgreen","darkgray")) +  
  guides(fill = guide_legend(reverse = TRUE)) + labs(fill="") +
  geom_label(aes(x=.75, y=.1, label="ReligionInPublic.blog"),hjust=0,family="serif",size=3) +
  theme(panel.grid.major.y=element_line(colour="grey50",linetype=3)) +
  theme(panel.grid.minor.y=element_blank()) +
  theme(panel.grid.major.x=element_blank())


## Just Protestants
prot <- rbind(evan, ml, bprot)

prot$candidate <- factor(prot$candidate, levels=unique(prot$candidate))

windows()
ggplot(prot, aes(1, count)) + geom_col(aes(fill=candidate), colour = "black") + coord_flip() + 
  theme(axis.title.y = element_blank()) + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) + ylab("Percent of Votes Cast") + 
  theme(legend.position="bottom") +
  ggtitle("2016 Presidential Election") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans")) + 
  scale_fill_manual(values=c("forestgreen", "goldenrod1", "dodgerblue3", "firebrick1")) +  
  guides(fill = guide_legend(reverse = TRUE)) + labs(fill="") + facet_grid(tradition ~ .)   

## Other Christians
oxtn <- rbind(catholic, mormon, jewish)

oxtn$candidate <- factor(oxtn$candidate, levels=unique(oxtn$candidate))

windows()
ggplot(oxtn, aes(1, count)) + geom_col(aes(fill=candidate), colour = "black") + coord_flip() + 
  theme(axis.title.y = element_blank()) + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) + ylab("Percent of Votes Cast") + 
  theme(legend.position="bottom") +
  ggtitle("2016 Presidential Election") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans")) + 
  scale_fill_manual(values=c("darkgrey", "forestgreen", "goldenrod1", "dodgerblue3", "firebrick1")) +  
  guides(fill = guide_legend(reverse = TRUE)) + labs(fill="") + facet_grid(tradition ~ .)   

## Non christians
non <- rbind(atheist, buddhist, hindu, muslim)

non$candidate <- factor(non$candidate, levels=unique(non$candidate))

windows()
ggplot(non, aes(1, count)) + geom_col(aes(fill=candidate), colour = "black") + coord_flip() + 
  theme(axis.title.y = element_blank()) + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) + ylab("Percent of Votes Cast") + 
  theme(legend.position="bottom") +
  ggtitle("2016 Presidential Election") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans")) + 
  scale_fill_manual(values=c("forestgreen", "goldenrod1", "dodgerblue3", "firebrick1")) +  
  guides(fill = guide_legend(reverse = TRUE)) + labs(fill="") + facet_grid(tradition ~ .)   

## Vote Share among Buddhists
colors <- c("dodgerblue3","firebrick1","forestgreen","goldenrod1")
windows()
ggplot(buddhist, aes(x= reorder(candidate,-count), y = count)) + theme_minimal() + 
  geom_col(fill = colors, colour = "black") +
  scale_y_continuous(labels=scales::percent) +
  ggtitle("Vote Choice Among Buddhists (w/Weights)",
          subtitle="2016 U.S. Presidential Election") + xlab("") + 
  ylab("") +
  theme(text=element_text(family="serif",size=14)) +
  theme(plot.title = element_text(hjust = 0, size=16,face="bold")) +
  theme(plot.subtitle=element_text(hjust=0, size=14,face="italic")) +
  annotate(geom="text", x=3.55, y=.1, label="ReligionInPublic.blog", colour="black",
           size=3, family="serif",hjust=0) +
  geom_text(label=c("24.3%","63.3%","3.1%","9.1%"),vjust=-.5,family="serif") +
  theme(panel.grid.major.y=element_line(colour="grey50",linetype=3)) +
  theme(panel.grid.minor.y=element_blank()) +
  theme(panel.grid.major.x=element_blank())

library(dplyr)
buddhist <- filter(cces, religpew=="Buddhist")
buddhist<-buddhist%>%filter(vote16=="Donald Trump (Republican)"|vote16=="Hillary Clinton (Democrat)"|vote16=="Gary Johnson (Libertarian)"|vote16=="Jill Stein (Green)")
detach("package:dplyr")
library(plyr)
count<-count(buddhist,vars=c("vote16","race"),wt_var="commonweight_post")
library(dplyr)
count1<-filter(count,vote16=="Donald Trump (Republican)")
count1$percent<-count1$freq/sum(unique(count1$freq))
count2<-filter(count,vote16=="Hillary Clinton (Democrat)")
count2$percent<-count2$freq/sum(unique(count2$freq))
count3<-filter(count,vote16=="Gary Johnson (Libertarian)")
count3$percent<-count3$freq/sum(unique(count3$freq))
count4<-filter(count,vote16=="Jill Stein (Green)")
count4$percent<-count4$freq/sum(unique(count4$freq))
count<-rbind(count1,count2,count3,count4)
count$vote<-NA
count$vote[count$vote16 == "Donald Trump (Republican)"] <- "Donald Trump"
count$vote[count$vote16 == "Hillary Clinton (Democrat)"] <- "Hillary Clinton"
count$vote[count$vote16 == "Gary Johnson (Libertarian)"] <- "Gary Johnson"
count$vote[count$vote16 == "Jill Stein (Green)"] <- "Jill Stein"
windows()
ggplot(count,aes(vote,percent,fill=race)) + theme_minimal() +
  geom_col(colour="black") +
  ylab("") + xlab("") +
  theme(legend.position="bottom") +
  ggtitle("Vote by Race/Ethnicity among Buddhists (w/ Weights)",
          subtitle = "2016 U.S. Presidential Election") +
  theme(plot.title = element_text(hjust = 0,size=16,face="bold")) +
  theme(plot.subtitle = element_text(hjust=0,size=14,face="italic")) +
  theme(text=element_text(size=14, family="serif")) + 
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  scale_y_continuous(labels=scales::percent) + 
  guides(fill = guide_legend(reverse = TRUE)) + labs(fill="") +
  geom_label(aes(x=.56, y=.25, label="ReligionInPublic.blog"),fill="white",hjust=0,family="serif",size=3) +
  theme(panel.grid.major.y=element_line(colour="grey50",linetype=3)) +
  theme(panel.grid.minor.y=element_blank()) +
  theme(panel.grid.major.x=element_blank())

library(dplyr)
buddhist2<-filter(buddhist,educ!="Skipped"|educ!="Not Asked")
detach("package:dplyr")
library(plyr)
count<-count(buddhist2,vars=c("educ","race"),wt_var="commonweight_post")
library(dplyr)
count1<-filter(count,race=="White")
count1$percent<-count1$freq/sum(unique(count1$freq))
count2<-filter(count,race=="Black")
count2$percent<-count2$freq/sum(unique(count2$freq))
count3<-filter(count,race=="Hispanic")
count3$percent<-count3$freq/sum(unique(count3$freq))
count4<-filter(count,race=="Asian")
count4$percent<-count4$freq/sum(unique(count4$freq))
count5<-filter(count,race=="Native American")
count5$percent<-count5$freq/sum(unique(count5$freq))
count6<-filter(count,race=="Mixed")
count6$percent<-count6$freq/sum(unique(count6$freq))
count7<-filter(count,race=="Other")
count7$percent<-count7$freq/sum(unique(count7$freq))
count<-rbind(count1,count2,count3,count4,count5,count6,count7)
windows()
ggplot(count,aes(race,percent,fill=educ)) + theme_minimal() +
  geom_col(colour="black") +
  ylab("") + xlab("") +
  theme(legend.position="bottom") +
  ggtitle("Education by Race/Ethnicity among Buddhists (w/ Weights)",
          subtitle = "2016 U.S. Presidential Election") +
  theme(plot.title = element_text(hjust = 0,size=16,face="bold")) +
  theme(plot.subtitle = element_text(hjust=0,size=14,face="italic")) +
  theme(text=element_text(size=14, family="serif")) + 
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  scale_y_continuous(labels=scales::percent) +
  guides(fill = guide_legend(reverse = TRUE)) + labs(fill="") +
  geom_label(aes(x=.56, y=.15, label="ReligionInPublic.blog"),fill="white",hjust=0,family="serif",size=3) +
  theme(panel.grid.major.y=element_line(colour="grey50",linetype=3)) +
  theme(panel.grid.minor.y=element_blank()) +
  theme(panel.grid.major.x=element_blank())

detach("package:dplyr")
library(plyr)
count<-count(buddhist2,vars=c("educ","vote16"),wt_var="commonweight_post")
library(dplyr)
count1<-filter(count,vote16=="Donald Trump (Republican)")
count1$percent<-count1$freq/sum(unique(count1$freq))
count2<-filter(count,vote16=="Hillary Clinton (Democrat)")
count2$percent<-count2$freq/sum(unique(count2$freq))
count3<-filter(count,vote16=="Gary Johnson (Libertarian)")
count3$percent<-count3$freq/sum(unique(count3$freq))
count4<-filter(count,vote16=="Jill Stein (Green)")
count4$percent<-count4$freq/sum(unique(count4$freq))
count<-rbind(count1,count2,count3,count4)
count$vote<-NA
count$vote[count$vote16 == "Donald Trump (Republican)"] <- "Donald Trump"
count$vote[count$vote16 == "Hillary Clinton (Democrat)"] <- "Hillary Clinton"
count$vote[count$vote16 == "Gary Johnson (Libertarian)"] <- "Gary Johnson"
count$vote[count$vote16 == "Jill Stein (Green)"] <- "Jill Stein"
windows()
ggplot(count,aes(vote,percent,fill=educ)) + theme_minimal() +
  geom_col(colour="black") + 
  ylab("") + xlab("") +
  theme(legend.position="bottom") +
  ggtitle("Vote by Education among Buddhists (w/ Weights)",
          subtitle = "2016 U.S. Presidential Election") +
  theme(plot.title = element_text(hjust = 0,size=16,face="bold")) +
  theme(plot.subtitle = element_text(hjust=0,size=14,face="italic")) +
  theme(text=element_text(size=14, family="serif")) + 
  theme(axis.text.x=element_text(angle=45,hjust=1)) +  
  scale_y_continuous(labels=scales::percent) +
  guides(fill = guide_legend(reverse = TRUE)) + labs(fill="") +
  geom_label(aes(x=.56, y=.15, label="ReligionInPublic.blog"),fill="white",hjust=0,family="serif",size=3) +
  theme(panel.grid.major.y=element_line(colour="grey50",linetype=3)) +
  theme(panel.grid.minor.y=element_blank()) +
  theme(panel.grid.major.x=element_blank())

library(dplyr)
buddhist3<-filter(buddhist,religpew_buddhist!="Skipped"|religpew_buddhist!="Not Asked")
detach("package:dplyr")
library(plyr)
count<-count(buddhist3,vars=c("religpew_buddhist","vote16"),wt_var="commonweight_post")
library(dplyr)
count1<-filter(count,vote16=="Donald Trump (Republican)")
count1$percent<-count1$freq/sum(unique(count1$freq))
count2<-filter(count,vote16=="Hillary Clinton (Democrat)")
count2$percent<-count2$freq/sum(unique(count2$freq))
count3<-filter(count,vote16=="Gary Johnson (Libertarian)")
count3$percent<-count3$freq/sum(unique(count3$freq))
count4<-filter(count,vote16=="Jill Stein (Green)")
count4$percent<-count4$freq/sum(unique(count4$freq))
count<-rbind(count1,count2,count3,count4)
count$vote<-NA
count$vote[count$vote16 == "Donald Trump (Republican)"] <- "Donald Trump"
count$vote[count$vote16 == "Hillary Clinton (Democrat)"] <- "Hillary Clinton"
count$vote[count$vote16 == "Gary Johnson (Libertarian)"] <- "Gary Johnson"
count$vote[count$vote16 == "Jill Stein (Green)"] <- "Jill Stein"
windows()
ggplot(count,aes(reorder(buddhist,freq),freq,fill=vote)) + theme_minimal() +
  geom_col(colour="black") +
  ylab("") + xlab("") +
  theme(legend.position="bottom") +
  ggtitle("Vote Count by Buddhist Denomination (w/ Weights)",
          subtitle = "2016 U.S. Presidential Election") +
  theme(plot.title = element_text(hjust = 0,size=16,face="bold")) +
  theme(plot.subtitle = element_text(hjust=0,size=14,face="italic")) +
  theme(text=element_text(size=14, family="serif")) + 
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  scale_fill_manual(values=c("firebrick1", "goldenrod1", "dodgerblue3", "forestgreen")) +  
  guides(fill = guide_legend(reverse = TRUE)) + labs(fill="") +
  geom_label(aes(x=.56, y=25, label="ReligionInPublic.blog"),fill="white",hjust=0,family="serif",size=3) +
  theme(panel.grid.major.y=element_line(colour="grey50",linetype=3)) +
  theme(panel.grid.minor.y=element_blank()) +
  theme(panel.grid.major.x=element_blank())

windows()
count$buddhist <- NA
count$buddhist[count$religpew_buddhist == "Other Buddhist"] <- "Other"
count$buddhist[count$religpew_buddhist == "Vajrayana (Tibetan) Buddhism"] <- "Tibetan"
count$buddhist[count$religpew_buddhist == "Mahayana (Zen) Buddhism"] <- "Zen"
count$buddhist[count$religpew_buddhist == "Theravada (Vipassana) Buddhism"] <- "Vipassana"
ggplot(count,aes(vote16,percent,fill=buddhist)) + theme_minimal() +
  geom_col(colour="black") +
  ylab("") + xlab("") +
  theme(legend.position="bottom") +
  ggtitle("Vote by Buddhist Denomination (w/ Weights)",
          subtitle = "2016 U.S. Presidential Election") +
  theme(plot.title = element_text(hjust = 0,size=16,face="bold")) +
  theme(plot.subtitle = element_text(hjust=0,size=14,face="italic")) +
  theme(text=element_text(size=14, family="serif")) + 
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  scale_y_continuous(labels=scales::percent) + 
  scale_x_discrete(labels=c("Donald Trump","Hillary Clinton","Gary Johnson","Jill Stein")) +
  guides(fill = guide_legend(reverse = TRUE)) + labs(fill="") +
  geom_label(aes(x=.56, y=.1, label="ReligionInPublic.blog"),fill="white",hjust=0,family="serif",size=3) +
  theme(panel.grid.major.y=element_line(colour="grey50",linetype=3)) +
  theme(panel.grid.minor.y=element_blank()) +
  theme(panel.grid.major.x=element_blank())
