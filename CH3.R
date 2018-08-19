# 118
library(Lahman)
a <- subset(Batting, yearID==2014)
b <- subset(Batting, yearID==2015)
c <- merge(a,b,by='playerID')
head(c)
d <- c[c$AB.x>10&c$AB.y>10,]
cor(d$HR.x, d$HR.y)
cor(d$H.x/d$AB.x, d$H.y/d$AB.y)

# 128
library(Lahman)
library(plyr)
a <- subset(Batting, yearID>2014)
str(a$teamID)
a$teamID <- as.numeric(as.factor(a$teamID))
str(a$teamID)
b <- function(a) {
  return(data.frame(team=ifelse(mean(a$teamID)==a$teamID, 0, 1),
                    a$playerID, a$lgID, a$SF, a$SH, a$H, a$yearID, a$teamID, a$RBI, a$AB))
}
d <- ddply(a, .(playerID), b)
head(d)

d$lag_team <- as.numeric(sapply(1:nrow(d), function(x){d$a.teamID[x-1]}))
d$lag_RBI <- as.numeric(sapply(1:nrow(d), function(x){d$a.RBI[x-1]}))
d$lag_AB <- as.numeric(sapply(1:nrow(d), function(x){d$a.AB[x-1]}))
d$lag_SF <- as.numeric(sapply(1:nrow(d), function(x){d$a.SF[x-1]}))
d$lag_SH <- as.numeric(sapply(1:nrow(d), function(x){d$a.SH[x-1]}))
d$lag_H <- as.numeric(sapply(1:nrow(d), function(x){d$a.H[x-1]}))
d$lag_playerID <- as.character(sapply(1:nrow(d), function(x){d$playerID[x-1]}))
head(d)

d$lag_team <- ifelse(d$a.playerID==d$lag_playerID, d$lag_team, 'NA')
d$lag_avg <- d$lag_H/d$lag_AB
d$sac <- d$lag_SF + d$lag_SH
e <- subset(d, a.AB>400&lag_AB>400)
e$change_rbi <- e$a.RBI/e$lag_RBI
e <- subset(e, !((lag_team=='NA')|(a.teamID==lag_team)))

cor(e$lag_avg, e$change_rbi)

names(e)
cor(e$a.RBI, e$lag_RBI)
e$avg <- e$a.H/e$a.AB
cor(e$avg, e$lag_avg)
cor(e$a.RBI, e$lag_RBI)

plot(e$lag_avg, e$change_rbi, main='Predictor of RBI', xlab='Batting Avg of Prior Yr',
     ylab='Change in RBI', las=1, cex.axis=0.8, pch=19, col=e$a.lgID)
text(x=0.3, y=1.6, label='r=-0.49')
abline(lm(change_rbi~lag_avg,e))

library(tableHTML)
f <- with(e, data.frame(change_rbi, sac, lag_avg))
colnames(f) <- c('c_rbi', 'sacrifice', 'avg')
cor(f)
tableHTML(round(cor(f),3))

# 137
library(Lahman)
a <- subset(Batting, yearID>2010, select=c(playerID, teamID))
a$teamID_fac <- factor(a$teamID)
a$teamID_cha <- as.character(a$teamID)
a$teamID_chafa <- as.character(a$teamID_fac)
str(a)
isTRUE(a$teamID_cha==a$teamID_chafa)
a$teamID_cha[5]
a$teamID_chafa[5]
isTRUE(a$teamID_cha[5]==a$teamID_chafa[5])

library(data.table)
move1 <- dcast(setDT(a)[,idx := 1:.N, by = playerID], playerID~idx, value.var=c('teamID_cha'))
move2 <- dcast(setDT(a)[,idx := 1:.N, by = playerID], playerID~idx, value.var=c('teamID_chafa'))
move1[is.na(move1)] <- ''
move2[is.na(move2)] <- ''
write.csv(move1, file='move1.csv')
write.csv(move2, file='move2.csv')

library(arules)
move11 <- read.transactions('move1.csv', sep=',')
move22 <- read.transactions('move2.csv', sep=',')

summary(move11)
summary(move22)

itemFrequencyPlot(move11, support=.01, cex.names=.6)
pattern <- apriori(move11, list(support=.0015, confidence=.5, minlen=2))
summary(inspect(pattern))


# 142
library(ggplot2)
library(ggmap)
stadium <- read.csv('stadium_location.csv', sep=',')
b <- get_map("Oklahoma City", zoom=4, maptype='roadmap')
d <- ggmap(b) +
  geom_point(data=stadium, aes(longitude, latitude), size=1) +
  geom_text(data=stadium, aes(longitude, latitude-1, label=id), size=2)
d

# 149
library(Lahman)
a <- subset(Pitching, yearID>2014&G>35, select=c('playerID', 'yearID', 'teamID'))
a$teamyear <- paste(a$teamID, a$yearID, sep='')
b <- subset(Managers, yearID>2014, select=c('playerID', 'yearID', 'teamID'))
b$teamyear <- paste(b$teamID, b$yearID, sep='')
network <- merge(a,b,by='teamyear')
network <- subset(network, select=c('playerID.x', 'playerID.y'))
library(igraph)
mlb_network <- graph.data.frame(network, directed=F)
V(mlb_network)$label <- ifelse(V(mlb_network)$name %in% c(b$playerID)>0,
                               as.character(b$teamyear), NA)
mlb_network
manager <- V(mlb_network)$name %in% c(b$playerID)+1
plot(mlb_network, vertex.laber.cex=.6, vertex.label.color='black', 
     vertex.size=c(8,22)[manager], vertex.color=c('gray','white')[manager])

# 152
library(Lahman)
a <- subset(Teams, yearID==2015)
b <- barplot(a$HR)
text(b, par('usr')[3], labels=a$teamID, srt=60, adj=c(1,.5), xpd=T)
rm(list=ls())

a <- subset(Batting, playerID=='jeterde01')
hist(a$HR, xlab='HR', main='Histogram of D. Jeter\'s HR', las=1)
hist(a$HR, xlab='HR', main='Histogram of D. Jeter\'s HR', las=1, breaks = seq(0,27,3))

# 155
library(Lahman)
a <- subset(Teams, yearID>1990)
a$attend <- a$attendance/a$G
a$affiliation <- paste(a$lgID, a$divID)

library(lattice)
histogram(~a$attend|a$affiliation, type='density', panel=function(x,...){
  panel.histogram(x,...,col='gray')
  apg <- seq(min(x), max(x))
  density <- dnorm(apg, mean(x), sd(x))
  panel.lines(apg, density, col='black')
})
