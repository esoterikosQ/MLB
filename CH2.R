library(Lahman)
data('Batting')
View(Batting)
View(Master)

# 84
a <- subset(Batting, playerID=='harpebr03')
a
b <- Batting[Batting$playerID=='harpebr03',]
b

# 92
a <- c('A','B','C','D','E'); b <- c(.280, .257, .312, .266, .295)
c <- cbind(a,b)
c
colnames(c) <- c('player', 'avg')
c
age <- c(26,23,31,27,24)
d <- cbind(c,age)
d

# 97
d <- matrix(c('C','D','E','B','A',26,23,31,27,24), ncol=2)
colnames(d) <- c('player', 'age')
d
e <- merge(c, d, by='player')
e

# 110
a <- c('A','B','C','D','E')
b <- c(.280, .257, .312, .266, .295)
c <- cbind(a,b)
colnames(c) <- c('player', 'avg')
d <- matrix(c('C','D','E','B','A',26,23,31,27,24),ncol=2)
colnames(d) <- c('player','age')
e <- merge(c,d,by='player')
e$age <- as.character(e$age)
g <- ifelse(e$age>25, 1, 0)
g
h <- cbind(e,g)
h

# 107
View(Master)
a <- subset(Batting, playerID=='altuvjo01'|playerID=='zobribe01')
a
b <- subset(a, yearID>2011&yearID<2017)
b
c <- subset(b, !(yearID==2014|yearID==2015))
c
d <- subset(c, select=c('playerID', 'HR', 'X3B'))
d

# 109
attach(Batting)
detach(Batting)
