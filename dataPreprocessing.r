library(ggplot2)
library(dplyr)
library(gridExtra)
library(RColorBrewer)
library(reshape2)
library(gridExtra)
library(ggthemes)
library(rworldmap)
library(tree)

library(viridis)
library(scales)
library(knitr)
library(DT)
library(psych)
library(rpart)
library(rpart.plot)
options(DT.options = list(pageLength = 4))

# Data Preprocessing
# Import the data sets from their respective directories.
fifaRanking <- read.csv('/Users/Carlos/Documents/Fifa/cleanedFifaDataset.csv',sep=',',stringsAsFactors=F)
fifa_ranking1 = fifaRanking

fifa_ranking1$region <- tolower(fifa_ranking1$region)
fifa_ranking1[fifa_ranking1$region == 'antigua and barbuda','region'] <- 'antigua'
fifa_ranking1[fifa_ranking1$region == 'brunei darussalam','region'] <- 'brunei'
fifa_ranking1[fifa_ranking1$region == 'cape verde islands','region'] <- 'cape verde'
fifa_ranking1[fifa_ranking1$region == "china pr","region"] <- 'china'
fifa_ranking1[fifa_ranking1$region == "côte d'ivoire","region"] <- 'ivory coast'
fifa_ranking1[fifa_ranking1$region == "congo dr","region"] <- 'democratic republic of the congo'
fifa_ranking1[fifa_ranking1$region == "chinese taipei","region"] <-"taiwan"
fifa_ranking1[fifa_ranking1$region == "fyr macedonia","region"] <- 'macedonia'
fifa_ranking1[fifa_ranking1$region == "ir iran","region"] <- 'iran'
fifa_ranking1[fifa_ranking1$region == "korea dpr","region"] <- 'north korea'
fifa_ranking1[fifa_ranking1$region == "korea republic","region"] <- 'south korea'
fifa_ranking1[fifa_ranking1$region == "kyrgyz republic","region"] <- 'kyrgyzstan'
fifa_ranking1[fifa_ranking1$region == "northern ireland","region"] <- 'ireland'
fifa_ranking1[fifa_ranking1$region == "trinidad and tobago","region"] <- 'trinidad'
fifa_ranking1[fifa_ranking1$region == "england","region"] <- 'uk'

fifaRanking = fifa_ranking1

# Repeat the same for the other four data sets
fifaPPP
fifaElo
fifaPenalities
fifaRankings

# Merge data sets by Year and Country
# Combine the tables by the left joins and start with the ELO table
# That way we will drop any records that are not in the ELO table.
combinedFifa <- fifaElo %>% left_join(fifaPPP, by=c("year","region"))
combinedFifa <- combinedFifa %>% left_join(fifaPenalities, by=c("year","region"))
combinedFifa <- combinedFifa %>% left_join(fifaRankings, by=c("year","region"))

#save to file
write.csv(combinedFifa,'combinedFifa2.csv')

fifa_ranking=fifaRanking
fifaMap <- data.frame(fifa_ranking %>% 
                                   dplyr::select(year, region, rank, confederation, elo) %>%
                                   arrange(region))

# Creates the World Map of All WC {articipants}
world_map = map_data("world")
world_map$region <-tolower(world_map$region)
res <- data.frame(left_join(world_map, fifaMap %>% select(region, confederation),by='region'))

# Combined Histogram of all ELO Scores
histo2 <- fifaMap %>% 
  ggplot(aes(x=reorder(region, elo), y=elo ,fill=confederation)) +
  geom_bar(stat='identity') + coord_flip() + theme_fivethirtyeight(10)  + 
  scale_fill_brewer(name='',palette='Paired') + 
  theme(legend.position='None', 
        panel.grid.major.y= element_blank()) +
  labs(title='Combined ELO Ranks of the World Cup Participants',
       subtitle='since 2006')

map<-ggplot(res) + 
  geom_polygon(aes(x=long, y=lat,group=group, fill = confederation)) + 
  theme_fivethirtyeight() + 
  theme(
    panel.grid.major = element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank(), 
    legend.text=element_text(size=10),legend.key.size = unit(.3, "cm")) +
  scale_fill_brewer(name='',palette='Paired', na.value='azure3') +
  coord_fixed(1.3) + labs(subtitle='World Cup Participants Since 2006')


fifaMap %>% 
  ggplot(aes(x=reorder(region,elo),y=elo, fill=confederation)) +
  geom_bar(stat='identity') + coord_flip() + theme_fivethirtyeight(10) + 
  scale_fill_brewer(name='',palette='Paired') + 
  theme(legend.position='None', 
        panel.grid.major.y= element_blank()) +
  labs(title='ELO Ranks of All Participating Countries in the World Cup',
       subtitle='starting from 2006') + facet_wrap(~confederation,scales='free')

fifaBox1<-fifaMap %>% group_by(confederation) %>% ggplot(aes(x=reorder(confederation, elo, FUN=mean),y=elo,fill=confederation)) + geom_boxplot(alpha=.75,size=.25) + geom_jitter(shape=16,position=position_jitter(0.2),size=1,alpha=.25) +
  theme_fivethirtyeight() + theme(legend.position='None') + 
  scale_fill_brewer(name='',palette='Paired') + coord_flip() + labs(title='Average ELO', subtitle = 'by confederation')
fifaBox2<-fifaMap %>% group_by(confederation) %>% ggplot(aes(x=reorder(confederation, -rank, FUN=mean),y=rank,fill=confederation)) + geom_boxplot(alpha=.75,size=.25) + geom_jitter(shape=16,position=position_jitter(0.2),size=1,alpha=.25) +
  theme_fivethirtyeight() + theme(legend.position='None') + 
  scale_fill_brewer(name='',palette='Paired') + coord_flip() + labs(title='Average FIFA Ranking', subtitle = 'by confederation')

grid.arrange(fifaBox1, fifaBox2,ncol=2)



fifa.tree = tree(elo ~ FoulsC + FoulsR + DoubleYellow +YellowCards + rank, data = fifa)
plot(fifa.tree)
summary(fifa.tree)

fifa.lm = lm(formula = elo ~ FoulsC + FoulsR + GoalsFor + GoalsAgainst + 
               DoubleYellow + YellowCards + rank + YellowCards * FoulsR, 
             data = fifa)
plot(fifa.lm)
plot(fifa$elo, predict(fifa.lm,fifa))
abline(c(1:64), c(1:64))

set.seed(2)
train <- sample(n,0.8*n)

fifa.test <- subset(bob[-train,], select=-elo)
elo.test <- bob$elo[-train]


fifa.randomforest <- randomForest(elo ~ .,
data=bob,
subset=train,
xtest = fifa.test,
ytest = elo.test,
ntree=100,
mtry=sqrt(p),
importance=T,
na.action = na.exclude)

