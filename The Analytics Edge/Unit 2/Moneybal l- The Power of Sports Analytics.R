#Moneyball: The Power of Sports Analytics


#Load required libraries
library(tidyverse)

#Loading the data
Money_ball <- read_csv(file = "baseball.csv")

#Take a look
head(Money_ball)
str(Money_ball)


#How many games does a team need to win in the regular season to make it to the
#playoffs?
Money_ball %>%
  ggplot(aes(x = W, y = Team)) +
  geom_point(aes(colour = factor(Playoffs)), size = 3) +
  geom_vline(xintercept = 95.1, lwd = 2, lty = 2)

#Lets find out on average how many points did teams score to make the playoffs
Money_ball %>%
  group_by(Playoffs) %>%
  summarise(Average = mean(W))

#Lets subset our data using data up to 2002
Subset_Index <- Money_ball$Year <= 2002

#Paul's Data
Pauls_Moneyball_Data <- Money_ball[Subset_Index,]  
  
#Lets create a "Run Difference" Variable capturing the difference between Runs
#Scored (RS) and Runs Allowed (RD)

Pauls_Moneyball_Data <- Pauls_Moneyball_Data %>%
  mutate(RD = RS - RA)

#Lets check the relationship between RD and Wins
Pauls_Moneyball_Data %>%
  ggplot(aes(x = RD, y = W)) +
  geom_point()

#Lets create a linear regression between Wins and RD
Wins_Reg <- lm(formula = W ~ RD, data = Pauls_Moneyball_Data)
summary(Wins_Reg)


#Since we want our Wins to be atleast 95, using the regression equation we can 
#deduce that RD required for a Win of 94 is 133.4 ~ 135


#Predicting Scoring Runs
#Two baseball statistics a good predictors of Runs i.e. On-Base Percentage and 
#Slugging Percentage. Also Batting Average is over-rated.

Runs_Scored_Reg <- lm(formula = RS ~ OBP + SLG + BA, data = Pauls_Moneyball_Data)
summary(Runs_Scored_Reg)

#The summary statistics reveal that the three predictors are highly correlated.
#Lets remove BA

Runs_Scored_Reg <- lm(formula = RS ~ OBP + SLG, data = Pauls_Moneyball_Data)
summary(Runs_Scored_Reg)  

#If we had instead removed OBP or SLG, the adjusted R-squarred would have reduced  


#Now we predict runs allowed using Opponents On-Base Percentage (OOBP) and Opponents
#Slugging Percentage

Runs_Allowed_Reg <- lm(formula = RA ~ OOBP + OSLG, data = Pauls_Moneyball_Data)
summary(Runs_Allowed_Reg)

#The key message here is the worth of simple regression models in answering tedious
#questions


#Now To Predict the teams score in 2003, we first need to predict how many runs the 
#team will score and how many rins the team will allow, i.e. team statistics.

#But since we do not know the team statistics in 2003 yet, we can estimate the 2003
#team statistics from past performance. This assumes past performance correlates
#(highly) with future performance. Also assumes few injuries

#Now you realize that the choice of players largely predicts the team statistics
#So you select players that impact positively the team statistic.


#We can't predict whether the team will win the world series due to the few number 
#of games playes. Insuffucient data.
