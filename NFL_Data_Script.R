# install.packages("ggpubr")
library(ggpubr)
library(ggplot2)
tg <- read.csv("AvgPerTeamGame.csv")
ts <- read.csv("AvgPerTeamSeason.csv")
total <- read.csv("TotalPerSeason.csv")

sum(duplicated(tg))
sum(duplicated(ts))
sum(duplicated(total))

# summary(tg)
# summary(ts)
# summary(total)


g1 <- ggplot(tg, aes(Year, Yds)) + 
  geom_line(color = "black") +
  ggtitle("Avg. Team Game P. Yards/Year")
g2 <- ggplot(tg, aes(Year, Att)) + 
  geom_line(color = "black") +
  ggtitle("Avg. Team Game P. Attempts/Year")
g3 <- ggplot(tg, aes(Year, Cmp.)) + 
  geom_line(color = "black") +
  ggtitle("Avg. Team Game P. Comp.%/Year")
g4 <- ggplot(total, aes(Year, TD)) + 
  geom_line(color = "black") +
  ggtitle("Avg. Team Game P. TD's/Year")
ggarrange(g1, g2, g3, g4,
          ncol = 2, nrow = 2)

s1 <- ggplot(ts, aes(Year, Yds)) + 
  geom_line(color = "darkgreen") +
  ggtitle("Avg. Team Season P. Yards/Year")
s2 <- ggplot(ts, aes(Year, Att)) + 
  geom_line(color = "darkgreen") +
  ggtitle("Avg. Team Season P. Attempts/Year")
s3 <- ggplot(ts, aes(Year, Cmp.)) + 
  geom_line(color = "darkgreen") +
  ggtitle("Avg. Team Season P. Comp.%/Year")
s4 <- ggplot(ts, aes(Year, TD)) + 
  geom_line(color = "darkgreen") +
  ggtitle("Avg. Team Season P. TD's/Year")
ggarrange(s1, s2, s3, s4,
          ncol = 2, nrow = 2)

t1 <- ggplot(total, aes(Year, Yds)) + 
  geom_line(color = "purple") +
  ggtitle("Total Passing Yards/Year")
t2 <- ggplot(total, aes(Year, Att)) + 
  geom_line(color = "purple") +
  ggtitle("Total Passing Attempts/Year")
t3 <- ggplot(total, aes(Year, Cmp.)) + 
  geom_line(color = "purple") +
  ggtitle("Total Passing Completion%/Year")
t4 <- ggplot(total, aes(Year, TD)) + 
  geom_line(color = "purple") +
  ggtitle("Total Passing Touchdowns/Year")
ggarrange(t1, t2, t3, t4,
          ncol = 2, nrow = 2)


Yds_tg <- tg$Yds
Year_tg <- tg$Year
g_cbm <- lm(Yds_tg ~ poly(Year_tg, 3))
summary(g_cbm)
par(mfrow=c(2,2))
plot(g_cbm) # Diagnositic plots shows that the model has no problems
years_x1 <- seq(1932, 2041)
predictions_tg <- predict(g_cbm, newdata = data.frame(Year_tg = years_x1))
par(mfrow=c(1,1))
plot(Yds_tg ~ Year_tg, type = "p", ylim = c(0, 400), xlim = c(1930, 2046),
     main = "Avg. Team Season Passing Yards/Year Projections (Until 2041)")
lines(years_x1, predictions_tg, col = "red")
p_20yrs1 <- data.frame(Year = tail(years_x1, 20), Yds = tail(predictions_tg, 20))
p_20yrs1

# -----------------------------------------------------------------------------

Yds_ts <- ts$Yds
Year_ts <- ts$Year
s_cbm <- lm(Yds_ts ~ poly(Year_ts, 3))
summary(s_cbm)
par(mfrow=c(2,2))
plot(s_cbm) # Diagnositic plots shows that the model has no problems
predictions_ts <- predict(s_cbm, newdata = data.frame(Year_ts = years_x1))
par(mfrow=c(1,1))
plot(Yds_ts ~ Year_ts, type = "p", ylim = c(0, 5000), xlim = c(1930, 2046),
     main = "Avg. Team Game Passing Yards/Year Projections (Until 2041)", 
     col = "darkgreen")
lines(years_x1, predictions_ts, col = "red")
p_20yrs2 <- data.frame(Year = tail(years_x1, 20), Yds = tail(predictions_ts, 20))
p_20yrs2

# -----------------------------------------------------------------------------

# Since the plot using Yds from "total" shows a linear pattern compared to the
# other data sets, we'll use linear regression instead of
# cubic regression like the previous two prediction models.
Yds_total <- total$Yds
Year_total <- total$Year
t_lm <- lm(Yds_total ~ Year_total)
summary(t_lm)
par(mfrow=c(2,2))
plot(t_lm) # Diagnositic plots shows that the model has no problems
predictions_total <- predict(t_lm, newdata = data.frame(Year_total = years_x1))
par(mfrow=c(1,1))
plot(Yds_total ~ Year_total, type = "p", ylim = c(0, 175000), xlim = c(1930, 2046),
     main = "Total Passing Yards/Year Projections (Until 2041)", 
     col = "purple")
lines(years_x1, predictions_total, col = "red")
p_20yrs3 <- data.frame(Year = tail(years_x1, 20), Yds = tail(predictions_total, 20))
p_20yrs3