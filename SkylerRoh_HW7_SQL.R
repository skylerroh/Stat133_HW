library(RMySQL)
drv = dbDriver("MySQL")
con = dbConnect(drv, user = "s133", 
                dbname = "lahman",
                password = "s133",
                host = "radagast.berkeley.edu")

dbListTables(con)

#create payroll df
query = "SELECT S.teamID as team, S.lgID as league, S.yearID as year, 
                SUM(salary) as payroll, round  
        FROM Salaries AS S
        LEFT OUTER JOIN SeriesPost as SP
        ON S.teamID = SP.teamIDwinner AND S.yearID = SP.yearID AND SP.round = 'WS'
        GROUP BY S.yearID, S.teamID;"
payroll = dbGetQuery(con, statement = query)

save(payroll, file = "payroll.rda")

#adjust for inflation
inflation = c(1, 0.96, 0.95, 0.91, 0.87, 0.84, 0.79, 0.76, 0.74,
              0.72, 0.70, 0.69, 0.66, 0.65, 0.64, 0.63, 0.61, 0.60, 0.58,
              0.57, 0.55, 0.54, 0.52, 0.50, 0.50, 0.49, 0.48, 0.47, 0.46, 0.45)
names(inflation) = 1985:2014
payroll$inflation = payroll$payroll * inflation[payroll$year - 1984]

#define colors
library(RColorBrewer)
WSColor = "red"
ALColor = rgb(254, 224, 139, 150, max = 256)
NLColor = rgb(230, 245, 152, 150, max = 256)
colors = c(WSColor, ALColor, NLColor)
allColor = colors[(payroll$league == "NL") + 2]
allColor[payroll$round == "WS"] = colors[1]

#plot
set.seed(220)
par(pin = c(5.5, 3))
plot(x = payroll$year + runif(length(payroll$year), min = -.1, max = .1), 
     y = log10(payroll$inflation), 
     main = "Payroll of MLB Teams",
     xlab = "Year", yaxt ="n", ylab = "Payroll (inflation-adjusted millions)", 
     ylim = c(log10(5 * 10^6), 8.1), pch = 19, cex = .4, col = allColor)
axis(2, at = log10(c(5, 10, 20, 50, 100)*10^6), labels = c("5", "10", "20", "50", "100"))
text(x = payroll$year[payroll$round == "WS"], 
     y = log10(payroll$inflation)[payroll$round == "WS"] + .05,
     labels = payroll$team[payroll$round == "WS"], cex = .4)
legend("topleft", legend = c("World Series Winner", "American League", "National League"), fill = colors, cex = .7)

