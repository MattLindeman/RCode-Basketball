library(readxl)
library(tidyverse)
library(purrr)

Synergy <- read_excel("BYU Basketball/SSQ Synergy.xlsx", 
                      sheet = "Sheet2")

temp <- data.frame(aSSQ = approx(Synergy$Game, Synergy$aSSQ, n = 500),
                        aPPS = approx(Synergy$Game, Synergy$aPPS, n = 500),
                        Game = approx(Synergy$Game, n = 500))

temp$midup <- pmin(temp$aSSQ.y, temp$aPPS.y)
xlow <- min(Synergy$Game)
xhigh <- max(Synergy$Game)

ggplot(data = temp) +
  geom_ribbon(ymin = temp$midup,
              aes(x = Game.y,
                  ymax = aPPS.y),
              fill = "#f2d4d4",
              show.legend = F) +
  geom_ribbon(ymax = temp$aSSQ.y,
              aes(x = Game.y,
                  ymin = midup),
              fill = "#c1daec",
              show.legend = F) +
  geom_line(aes(Game.y, aPPS.y)) +
  geom_line(aes(Game.y, aSSQ.y), color = "#838383") +
  guides(linetype = "none", fill = "none") +
  labs(x = "2021-22 Season Shot Log", y = "Points Per Shot",
       title = "Rudi Williams Shot Making",
       subtitle = "Points per shot and expected points per shot during the 2021-22 CBB regular season, as 5-game rolling averages") +
  coord_cartesian(xlim = c(xlow + 1.4, xhigh - 1.4),
                  ylim = c(0.5, 1.5)) +
  theme_light()
