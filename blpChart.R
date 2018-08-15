library(ggplot2)
library(scales)
library(plotly)

setwd("~/causalApp")
blp <- read.csv("cf2.csv")

blp$ID <- paste0(blp$state, blp$district)
blp$rPct2 <- 1-blp$dPct2

bl <- colorRampPalette(c("lightskyblue", "royalblue", "navy"))(200)                      
re <- colorRampPalette(c("darkred", "red2", "mistyrose"))(200)



p <- ggplotly(ggplot(blp, aes(x = reorder(ID, winDContDiff2), y = winDContDiff2/1000000, fill = dPct2, width = 1, text = paste0(state, " District ", district, "<br>", "Dem: ", round(dPct2*100,1), "%", "<br>", "$", round(winDContDiff2/1000000,1), "M"))) + 
  geom_bar(stat = "identity") + 
  scale_fill_gradientn(colors = c(re, "white",bl), limits = c(.3,.7), oob = squish) + 
  coord_cartesian(ylim=c(-25,50)) + 
  scale_x_discrete() + 
  ylab("Democrat Spending Difference (Millions $)") + 
  theme_minimal() + 
  theme(axis.text.x = element_blank(), panel.grid.major.x = element_blank()),
  tooltip = "text"
)

for (i in 1:length(p$x$data)){
  p$x$data[[i]]$text <- c(p$x$data[[i]]$text, "") 
}

p


bl2 <- colorRampPalette(c("navy", "royalblue", "lightskyblue"))(200)                      
re2 <- colorRampPalette(c("mistyrose", "red2", "darkred"))(200)

p2 <- ggplotly(ggplot(blp, aes(x = reorder(ID, winRContDiff2), y = winRContDiff2/1000000, fill = rPct2, width = 1, text = paste0(state, " District ", district, "<br>", "Rep: ", round(rPct2*100,1), "%", "<br>", "$", round(winRContDiff2/1000000,1), "M"))) + 
  geom_bar(stat = "identity") + 
  scale_fill_gradientn(colors = c(bl2, "white",re2), limits = c(.3,.7), oob = squish) + 
  coord_cartesian(ylim=c(-25,50)) + 
  scale_x_discrete() + 
  ylab("Republican Spending Difference (Millions $)") + 
  theme_minimal() + 
  theme(axis.text.x = element_blank(), panel.grid.major.x = element_blank()),
  tooltip = "text"
)

for (i in 1:length(p2$x$data)){
  p2$x$data[[i]]$text <- c(p2$x$data[[i]]$text, "") 
}

p2
