library(ggflags)

data(lflags)
set.seed(1234)

# 15% opacity gives 2B in the alpha channel
.flaglist[["lv"]]@content[[1]]@content[[2]]@content[[1]]@gp$fill <- "#C28FEF2B"
# 33% opacity gives 54 in the alpha channel
.flaglist[["qa"]]@content[[1]]@content[[2]]@content[[1]]@gp$fill <- "#9450E054"
d <- data.frame(x=rnorm(10), y=rnorm(10), 
                country=sample(c("lv","qa"), 10, TRUE), 
                stringsAsFactors = FALSE)
ggplot(d, aes(x=x, y=y, country=country, size=x)) + 
  geom_flag() + 
  scale_country()

