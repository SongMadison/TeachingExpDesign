ggplot(data.frame(x = group.mean$`mean(weightloss)`,y =rep(1,5)), aes(x,y)) + 
  geom_point( )+
  ylim(c(0,4)) 
geom_point(data.frame(x= trt.mean$`mean(weightloss)`,y = rep(2,3)), aes(x,y, color = "red"))+
  geom_point(data.frame(x= aov1$residuals, y = rep(3,length(aov1$residuals))), aes(x,y))
