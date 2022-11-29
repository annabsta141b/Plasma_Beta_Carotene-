cor(bplasma[c(1,5:8)])
cor(bplasma[9:13])

library(corrplot)
M = cor(bplasma[c(1,9:13)])
corrplot(M, method = "number")

M2 = cor(bplasma[c(1,5:8)])
corrplot(M2, method = "number")

table(bplasma$sex,  bplasma$vituse, bplasma$smokstat)
table(bplasma$sex,  bplasma$smokstat)
#----------------------- plot Y vs Xi ---------------------------------------------#
library(latex2exp)
library(ggplot2)
library(gridExtra)


g1 =ggplot(bplasma)+ 
  geom_point(aes(x = age , y= betaplasma))+
  xlab(TeX("Age (yrs)"))+
  ylab(TeX("BC Plasma ($\\frac{ng}{ml}$)"))+ggtitle("(1)")+
  theme_minimal()+theme(plot.title = element_text(hjust = 0.5))

g2 = ggplot(bplasma)+ 
  geom_point(aes(x = bmi , y= betaplasma))+
  xlab(TeX("Body mass index ($\\frac{weight}{height^{2}}$)"))+
  ylab(TeX("BC Plasma ($\\frac{ng}{ml}$)"))+
  theme_minimal()+ggtitle("(2)")+
  theme(plot.title = element_text(hjust = 0.5))

g3 = ggplot(bplasma)+ 
  geom_point(aes(x = calories , y= betaplasma))+
  xlab(TeX("Calories ($\\frac{kcal}{day}$)"))+
  ylab(TeX("BC Plasma ($\\frac{ng}{ml}$)"))+ggtitle("(3)")+
  theme_minimal()+theme(plot.title = element_text(hjust = 0.5))


g4 = ggplot(bplasma)+ 
  geom_point(aes(x = fat , y= betaplasma))+
  xlab(TeX("Fat Intake ($\\frac{g}{day}$)"))+
  ylab(TeX("BC Plasma ($\\frac{ng}{ml}$)"))+ ggtitle("(4)")+
  theme_minimal()+theme(plot.title = element_text(hjust = 0.5))

g5 = ggplot(bplasma)+ 
  geom_point(aes(x = fiber , y= betaplasma))+
  xlab(TeX("Fiber Intake ($\\frac{g}{day}$)"))+
  ylab(TeX("BC Plasma ($\\frac{ng}{ml}$)"))+ggtitle("(5)")+
  theme_minimal()+theme(plot.title = element_text(hjust = 0.5))

g6 = ggplot(bplasma)+ 
  geom_point(aes(x = alcohol , y= betaplasma))+
  xlab(TeX("Alcohol ($\\frac{drinks}{week}$)"))+
  ylab(TeX("BC Plasma ($\\frac{ng}{ml}$)"))+ggtitle("(6)")+
  theme_minimal()+theme(plot.title = element_text(hjust = 0.5))

g7 = ggplot(bplasma)+ 
  geom_point(aes(x = cholesterol , y= betaplasma))+
  xlab(TeX("Cholesterol ($\\frac{mcg}{day}$)"))+
  ylab(TeX("BC Plasma ($\\frac{ng}{ml}$)"))+ggtitle("(7)")+
  theme_minimal()+theme(plot.title = element_text(hjust = 0.5))

g8 = ggplot(bplasma)+ 
  geom_point(aes(x = betadiet , y= betaplasma))+
  xlab(TeX("Dietary Betacaroten Consumed ($\\frac{mcg}{day}$)"))+
  ylab(TeX("BC Plasma ($\\frac{ng}{ml}$)"))+ggtitle("(8)")+
  theme_minimal()+theme(plot.title = element_text(hjust = 0.5))

g9 = ggplot(bplasma)+ 
  geom_point(aes(x = retdiet , y= betaplasma)) + 
  xlab(TeX("Dietary Retinol Consumed ($\\frac{mcg}{day}$)"))+
  ylab(TeX("BC Plasma ($\\frac{ng}{ml}$)"))+ggtitle("(9)")+
  theme_minimal()+theme(plot.title = element_text(hjust = 0.5))

g9 = ggplot(bplasma)+ 
  geom_point(aes(x = retdiet , y= betaplasma)) + 
  xlab(TeX("Dietary Retinol Consumed ($\\frac{mcg}{day}$)"))+
  ylab(TeX("BC Plasma ($\\frac{ng}{ml}$)"))+ggtitle("(9)")+
  theme_minimal()+theme(plot.title = element_text(hjust = 0.5))

g9 = ggplot(bplasma)+ 
  geom_point(aes(x = retdiet , y= betaplasma)) + 
  xlab(TeX("Dietary Retinol Consumed ($\\frac{mcg}{day}$)"))+
  ylab(TeX("BC Plasma ($\\frac{ng}{ml}$)"))+ggtitle("(9)")+
  theme_minimal()+theme(plot.title = element_text(hjust = 0.5))

g9 = ggplot(bplasma)+ 
  geom_point(aes(x = retdiet , y= betaplasma)) + 
  xlab(TeX("Dietary Retinol Consumed ($\\frac{mcg}{day}$)"))+
  ylab(TeX("BC Plasma ($\\frac{ng}{ml}$)"))+ggtitle("(9)")+
  theme_minimal()+theme(plot.title = element_text(hjust = 0.5))


grid.arrange(g1,g2,g3,g4,g5,g6,g7,g8,g9, nrow = 3, ncol = 3)

##--------
library(ggplot2)
#first Alcohol Outlier # 62
ggplot(data = bplasma, aes(x = alcohol), colour = barlines, fill = barfill) +
  geom_histogram(aes(fill = ..count..), bins = 20) +
  labs(title = "Alcohol")  + theme_light()
#scale_fill_gradient("Count", low = "Dark Blue", high = "Red")
#----------------------------------- BoxPlots ------------------------------------------#

# Sex Characteristic BoxPlots

library(grid.text)
# "#21908CFF" aqua
# "#FDE725FF" yellow
# "#E78AC3" pink
# "#66C2A5 light green

gender_bc =  ggplot(bplasma) +
  geom_boxplot(aes(x = sex, y = betaplasma, fill=sex),outlier.size = 1) +
  labs(title = "(1)")+ xlab("Gender") + ylab("Beta Plasma")+
  coord_flip() +
  scale_fill_manual(values=c("#21908CFF", "#FDE725FF", "#E78AC3"))+
  theme_minimal()+theme(legend.position = 'none')+theme(plot.title = element_text(hjust = 0.5))

smoke = ggplot(data = bplasma) +
  geom_boxplot(aes(x = smokstat, y = betaplasma, fill=smokstat)) +
  labs(title = "(2)")+ ylab("Beta Plasma") + xlab("Smoking Status")+
  coord_flip() +
  scale_fill_manual(values=c("#21908CFF", "#FDE725FF", "#E78AC3"))+theme_minimal()+
  theme(legend.position = 'none') +theme(plot.title = element_text(hjust = 0.5))

vitaminUse  = ggplot(data = bplasma, aes(x = vituse, y = betaplasma)) +
  geom_boxplot(outlier.size = 1, aes(fill=vituse)) +
  labs(title = "(3)")+ xlab("Vitamin Use") + ylab("Beta Plasma")+
  coord_flip() +
  scale_fill_manual(values=c("#21908CFF", "#FDE725FF", "#E78AC3"))+ theme_minimal()+
  theme(legend.position="none")+theme(plot.title = element_text(hjust = 0.5))

bar = ggplot(bplasma)+
  geom_bar(aes(x = smokstat, fill = sex), position = "dodge")+
  scale_fill_manual(values=c("#21908CFF", "#FDE725FF", "#E78AC3"))+
  labs(title = "(4)", y = "Count of Observations", x = "Smoking Status")+theme_minimal()+
  theme(legend.title = element_blank()) +theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.6))

bar2 = ggplot(bplasma)+
  geom_bar(aes(x = vituse , fill = smokstat ), position = "dodge")+
  scale_fill_manual(values=c("#21908CFF", "#FDE725FF", "#E78AC3"))+
  labs(title = "(5)", y = "Count of Observations", x = " Vitamin Use")+
  theme_minimal()+labs(fill="Smoking Status")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.6))

bar3 = ggplot(bplasma)+
  geom_bar(aes(x = vituse, fill = sex), position = "dodge")+
  scale_fill_manual(values=c("#21908CFF", "#FDE725FF", "#E78AC3"))+
  labs(title = "(6)", y = "Count of Observations", x = "Vitamin Use")+theme_minimal()+
  theme(legend.title = element_blank()) +theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.6))


#BoxPlots Graph Display
grid.arrange(gender_bc, smoke, vitaminUse,bar,bar2,bar3,  ncol=3, nrow = 2)
