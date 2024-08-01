library(shapviz)
library(ggplot2)
library(gridExtra)
library(ggpubr)

# mean absolute SHAP values
## prepare models for SHAP visualization
shap.logistic <- shapviz(xgb.logistic, X_pred = data.matrix(df_test[, -1]), X = df_test)
shap.hinge <- shapviz(xgb.hinge, X_pred = data.matrix(df_test[, -1]), X = df_test)
## plots
p.logistic.m <- sv_importance(shap.logistic) + coord_flip() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7), 
        plot.title = element_text(size = 10, hjust = 0.5),
        axis.title.y = element_text(size = 10)) + 
  labs(x = "Mean Absolute SHAP Values", y = "") +
  ggtitle("Logistic Loss")
p.hinge.m <- sv_importance(shap.hinge) + coord_flip() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        plot.title = element_text(size = 10, hjust = 0.5)) +
  labs(y = "", x = "") +
  ggtitle("Hinge Loss")
grid.arrange(p.logistic.m, p.hinge.m, ncol = 2)


# beeswarm plot
bee.logistic <- sv_importance(shap.logistic, kind = "beeswarm") +
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        axis.title.x = element_text(size = 10)) + 
  labs(x = "SHAP Values", y = "") +
  ggtitle("Logistic Loss")
bee.hinge <- sv_importance(shap.hinge, kind = "beeswarm") + 
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        axis.title.x = element_text(size = 10)) + 
  labs(x = "SHAP Values", y = "") +
  ggtitle("Hinge Loss")
grid.arrange(bee.logistic, bee.hinge, nrow = 2)

# individual SHAP values barplot, ID = 1
shap1.logistic <- shapviz(xgb.logistic, X_pred = data.matrix(df_test[1, -1]), X = df_test[1, ])
shap1.hinge <- shapviz(xgb.hinge, X_pred = data.matrix(df_test[1, -1]), X = df_test[1, ])

p.logistic1 <- sv_importance(shap1.logistic) + coord_flip() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7), 
        plot.title = element_text(size = 10, hjust = 0.5),
        axis.title.y = element_text(size = 10)) + 
  labs(x = "Absolute SHAP Values", y = "") +
  ggtitle("Logistic Loss")
p.hinge1 <- sv_importance(shap1.hinge) + coord_flip() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        plot.title = element_text(size = 10, hjust = 0.5)) +
  labs(y = "", x = "") +
  ggtitle("Hinge Loss")
grid.arrange(p.logistic1, p.hinge1, ncol = 2)


# individual SHAP values waterplot, ID = 1
sv_waterfall(shap1.logistic) + 
  theme(axis.title.y = element_text(size = 7)) +
  labs(x = "SHAP Values", y = "") 
sv_waterfall(shap1.hinge) + 
  theme(axis.title.y = element_text(size = 7)) +
  labs(x = "SHAP Values", y = "") 


# dependence plots
## section 1: first three variables
## KIDPROP
p.kp.l <- sv_dependence(shap.logistic, "KIDPROP", color_var = NULL, size = 0.5) +
  theme(plot.title = element_text(size = 7, hjust = 0.5),
        axis.title.y = element_text(size = 7)) +
  labs(y = "SHAP Values: \n Logistic Loss", x = "") +
  ggtitle("KIDPROP") 
p.kp.h <- sv_dependence(shap.hinge, "KIDPROP", color_var = NULL, size = 0.5) +
  labs(y = "SHAP Values: \n Hinge Loss", x = "") +
  theme(plot.title = element_text(size = 7, hjust = 0.5),
        axis.title.y = element_text(size = 7)) +
  ggtitle("KIDPROP") 
## RACE
p.r.l <- sv_dependence(shap.logistic, "RACE", color_var = NULL, size = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5),
        plot.title = element_text(size = 7, hjust = 0.5),
        axis.title.y = element_text(size = 7)) +
  labs(y = "SHAP Values: \n Logistic Loss", x = "") +
  ggtitle("RACE")
p.r.h <- sv_dependence(shap.hinge, "RACE", color_var = NULL, size = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5),
        plot.title = element_text(size = 7, hjust = 0.5),
        axis.title.y = element_text(size = 7)) +
  labs(y = "SHAP Values: \n Hinge Loss", x = "") +
  ggtitle("RACE")
## EEDUC
p.ed.l <- sv_dependence(shap.logistic, "EEDUC", color_var = NULL, size = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5),
        plot.title = element_text(size = 7, hjust = 0.5),
        axis.title.y = element_text(size = 7)) +
  labs(y = "SHAP Values: \n Logistic Loss", x = "") +
  ggtitle("EEDUC")
p.ed.h <- sv_dependence(shap.hinge, "EEDUC", color_var = NULL, size = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5),
        axis.title.y = element_text(size = 7),
        plot.title = element_text(size = 7, hjust = 0.5)) +
  labs(y = "SHAP Values: \n Hinge Loss", x = "") +
  ggtitle("EEDUC")

## section 2: EST_ST
p.est.l <- sv_dependence(shap.logistic, "EST_ST", color_var = NULL, size = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5),
        plot.title = element_text(size = 7, hjust = 0.5),
        axis.title.y = element_text(size = 7)) + 
  labs(y = "SHAP Values: \n Logistic Loss", x = "") +
  ggtitle("EST_ST") 
p.est.h <- sv_dependence(shap.hinge, "EST_ST", color_var = NULL, size = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5),
        axis.title.y = element_text(size = 7)) +
  labs(y = "SHAP Values: \n Hinge Loss", x = "")
## combined plots
grid.arrange(p.est.l, p.est.h, nrow = 2)


# section 3: last four variables
## MS
p.ms.l <- sv_dependence(shap.logistic, "MS", color_var = NULL, size = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5),
        plot.title = element_text(size = 7, hjust = 0.5),
        axis.title.y = element_text(size = 7)) +
  labs(y = "SHAP Values: \n Logistic Loss", x = "") +
  ggtitle("MS")
p.ms.h <- sv_dependence(shap.hinge, "MS", color_var = NULL, size = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5),
        axis.title.y = element_text(size = 7),
        plot.title = element_text(size = 7, hjust = 0.5)) +
  labs(y = "SHAP Values: \n Hinge Loss", x = "") +
  ggtitle("MS")
## WRKLOSSRV
p.wl.l <- sv_dependence(shap.logistic, "WRKLOSSRV", color_var = NULL, size = 0.5) +
  theme(plot.title = element_text(size = 7, hjust = 0.5),
        axis.title.y = element_text(size = 7)) +
  labs(y = "SHAP Values: \n Logistic Loss", x = "")+ 
  ggtitle("WRKLOSSRV")
p.wl.h <- sv_dependence(shap.hinge, "WRKLOSSRV", color_var = NULL, size = 0.5)+
  theme(plot.title = element_text(size = 7, hjust = 0.5),
        axis.title.y = element_text(size = 7)) +
  labs(y = "SHAP Values: \n Hinge Loss", x = "") + 
  ggtitle("WRKLOSSRV")
## TENURE
p.tn.l <- sv_dependence(shap.logistic, "TENURE", color_var = NULL, size = 0.5)  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5),
        axis.title.y = element_text(size = 7),
        plot.title = element_text(size = 7, hjust = 0.5)) +
  labs(y = "SHAP Values: \n Logistic Loss", x = "") + 
  ggtitle("TENURE")
p.tn.h <- sv_dependence(shap.hinge, "TENURE", color_var = NULL, size = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5),
        axis.title.y = element_text(size = 7),
        plot.title = element_text(size = 7, hjust = 0.5)) +
  labs(y = "SHAP Values: \n Hinge Loss", x = "") + 
  ggtitle("TENURE")
## INCOME
p.ic.l <- sv_dependence(shap.logistic, "INCOME", color_var = NULL, size = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5),
        axis.title.y = element_text(size = 7),
        plot.title = element_text(size = 7, hjust = 0.5)) +
  labs(y = "SHAP Values: \n Logistic Loss", x = "") + 
  ggtitle("INCOME")
p.ic.h <- sv_dependence(shap.hinge, "INCOME", color_var = NULL, size = 0.5)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5),
        axis.title.y = element_text(size = 7),
        plot.title = element_text(size = 7, hjust = 0.5)) +
  labs(y = "SHAP Values: \n Hinge Loss", x = "") + 
  ggtitle("INCOME")
## combined plots


ggarrange(p.kp.l, p.kp.h, p.r.l, p.r.h, 
         p.ed.l, p.ed.h, p.ms.l, p.ms.h, 
         p.wl.l, p.wl.h, p.tn.l, p.tn.h, 
         p.ic.l, p.ic.h, nrow = 4, ncol = 4)



