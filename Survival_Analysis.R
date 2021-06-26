librarian::shelf(survival, survminer, dplyr, ggplot2, svglite, sass)
survdata <- read.csv("survival.csv")


head(survdata, n=5)
survdata$churn_factor <- as.factor(survdata$churn_value)
svg(filename = "hist.svg")
hist(survdata$months, xlab="Length of Survival Time (Months)", main="Histogram of Survial Time for Customer Churn")
dev.off() 

survfit1 <- survfit(Surv(months, churn_value) ~ 1, data = survdata)
basic_plot <- ggsurvplot(survfit1,
                         conf.int = T,
                         conf.int.fill = "red",
                         ylim = c(0.5, 1),
                         xlim = c(0, max(survdata$months)),
                         xlab = "Time (months)",
                         legend = "none",
                         title = "Kaplan-Meier Curve for Time to Customer Churn (full sample)",
                         ggtheme = theme_bw())
print(basic_plot)
ggsave(filename = "Basic_Survival_Plot.svg", print(basic_plot))

multiple <- survdata %>%
      filter(multiple == "Yes") %>%
      ggplot() + 
      geom_histogram(aes(x = months, fill  = as.factor(churn_value)), bins = 11) +
      scale_fill_discrete(name = "Churned?", labels = c("No", "Yes")) +
      ggtitle("Customers with Multiple Services") +
      xlab("Time (months)")
single <-   survdata %>%
      filter(multiple == "No") %>%
      ggplot() + 
      geom_histogram(aes(x = months, fill  = as.factor(churn_value)), bins = 11) +
      scale_fill_discrete(name = "Churned?", labels = c("No", "Yes")) +
      ggtitle("Customers with a Single Service")+
      xlab("Time (months)")

egg::ggarrange(plots = list(single, multiple), nrow = 1) %>%
      ggsave(filename = "Services_Bar_Chart.svg", print(.))


survfit2 <- survfit(Surv(months, churn_value) ~ multiple, data = survdata)
services_plot <- ggsurvplot(survfit2,
                            conf.int = T,
                            risk.table = TRUE,
                            tables.height = 0.2,
                            tables.theme = theme_cleantable(),
                            ylim = c(0.5, 1),
                            xlim = c(0, max(survdata$months)),
                            xlab = "Time (months)",
                            legend.title = "Services",
                            legend.labs = c("Single", "Multuple"),
                            title = "Kaplan-Meier Curve for Time to Customer Churn (full sample)",
                            ggtheme = theme_bw())
print(services_plot)
ggsave(filename = "Services_Survival_Plot.svg", print(services_plot))


coxfit1 <- coxph(Surv(months, churn_value) ~ multiple, data = survdata)
summary(coxfit1)

cox_services <- ggadjustedcurves(coxfit1, variable = "multiple",
                                 ylim = c(0.5, 1),
                                 ggtheme = theme_bw(),
                                 xlab = "Time (months)",
                                 legend.title = "Multiple Services",
                                 legend.labs = c("Single", "Multuple"),
                                 title = 'Survival Curves for Cox Proportional Hazards Model, by Number of "Services"') 
print(cox_services)

ggsave(filename = "Services_Cox_Plot.svg", print(cox_services))



coxfit2 <- coxph(Surv(months, churn_value) ~ security + backup + protection + support + satisfaction, data = survdata)
summary(coxfit2)
cox_satisfaction <- ggadjustedcurves(coxfit2, variable = "satisfaction",
                                     ylim = c(0, 1),
                                     ggtheme = theme_bw(),
                                     xlab = "Time (months)",
                                     title = 'Survival Curves for Cox Proportional Hazards Model, by "Satisfaction"',
                                     legend.title = "Product Satisfaction")
print(cox_satisfaction)
ggsave(filename = "Satisfaction_Cox_Plot.svg", print(cox_satisfaction))


