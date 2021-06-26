Survival\_Project
================
Corey Neff
6/25/2021

## Introduction

This is a short project investigating customer time-to-churn of a public
dataset. The main goal is to discover the role that subscribing to
multiple services at this company plays in probability of churn, but it
will also consider customer satisfaction, as well.

We will start by reading the data, and turning churn value (“Yes”, “No”)
into a factor, while doing some basic visualizations.

``` {r}
librarian::shelf(survival, survminer, dplyr, ggplot2)
survdata <- read.csv("survival.csv")

head(survdata, n=5)
survdata$churn_factor <- as.factor(survdata$churn_value)
hist(survdata$months, xlab="Length of Survival Time (Months)", main="Histogram of Survial Time for Customer Churn")
```

## First Kaplan-Meier Curve

Kaplan-Meier Curves are a non-parametric way to vizualize survival data
such as these. The Kaplan-Meier survival curve is defined as the
probability of “surviving” in a given length of time while considering
time in many small intervals. Here, “survival time” is the time-to-churn
(i.e. length of time before a cutomer stopped using services, or
customer attrition).

`{r pressure, warning=FALSE} survfit1 <- survfit(Surv(months, churn_value) ~ 1, data = survdata) basic_plot <- ggsurvplot(survfit1,            conf.int = T,            conf.int.fill = "red",            ylim = c(0.5, 1),            xlim = c(0, max(survdata$months)),            xlab = "Time (months)",            legend = "none",            title = "Kaplan-Meier Curve for Time to Customer Churn (full sample)",            ggtheme = theme_bw()) print(basic_plot)`

## Multiple Services – Important?

As we can see, change in customer churn appears to be rather linear with
respect to time, but this model includes the full sample of data. I’d
like to know how the number of services a customer is subscribed to
affects survival time. Do customers with less services (i.e. a single
service) churn faster than those with more? First we will start with a
simple bar chart vizualization to begin to investigate this issue.

``` {r}
multiple <- survdata %>%
                  filter(multiple == "Yes") %>%
                  ggplot() + 
                        geom_histogram(aes(x = months, fill  = as.factor(churn_value)), bins = 11) +
                        scale_fill_discrete(name = "Churned?", labels = c("No", "Yes")) +
                        ggtitle("Customers with Multiple Services") +
                        xlab("Time (months)")+
                        theme_bw()

single <-   survdata %>%
                  filter(multiple == "No") %>%
                  ggplot() + 
                        geom_histogram(aes(x = months, fill  = as.factor(churn_value)), bins = 11) +
                        scale_fill_discrete(name = "Churned?", labels = c("No", "Yes")) +
                        ggtitle("Customers with a Single Service")+
                        xlab("Time (months)") +
                        theme_bw()

egg::ggarrange(plots = list(single, multiple), nrow = 1)
```

## Kaplan-Meier Estimator Part Deux

As we can see, customers subscribing to multiple services tend to
survive (i.e. not churn) a much longer time than those subscribed to a
single service. We will be able to see this a bit more clearly with
another Kaplan-Meier Curve.

``` {r}
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
```

## Kaplan-Meier Estimator Part Deux

So, it seems that customers subscribed to a single service tend to face
attrition at a generaly faster rate early-on. That is, early in their
patronage, they are much more likely to churn than those subsccribed to
multiple services. In the long-run, however, the survival probaility
appears to be about the same for both groups.

Now we will begin to investigate a more parametric route – the Cox
Proportional Hazards Model, specifically. Cox models handle multiple
covariates much better, which will be useful later. In addition, in a
Cox proportional hazards regression model, the measure of effect is the
hazard rate, which is the risk of failure (i.e., the risk or probability
of suffering the event of interest), given that the participant has
survived up to a specific time. Comparing hazard rates allows us to
create a hazard ratio, which is similar to an odds ratio.

``` {r,warning=false}
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
```

## Cox Proportional Hazards Model – Univariate

The Cox distribution yields similar conclusions to the Kaplan-Meier
curve before. Generally, people with multiple services have a higher
survival rate with respect to time. From the output, we might turn our
attention to the exponentiated coefficient of multiple services
variable: 0.788. FIn this case, subscribing to multiple services reduces
the hazard by a factor of 0.79, or 21%. Therefore, subscribing to
multiple services is associated with a good prognosis as far as churn
goes. Additionally, we can see that the covariate and model are both
significant.

Now that we understands that, generally, subscribing to multiple
services corresponds to less churn, I think a solid next step would be
to see which services in particular are better in this regard. With that
in mind, this next model will include the individual services being
subscribed to (security, backup, protection, and support) as well as
customer satisfaction, for good measure.

`{r, warning=FALSE} coxfit2 <- coxph(Surv(months, churn_value) ~ security + backup + protection + support + satisfaction, data = survdata) summary(coxfit2)`

## Cox Proportional Hazards Model – Multivariate

As we can see, the model and all covariates are statistically
significant. Additionally, it seems that subscribing to the security
service yields the greatest reduction in hazard at a hazard ratio of
0.3482. Even so, it appears that customer satisfaction has the greatest
impact on hazard as indicated by it beign the lowest hazard ratio among
the included predictors. For that reason, it may be beneficial to see
this effect visually.

`{r, warning=FALSE} cox_satisfaction <- ggadjustedcurves(coxfit2, variable = "satisfaction",                  ylim = c(0, 1),                  ggtheme = theme_bw(),                  xlab = "Time (months)",                  title = 'Survival Curves for Cox Proportional Hazards Model, by "Satisfaction"',                  legend.title = "Product Satisfaction") print(cox_satisfaction)`

## Cox Proportional Hazards Model – Customer Satisfaction

Customer satisfaction seems to have a particularly pronounced (and
understandable) effect on customer attrition over time. Customers
reporting high satisfaction (i.e. a 5) survive at a rate of what appears
to be nearly 100% at the end of the reporting period, while those
reporting low satisfaction (i.e. a 1) survive at an abysmal rate –
nearly entirely gone at 70 months. For that reason, the wisest business
decision may be to maintain high levels of customer satisfaction;
albeit, this is a rather unsurprising takeaway.