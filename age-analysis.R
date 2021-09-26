# importing the necessary libraries
library(dplyr)
library(plotly)
library(ggplot2)
library(reshape2)
library(tidyr)
library(descr)

# reading the clean data set which was created in mutating-data-for-eda.R
data <- read.csv("data/eda-german-credit.csv")



###############################################################################
#                                                                             #  
#               AGE ANALYSIS OF PEOPLE WITH GOOD AND BAD CREDIT               #
#                                                                             #
###############################################################################

# Function takes data, feature to plot the distribution of, and separating label
plot_multi_histogram <- function(df, feature, label_column) {
  
  plt <- ggplot(df, aes(x=eval(parse(text=feature)), 
                        fill=eval(parse(text=label_column)))) +
    geom_histogram(alpha=0.5, position="identity",  
                   aes(y = ..density..), color="black") +
    geom_density(alpha=0.1) +
    geom_vline(aes(xintercept=mean(eval(parse(text=feature)))), 
               color="black", linetype="dashed", size=1) +
    labs(x=feature, 
         y = "Density") 
  
  plt + guides(fill=guide_legend(title=label_column))
}


fig1 <- plot_multi_histogram(data, "Age.in.Years", "Outcome")

# Un-Comment the following line to print the figure
# fig1




###############################################################################
#                                                                             #  
#              BOX-PLOT OF CREDIT OF PEOPLE IN DIFFERENT AGE GROUPS           #
#                                                                             #
###############################################################################

#Let's look the Credit Amount column
#Let's look the Credit Amount column
interval = c(18, 25, 35, 60, 120)

cats = c('Young', 'Young Adult', 'Adult', 'Senior')
data["Age_Group"] = cut(data$Age.in.Years, interval, labels=cats)

data_good = data.frame(data[data["Outcome"] == 'Good', ])
data_bad = data.frame(data[data["Outcome"] == 'Bad', ])



fig2 <- plot_ly(
  y = data_good$Credit.Amount, 
  x = data_good$Age_Group, 
  name="Good credit",
  color = '#3D9970', 
  type = "box"
)

fig2 <- fig2 %>%
  add_trace(
    y = data_bad$Credit.Amount, 
    x = data_bad$Age_Group, 
    name="Bad credit", 
    color = "Blue", 
    type = "box"
  )


fig2 <- fig2 %>%
  layout(
    yaxis=list(
      title='Credit Amount (US Dollar)',
      zeroline=F
    ),
    xaxis=list(
      title='Age Categorical'
    ),
    boxmode='group'
  )

# Un-Comment the following line to print the figure
# fig2



###############################################################################
#                                                                             #  
#              VIOLIN PLOT OF GOOD AND BAD CREDIT ACROSS AGE GROUPS           #
#                                                                             #
###############################################################################



fig3 <- data %>%
  plot_ly(type = 'violin') 
fig3 <- fig3 %>%
  add_trace(
    x = data_good$Age_Group,
    y = data_good$Credit.Amount,
    legendgroup = 'Good Credit',
    scalegroup = 'Good Credit',
    name = 'Good Credit',
    side = 'negative',
    box = list(
      visible = T
    ),
    meanline = list(
      visible = T
    ),
    color = I("blue")
  ) 
fig3 <- fig3 %>%
  add_trace(
    x = data_bad$Age_Group,
    y = data_bad$Credit.Amount,
    legendgroup = 'Bad Credit',
    scalegroup = 'Bad Credit',
    name = 'Bad Credit',
    side = 'positive',
    box = list(
      visible = T
    ),
    meanline = list(
      visible = T
    ),
    color = I("red")
  ) 

fig3 <- fig3 %>%
  layout(
    xaxis = list(
      title = ""  
    ),
    yaxis = list(
      title = "",
      zeroline = F
    )
  )

#fig3


###############################################################################
#                                                                             #  
#              DISTRIBUTION OF CREDIT AGAINST DIFFERENT AGE GROUPS            #
#                                                                             #
###############################################################################

young_good = sum(na.omit(data_good$Credit.Amount[data$Age_Group == "Young"]))
young_bad = sum(na.omit(data_bad$Credit.Amount[data$Age_Group == "Young"]))

young_adult_good = sum(na.omit(data_good$Credit.Amount[data$Age_Group == "Young Adult"]))
young_adult_bad = sum(na.omit(data_bad$Credit.Amount[data$Age_Group == "Young Adult"]))

adult_good = sum(na.omit(data_good$Credit.Amount[data$Age_Group == "Adult"]))
adult_bad = sum(na.omit(data_bad$Credit.Amount[data$Age_Group == "Adult"]))

elder_good = sum(na.omit(data_good$Credit.Amount[data$Age_Group == "Senior"]))
elder_bad = sum(na.omit(data_bad$Credit.Amount[data$Age_Group == "Senior"]))

young_good_p = young_good/(young_good + young_bad) * 100
young_bad_p = young_bad/(young_good + young_bad) * 100
young_adult_good_p = young_adult_good/(young_adult_good + young_adult_bad) * 100
young_adult_bad_p = young_adult_bad/(young_adult_good + young_adult_bad) * 100
adult_good_p = adult_good/(adult_good + adult_bad) * 100
adult_bad_p =  adult_bad/(adult_good + adult_bad) * 100
elder_good_p = elder_good/(elder_good + elder_bad) * 100
elder_bad_p = elder_bad/(elder_good + elder_bad) * 100


young_good_p = round(young_adult_bad_p, 3)
young_bad_p = round(young_bad_p, 3)
young_adult_good_p = round(young_adult_good_p, 3)
young_adult_bad_p = round(young_adult_bad_p, 3)
adult_good_p = round(adult_good_p, 3)
adult_bad_p =  round(adult_bad_p, 3)
elder_good_p = round(elder_good_p, 3)
elder_bad_p = round(elder_bad_p, 3)


good_text <- c(paste(young_good_p, '%'), paste(young_adult_good_p, '%'), 
                  paste(adult_good_p, '%'), paste(elder_good_p, '%'))

bad_text <- c(paste(young_bad_p, '%'), paste(young_adult_bad_p, '%'), 
                 paste(adult_bad_p, '%'), paste(elder_bad_p, '%'))


good_loans <- plot_ly(
  x=cats,
  y=c(young_good, young_adult_good, adult_good, elder_good),
  type = "bar", 
  name="Good Loans",
  text=good_text,
  textposition = 'auto',
  marker=list(
    color='rgb(111, 235, 146)',
    line=list(
      color='rgb(60, 199, 100)',
      width=1.5)
  ),
  opacity=0.6
)

overall_loan <- good_loans %>%  add_trace(
  x=cats,
  y = c(young_bad, young_adult_bad, adult_bad, elder_bad), 
  text=bad_text,
  type = "bar",
  name="Bad Loans",
  textposition = 'auto',
  marker=list(
    color='rgb(247, 98, 98)',
    line=list(
      color='rgb(225, 56, 56)',
      width=1.5)
  ),
  opacity=0.6
)


overall_loan <- overall_loan %>% layout(
  title="Type of Loan by Age Group", 
  xaxis = list(title="Age Group"),
  yaxis= list(title="Credit Amount")
)


overall_loan









