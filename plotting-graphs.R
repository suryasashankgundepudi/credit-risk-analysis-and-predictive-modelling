# importing the necessary libraries
library(dplyr)
library(plotly)
library(ggplot2)

# reading the clean data set which was created in mutating-data-for-eda.R
data <- read.csv("data/eda-german-credit.csv")

  
###############################################################################
#                                                                             #  
#                 PLOTTING THE CLASS SPLIT OF TARGET VARIABLE                 #
#                                                                             #
###############################################################################

fig1 <- plot_ly(
  data = data, 
  x = c("Good", "Bad"), 
  y = c(length(data$Outcome[data$Outcome == "Good"]), 
        length(data$Outcome[data$Outcome == "Bad"])),  
  type = "bar", 
  marker = list(color = c("Green", "Red"))
)

fig1 <- fig1 %>%   layout(
  title = 'Target Variable Distribution', 
  xaxis = list(title = 'Outcome of Risk Taken'), 
  font=t, 
  plot_bgcolor = "#e5ecf6",
  yaxis = list(title = 'Count'), 
  legend = list(title=list(text='Legend Title')))

# Un-comment this line to get the figure
#fig1


###############################################################################
#                                                                             #  
#     PLOTTING THE DISTRIBUTION OF AGE FOR GOOD OUTCOME AND BAD OUTCOME       #
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


fig2 <- plot_multi_histogram(data, "Age.in.Years", "Outcome")

# Un-Comment the following line to print the figure
# fig2

###############################################################################
#                                                                             #  
#            EXPLORING THE CREDIT OF PEOPLE IN DIFFERENT AGE GROUPS           #
#                                                                             #
###############################################################################

#Let's look the Credit Amount column
#Let's look the Credit Amount column
interval = c(18, 25, 35, 60, 120)

cats = c('Student', 'Young', 'Adult', 'Senior')
data["Age_Group"] = cut(data$Age.in.Years, interval, labels=cats)

data_good = data.frame(data[data["Outcome"] == 'Good', ])
data_bad = data.frame(data[data["Outcome"] == 'Bad', ])



fig3 <- plot_ly(
  y = data_good$Credit.Amount, 
  x = data_good$Age_Group, 
  name="Good credit",
  color = '#3D9970', 
  type = "box"
)

fig3 <- fig3 %>%
  add_trace(
    y = data_bad$Credit.Amount, 
    x = data_bad$Age_Group, 
    name="Bad credit", 
    color = "Blue", 
    type = "box"
  )


fig3 <- fig3 %>%
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
# fig3


###############################################################################
#                                                                             #  
#       LOOKING AT THE RISK WE TAKE FOR DIFFERENT TYPE OF HOME-OWNERRS        #
#                                                                             #
###############################################################################

fig4 <- plot_ly(data = data, 
                x = names(table(data[data$Outcome == "Good", "Housing"])), 
                y = table(data[data$Outcome == "Good", "Housing"]), 
                type = 'bar', 
                name = 'Good Credit')

fig4 <- fig4 %>% 
  add_trace(y = table(data[data$Outcome == "Bad", "Housing"]), 
            name = 'Bad Credit')

fig4 <- fig4 %>% 
  layout(barmode = 'group')

# Un-Comment the following line to print the figure
fig4
