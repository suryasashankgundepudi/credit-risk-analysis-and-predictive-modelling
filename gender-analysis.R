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
#                      DISTTRIBUTION OF AGE VS GENDER                         #
#                                                                             #
###############################################################################


data_age = data[, "Age.in.Years"]
age_male = data[data$Sex == "Male ", "Age.in.Years"]
age_female = data[data$Sex == "Female ", "Age.in.Years"]
trace0 <- plot_ly(
  x = age_male, 
  type = "histogram", 
  histnorm='probability',
  name="German Male",
  marker = list(
    color = 'rgba(100, 149, 247, 0.7)'
  )
)
trace0 <- trace0 %>% 
  layout(bargap = 0.05)

trace1 <- plot_ly(
  x = age_female, 
  type = "histogram", 
  histnorm='probability',
  name="German Female",
  marker = list(
    color = 'rgba(255, 105, 180, 0.7)'
  )
)
trace1 <- trace1 %>% 
  layout(bargap = 0.05)

trace2 <- plot_ly(
  x = data_age, 
  type = "histogram", 
  histnorm='probability',
  name="Overall Distribution",
  marker = list(
    color = 'rgba(195, 177, 225, 0.7)'
  )
)
trace2 <- trace2 %>% 
  layout(bargap = 0.05)

sub1 <- subplot(trace0, trace1)
fig1 <- subplot(sub1, trace2, nrows = 2) 

# Un-Comment the following line to print the figure
# fig1




###############################################################################
#                                                                             #  
#                    DISTTRIBUTION OF GENDER VS PURPOSE                       #
#                                                                             #
###############################################################################


purpose_list <- c(" New Car", "Business", "Domestic Appliances", "Education",         
                  "Furniture/Equipment", "Others", "Radio or Television",
                  "Repairs", "Retraining", "Used Car")

purpose_male <- unname(prop.table(table(data[data$Sex == "Male ", "Purpose"])))
purpose_female <- unname(prop.table(table(data[data$Sex == "Female ", "Purpose"])))


# VARIATION OF GENDER WITH PURPOSE 

trace0 <- plot_ly(
  x = purpose_male,
  y = purpose_list,
  name = "Male Purpose Distribution",
  type = "bar", 
  marker = list(color = 'rgba(100, 149, 247, 0.7)',
                line = list(color = 'rgb(8,48,107)',
                            width = 1.5))
)

fig2 <- trace0 %>%  add_trace(
  x = purpose_female,
  y = purpose_list,
  name = "Female Purpose Distribution",
  type = "bar", 
  marker = list(color = 'rgba(255, 105, 180, 0.7)',
                line = list(color = 'rgb(8,48,107)',
                            width = 1.5))
)
fig2 <- fig2 %>% layout(
  yaxis = list(title = 'Count'), 
  xaxis = list(title = "Percentage of Gender"), 
  barmode = 'group'
)


###############################################################################
#                                                                             #  
#                    DISTTRIBUTION OF GENDER VS CREDIT                        #
#                                                                             #
###############################################################################

trace3 <- ggplot(data[data$Sex == "Male ", ], aes(x =Credit.Amount)) + 
  geom_histogram(alpha=0.5, position="identity",  bins = 50, 
                 color="blue", fill = "lightblue") + 
  geom_vline(aes(xintercept=mean(data[data$Sex == "Male ", "Credit.Amount"])), 
             color="black", linetype="dashed", size=0.2)


trace4 <- ggplot(data[data$Sex == "Female ", ], aes(x =Credit.Amount)) + 
  geom_histogram(alpha=0.5, position="identity",  bins = 50,
                 color="red", fill = "pink") +  
  geom_vline(aes(xintercept=mean(data[data$Sex == "Female ", "Credit.Amount"])), 
             color="black", linetype="dashed", size=0.3)


fig3 <- subplot(trace3, trace4, nrows = 2, shareX = TRUE)

# fig3



###############################################################################
#                                                                             #  
#                    DISTTRIBUTION OF GENDER VS RISK                        #
#                                                                             #
###############################################################################

trace5 <- plot_ly(data = data, 
                x = names(table(data[data$Outcome == "Good", "Sex"])), 
                y = table(data[data$Outcome == "Good", "Sex"]), 
                type = 'bar', 
                name = 'Good Credit')

fig5 <- trace5 %>% 
  add_trace(y = table(data[data$Outcome == "Bad", "Sex"]), 
            name = 'Bad Credit')

fig4 <- fig5 %>% 
  layout(barmode = 'group')


fig4




