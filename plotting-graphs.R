# importing the necessary libraries
library(dplyr)
library(plotly)
library(ggplot2)
library(reshape2)
library(tidyr)
library(descr)


# reading the clean data set which was created in mutating-data-for-eda.R
data <- read.csv("data/eda-german-credit.csv")


data_good = data.frame(data[data["Outcome"] == 'Good', ])
data_bad = data.frame(data[data["Outcome"] == 'Bad', ])


###############################################################################
#                                                                             #  
#                        TARGET VARIABLE DISTRIBUTION                         #
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
#       LOOKING AT THE RISK WE TAKE FOR DIFFERENT TYPE OF HOME-OWNERRS        #
#                                                                             #
###############################################################################

fig2 <- plot_ly(data = data, 
                x = names(table(data[data$Outcome == "Good", "Housing"])), 
                y = table(data[data$Outcome == "Good", "Housing"]), 
                type = 'bar', 
                name = 'Good Credit')

fig2 <- fig2 %>% 
  add_trace(y = table(data[data$Outcome == "Bad", "Housing"]), 
            name = 'Bad Credit')

fig2 <- fig2 %>% 
  layout(barmode = 'group')

# Un-Comment the following line to print the figure
# fig2




###############################################################################
#                                                                             #  
#                             PURPOSE ANALYSIS                                #
#                                                                             #
###############################################################################

fig3 <- ggplot(data, aes(x=Purpose, y=Credit.Amount, fill = Purpose)) + 
  geom_boxplot() + 
  labs(title="Distribution of Credit VS Purpose",x="Purpose", 
       y = "Credit ammount (DK)") + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 10))


# Un-Comment the following line to print the figure
# fig3





###############################################################################
#                                                                             #  
#                        JOB TYPE VS CREDIT AMOUNT                            #
#                                                                             #
###############################################################################


fig4 <- plot_ly(
  y = data_good$Credit.Amount, 
  x = data_good$Job, 
  name="Good credit",
  color = '#3D9970', 
  type = "box"
)

fig4 <- fig4 %>%
  add_trace(
    y = data_bad$Credit.Amount, 
    x = data_bad$Job, 
    name="Bad credit", 
    color = "Blue", 
    type = "box"
  )


fig4 <- fig4 %>%
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
# fig4



###############################################################################
#                                                                             #  
#                        JOB TYPE VS CREDIT AMOUNT                            #
#                                                                             #
###############################################################################

fig5 <- data %>%
  plot_ly(type = 'violin') 
fig5 <- fig5 %>%
  add_trace(
    x = data_good$Job,
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
fig5 <- fig5 %>%
  add_trace(
    x = data_bad$Job,
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

fig5 <- fig5 %>%
  layout(
    xaxis = list(
      title = ""  
    ),
    yaxis = list(
      title = "",
      zeroline = F
    )
  )

#fig5
















