library(dplyr)
library(CatEncoders)
library(tidyr)
library(stringr)
library(superml)

# Reading thre data
df <- read.csv("data/german_credit_data.csv")


# Start with changing the column names to the required column names
columnNames <- c("Checking.Account", "Duration", "Credit.History", "Purpose", 
                 "Credit.Amount", "Savings.Account/Bonds", "Present.employee", 
                 "Installment.rate", "Personal.Status.and.Sex", "Other.Debters", 
                 "Present.Residence.Since", "Property", "Age.in.Years", 
                 "Other.Installment.plans", "Housing", 
                 "Number.of.existing.credits.at.this.bank", 
                 "Job", "Liable.to.Provide.Maintainance", 
                 "Telephone", "Foreign.Worker", "Outcome")

colnames(df) <- columnNames


###############################################################################
#                                                                             #  
#                             CHANGING DATA FOR EDA                           #
#                                                                             #
###############################################################################

# Defining a function to change the values in the data frame
# This is done using the documentation available at the UCI database
changedataframe <- function(x = "q"){
  if (x == "A11"){
    x = "< 0"
  } 
  else if (x == "A12"){ 
    x = "0 <= Checking < 200"
  } 
  else if (x == "A13"){
    x = "Checking >= 200"
  } 
  else if (x == "A14"){
    x = "No Checking account"
  }
  else if (x == "A30"){
    x = "no credits taken/all credits paid back duly"
  }
  else if (x == "A31"){
    x = "all credits at this bank paid back duly"
  }
  else if (x == "A32"){
    x = "existing credits paid back duly till now"
  }
  else if (x == "A33"){
    x = "delay in paying off in the past"
  }
  else if (x == "A34"){
    x = "critical account/other credits existing (not at this bank)"
  }
  else if (x == "A40"){
    x = " New Car"
  }
  else if (x == "A41"){
    x = "Used Car"
  }
  else if (x == "A42"){
    x = "Furniture/Equipment"
  }
  else if (x == "A43"){
    x = "Radio or Television"
  }
  else if (x == "A44"){
    x = "Domestic Appliances"
  }
  else if (x == "A45"){
    x = "Repairs"
  }
  else if (x == "A46"){
    x = "Education"
  }
  else if (x == "A47"){
    x = "Vacation"
  }
  else if (x == "A48"){
    x = "Retraining"
  }
  else if (x == "A49"){
    x = "Business"
  }
  else if (x == "A410"){
    x = "Others"
  }
  else if (x == "A61"){
    x = "Less than 100"
  }
  else if (x == "A62"){
    x = "100 <= Savings < 500"
  }
  else if (x == "A63"){
    x = "500 <= Savings < 1000"
  }
  else if (x == "A64"){
    x = "Savings >=1000"
  }
  else if (x == "A65"){
    x = "Unkown/No Savings Account"
  }
  else if (x == "A71"){
    x = "Unemployed"
  }
  else if (x == "A73"){
    x = "1 <= Exp < 4"
  }
  else if (x == "A74"){
    x = "4 <= Exp <7"
  }
  else if (x == "A75"){
    x = "Exp >= 7"
  }
  else if (x == "A72"){
    x = "Exp < 1"
  }
  else if (x == "A91"){
    x = "Male and Divorced/Seperated"
  }
  else if (x == "A92"){
    x = "Female and Divorced/Seperated/Married"
  }
  else if (x == "A93"){
    x = "Male and Single"
  }
  else if (x == "A94"){
    x = "Male and Married"
  }
  else if (x == "A95"){
    x = "Female and Single"
  }
  else if (x == "A101"){
    x = "None"
  }
  else if (x == "A102"){
    x = "Co-Applicant"
  }
  else if (x == "A103"){
    x = "Guarantor"
  }
  else if (x == "A121"){
    x = "Real Estate"
  }
  else if (x == "A122"){
    x = "Life Insurance"
  }
  else if (x == "A123"){
    x = "Car or other Property"
  }
  else if (x == "A124"){
    x = "Unkown/No Property"
  }
  else if (x == "A141"){
    x = "Bank"
  }
  else if (x == "A142"){
    x = "stores"
  }
  else if (x == "A143"){
    x = "None"
  }
  else if (x == "A151"){
    x = "Rent"
  }
  else if (x == "A152"){
    x = "Own"
  }
  else if (x == "A153"){
    x = "For Free"
  }
  else if (x == "A171"){
    x = "Unemployed/Unskilled"
  }
  else if (x == "A172"){
    x = "Unskilled"
  }
  else if (x == "A173"){
    x = "Skilled-Employee/Official"
  }
  else if (x == "A174"){
    x = "Self-Employed/Highly Qualified"
  }
  else if (x == "A191"){
    x = "No Number"
  }
  else if (x == "A192"){
    x = "Registered Number"
  }
  else if (x == "A201"){
    x = "Yes"
  }
  else if (x == "A202"){
    x = "No"
  }
  
  
  
  return(x)
  
}



# Now to change the target variable we can make another function
changetargetvar <- function(x = 4){
  if (x == 1){
    x = "Good"
  }
  else if (x == 2){
    x = "Bad"
  }
}



# Now mutating the qualitative values to get better understanding
new_data <- data.frame(apply(df, MARGIN = c(1, 2), changedataframe))

new_data[, "Outcome"] <- sapply(new_data[, "Outcome"], changetargetvar)



# Splitting the Personal Status and Sex Column into 2
per_stat <- data.frame(str_split_fixed(new_data[, "Personal.Status.and.Sex"], 
                                            "and ", 2))
colnames(per_stat) <- c("Sex", "Personal Status")
new_data <- subset(new_data, select = -c(9))
new_data <- cbind(new_data, per_stat[!names(per_stat) %in% names(new_data)])


# We can save this data for exploratory data analysis
write.csv(new_data, "data/eda-german-credit.csv", row.names = FALSE)


###############################################################################
#                                                                             #  
#                    CHANGING DATA FOR MACHINE LEARNING                       #
#                                                                             #  
###############################################################################

# Defining a new variable which takes col names of qualitative columns
catColumns <- c("Checking.Account", "Credit.History", "Purpose", 
                "Savings.Account.Bonds", "Present.employee", 
                "Other.Debters", "Property", "Other.Installment.plans", 
                "Housing", "Job", "Telephone", "Foreign.Worker", "Outcome", 
                "Sex", "Personal.Status")


tf_data <- data.frame(new_data)


for (column in catColumns){
  label <- LabelEncoder$new()
  tf_data[, column] <- label$fit_transform(tf_data[, column])
}




write.csv(tf_data, "data/machine-ready-credit.csv", row.names = FALSE)

