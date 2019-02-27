########################################
# Teste 1B          
# Nome(s):HIRLEY DAYAN LOURENCO DA SILVA
########################################

#ATENÇÃO: Você precisa fazer o download do arquivo chustomer_churn.csv e
#           deixá-lo na mesma pasta que o arquivo teste1b.R
#         Depois, Fornecer o caminho completo até o arquivo csv.
#         Exemplos:
#              -Windows:
#                  "C:/Users/Andre/Downloads/customer_churn.csv"
#              -Ubuntu
#                  "/home/andre/Downloads/customer_churn.csv"
#              -Mac OS
#                  "/Users/andre/Downloads/customer_churn.csv"

customer_churn <- read.table("customer_churn.csv", sep=",", header = TRUE, stringsAsFactors= FALSE)

########################################
# Item 1 (0.5 ponto)
########################################

customer_churn <- customer_churn[!duplicated(customer_churn),]
customer_churn
customer_churn$gender <- as.factor(customer_churn$gender)
customer_churn$Contract <- as.factor(customer_churn$Contract)

########################################
# Item 2 (0.5 ponto)
########################################

customer_churn
customer_churn[customer_churn=='Yes'] <- TRUE
customer_churn[customer_churn=='No'] <- FALSE
sapply(customer_churn, class)
customer_churn$Partner <- as.logical(customer_churn$Partner)
customer_churn$Dependents <- as.logical(customer_churn$Dependents)
customer_churn$Churn <- as.logical(customer_churn$Churn)
sapply(customer_churn, class)

########################################
# Item 3 (0.5 ponto)
########################################

max_tenure <- customer_churn[customer_churn$tenure==max(customer_churn$tenure),]
max_tenure

########################################
# Item 4 (1.0 ponto)
########################################

max_tenure_50 <- customer_churn[customer_churn$MonthlyCharges >= 50,]
max_tenure_50 <- max_tenure_50[max_tenure_50$tenure==max(max_tenure_50$tenure),]
max_tenure_50

########################################
# Item 5 (1.0 ponto)
########################################

min_tenure_mtm <- customer_churn[customer_churn$Contract == "Month-to-month",]
min_tenure_mtm <- min_tenure_mtm[min_tenure_mtm$tenure==min(min_tenure_mtm$tenure),]
min_tenure_mtm

########################################
# Item 6a (1.0 ponto)
########################################

customer_churn.nochurn <- customer_churn[!customer_churn$Churn,]
total_mtm <- sum(customer_churn.nochurn$MonthlyCharges[customer_churn.nochurn$Contract=="Month-to-month"])
total_mtm
total_year <- sum(customer_churn.nochurn$MonthlyCharges[customer_churn.nochurn$Contract=="One year"])
total_year
total_two_year <- sum(customer_churn.nochurn$MonthlyCharges[customer_churn.nochurn$Contract=="Two year"])
total_two_year

########################################
# Item 6b (0.5 ponto)
########################################

regular_customers <- length(customer_churn.nochurn[customer_churn.nochurn$tenure > 12, "customerID"])
regular_customers

########################################
# Item 7a (0.5 ponto)
########################################

customer_churn.churn <- customer_churn[customer_churn$Churn,]
customers_with_dependents <- length(customer_churn.churn[customer_churn.churn$Partner & customer_churn.churn$Dependents, "customerID"])
customers_with_dependents

########################################
# Item 7b (0.5 ponto)
########################################

customers_mtm <- length(customer_churn.churn[customer_churn.churn$Contract=="Month-to-month", "customerID"]); 
customers_mtm
customers_year <- length(customer_churn.churn[customer_churn.churn$Contract=="One year", "customerID"]); 
customers_year
customers_two_year <- length(customer_churn.churn[customer_churn.churn$Contract=="Two year", "customerID"]); 
customers_two_year

########################################
# Item 7c (0.5 ponto)
########################################

customers_two_years <-length(customer_churn.churn[customer_churn.churn$tenure >=24, "customerID"])
customers_two_years

########################################
# Item 7d (0.5 ponto)
########################################

customer_churn[customer_churn$Churn==TRUE,"customerID"]
customer_churn[customer_churn$Churn==TRUE,"MonthlyCharges"]
accumulated_discount <- as.data.frame(cbind(customer_churn.churn$customerID, customer_churn.churn$MonthlyCharges * 5 * 12 / 100))
names(accumulated_discount) <- c("customerID", "discount")
accumulated_discount
