r = getOption("repos")
r["CRAN"] = "http://cran.us.r-project.org"
options(repos = r)
install.packages("contrib.url")

install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
#question 1
Customer <- read.csv("D:/praneeta/praneeta/R/R case study 1 (Retail)/Customer.csv")
Transactions <- read.csv("D:/praneeta/praneeta/R/R case study 1 (Retail)/Transactions.csv")
prod_cat_info <- read.csv("D:/praneeta/praneeta/R/R case study 1 (Retail)/prod_cat_info.csv")
customer_trans <- merge(Customer,Transactions,by.x = 'customer_Id',by.y='cust_id')
final_merge <- merge(customer_trans, prod_cat_info, by = 'prod_cat_code', all = T)
View(final_merge)

#by dplyr
custtrans <- full_join(Customer,Transactions,by=c("customer_Id" = "cust_id"))
f_data <- full_join(custtrans,prod_cat_info,by = 'prod_cat_code')
View(f_data)
final_merge$Gender = as.character(final_merge$Gender)
final_merge$DOB = as.Date(final_merge$DOB, format = "%d-%m-%Y")
final_merge$tran_date = as.Date(final_merge$tran_date, format = "%d-%m-%Y")

#question2
#2a
str(final_merge) #column names with datatypes
#2b
head(final_merge,10)
tail(final_merge,10)

#2c
summary(final_merge$Qty)
summary(final_merge$Rate)
summary(final_merge$Tax)
summary(final_merge$total_amt)

#2d
table(factor(final_merge$customer_Id),exclude=NULL)
table(final_merge$DOB,exclude=NULL)
table(final_merge$Gender,exclude=NULL)
table(factor(final_merge$city_code),exclude = NULL)
table(factor(final_merge$transaction_id),exclude = NULL)
table(final_merge$tran_date, exclude=NULL)
table(factor(final_merge$prod_subcat_code), exclude = NULL)
table(factor(final_merge$prod_cat_code),exclude=NULL)
table(final_merge$Store_type, exclude=NULL)
table(final_merge$prod_cat,exclude=NULL)
table(final_merge$prod_subcat, exclude=NULL)

#question 3
#histograms for continuous
library(ggplot2)
ggplot(data = final_merge,aes(x=Qty))+geom_histogram(color="red",bins=10)+ggtitle("Quantity Count")
ggplot(data=final_merge,aes(x=Rate))+geom_histogram(color="blue",bins=10)+ggtitle("Count of Rate")
ggplot(data=final_merge,aes(x=Tax))+geom_histogram(color="blue",bins=10)+ggtitle("Count of Tax")
ggplot(data=final_merge,aes(x=total_amt))+geom_histogram(color="blue",bins=10)+ggtitle("Count of Revenue")

#frequency tables for categorical
barplot(table(factor(final_merge$customer_Id),exclude=NULL),main="Customer ID")
barplot(table(final_merge$DOB,exclude=NULL),main = "DOB")
barplot(table(final_merge$Gender,exclude=NULL),main = "Gender")
barplot(table(factor(final_merge$transaction_id),exclude = NULL),main = "Transaction ID")
barplot(table(factor(final_merge$city_code),exclude = NULL), main = "City Code")
barplot(table(factor(final_merge$prod_subcat_code), exclude = NULL),main = "Product Subcategory Code")
barplot(table(factor(final_merge$prod_cat_code),exclude=NULL),main = "Product Category Code")
barplot(table(final_merge$Store_type, exclude=NULL), main = "Store Type")
barplot(table(final_merge$prod_cat,exclude=NULL),main = "Product Category")
barplot(table(final_merge$prod_subcat, exclude=NULL),main="Product Subcategory")

#q4a
min_date <- min(final_merge$tran_date,na.rm=TRUE)
max_date <- max(final_merge$tran_date,na.rm=TRUE)
range_date <- max_date - min_date
range_date

#q4b
final_merge %>% filter(final_merge$total_amt < 0) %>% nrow()

#question 5
fm <- final_merge %>%
  group_by(Gender,prod_cat) %>%
  summarise(Totalsales = sum(total_amt,na.rm = TRUE))

ggplot(fm, aes(fill=fm$prod_cat, y=fm$Totalsales, x=fm$Gender)) + 
  geom_bar(position="stack", stat="identity") +
  labs(x="Gender", y= "Total Sales", colour = "Product Category") +
  ggtitle("Which product categories are more popular in M and F?") 
  
#question 6
a <- final_merge %>% group_by(final_merge$city_code) %>% summarise(n=length(city_code)) 
max_city <- a[which.max(a$n),]
max_city
paste("max no. of customers are from city code ",max_city$`final_merge$city_code`,":",max_city$n) #percentage
paste("Percentage of customers:",round((max_city$n/sum(a$n))*100,2),"%") #percentage


#question 8
dataa <- final_merge %>%
  group_by(Store_type,prod_cat) %>%
  summarise(TotalRevenue = sum(final_merge$total_amt,na.rm=T))
dataa[dataa$Store_type =='Flagship store' & dataa$prod_cat == 'Clothing',]
dataa[dataa$Store_type =='Flagship store' & dataa$prod_cat == 'Electronics',]

#question 9
datab <- final_merge %>%
  group_by(Gender,prod_cat) %>%
  summarise(TotalRevenue = sum(final_merge$total_amt,na.rm=T))
datab[datab$Gender =='M' & datab$prod_cat == 'Electronics',]

#question 10
t1 <- final_merge %>%
  group_by(customer_Id,total_amt) %>% 
  summarise() 
t2 <- t1 %>%
  group_by(customer_Id) %>%
  summarise(nonneg = length(which(total_amt>0)))
t2
t2 %>% summarise(numberofcust = length(which(t2$nonneg > 10)))
  
#q11a
cust_age <- ((final_merge$tran_date - final_merge$DOB)/365.25) 
final_merge$age_grp <- ifelse(cust_age >=25 & cust_age <=35,"O","Y")

age <- final_merge %>%
  group_by(age_grp,prod_cat) %>%
  summarise(Totalrevenue = sum(total_amt,na.rm=T))

age[age$age_grp == "O" & age$prod_cat == "Electronics",]
age[age$age_grp == "O" & age$prod_cat == "Books",]

#q11b
tran1 <- final_merge %>%
  group_by(tran_date,age_grp) %>%
  summarise(totalrevenue = sum(total_amt,na.rm = T))

tran1[tran1$tran_date >= '2014-01-01' & tran1$tran_date <= '2014-03-01' & tran1$age_grp == "O",]




