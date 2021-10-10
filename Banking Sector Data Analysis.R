# Set the working directory
setwd("C:/Users/HATYTUDE CONSULTING/Downloads/Practice Datasets")

# Inspect the data files available in the directory
dir()

# Load in the required packages
library(dplyr)
library(tidyr)
library(broom)
library(data.table)
library(purrr)
library(NLP)
library(tm)
library(readr)
library(readxl)
library(ggplot2)
library(gganimate)
library(ggthemes)
library(tabulizer)
library(pdftools)
library(stringr)
library(rebus)

# Import the files into workspace
excel_sheets("Selected Banking Data Q4 2020.xlsx")
credit_by_sector <- read_excel("Selected Banking Data Q4 2020.xlsx", sheet = 2, col_names = FALSE)
non_perf_loan <- read_excel("Selected Banking Data Q4 2020.xlsx", sheet = 4)

# Separate the credit by sector data into two tables
dmb_sectorial_credit_all <- credit_by_sector[4:11, -10]
dmb_sectorial_percent_total <- credit_by_sector[16:23, -c(10, 21)]

# Name the columns of the new tables
col <- c("Month-year", "Agriculture", "Mining & Quarrying", "Manufacturing", 
         "Oil & Gas", "Power & Energy", "Construction", "General Commerce", 
         "Government", "Real Estate", "Fin&Cap Market", "Education", 
         "O&G (Services)", "Power & Energy (Services)", "General", 
         "Info & Comm", "Transport & Storage", "Others", 
         "Total Credit", "QQGrowth Rate (%)")
colnames(dmb_sectorial_credit_all) <- col
colnames(dmb_sectorial_percent_total) <- col


num_dat <- dmb_sectorial_credit_all %>% select(-`Month-year`)
date_dat <- dmb_sectorial_credit_all %>% select(`Month-year`)
num_dat <- data.frame(sapply(num_dat, MARGIN = 2, FUN = as.numeric))
dmb_sectorial_credit_all <- cbind(date_dat, num_dat)
dmb_sectorial_credit_all$Total.Credit <- NULL
dmb_sectorial_credit_all <- gather(dmb_sectorial_credit_all, key = "Sector", value = col, 2:19)
dmb_sectorial_credit_all$credit_amount <- dmb_sectorial_credit_all$col
dmb_sectorial_credit_all$Credit_amount <- dmb_sectorial_credit_all$credit_amount/1000000
dmb_sectorial_credit_all$col <- NULL

# Tidying up the QQGrowth Rate to reflect correct percentages
qq_dat <- dmb_sectorial_credit_all[137:144, ]
qq_dat$Credit_amount <- qq_dat$Credit_amount * 1000000
qq_dat$Credit_amount <- replace_na(qq_dat$Credit_amount, replace = 0)
dmb_sectorial_credit_all <- rbind(dmb_sectorial_credit_all[1:136, ], qq_dat)

# Performing the same function as the previous one on the table of sectorial share as a % of Total Credit
num_dat <- dmb_sectorial_percent_total %>% select(-`Month-year`)
date_dat <- dmb_sectorial_percent_total %>% select(`Month-year`)
num_dat <- data.frame(sapply(num_dat, MARGIN = 2, FUN = as.numeric))
dmb_sectorial_percent_total <- cbind(date_dat, num_dat)
dmb_sectorial_percent_total$Total.Credit <- NULL
dmb_sectorial_percent_total <-   gather(dmb_sectorial_percent_total, key = "Sector", value = col, 2:18)
dmb_sectorial_percent_total$Percentage_of_total_credit <- dmb_sectorial_percent_total$col
dmb_sectorial_percent_total$col <- NULL
dmb_sectorial_percent_total$Percentage_of_total_credit <- as.numeric(dmb_sectorial_percent_total$Percentage_of_total_credit)

# Drop the last column of the NPL data
non_perf_loan$...6 <- NULL

# Separate the two tables in the NPL data
npl_summary <- non_perf_loan[1:56, -6]
yoy_sect_change <- non_perf_loan[63:85, ]

# Convert all necessary columns in npl_summary to numeric
num_dat <- npl_summary %>% select(-ITEMS)
item_dat <- npl_summary %>% select(ITEMS)
num_dat <- data.frame(sapply(num_dat, MARGIN = 2, FUN = as.numeric))
npl_summary <- cbind(item_dat, num_dat)
npl_summary$NPL.Gross.Loans.. <- round(npl_summary$NPL.Gross.Loans.., digits = 1)
npl_summary$percent.npl <- npl_summary$NPL.Gross.Loans..
npl_summary$NPL.Gross.Loans.. <- NULL
npl_summary$period <- npl_summary$ITEMS
npl_summary$ITEMS <- NULL
npl_summary$period <- str_to_upper(npl_summary$period)
npl_summary$Gross.loans_in_trillion <- npl_summary$Gross.loans/1000000000000
npl_summary$Specific.provisions_in_trillions <- npl_summary$Specific.provisions/1000000000000
npl_summary$Nonperforming.loans_in_trillions <- npl_summary$Nonperforming.loans/1000000000000
npl_summary$Gross.loans <- NULL
npl_summary$Nonperforming.loans <- NULL
npl_summary$Specific.provisions <- NULL
npl_summary <- npl_summary %>% select(period, Gross.loans_in_trillion, Specific.provisions_in_trillions, Nonperforming.loans_in_trillions, percent.npl)

# Tidy the year on year sectorial npl change data
  # Drop the Items column
  yoy_sect_change$ITEMS <- NULL
  # Change variable names
  new.names <- c("Sector", "Total NPL Dec-2019 (N'Bn)", "Total NPL Dec-2020 (N'Bn)", "Change in NPL (N'Bn)", "% Change in NPL")
  colnames(yoy_sect_change) <- new.names
  # Remove the rows with zero values
  yoy_sect_change <- yoy_sect_change[-19, ]
  # Convert all variables except the first variable to numeric
  num_dat <- yoy_sect_change %>% select(-Sector)
  sec_dat <- yoy_sect_change %>% select(Sector)  
  num_dat <- data.frame(sapply(num_dat, MARGIN =2, FUN = as.numeric))
  yoy_sect_change <- cbind(sec_dat, num_dat)  
  yoy_sect_change$X..Change.in.NPL <- round(yoy_sect_change$X..Change.in.NPL, digits = 1)
  yoy_sect_change$percent_change_npl <-   yoy_sect_change$X..Change.in.NPL
  yoy_sect_change$X..Change.in.NPL <- NULL  
  yoy_sect_change <- yoy_sect_change[-22, ]
  # Adjusting the calculation orientation of the change in npl variable
  yoy_sect_change$Change.in.NPL..N.Bn. <- (yoy_sect_change$Total.NPL.Dec.2019..N.Bn. - yoy_sect_change$Total.NPL.Dec.2020..N.Bn.)
  yoy_sect_change$percent_change_npl <- (yoy_sect_change$Change.in.NPL..N.Bn./yoy_sect_change$Total.NPL.Dec.2019..N.Bn.) * 100
  yoy_sect_change$percent_change_npl <- round(yoy_sect_change$percent_change_npl, digits = 1)
  
# Remove unused objects from workspace
non_perf_loan <- NULL  

# Inspect the various characteristics of our data with visual aids
# Top 5 sectors based on credit allocation
dmb_sectorial_credit_all %>% group_by(`Month-year`) %>% 
  filter(Sector != "QQGrowth.Rate....") %>% 
  top_n(5, Credit_amount) %>% ggplot(aes(x = reorder(Sector, -Credit_amount), y = Credit_amount)) + 
  geom_col() + scale_y_continuous() + labs(x= "Sectors", y = "Credit Amount in Trillion", title = "Sectors with the Highest Credit Allocation across 2019 and 2020") + 
  facet_wrap(vars(`Month-year`), scales = "free") 

# The growth rate of credit allocation
dmb_sectorial_credit_all %>% filter(Sector == "QQGrowth.Rate...." & `Month-year` != "Q1 2019") %>% 
  ggplot(aes(reorder(`Month-year`, Credit_amount), y = Credit_amount)) + geom_hline(yintercept = 0, linetype = 2) + 
  geom_col() + theme_bw() + theme_minimal() + labs(x = "Quarter", y = "Growth Rate (%)", title = "Growth Rate of the Total Credit Given out by Banks at the end of each Quarter")

# 5 least sectors based on credit allocation
dmb_sectorial_credit_all %>% filter(row(dmb_sectorial_credit_all) < 129) %>% 
  group_by(`Month-year`) %>% top_n(-5, Credit_amount) %>% 
  ggplot(aes(x = reorder(Sector, -Credit_amount), y = Credit_amount, fill = Sector)) + 
  geom_col() + scale_y_continuous() + labs(x= "Sectors", y = "Credit Amount(Trillion)", title = "Quarterly Distribution of the 5 Sectors with the Least Amount of Credit Available to them") + 
  facet_wrap(vars(`Month-year`), scales = "free") + 
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())

 Inspect the growth rate of credit allocation to each sector between 2019 and 2020
dmb_sectorial_percent_total %>% group_by(`Month-year`) %>% filter(Sector != c("General", "General Commerce")) %>% 
  ggplot(aes(x = Sector, y = Percentage_of_total_credit, fill = Sector)) + geom_col() + 
  theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank()) + 
  coord_flip() + facet_wrap(vars(`Month-year`)) + labs(title = "The Percentage of the Credit given to each Sector relative to the Total Credit  between 2019 and 2020")

# Inspect the top 5 sectors with highest percent share of total credit across the first quarter of 2019 and 2020
dmb_sectorial_percent_total %>% 
  filter(`Month-year` %in% str_subset(dmb_sectorial_credit_all$`Month-year`, pattern = START %R% "Q1")) %>% 
  group_by(`Month-year`) %>% top_n(5, wt = Percentage_of_total_credit) %>% 
  ggplot(aes(x = reorder(Sector, Percentage_of_total_credit), y = Percentage_of_total_credit)) + 
  geom_col(fill = "green") + facet_wrap(vars(`Month-year`), scales = "free") + 
  labs(x = "Sector", title = "The 5 Sectors with the Highest Share of Total Credit Allocated")


# Inspect the top 5 sectors with highest percent share of total credit across the second quarter of 2019 and 2020
dmb_sectorial_percent_total %>% 
  filter(`Month-year` %in% str_subset(dmb_sectorial_credit_all$`Month-year`, pattern = START %R% "Q2")) %>% 
  group_by(`Month-year`) %>% top_n(5, wt = Percentage_of_total_credit) %>% 
  ggplot(aes(x = reorder(Sector, Percentage_of_total_credit), y = Percentage_of_total_credit)) + 
  geom_col(fill = "red") + facet_wrap(vars(`Month-year`), scales = "free") + 
  labs(x = "Sector", title = "The 5 Sectors with the Highest Share of Total Credit Allocated")

# Inspect the top 5 sectors with highest percent share of total credit across the third quarter of 2019 and 2020
dmb_sectorial_percent_total %>% 
  filter(`Month-year` %in% str_subset(dmb_sectorial_credit_all$`Month-year`, pattern = START %R% "Q3")) %>% 
  group_by(`Month-year`) %>% top_n(5, wt = Percentage_of_total_credit) %>% 
  ggplot(aes(x = reorder(Sector, Percentage_of_total_credit), y = Percentage_of_total_credit)) + 
  geom_col(fill = "orange") + facet_wrap(vars(`Month-year`), scales = "free") + 
  labs(x = "Sector", title = "The 5 Sectors with the Highest Share of Total Credit Given Out")

# Inspect the top 5 sectors with highest percent share of total credit across the fourth quarter of 2019 and 2020
dmb_sectorial_percent_total %>% 
  filter(`Month-year` %in% str_subset(dmb_sectorial_credit_all$`Month-year`, pattern = START %R% "Q4")) %>% 
  group_by(`Month-year`) %>% top_n(5, wt = Percentage_of_total_credit) %>% 
  ggplot(aes(x = reorder(Sector, Percentage_of_total_credit), y = Percentage_of_total_credit)) + 
  geom_col(fill = "purple") + facet_wrap(vars(`Month-year`), scales = "free") + 
  labs(x = "Sector", title = "The 5 Sectors with the Highest Share of Total Credit Given Out")

# What is the pattern of increase or decrease of the amount of credit extended to each of the top sectors across 2019 and 2020
dmb_sectorial_credit_all %>% filter(Sector %in% c("Fin.Cap.Market", "Government", "Manufacturing", "Oil...Gas", "O.G..Services.", "General")) %>% 
  ggplot(aes(x = `Month-year`, y = Credit_amount)) + 
  geom_col(fill = "violet") + facet_wrap(vars(Sector), scales = "free") + 
  labs(x = "Quarter", y = "Credit allocated (Trillion)", title = "Credit Distribution for the Top Sectors across 2019 and 2020")

# Investigate the rate at which each sector improved on its non performing loans between 2019 and 2020
yoy_sect_change %>% ggplot(aes(x = Sector, y = percent_change_npl)) + 
  geom_col(fill = "blue") + scale_y_continuous(n.breaks = 20) + 
  ylab("Percentage Change in Non Performing Loans") + labs(title = "Rate of Improvement on Non Performing Loans between 2019 and 2020") + coord_flip()

# How has the total amount of loans given out by banks increased across each quarter between 2010 and 2020
npl_summary %>% filter(row(npl_summary) >= 13 & period %in% str_subset(npl_summary$period, pattern = START %R% "1ST QTR")) %>% 
  ggplot(aes(x = period, y = Gross.loans_in_trillion)) + geom_col() + labs(x = "Quarterly Period", title = "Total Credit by Banks every first quarter, 2010 to 2020")

npl_summary %>% filter(row(npl_summary) >= 13 & period %in% str_subset(npl_summary$period, pattern = START %R% "2ND QTR")) %>% 
  ggplot(aes(x = period, y = Gross.loans_in_trillion)) + geom_col() + labs(x = "Quarterly Period", title = "Total Credit by Banks every second quarter, 2010 to 2020")

npl_summary %>% filter(row(npl_summary) >= 13 & period %in% str_subset(npl_summary$period, pattern = START %R% "3RD QTR")) %>% 
  ggplot(aes(x = period, y = Gross.loans_in_trillion)) + geom_col() + labs(x = "Quarterly Period", title = "Total Credit by Banks every third quarter, 2010 to 2020")

npl_summary %>% filter(row(npl_summary) >= 13 & period %in% str_subset(npl_summary$period, pattern = START %R% "4TH QTR")) %>% 
  ggplot(aes(x = period, y = Gross.loans_in_trillion)) + geom_col() + labs(x = "Quarterly Period", title = "Total Credit by Banks every fourth quarter, 2010 to 2020")

# How has the non performing loans increase across each quarter between 2010 and 2020
npl_summary %>% filter(row(npl_summary) >= 13 & period %in% str_subset(npl_summary$period, pattern = START %R% "1ST QTR")) %>% 
  ggplot(aes(x = period, y = Nonperforming.loans_in_trillions)) + geom_col() + labs(x = "Quarterly Period", title = "Total Amount of Non Performing Credits recorded by Banks every first quarter, 2010 to 2020")

npl_summary %>% filter(row(npl_summary) >= 13 & period %in% str_subset(npl_summary$period, pattern = START %R% "2ND QTR")) %>% 
  ggplot(aes(x = period, y = Nonperforming.loans_in_trillions)) + geom_col() + labs(x = "Quarterly Period", title = "Total Amount of Non Performing Credits recorded by Banks every second quarter, 2010 to 2020")

npl_summary %>% filter(row(npl_summary) >= 13 & period %in% str_subset(npl_summary$period, pattern = START %R% "3RD QTR")) %>% 
  ggplot(aes(x = period, y = Nonperforming.loans_in_trillions)) + geom_col() + labs(x = "Quarterly Period", title = "Total Amount of Non Performing Credits recorded by Banks every third quarter, 2010 to 2020")

npl_summary %>% filter(row(npl_summary) >= 13 & period %in% str_subset(npl_summary$period, pattern = START %R% "4TH QTR")) %>% 
  ggplot(aes(x = period, y = Nonperforming.loans_in_trillions)) + geom_col() + labs(x = "Quarterly Period", title = "Total Amount of Non Performing Credits recorded by Banks every fourth quarter, 2010 to 2020")


dmb_sectorial_credit_all %>% group_by(`Month-year`) %>% 
  filter(Sector != "QQGrowth.Rate....") %>% 
  top_n(5, Credit_amount) %>% sum(Credit_amount)

dmb_sectorial_credit_all %>% filter(row(dmb_sectorial_credit_all) < 129) %>% group_by(`Month-year`) %>% 
  top_n(-5, Credit_amount) %>% summarise(Total = sum(Credit_amount))
