#' Author: Lawrence Fung
#'
#' Description: Filter contacts that paid $25 through Paypal for the Oakland Expo 2024 
#' from the "Activity Report" under the tab Activities created by Paypal
#'
#' Date: 6/25/2024

library(tidyverse)

# read the csv file
paypal <- read.csv('Download.CSV', header = TRUE)  # import paypal file  # people who paid for the swag bag
wix <- read.csv('Guest list kmj-health-wellness-expo-in-oakland-hosted-by-scaasf-1 2024-09-14.csv', header = TRUE)  # import WIX file  # for registration


modified_paypal <- paypal %>% 
  mutate(Total.Paid = Gross, Total.After.Fees = Net) %>%  # need to add tick marks if there is white space, even in variable, to ensure R understands it as a single entity
  filter(Item.Title == "Paid Admission $25 (Includes: Food + swag and more)") %>%  # took about  | Type == "Donation Payment"
  select(Name, Date, Status, Type, Subject, Total.Paid, Total.After.Fees, From.Email.Address, Contact.Phone.Number)

modified_paypal_donate <- paypal %>% 
  mutate(Total.Paid = Gross, Total.After.Fees = Net) %>%  # need to add tick marks if there is white space, even in variable, to ensure R understands it as a single entity
  filter(Type == "Donation Payment") %>%  # took about
  select(Name, Date, Status, Type, Subject, Total.Paid, Total.After.Fees, From.Email.Address, Contact.Phone.Number)

  
wix <- wix %>% mutate(Email = tolower(Email))  # change all emails to lowercase from WIX

# Registered$modified_paypal <- ifelse(wix$Email == modified_paypal$From.Email.Address, 'Yes', 'No')
modified_paypal <- modified_paypal %>% 
  mutate(From.Email.Address = tolower(From.Email.Address)) %>% 
  mutate(Registered = ifelse(From.Email.Address %in% wix$Email, 'Yes', 
                             ifelse(tolower(Name) %in% tolower(wix$Guest.name), 'Yes', 'No')))  # registered in WIX?

modified_paypal_donate <- modified_paypal_donate %>% 
  mutate(From.Email.Address = tolower(From.Email.Address)) %>% 
  mutate(Registered = ifelse(From.Email.Address %in% wix$Email, 'Yes', 
                             ifelse(tolower(Name) %in% tolower(wix$Guest.name), 'Yes', 'No'))) 

total_wix <- wix %>% count()  # this is the total number of people registered in WIX

# total_25_people <- modified_paypal %>% count(Subject == 'Paid Admission $25 (Includes: Food + swag and more)')

# create a summary of who registered from the list of those who paid $25
count_registered <- modified_paypal %>% 
  group_by(Registered, Name) %>%
  count(From.Email.Address) %>%
  mutate('# of Tickets' = n) %>%
  select(-c(n)) %>%
  mutate(Total.Number.Registered.of.Paypal = sum(modified_paypal$Registered == 'Yes'))

# seeing just those who paid $25 and how many tickets
write.csv(count_registered, 'C:\\Users\\lawfu\\Documents\\Coding Projects - Self\\Paypal Project\\USE  Just $25 People Paypal Summary.csv', row.names = FALSE)


final_modified_paypal <- rbind(modified_paypal, modified_paypal_donate) %>% arrange(Name) %>% # combining the paid tickets and the donated people
  mutate('# of tickets' = as.numeric(Total.Paid)/25) # calculate the number of tickets possible from the donation

# filter out calculated tickets less than 1 and keep those 1 or more
filtered_final_modified_paypal <- final_modified_paypal %>%
  filter(`# of tickets` >= 1) %>%
  mutate('Registered.on.Wix?' = Registered) %>%
  select(-c(Registered)) %>%
  select(Name, Date, Subject, Total.Paid, From.Email.Address, Contact.Phone.Number, '# of tickets', 'Registered.on.Wix?') %>%
  mutate(Subject = ifelse(Subject == 'Paid Admission $25 (Includes: Food + swag and more)', 'Paid Admission $25', 'Donation/Paid Tickets'))
# seeing all the $25 Tickets and Donations
write.csv(filtered_final_modified_paypal, 'C:\\Users\\lawfu\\Documents\\Coding Projects - Self\\Paypal Project\\USE  Expo Paid Tickets.csv', row.names = FALSE)


# summary of tickets
donation_tickets <- filtered_final_modified_paypal %>%
  filter(Subject == 'Donation/Paid Tickets')
total_donation_tickets <- sum(donation_tickets$'# of tickets')

dollar25_tickets <- modified_paypal %>%
  count()

total_tickets <- sum(filtered_final_modified_paypal$'# of tickets')

total_donation_tickets  # number of tickets possible from donations >= 1 ticket/s
dollar25_tickets  # number of tickets from $25 Admission
total_tickets  # total number of tickets possible from filtered



# write.csv(data_set, file = "Oak Expo Paid Tickets", row.names = FALSE) 
# use ',row.names = FALSE' if you do not want row names
# use 'append = TRUE' if you want to overwrite the file with the same name

# You can specify a different directory or path for the output file. 
# For example, write.csv(df, file = "C:/path/to/output.csv") will 
# create the output.csv file in the specified directory (C:/path/to/).


# test <- paypal %>% filter(Name == "Moira Redmond") %>%
#   select(Name, From.Email.Address)
# head(test)

# (Ctrl + Shift + Enter) to run All
