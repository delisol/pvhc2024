library(here)
library(tidyverse)
library(janitor)
library(ggplot2)
library(extrafont)
library(forcats)
library(blsR)
# library(epiDisplay)
library(gt)

pvhcAccountTree <- read_csv("C:/Users/dsole/OneDrive/Personal Vault/data/pvhc/pvhcAccountTree.csv") %>% 
  clean_names()

pvhcAllTrans <- read_csv("C:/Users/dsole/OneDrive/Personal Vault/data/pvhc/pvhcAllTrans.csv" , 
                         col_types = cols(Date = col_date(format = "%m/%d/%Y"))) %>%
  clean_names() %>%
  left_join(. , 
            pvhcAccountTree %>% select(type, full_account_name) ,
            by = 'full_account_name') %>%
  fill(date) %>%
  mutate(amount_num = str_remove_all(amount_num , '\\)')) %>%
  mutate(amount_num = str_replace_all(amount_num , '\\(' , '-')) %>%
  mutate(amount_num = str_remove_all(amount_num , ',')) %>%
  mutate(amount_num = as.numeric(amount_num)) %>%
  separate(full_account_name , into = c('acct1' , 'acct2') , 
           sep = ':' , remove = FALSE) %>%
  mutate(acct1 = str_replace_all(acct1 , 'Expenses' , 'Expense')) %>%
  mutate(acct1 = ifelse(
    str_detect(acct1 , "Own") , "Owners draw" , acct1))


pvhcAllTrans %>% filter(type == 'INCOME' | type == 'EXPENSES') %>%
  group_by(
    year(date) , 
    type , 
    full_account_name) %>%
  summarise(amount_num = sum(amount_num))

# All urban consumers, not seasonally adjusted
t <- get_series_table(series_id = 'CUUR0000SA0' , start_year = 2017, 
                      end_year = as.numeric(format(Sys.Date() , '%Y'))) %>%
  filter(periodName == 'December')




t %>%
  rename(index = value) %>%
  mutate(currIndex = 
           as.numeric(head(. , 1) %>%
                        select(index))) %>%
  filter(year == y & periodName == m) %>%
  mutate(amt = d) %>%
  mutate(realAmt = amt*(index/currIndex)) %>%
  mutate(pctOld = realAmt / amt , 
         pctDec = (amt - realAmt)/amt , 
         amtDec = (amt - realAmt) , 
         indic = sprintf('%s ($%s)' , i , amt) , 
         impDate = sprintf('$%s in %s %s' , 
                           d , periodName , year)) %>%
  mutate(then = sprintf('%s %s' , periodName , year)) %>%
  mutate(yrsToNow = as.numeric(trunc(interval(my(then) , now) / years(1)))) %>%
  mutate(mosToNow = as.numeric(trunc(interval(my(then) , now) / months(1)))) %>%
  mutate(timeToNow = 
           sprintf('%s years , %s months' ,
                   yrsToNow , mosToNow-12*yrsToNow)) %>%
  select(indic, timeToNow , impDate , amt , realAmt , pctOld , amtDec , pctDec)
}


pvhcByYr <- pvhcAllTrans %>% 
  filter(type == 'INCOME' | type == 'EXPENSE') %>%
  filter(year(date) > 2016) %>%
  filter(str_detect(full_account_name , 'draw' , negate = TRUE)) %>%
  group_by(
    year(date) , 
    type) %>%
    summarise(amount_num = sum(amount_num)) %>%
  rename(year = `year(date)`) %>%
  pivot_wider(names_from = type , 
               values_from = amount_num) %>%
  mutate(INCOME = -INCOME) %>%
  mutate(netIncome = INCOME - EXPENSE)

infAdjPVHC <-
  t %>% 
  left_join(pvhcByYr , by = 'year') %>%
  rename(currYr = value) %>%
  mutate(baseYr = as.numeric(tail(t,1) %>% select(value))) %>%
  mutate(indexAdj = baseYr/currYr) %>%
  mutate(realNet = indexAdj*netIncome , 
         realExp = indexAdj*EXPENSE , 
         realInc = indexAdj*INCOME) %>%
  arrange(year) %>%
  mutate(year = as.character(year)) %>%
  select(year , INCOME , realInc , EXPENSE , realExp , netIncome , realNet) %>%
  gt() %>%
  tab_header(title = 'Trends in income and expense' , 
             subtitle = 'Portland Vet House Calls, 2017 - 2023') %>%
  cols_label(year = 'Year' , 
             INCOME = html('Nominal <br>income') , 
             realInc = html('Real <br>income') , 
             EXPENSE = html('Nominal <br>expense') , 
             realExp = html('Real <br>expense') , 
             netIncome = html('Nominal <br>net income') ,
             realNet = html('Real <br>net income')) %>%
  fmt_currency(columns = c(INCOME:realNet) , decimals = 0) %>%
  tab_footnote(footnote = sprintf('Adjusted to %s dollars.' ,
                                  tail(t,1) %>% select(year)) , 
               locations = cells_column_labels(c(3,5,7))) %>%
  tab_footnote(footnote = 'PPP dollars included in income for 2020 and 2021.' , 
               locations = (cells_body(columns = 1 , rows = c(4,5))) , 
               placement = 'right') %>%
  tab_footnote(footnote = 
                 'Source: Consumer Price Index for All Urban Consumers, 
               U.S. Bureau of Labor Statistics')
  
gtsave(data = infAdjPVHC , filename = 'Inflation adjustement PVHC.pdf')  

