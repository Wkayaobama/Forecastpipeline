install.packages("tidyverse")
install.packages("DBI")
install.packages("odbc")
install.packages("lubridate")
install.packages("janitor")
install.packages("plotly")


library(tidyverse)
library(odbc)
library(DBI)
library(dbplyr)
library(dplyr)
library(janitor)
library(plotly)
# con <- DBI::dbConnect(odbc(),
#               DRIVER = "odbc driver 17 for sql server",
#               SERVER = 'wk-hubspot-srv.database.windows.net',
#               DATABASE = 'WK_Hubspot_WH',
#               UID = 'ayaobama',
#               PWD = 'Wisekey18$'
#                       
#                       )
# 
# 
# select_sql ="SELECT*from dbo.DealCompanyAssociations"
# dbGetQuery(con,select_sql)
# db_dca <- tbl(con,in_schema("dbo","DealCompanyAssociations"))
# db_dca2 <- tbl(con,in_schema("dbo","DealCompanyAssociations"))
# db_dca %>% glimpse()
# db_dca %>% count(CompanyId)
# 
# db_contact <- tbl(con,in_schema("dbo","DealPipeline"))
# db_pipe <- tbl(con,in_schema("dbo","Pipeline"))
# 
# 
# db_contact
# dbListTables(con)
# db_dca %>% left_join(db_dca2)
# 


df_pipe <- read_csv("C:/Users/jleonor/Downloads/dataformatter.csv")
df_pipe
names(df_pipe)
a <- df_pipe %>% 
  select(`INFO - Sales Region`,`OPP CUST - Final Customer`,`Deal Stage`,contains('Pipeline'),`Amount`) %>% 
  pivot_longer(cols = c(contains('Pipeline'),Amount),names_to ='Year',names_prefix = 'Pipeline ',names_pattern = '(\\d{4})',values_to = 'PipeValue') %>% 
  mutate(weighted_pipe_value = case_when(
                        `Deal Stage`== 'Design In' ~ PipeValue * 0.5,
                        `Deal Stage`== 'Closed Won' ~ PipeValue * 1,
                        `Deal Stage`== 'Identified' ~ PipeValue * 0.1,
                        `Deal Stage`== 'Qualified' ~ PipeValue * 0.2
                        
                        
                      
                        ))

a <- clean_names(a)
a
plot1 <- a %>% ggplot(aes(x=info_sales_region,y=pipe_value,fill=year))+geom_col(position = "dodge")+theme_minimal()
ggplotly(plot1)

a %>% group_by(info_sales_region,year) %>%
        summarise(pipe_value = sum(pipe_value)
) %>% dplyr::rename("region" = info_sales_region )%>%
  pivot_wider(id_cols= year,names_from = region, values_from = pipe_value) %>% 
  rowwise() %>% mutate(total= sum(c_across(2:4))) %>% 
  ungroup() %>% 
  pivot_longer(cols = -c(year),names_to = "region",values_to = "pipe_value") %>% 
  ggplot(aes(x=region,y=pipe_value,fill=year))+geom_col(position = "dodge")+theme_minimal()

