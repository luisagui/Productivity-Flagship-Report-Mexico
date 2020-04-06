# There are some errors in the panel, this script will describe and solve each of them.

# First, there Classification codes in 1994 are different.

# We have to map the CMAP classification from 1994 into SCIAN, the latter is used in the rest of the Census years.
# This file maps CMAP to SCIAN

CMAPtoSCIAN<-haven::read_dta("/CMAP to SCIAN corresp (6dig) (adhoc).dta")
CMAPtoSCIAN %>% mutate(year=1994)->CMAPtoSCIAN
Base %>% left:join(CMAPtoSCIAN,by=c("anio"="year","e17=e17")) %>%mutate(e17=if_else(anio==1994,SCIAN,e17)) %>% select(-SCIAN)->Base

# Second, the NIC_NOP_2014 used to match the database is not correct. We have to replace it with the correct CDE.

# read data base from Luis
# read base 14

Base14 %>% %>% left_join(,by=("NIC_NOP_2014"="NIC_NOP_2014")) %>%
data.table::rbindlist(Base,Base14,fill=TRUE)->Base
dim(Base %>% filter(is.na(CDE)))
Base %>% Select(CDE) %>% distinct(CDE) %>% arrange(CDE)->CDES



# Third, there are some variables that have different names but they have the same definition and values, this changes
# betweem census, the decision of which variable to keep has to be made by year.
  
  # The next line is supposed to do the job, but it is too slow, because it is doing it row by row and the final base has more
  # than 15 millions.
  
  # Base %>% rowwise() %>% mutate(k010a_1=if_else(anio<2014,sum(k010a,k100a,na.rm=TRUE),k010a),m030a_1=if_else(anio<2014,sum(m030a,m310a,na.rm=TRUE),m030a),m010a_1=if_else(anio<2014,sum(m010a,m100a,na.rm=TRUE),m010a),k040a_1=if_else(anio<2014,sum(k040a,k411a,na.rm=TRUE),k040a),k041a_1=if_else(anio<2014,sum(k041a,k412a,na.rm=TRUE),k041a)) %>% select(-c(k010a,k100a,m030a,m310a,m010a,m100a,k040a,k411a,k041a,k412a) %>% rename(k010a=k010a_1,m030a=m030a_1,m010a=m010a_1,k040a=k040a_1,k041a=k041a_1))->Base
  
  # This is an alternative, without iterating by row./ I have to check Electricity.
  
  Base %>% mutate(k010a=if_else((anio==1994 | anio==2004 | anio==2009) ,k100a,k010a), m030a=if_else((anio==1999 | anio==2004),m310a,m030a), m010a=if_else((anio==1994 | anio==2004 | anio==2009) ,m100a,m010a) ,k040a=if_else((anio==2004 | anio==2009),k411a,k040a) ,k041a=if_else((anio==2004 | anio==2009),k412a,k041a)) %>% select(-c(k100a,m310a,m100a,k411a,k412a))->Base

# Fourth, there are some variables that were aggregated, for 1994 and 1999, I have to check how they sum up in case of NA's.
# I have to make sure they did not turn into NA's when there is one in the sum.
