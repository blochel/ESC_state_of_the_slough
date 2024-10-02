
# State of the Slough -----------------------------------------------------


# set up ------------------------------------------------------------------


library("tidyverse")
library("readxl")
library("lubridate")
library("dplyr")




# hydro data --------------------------------------------------------------

TR_slough<-
  file.path(
    '/Provisional Hydrolab Data 23-24',
    'TR_Hydrolab_Data.xlsx') %>% 
  read_excel(sheet="TR") %>% 
  as.data.frame() %>% 
  #rename columns with a space
  dplyr::rename("Salinity" = "Salinity...6") %>% 
  #select columns of interest
  dplyr::select(TMSTAMP, Temp, Cond, Salinity, Depth) %>% 
  #add PCT
  mutate(PCT = 13) %>%
  mutate(LABEL =  as.Date(as.character(as.POSIXct(TMSTAMP,"%Y-%m-%d")))) %>% 
  group_by(LABEL) %>% 
  dplyr::summarize(ave_depth = mean(Depth, na.rm = TRUE), 
            ave_sal = mean(Salinity,  na.rm = TRUE),
            PCT = PCT,
            .groups = 'drop') %>% 
  mutate(date_number = paste(month(LABEL), day(LABEL), sep = '-')) 


old_data<- 
  file.path(
    '/Hydrology/Quality Checked Data_all years/Hydro Files',
    'TR_HYDRO.xlsx') %>% 
  read_excel(sheet="TR Complete Hydrology")%>% 
  as.data.frame() %>% 
  dplyr::select(LABEL, Depth, Salinity) %>% 
  mutate(date_number = paste(month(LABEL), day(LABEL), sep = '-')) %>% 
  group_by(date_number) %>% 
  dplyr::summarize(min_depth = min(Depth, na.rm = T),
            max_depth = max(Depth, na.rm = T), 
            min_sal = min(Salinity, na.rm = T),
            max_sal = max(Salinity, na.rm = T),
            D_Q25 = quantile(Depth, probs = c(.25), na.rm = TRUE),
            D_Q75 = quantile(Depth, probs = c(.75), na.rm = TRUE),
            S_Q25 = quantile(Salinity, probs = c(.25), na.rm = TRUE),
            S_Q75 = quantile(Salinity, probs = c(.75), na.rm = TRUE),
            .groups = 'drop') 


TR_slough2<-
  left_join(old_data,TR_slough, by = 'date_number') 


#pivot_longer(
#  cols = min_depth:PCT,
#  names_to = 'variable',
#  values_to = 'value') 
#%>% 




ggplot(TR_slough2%>% 
         filter(day(LABEL) != '29' | month(LABEL) == 2),
       aes(x = LABEL, 
           y= ave_depth, 
           ymin = D_Q25, 
           ymax = D_Q75)) +
  geom_ribbon(fill = "grey90")+ 
  geom_line(aes(y = min_depth), 
            color = 'black', 
            linetype  = 2,
            size = 0.5) +
  geom_line(aes(y = max_depth), 
            color = 'black', 
            linetype  = 2,
            size = 0.5) +
  geom_line(color = 'red',
            size = 1)+
  geom_line(aes(y = PCT), 
            color = 'pink', 
            size = 1)+
  labs(y = 'Depth (cm)', x = '') +
  theme_bw() +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20)) +
  ggtitle("Taylor River - Depth")+
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b")



ggplot(TR_slough2%>% 
         filter(LABEL != '2024-02-29'),
       aes(x = LABEL, 
           y= ave_sal, 
           ymin = S_Q25, 
           ymax = S_Q75)) +
  geom_ribbon(fill = "grey90")+
  geom_line(color = 'red',
            size = 1)+
  geom_line(aes(y = min_sal), 
            color = 'black', 
            linetype  = 2,
            size = 0.5) +
  geom_line(aes(y = max_sal), 
            color = 'black', 
            linetype  = 2,
            size = 0.5) +
  labs(y = 'Salinity (psu)', x = '') +
  theme_bw() +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20)) +
  ggtitle("Taylor River - Salinity")+
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b")



# data frames HYDRO -------------------------------------------------------


# old code ----------------------------------------------------------------



TRday_old <- lapply("/Hydrology/Quality Checked Data_all years/Hydro Files/TR_HYDRO.xlsx", 
                    function(x) read_excel(x, guess_max = Inf)) %>% 
  as.data.frame() %>% 
  rename( temp = Water.Temp,
          sal = Salinity, 
          depth = Depth) %>% 
  mutate( LABEL = as.Date(LABEL, tz = "", '%d-%b-%y'), 
          data_type_old = "old",
          PCT = 13,
          month = factor(month(LABEL), c(6,7,8,9,10,11,12,1,2,3,4,5)),
          date = paste(month.abb[month(LABEL)],day(LABEL),sep = "-")) %>% 
  select(LABEL:temp, data_type_old:date) %>% 
  filter(day(LABEL) != '29' | month(LABEL) == 2) %>% 
  mutate(date = paste(month.abb[month(LABEL)],day(LABEL),sep = "-"),
         month = format(LABEL, "%m"),
         day = format(LABEL, "%d")) %>% 
  filter(date != '2-29') %>% 
  dplyr::group_by(month, day) %>% 
  dplyr::summarize(min_depth_old = min(depth, na.rm = TRUE),
                   max_depth_old = max(depth, na.rm = TRUE),
                   depth_old = mean(depth, na.rm = TRUE),
                   min_sal_old = min(sal, na.rm = TRUE),
                   max_sal_old = max(sal, na.rm = TRUE),
                   sal_old = mean(sal, na.rm = TRUE),
                   .groups = 'drop') %>% 
  mutate(month = factor(as.numeric(month), 
                        c(6,7,8,9,10,11,12,1,2,3,4,5)),
         day = factor(as.numeric(day)),
         date = paste(month,day,sep = "-"),
         date=fct_inorder(date),
         data_type = "old") %>% 
  ungroup() 


TRday_now <- file.path(
  #new hydro year => need new path '/Provisional Hydrolab Data xx-xx'
  '/Provisional Hydrolab Data 23-24', 'TR_Hydrolab_Data.xlsx') %>% 
  read_excel(sheet='TR') %>% 
  rename(sal = Salinity...6,
         depth = Depth,
         temp = Temp) %>% 
  mutate(PCT = 13,
         LABEL = as.Date(as.character(as.POSIXct(TMSTAMP, '%Y-%m-%d')))) %>% 
  select(LABEL, depth, sal, temp, PCT) %>% 
  dplyr::group_by(LABEL) %>% 
  dplyr::summarize(min_depth_now = min(depth, na.rm = TRUE),
            max_depth_now = max(depth, na.rm = TRUE),
            depth_now = mean(depth, na.rm = TRUE),
            min_sal_now = min(sal, na.rm = TRUE),
            max_sal_now = max(sal, na.rm = TRUE),
            sal_now = mean(sal, na.rm = TRUE),
            PCT = mean(PCT),
            .groups = 'drop') %>% 
  mutate(data_type_now = "now",
         date = paste(month.abb[month(LABEL)],day(LABEL),sep = "-"),
         day = factor(day(LABEL)),
         month = factor(month(LABEL), 
                        c(6,7,8,9,10,11,12,1,2,3,4,5))) %>% 
  arrange(month) %>%              #sort by rating
  mutate(date=fct_inorder(date)) %>% 
  ungroup() %>% 
  select(-LABEL)

 



# end of the season, special case -----------------------------------------

# old code ----------------------------------------------------------------


TR_SOS_ribbon<-
  lapply("/Hydrology/Quality Checked Data_all years/Hydro Files/TR_HYDRO.xlsx", 
         function(x) read_excel(x, guess_max = Inf)) %>% 
  as.data.frame() %>% 
  rename( temp = Water.Temp,
          sal = Salinity, 
          depth = Depth) %>% 
  mutate( LABEL = as.Date(LABEL, tz = "", '%d-%b-%y'), 
          data_type_old = "old",
          PCT = 13,
          month = factor(month(LABEL), c(6,7,8,9,10,11,12,1,2,3,4,5)),
          date = paste(month.abb[month(LABEL)],day(LABEL),sep = "-")) %>% 
  select(LABEL:temp, data_type_old:date) %>% 
  filter(day(LABEL) != '29' | month(LABEL) == 2) %>% 
  mutate(date = paste(month.abb[month(LABEL)],day(LABEL),sep = "-"),
         month = format(LABEL, "%m"),
         day = format(LABEL, "%d")) %>% 
  filter(HY != 2021-22) %>% 
  mutate(month = factor(as.numeric(month), 
                        c(6,7,8,9,10,11,12,1,2,3,4,5)),
         day = factor(as.numeric(day)),
         date = paste(month,day,sep = "-"),
         date=fct_inorder(date),
         data_type = "old")

TR_SOS_end<- 
  left_join(
    lapply("/Hydrology/Quality Checked Data_all years/Hydro Files/TR_HYDRO.xlsx", 
           function(x) read_excel(x, guess_max = Inf)) %>% 
      as.data.frame() %>% 
      rename( temp = Water.Temp,
              sal = Salinity, 
              depth = Depth) %>% 
      mutate( LABEL = as.Date(LABEL, tz = "", '%d-%b-%y'), 
              data_type_old = "old",
              PCT = 13,
              month = factor(month(LABEL), c(6,7,8,9,10,11,12,1,2,3,4,5)),
              date = paste(month.abb[month(LABEL)],day(LABEL),sep = "-")) %>% 
      select(LABEL:temp, data_type_old:date) %>% 
      filter(day(LABEL) != '29' | month(LABEL) == 2) %>% 
      mutate(date = paste(month.abb[month(LABEL)],day(LABEL),sep = "-"),
             month = format(LABEL, "%m"),
             day = format(LABEL, "%d")) %>% 
      filter(HY != 2021-22) %>% 
      dplyr::group_by(month, day) %>% 
      dplyr::summarize(min_depth_old = min(depth, na.rm = TRUE),
                       max_depth_old = max(depth, na.rm = TRUE),
                       depth_old = mean(depth, na.rm = TRUE),
                       depth_old_25 = quantile(depth, probs = c(.25)),
                       depth_old_75 = quantile(depth, probs = c(.75)),
                       min_sal_old = min(sal, na.rm = TRUE),
                       max_sal_old = max(sal, na.rm = TRUE),
                       
                       sal_old_25 = quantile(sal, probs = c(.25), na.rm = TRUE),
                       sal_old_75 = quantile(sal, probs = c(.75), na.rm = TRUE),
                       
                       sal_old = mean(sal, na.rm = TRUE),
                       .groups = 'drop') %>% 
      mutate(month = factor(as.numeric(month), 
                            c(6,7,8,9,10,11,12,1,2,3,4,5)),
             day = factor(as.numeric(day)),
             date = paste(month,day,sep = "-"),
             date=fct_inorder(date),
             data_type = "old") %>% 
      ungroup() 
    ,
    lapply("/Hydrology/Quality Checked Data_all years/Hydro Files/TR_HYDRO.xlsx", 
           function(x) read_excel(x, guess_max = Inf)) %>% 
      as.data.frame() %>% 
      rename( temp = Water.Temp,
              sal = Salinity, 
              depth = Depth) %>% 
      mutate( LABEL = as.Date(LABEL, tz = "", '%d-%b-%y'), 
              data_type_old = "old",
              PCT = 13,
              month = factor(month(LABEL), c(6,7,8,9,10,11,12,1,2,3,4,5)),
              date = paste(month.abb[month(LABEL)],day(LABEL),sep = "-")) %>% 
      select(LABEL:temp, data_type_old:date) %>% 
      filter(day(LABEL) != '29' | month(LABEL) == 2) %>% 
      mutate(date = paste(month.abb[month(LABEL)],day(LABEL),sep = "-"),
             month = format(LABEL, "%m"),
             day = format(LABEL, "%d")) %>% 
      filter(HY == '2021-22') %>% 
      dplyr::group_by(month, day) %>% 
      dplyr::summarize(min_depth_now = min(depth, na.rm = TRUE),
                       max_depth_now = max(depth, na.rm = TRUE),
                       depth_now = mean(depth, na.rm = TRUE),
                       min_sal_now = min(sal, na.rm = TRUE),
                       max_sal_now = max(sal, na.rm = TRUE),
                       sal_now = mean(sal, na.rm = TRUE),
                       .groups = 'drop') %>% 
      mutate(month = factor(as.numeric(month), 
                            c(6,7,8,9,10,11,12,1,2,3,4,5)),
             day = factor(as.numeric(day)),
             date = paste(month,day,sep = "-"),
             date=fct_inorder(date),
             data_type = "new") %>% 
      ungroup() 
    , 
    by = c('month','day')) %>% 
  mutate(date = date.y) %>% 
  select(- c(date.x,data_type.x,data_type.y,date.y)) %>% 
  mutate(new_date = paste(month.abb[as.numeric(as.character(month))], 
                          as.numeric(day),sep = "-")) %>% 
  arrange(month) %>%              #sort by rating
  mutate(new_date=fct_inorder(new_date)) %>% 
  filter(new_date != 'Feb-29' ) 





ggplot(TR_SOS_end, aes( x=new_date, group=1))+
  geom_line(aes(y=max_depth_old), col='grey')+
  geom_line(aes(y=min_depth_old), col='grey')+
  geom_ribbon(aes(x=new_date, y=depth_old, ymin=depth_old_25, ymax=depth_old_75, group=1), 
              alpha=0.1)+
  geom_line(aes(y=13), col='pink')+
  geom_line(aes(y=depth_now), 
            col='red', size = 1)+
  
  theme_bw()+
  labs(title = "State of the Slough - Depth", 
       x = "", 
       y = "Daily Water Depth at Taylor River (cm)") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  scale_x_discrete(breaks = c("Jun-1", "Jul-1", "Aug-1", "Sep-1",
                              "Oct-1", "Nov-1", "Dec-1", "Jan-1",
                              "Feb-1", "Mar-1", "Apr-1", "May-1" ))


ggplot(TR_SOS_end, aes( x=new_date, group=1))+
  geom_line(aes(y=max_sal_old), col='grey')+
  geom_line(aes(y=min_sal_old), col='grey')+
  geom_ribbon(aes(x=new_date, y=sal_old, ymin=sal_old_25, ymax=sal_old_75, group=1), 
              alpha=0.1)+
  geom_line(aes(y=sal_now), 
            col='red', size = 1)+
  theme_bw()+
  labs(title = "State of the Slough - Salinity", 
       x = "", 
       y = "Salinity at Taylor River (psu)")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  scale_x_discrete(breaks = c("Jun-1", "Jul-1", "Aug-1", "Sep-1",
                              "Oct-1", "Nov-1", "Dec-1", "Jan-1",
                              "Feb-1", "Mar-1", "Apr-1", "May-1" ))



# bind the two data frames HYDRO ------------------------------------------



TR_SOS<-  left_join(TRday_old, TRday_now, 
                  by = c('month','day')) %>% 
  mutate(date = date.y) %>% 
  select(- c(date.x,data_type,data_type_now,date.y)) %>% 
  mutate(new_date = paste(month.abb[as.numeric(as.character(month))], 
                          as.numeric(day),sep = "-")) %>% 
  arrange(month) %>%              #sort by rating
  mutate(new_date=fct_inorder(new_date)) %>% 
  filter(new_date != 'Feb-29' ) 






# plot HYDRO  -------------------------------------------------------------



ggplot(TR_SOS, aes( x=new_date, group=1))+
  geom_line(aes(y=max_depth_old), col='grey')+
  geom_line(aes(y=min_depth_old), col='grey')+
  geom_line(aes(y=depth_now), col='red')+
  theme_bw()+
  labs(title = "State of the Slough - Depth", 
       x = "", 
       y = "Depth(cm)") +
  scale_x_discrete(breaks = c("Jun-1", "Jul-1", "Aug-1", "Sep-1",
                              "Oct-1", "Nov-1", "Dec-1", "Jan-1",
                              "Feb-1", "Mar-1", "Apr-1", "May-1" ))


ggplot(TR_SOS, aes( x=new_date, group=1))+
  geom_line(aes(y=max_sal_old), col='grey')+
  geom_line(aes(y=min_sal_old), col='grey')+
  geom_line(aes(y=sal_now), col='red')+
  theme_bw()+
  labs(title = "State of the Slough - Salinity", 
       x = "", 
       y = "Salinity(psu)")+
  scale_x_discrete(breaks = c("Jun-1", "Jul-1", "Aug-1", "Sep-1",
                              "Oct-1", "Nov-1", "Dec-1", "Jan-1",
                              "Feb-1", "Mar-1", "Apr-1", "May-1" ))



# data frames SAV ---------------------------------------------------------


SAV_TR<- 
  file.path(
    #new hydro year => need new path '/Provisional Hydrolab Data xx-xx'
    '/PLANTS/TR', 'TR1.xls') %>% 
  read_excel(sheet='TOTAL',
             skip=2 ) %>% 
  rename(HY = `Hydro. Year`,
         Utric = `Utr sp.`,
         Chara = `Cha hor`,
         Naja =`Naj mar`,
         Rup = `Rup mar`,
         Bat = `Bat sp.`,
         Clad = `Cla sp.`,
         Nit = `Nit sp.`,
         Sprio = `Spi sp.`) %>% 
  filter(HY == '21-22') %>% 
  dplyr::group_by(Month) %>% 
  dplyr::summarize((across(Utric:Sprio, ~ mean(.x, na.rm = TRUE)))) %>% 
  ungroup() %>% 
  as.data.frame() 


SAV_TR_toPlot <- 
  SAV_TR %>% 
  mutate(Total_SAV = rowSums(SAV_TR %>% 
                           select(Utric:Sprio)),
         No_SAV = 100 - Total_SAV) %>% 
  pivot_longer(Utric:No_SAV, names_to = 'data', values_to = 'values') %>% 
  filter(values != 0) 



# plot SAV ----------------------------------------------------------------

ggplot(SAV_TR_toPlot %>% 
         filter(data != 'Total_SAV' & data != 'No_SAV'), 
       aes(x="", y=values, fill=data)) +
  geom_bar(stat="identity",width=1) +
  coord_polar("y", start=0)+
  theme_void()+
  facet_grid(~Month)


pie(SAV_TR_toPlot %>% 
      filter(data != 'Total_SAV' & data != 'No_SAV') %>% 
      filter(Month == 'JAN') %>% 
      pull(values),
    labels = SAV_TR_toPlot %>% 
      filter(data != 'Total_SAV' & data != 'No_SAV') %>% 
      filter(Month == 'JAN') %>% 
      pull(data),
    main = "State of the Slough - SAV, JAN")


ggplot(SAV_TR_toPlot %>% 
         filter(data == 'Total_SAV' | data == 'No_SAV'), 
       aes(x="", y=values, fill=data)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void()+
  facet_grid(~Month) 


pie(SAV_TR_toPlot %>% 
      filter(data == 'Total_SAV' | data == 'No_SAV') %>% 
      filter(Month == 'JAN') %>% 
      pull(values),
    labels = SAV_TR_toPlot %>% 
      filter(data == 'Total_SAV' | data == 'No_SAV') %>% 
      filter(Month == 'JAN') %>% 
      pull(data),
    main = "State of the Slough - SAV, JAN")

