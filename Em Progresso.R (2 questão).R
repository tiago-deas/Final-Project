diabetes <- brfss2013 %>% 
  select(diabete3, blind, diabeye) %>% 
  mutate(diabete3 = recode(diabete3,
                           "No, pre-diabetes or borderline diabetes" = "No",
                           "Yes, but female told only during pregnancy" = "Yes"),
         blind_bin = recode(blind,
                            "Yes" = 1,
                            "No" = 0),
         dbeye_bin = recode(diabeye,
                            "Yes" = 1,
                            "No" = 0)) %>% 
  select(everything(), -(2:3))

diab <- diabetes %>% 
  filter(diabete3 == "Yes") %>% 
  mutate(total = rowSums(diab[ ,2:3], na.rm = TRUE)) 


total_sample2 <- nrow(diabetes)
total_diabetes <- nrow(diab)
paste(round(total_diabetes*100/total_sample2, digits = 2), "%", sep = "")

noarth_summary <- noarth %>% count(total) %>% 
  mutate(percent = round(noarth_summary$n*100/total_noarth, digits = 2)) 

print(noarth_summary)

diabetes_blind <- diab %>% count(blind_bin, dbeye_bin) %>% 
  filter(blind_bin == 1 & dbeye_bin == 1) %>% 
  mutate(percent = round(diabetes_blind*100/total_diabetes, digits = 2)) %>% 
  rename("blindness" = blind_bin,
         "awareness" = dbeye_bin,
         "total" = n)
total_blind <- 

diabetes_summary <- diab %>% count(blind_bin, dbeye_bin) %>% 
  rename("blindness" = blind_bin,
        "awareness" = dbeye_bin,
        "total" = n) %>% 
  mutate(diab_per = round(diabetes_summary$total*100/total_diabetes, digits = 2))
 diabetes_summary <- diabetes_summary %>%  
 filter(blind_bin == 1) %>% 
  mutate(blind_per = round(diabetes_summary$total))
  
 contagem <- brfss2013 %>% 
   select(diabete3, blind, diabeye) %>% 
   filter(diabete3 == "Yes") %>% 
   count(diabete3, blind, diabeye)
 
 
 ggplot(insomnia_summary, aes(x = n, y = sleep_hours, size = n,
                              color = hypertension, shape = hypertension)) +
   geom_point(stat = 'identity' ) +
   labs(x = "Total")
   
 ggplot(insomnia_summary, aes(x = n, y = sleep_hours, size = n, 
                              color = depression, shape = depression )) +
   geom_point(stat =  "identity")+
   labs(x = "Total")
 
 encondata <-  gather(insomnia_summary, key = "measure", value = "value", 
                      c("hypertension", "depression" ))
 
 ggplot(encondata) +
   geom_point(aes(x = n, y = sleep_hours, size = n)) +
   facet_wrap(~measure)
 
 ggplot(noarth, aes(x =total, y = havarth3)) +
   geom_bar() +
   geom_text(aes(label = total))
 
 ggplot(noarth) +
   geom_bar(aes(x = total), fill = "darkgreen") +
   labs(x = "Amount of Problems",
        y = "Sample Size",
        title = "Sample of People Without Arthritis") 
 
 
 