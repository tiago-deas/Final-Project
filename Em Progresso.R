artrite <- brfss2013 %>% 
   select(havarth3 ,diffwalk, diffdres, diffalon) %>% 
  mutate(havarth3_bin = recode(havarth3,
                            "Yes" = 1,
                            "No" = 0),
          diffwalk_bin = recode(diffwalk,
                            "Yes" = 1,
                            "No" = 0),
         diffdress_bin = recode(diffdres,
                            "Yes" = 1,
                            "No" = 0),
         diffalon_bin = recode(diffalon,
                            "Yes" = 1,
                            "No" = 0)) %>% 
  select(everything(), -(1:4))
  


people_noarth <- artrite %>% 
  filter(havarth3_bin == 0, na.rm = TRUE)

people_noarth <- people_noarth %>%
  mutate(total = sum(as.integer(, na.rm = TRUE))) 
  
  

people_arth <- artrite %>% 
  filter(havarth3_bin == 1, na.rm = TRUE)
 


  
  

  
  