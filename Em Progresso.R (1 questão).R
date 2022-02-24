artrite <- brfss2013 %>% 
   select(havarth3 ,diffwalk, diffdres, diffalon) %>% 
  mutate(diffwalk_bin = recode(diffwalk,
                            "Yes" = 1,
                            "No" = 0),
         diffdress_bin = recode(diffdres,
                            "Yes" = 1,
                            "No" = 0),
         diffalon_bin = recode(diffalon,
                            "Yes" = 1,
                            "No" = 0)) %>% 
  select(everything(), -(2:4))


  


noarth <- artrite %>% 
  filter(havarth3 == "No") %>% 
  mutate(total = rowSums(noarth[ ,2:4], na.rm = TRUE)) 

noarth_summary <- noarth %>% count(total)

arth_summary <- arth %>% count(total)


ggplot(noarth) +
  geom_bar(aes(x = total), fill = "darkgreen") +
  labs(x = "Amount of problems",
       y = "Sample Size",
       title = "Sample of People Without Arthritis")
       

total <- nrow(artrite)
total_noarth <-  nrow(noarth)
paste(round(total_noarth*100/total, digits = 2), "%", sep= "")



arth <- artrite %>% 
  filter(havarth3 == "Yes") %>% 
  mutate(total = rowSums(arth[ ,2:4], na.rm = TRUE))

ggplot (arth) +
  geom_bar(aes(x = total), fill = "red") +
  labs( x = "Amount of Porblems",
        y = "Sample Size",
        title = "Sample of People With Arthritis")



comp_plot <-  gather(artrite, key = "measure", value =  "Value", c("arth", "noarth"))
  

  
  