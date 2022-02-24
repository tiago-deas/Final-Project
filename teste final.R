diabetes_summary_plot <- diab %>% count(blind_bin, dbeye_bin) %>%
  filter(blind_bin == 1) %>%
  rename("blindness" = blind_bin,
         "awareness" = dbeye_bin,
         "total" = n) 

ggplot(diabetes_summary) +
  geom_point(aes(x = total, y = blindness))


ggplot(diab, aes(x = diabte3)) +
  geom_dotplot()

ggplot(blind_awar) +
  geom_col(x = n, y = blind_awar$total)


blindness <- c("No", "Yes", "Yes", "Yes", "Na")
awareness <- c("Yes", "Yes", "No", "Na", "Yes")
total <- c(4990, 2358, 1907, 3099, 150)

df1 <- data.frame(blindness, awareness, total)
df2 <- melt(df1, id.vars = "total")


ggplot(df2, aes(x = value, y = total, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") 
  
df2 %>% count(value, total)

df2 %>% filter(blindness == "Yes")
  
insomnia %>% 
  mutate(hypertension = recode(hypertension,
                               "Yes, but female told only during pregnancy" = "Yes",
                               "Told borderline or pre-hypertensive" = "Yes")) %>% 
  count(sleep_hours, hypertension, depression) %>% 
  group_by(sleep_hours)


insomnia_summary1 <- insomnia %>% 
  mutate(hypertension = recode(hypertension,
                               "Yes, but female told only during pregnancy" = "Yes",
                               "Told borderline or pre-hypertensive" = "Yes")) %>% 
  count(sleep_hours)
  filter(hypertension == "Yes" | depression == "Yes") %>% 
  count(sleep_hours)

ggplot(insomnia_summary) +
  geom_point(aes(x = n, y = sleep_hours, color = hypertension, shape = depression ))

155 + 819 + 2623 + 9842 + 20059 + 55532

insomnia_summary2 <- insomnia %>% 
  mutate(hypertension = recode(hypertension,
                               "Yes, but female told only during pregnancy" = "Yes",
                               "Told borderline or pre-hypertensive" = "Yes")) %>% 
  count(sleep_hours)

completo <- left_join(insomnia_summary1, insomnia_summary2, by = "sleep_hours")


diabetes_teste <- diabetes %>% 
  count(diabete3)

ggplot(diabetes_teste) +
  geom_col(aes(x = diabete3, y = n, fill = diabete3)) +
  geom_text(aes(x = diabete3, y = n, label = n), vjust = -0.5) +
  labs(title = "Diabetes Prevalence in the Sample",
      x = "Diabetes",
      y = "Sample Size")