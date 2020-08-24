#example code for doing simple, robust data flagging using dplyr(), I use "robust" meaning
  #it can be ran over data over and over, depending on how you set up filtering logic 
  #(e.g. having appropriate grouping variables). This was originally written by
  #Dylan Glaser March 15, 2019 - dylan.glaser1@ucalgary.ca to help my field crew locate
  #errors in data entry | updated Aug. 24, 2020

#load your "data" (an example in this case) and packages
library(dplyr) #to wrangle data 
library(ggplot2) #to make pretty plots 
data <- mtcars %>%
  mutate(make_mod = rownames(mtcars), #switch rownames to column because I like that
         data_flag = as.character("")) #new empty column to populate with flags

#I've created the flag column. Now let's use logic to create a flag. Say I want to check 
  #for low values of mpg below a certain threshold. You can figure out a good cutoff
  #by plotting your data and looking for potential outliers. If looking at a single 
  #variable (i.e. not a relationship among variables) you can check with a histogram.
ggplot(data, aes(carb)) +
  geom_histogram()

#looks like there could be potential outliers in cars with more than 5 carbs - we can flag
  #that to check it later
data <- data %>%
  mutate(data_flag = ifelse(carb > 5, #what to flag 
                            "high_carb, ", #how to flag leave a comma for later
                            data_flag)) #what to do with rows that don't meet that critera

#sometimes you want to look at specific relationships. Lets look at how much power a car has
  #via ("cyl") and compare it to "mpg" to find potential values that are "too good to be
  #true" for each cylinder group
ggplot(data, aes(x=cyl, y=mpg)) +
  geom_point()

#the values look pretty normally distributed but let's flag the top 2 best ones just in case
  #you wanted to double check the data. Note how now we need a paste argument to retain the
  #old data flag from the previous step. You need this in all flags except the first. 
data <- data %>%
  group_by(cyl) %>%
  mutate(best_mpg_by_cyl = dense_rank(-mpg)) %>% #helper column to sort (rank by highest)
  mutate(data_flag = ifelse(best_mpg_by_cyl >= 2, #what to flag 
                            paste("high_mpg_by_cyl, ", data_flag), #how to flag
                            data_flag)) %>% #what to do with rows that don't meet that critera
  select(-best_mpg_by_cyl) #drop the helper column


#conditionally subset and mutate data - here I filter out a slice of rows I am interested
  #in - in this case 3 and 4 gear cars. This could be anything (e.g.species, age, site,
  #whatever). It's important to build a conceptual flow chart before flagging data to 
  #identify when subsetting is important. 
sub <- data %>%
  filter(gear %in% c(3,4))%>%
  arrange(cyl, hp)%>%
  group_by(cyl)%>% 
  mutate(HpByCyl=dense_rank(hp)) 
  
#let's say I want to double check "manipulated$HpByCyl" that is ranked <2 because that data 
  #is such low values that I suspect it could be errors. I'll do this by flagging the data 
  #so I can go into my source file and go through them case by case. 
sub <- sub %>%
  mutate(data_flag = ifelse(HpByCyl < 2, #tell it the logic to flag on
                paste("Check_HpByCyl, ", data_flag), #tell it what to use as the flag
                data_flag)) %>% #tell it what to do if the condition is not met. In this case, 
  select(-HpByCyl)                              #we want to preserve the old flag

#now we need to put this subsetted data back in with the rest of the data. Need to pull
  #data out of sub that was not filtered for flagging, and do this with a "by" argument
  #this "by" needs to be a unique row identifier, in the case of fish you may need to have
  #multiple arguments passed to by (e.g. by=c("site", "date", "species", "length"))
sub_2 <- anti_join(data, sub, by="make_mod") 
                            
#then bind the data back together. if this New object is a different size than the first
  #there is a problem with the logic. 
NewData <- bind_rows(sub_2, sub)

#then you can write this dataframe to a csv and go through the rows and look at field 
  #notes for errors one by one. I'll filter to some common thing in excel like identifiers
  #that match the data sheets I am looking at, like site and day. 