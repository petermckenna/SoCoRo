# SoCoRo
Postdoc research


###########################################################
## Create tidy data frame from participant q'nnaire data ##--------
###########################################################

# clear environment

rm(list=ls())

# Set default directory for user installed packages
.libPaths(c("C:\\Users\\pm72\\R", .libPaths()))

# load packages

library(ggsci)
library(tidyr)
library(dplyr)
library(ggplot2)


##### Housekeeping ####----

# set working directory

setwd("H:/.windows_settings/Desktop/Peter/20171010_Robotarium-backup/SoCoRo/Experiments/Exp_2/R_source/exp_2") # at work

setwd("~/University/Heriot-Watt University/SoCoRo/Experiments/Experiment 2_Autistic traits and emotion recognition with an expressive robot/R_source/exp_2") # at home

# read in required datasets and merge 

quest <- read.csv("20171013_q-data.csv", header = T)

resp <- read.csv("resp_data.csv")

# merge datsets by ID

mast_data <- merge(quest,resp, by = "ID")

write.csv(mast_data, "mast_data.csv")

# read in master dataset

mast <- read.csv("mast_data.csv")



#### Data Wrangling ####----



# Step 1: Deal reshape the data

# computer cannot hadle all of the piping in one go so I have split into different stages to releave computation. 


# Data wrangling step 1

a <- mast                                                                  %>%
          filter(ID != "pilot")                                                 %>%                  # remove pilot data
          rowwise()                                                             %>%
          select(ID:expdate, q1:resp_acc)                                       %>%                  # select rows of interest
          
          gather(AQ_item, AQ_resp, q1:q50)                                      %>%                  # convert data into long view
          mutate_at('AQ_item', funs(as.character))                              %>%                  # change data type of new vector
          mutate(AQ_num = substring(AQ_item,2))                                 %>%                  # create new varaible for AQ item number ( for applying logical arguments)
          mutate_at('AQ_num', funs(as.numeric))                                 %>%                  # change data type to numeric
          mutate('AQ_score' = ifelse(AQ_num %in% c(2,4:7,9,12:13,16,18:23,26,33,35,39,41:43,45:46) & AQ_resp %in% c("definitely agree","slightly agree"), 1,
                              ifelse(AQ_num %in% c(1,3,8,10:11,14:15,17,24:25,27:32,34,36:38,40,44,47:50) & AQ_resp %in% c("slightly disagree","definitely disagree"), 1, 0))) %>% # use conditional statements to mark AQ items
  
          print

# Data wrangling step 2
          
b <-     a                                                                      %>%
          
          rename(Friendliness = Unfriendly.vs.Friendly,                                  
            Likeability = Not.at.all.vs.Very.much,
            Perceived_positiveness = No..I.disagree.vs.Yes..I.agree,
            Voice_clarity = Hard.to.understand.vs.Easy.to.understand,
            Performance_rating = Very.badly.vs.Very.well,
            Interaction_rating = No..I.disagree.vs.Yes..I.agree.1)              %>%                  # rename robot q'nnaire items
          print
            
          
# Data wrangling step 3
                    
c <-      b                                                                     %>%
          gather(robot_q_item, robot_resp, Friendliness:Interaction_rating)     %>%                  # convert from wide into long view     
          mutate_at('robot_q_item', funs(as.character))                         %>%                  # change data type of new vector
          
          print
  

# Data wrangling step 4
          

d <-      c                                                                     %>%
          mutate(bin_acc = ifelse(resp_acc == "Correct", 1, 0))                 %>%                  # create binary vector for participants accuracy
          mutate(pad = ifelse(expression %in% 1:2, "Approval", "Disapproval"))  %>%                  # add expression category row
          mutate_if(is.factor, as.character)                                    %>%
          mutate_if(is.integer, as.numeric)                                     %>%
          #arrange(ID)                                                           %>%                  # arrange data by ID
          #str                                                                   %>%
          print                                                                  #%>%                                                                 
          
          
  
#### Outputs ####----

# write csv to explore

write.csv(d, "exp2_data.csv")

##### Data exploration #####----


# Plot 1: AQ distribution

tbl_df <- read.csv("exp2_data.csv", header = T)


# need to sort out object and data.frame creation
# doesnt' always appear necessary


# subset the data you are interested in and pipe directly into ggplot2

AQ_hist<- tbl_df                               %>%
       filter(expression == 1)                 %>%   # select only data from single expression, this fixes propogation error
       group_by(ID, AQ_num)                    %>%   # group data by ID
       select(ID, AQ_score, AQ_num)            %>%   # select rows of interest 
       arrange(ID)                             %>%
       distinct                                %>%
       group_by(ID)                            %>%
       summarize(AQ_tot = sum(AQ_score))       %>%   # summarise by AQ score, divided by foru to account for pseudoreplication
              
  
ggplot(., aes(AQ_tot)) + 
      geom_histogram(bins = NULL,
                     binwidth = NULL,
                     col = "deepskyblue4",
                     fill = "deepskyblue",
                     alpha = .8) +
                     theme(plot.title = element_text(hjust = .5)) +
                     labs(x = "\nAQ Score", y = "Count\n")
                     
ggsave("AQ-score-hist.png", width = 9.6, height = 7.2)

# ERROR meeesage: `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

# can see that the distribution is roughly parametric


# desriptive statistics for AQ

AQ_des <- tbl_df %>%
          filter(expression == 1)                 %>%   # select only data from single expression, this fixes propogation error
          group_by(ID, AQ_num)                    %>%   # group data by ID
          select(ID, AQ_score, AQ_num)            %>%   # select rows of interest 
          arrange(ID)                             %>%
          distinct                                %>%
          group_by(ID)                            %>%   # group data by ID
          summarize(AQ_tot    = sum(AQ_score))    %>%   # summarise by AQ, adjust for pseudoreplication
          summarise(AQ_min    = min(AQ_tot),            # calc max
                    AQ_max    = max(AQ_tot),            # calc mean 
                    AQ_median = median(AQ_tot),         # calc median
                    AQ_mean   = mean(AQ_tot),
                    AQ_sd     = sd(AQ_tot)) 

# min = 4
# max = 35
# median = 17
# mean = 16.77
# sd = 6.89



# Plot 2: Participant responses
  

ppt_resp <- tbl_df                                                           %>%
          mutate(resp = factor(resp, levels = c("Like", "Dislike", "Miss"))) %>%  # reorder levels of resp
          mutate(pad = factor(pad, levels = c("Approval", "Disapproval")))   %>%  # reorder levels of resp
          mutate(expression = factor(expression, levels = c("1", "2", "3", "4")))   %>%
          rename(Response = resp)                                            %>%
          group_by(ID, expression)                                           %>%
          select(ID, expression, Response, pad)                              %>%
          arrange(ID)                                                        %>%
          distinct                                                           %>%
          #View

ggplot(.) +
          aes(x = expression, fill = Response) +
          geom_bar(position = "dodge") + 
          xlab("\nExpression") + ylab("Count\n") + 
          scale_fill_manual(values = c("deepskyblue4", "firebrick4", "gray60")) + 
          facet_grid(. ~ pad, scales = "free", space = "free")

ppt_resp
          
ggsave("expression~resp.png", width = 9.6, height = 7.2)


# still need to figure out how to add images to the x-axis but this will do for now


# total number of rows = 4 * 57 = 228

tbl_df %>%
  mutate(resp = factor(resp, levels = c("Like", "Dislike", "Miss")))      %>%  # reorder levels of resp
  select(ID, expression, resp)                                            %>%
  group_by(ID, expression)                                                %>%
  distinct                                                                %>%
  group_by(expression, resp)                                              %>%
  summarise(n = n())                                                      %>%
  mutate(prop = n/sum(n))                                                 %>%  # count observations in response
  View
  



# Plot 3: Participant accuracy

ppt_acc <- tbl_df %>%
  mutate(resp_acc = factor(resp_acc, levels = c("Correct", "Incorrect")))%>%  # reorder levels of resp
  mutate(pad = factor(pad, levels = c("Approval", "Disapproval")))   %>%  # reorder levels of resp
  mutate(expression = factor(expression, levels = c("1", "2", "3", "4")))   %>%
  rename(Accuracy = resp_acc)                                            %>%
  group_by(ID, expression)                                           %>%
  select(ID, expression, Accuracy, pad)                              %>%
  arrange(ID)                                                        %>%
  distinct                                                           %>%
  #View
  
  ggplot(.) +
  aes(x = expression, fill = Accuracy) +
  geom_bar(position = "dodge") + 
  xlab("\nExpression") + ylab("Count\n") + 
  scale_fill_manual(values = c("deepskyblue4", "firebrick4")) +
  facet_grid(. ~ pad, scales = "free", space = "free")

ppt_acc  
  
ggsave("expression~resp_acc.png", width = 9.6, height = 7.2)


# descriptives for resp_acc


tbl_df %>%
  filter(AQ_item == "q1")                                                 %>%  # select sample of data from single expression
  select(expression, resp_acc)                                            %>%  # select vectors of interest
  mutate(resp_acc = factor(resp_acc, levels = c("Correct", "Incorrect"))) %>%  # reorder levels of resp_acc
  group_by(expression, resp_acc)                                          %>%  # group data by ID
  mutate(count = n())                                                     %>%  # count observations in resp_acc
  mutate(prop = count/57)                                                 %>%  # create column for proportion of responses per expression
  arrange(expression)                                                     %>%  # arrange by expression
  distinct()                                                                   # remove duplicate rows  


# Plot 4: Bivariate plot of accuracy ~ AQ

tbl_df %>%
  group_by(ID)                                                            %>%  # group data by participant
  mutate(AQ_tot = sum(AQ_score)/4)                                        %>%  # create vetor of total AQ score 
  filter(AQ_item == "q1")                                                 %>%  # remove replication
  arrange(ID)                                                             %>%  # order data by ID
  distinct()                                                              %>%  # select unique rows
  mutate(resp_acc = factor(resp_acc, levels = c("Incorrect", "Correct"))) %>%  # reorder levels of resp
  ggplot(.,aes(x = AQ_tot, y = resp_acc)) +
  geom_jitter() + xlab("\nAQ score") + ylab("Accuracy\n")

ggsave("resp_acc ~ AQ.png", width = 9.6, height = 7.2)

# need to add a row of participants AQ scores to graph values


# Plot 5: Descriptives from q'nnaire data

q_des <- tbl_df %>%
  select(ID, robot_q_item, robot_resp)                                              %>%
  group_by(ID, robot_q_item)                                                        %>%
  distinct                                                                          %>%
  arrange(ID) %>%
  group_by(robot_q_item) %>%
  summarise(mean = mean(robot_resp),
            se   = sd(robot_resp)/sqrt(57))                                         %>%  # standard error is sd divided by the square root of N
  #View                            #  %>%
            
  ggplot(.) +
  aes(x = robot_q_item, y = mean) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) +
  xlab("\nRobot questionnare items") + 
  ylab("Mean participant rating\n") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 
  
q_des 
  
  
#  Plot 6: Accuracy by gender
  
 tbl_df %>%
    mutate(resp_acc = factor(resp_acc, levels = c("Correct", "Incorrect"))) %>%  # reorder levels of resp
    filter(AQ_item == "q1")                                             %>%  # take a subset of the data from the first item of the AQ        
    ggplot(.) +
    aes(x = sex, fill = resp_acc) +
    geom_bar(position = "dodge") + xlab("\nExpression") + ylab("Count\n") + scale_fill_discrete(name = "Response")
  #facet_wrap( ~ pad)
  
  ggsave("expression~resp_acc.png", width = 9.6, height = 7.2)  
  
  
  #### Data Analysis ----
 
