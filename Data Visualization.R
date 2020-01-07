rm(list=ls())

library("dplyr")
library("ggplot2")
library("gridExtra")

# Import public Open University dataset
assessments <- read.csv("assessments.csv")
courses <- read.csv("courses.csv")
studentAssessment <- read.csv("studentAssessment.csv")
studentInfo <- read.csv("studentInfo.csv")
studentRegistration <- read.csv("studentRegistration.csv")
studentVle <- read.csv("studentVle.csv")
vle <- read.csv("vle.csv")

# Barchart student distribution per course
ggplot(data = studentInfo, aes(x = code_presentation,                       # Create a barchart with the data from the studentInfo dataframe
                               fill = code_module)) +           
  geom_bar(stat = 'count', position=position_dodge()) +                     # Make sure the height of the bars represents the count of students
  facet_grid(.~code_module) +                                               # Create the bar charts for each course (code_module)
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),                # Add some visual polish
        axis.ticks.x = element_blank(),       
        panel.grid.major.x = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "grey85"),
        plot.background = element_rect(fill = "transparent", color = NA),
        strip.background = element_rect(fill = "grey75", colour = NA))+
  scale_fill_brewer(palette="Set2") +
  guides(fill=FALSE) +
  ggtitle("Student Distribution") +                                         # Add plot title
  xlab("Code presentation") +                                               # Add x-label
  ylab("Student count") +                                                   # Add y-label
  ggsave(filename = "Student Distribution.png",                             # Export the graph as a .png file
         bg = "transparent", dpi = 600)            



# Only retain students in courses BBB, DDD and FFF & remove those that dropped out
studentInfo <- studentInfo[studentInfo$code_module %in% c("BBB","DDD","FFF"),]
studentInfo <- studentInfo[studentInfo$final_result != "Withdrawn",]
studentVle <- studentVle[studentVle$code_module %in% c("BBB","DDD","FFF"),]

studentInfo$final_result[studentInfo$final_result == "Distinction"] <- "Pass"


# Barchart student distribution for course BBB, DDD and FFF
studentInfo %>%
  mutate(code_presentation = as.factor(code_presentation),     # Make sure factors have the right type
         final_result = as.factor(final_result),
         code_module = as.factor(code_module)) %>%
  group_by(code_module, code_presentation, final_result) %>%   # Group the data
  summarise(count_final_result = n()) %>%                      # Get the count of students per group   
  mutate(count_man = sum(count_final_result)) %>%              # Get the count of student in each course presentation
  mutate(percent = count_final_result / count_man * 100) %>%   # Compute the pass rate per presentation
  ungroup() %>%
  ggplot(aes(x = code_presentation,                            # Plot barchart with student pass rates
             y = count_final_result,
             group = final_result)) +
  geom_bar(aes(fill = final_result), 
           stat = "identity") +
  geom_text(aes(label = sprintf("%0.0f%%", percent)),          # Add visual polish 
            position = position_stack(vjust = 0.5),
            check_overlap = TRUE) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "grey85"),
        plot.background = element_rect(fill = "transparent", color = NA),
        strip.background = element_rect(fill = "grey75", colour = NA),
        legend.background = element_rect(fill = "transparent", color = NA)) +
  scale_fill_manual(values = c("#E68A99","#A3B9AA")) +         # Bar colors
  ggtitle("Student Pass Rate by Period") +                     # Add plot title
  xlab("Code presentation") +                                  # Add x-label
  ylab("Student count") +                                      # Add y-label
  labs(fill = "Final result") +                                # Add legend title
  facet_grid(.~code_module) +                                  # Create the bar charts for each course (code_module)
  ggsave(filename = "PR_plot.png",                             # Export the graph as a .png file
         bg = "transparent", dpi = 600)   


# Barchart gender distribution per course
studentInfo %>%
  mutate(gender = as.factor(gender),                           # Use gender instead of course presentation
         final_result = as.factor(final_result),
         code_module = as.factor(code_module)) %>%
  group_by(code_module, gender, final_result) %>%              
  summarise(count_final_result = n()) %>%
  mutate(count_man = sum(count_final_result)) %>%
  mutate(percent = count_final_result / count_man * 100) %>%
  ungroup() %>%
  ggplot(aes(x = gender,
             y = count_final_result,
             group = final_result)) +
  geom_bar(aes(fill = final_result), 
           stat = "identity") +
  geom_text(aes(label = sprintf("%0.0f%%", percent)),
            position = position_stack(vjust = 0.5), check_overlap = TRUE) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "grey85"),
        plot.background = element_rect(fill = "transparent", color = NA),
        strip.background = element_rect(fill = "grey75", colour = NA),
        legend.background = element_rect(fill = "transparent", color = NA)) +
  scale_fill_manual(values = c("#E68A99","#A3B9AA")) +
  ggtitle("Student Pass Rate by Gender") +                     
  xlab("Gender") +                                  
  ylab("Student count") +    
  labs(fill = "Final result") +
  facet_grid(.~code_module) +
  ggsave(filename = "PR_Gender_plot.png",                      
         bg = "transparent", dpi = 600)  

# Barchart age distribution per course
studentInfo %>%                                                # Use age band instead of gender
  mutate(age_band = as.factor(age_band),
         final_result = as.factor(final_result),
         code_module = as.factor(code_module)) %>%
  group_by(code_module, age_band, final_result) %>%
  summarise(count_final_result = n()) %>%
  mutate(count_man = sum(count_final_result)) %>%
  mutate(percent = count_final_result / count_man * 100) %>%
  ungroup() %>%
  ggplot(aes(x = age_band,
             y = count_final_result,
             group = final_result)) +
  geom_bar(aes(fill = final_result), 
           stat = "identity") +
  geom_text(aes(label = sprintf("%0.0f%%", percent)),
            position = position_stack(vjust = 0.5), check_overlap = TRUE) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "grey85"),
        plot.background = element_rect(fill = "transparent", color = NA),
        strip.background = element_rect(fill = "grey75", colour = NA),
        legend.background = element_rect(fill = "transparent", color = NA)) +
  scale_fill_manual(values = c("#E68A99","#A3B9AA")) +        
  ggtitle("Student Pass Rate by Age") +                     
  xlab("Age Band") +                                  
  ylab("Student count") +   
  labs(fill = "Final result") +
  facet_grid(.~code_module) +
  ggsave(filename = "PR_Age_plot.png",                      
         bg = "transparent", dpi = 600)  


# Reorder factor levels for highest education level
studentInfo$highest_education <- factor(x = studentInfo$highest_education, levels = c("No Formal quals",
                                                                                      "Lower Than A Level",
                                                                                      "A Level or Equivalent",
                                                                                      "HE Qualification",
                                                                                      "Post Graduate Qualification"))
levels(studentInfo$highest_education)

# Barchart education level distribution per course
studentInfo %>%                         
  mutate(highest_education = as.factor(highest_education),                 # Use Highest Education instead of Age Band
         final_result = as.factor(final_result),
         code_module = as.factor(code_module)) %>%
  group_by(code_module, highest_education, final_result) %>%
  summarise(count_final_result = n()) %>%
  mutate(count_man = sum(count_final_result)) %>%
  mutate(percent = count_final_result / count_man * 100) %>%
  ungroup() %>%
  ggplot(aes(x = highest_education,
             y = count_final_result,
             group = final_result)) +
  geom_bar(aes(fill = final_result), 
           stat = "identity") +
  geom_text(aes(label = sprintf("%0.0f%%", percent)),
            position = position_stack(vjust = 0.5), check_overlap = TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "grey85"),
        plot.background = element_rect(fill = "transparent", color = NA),
        strip.background = element_rect(fill = "grey75", colour = NA),
        legend.background = element_rect(fill = "transparent", color = NA)) +
  scale_fill_manual(values = c("#E68A99","#A3B9AA")) +  
  ggtitle("Student Pass Rate by Highest Education Level") +                     
  xlab("Highest Education Level") +                                  
  ylab("Student count") +
  labs(fill = "Final result") +
  facet_grid(.~code_module)
  ggsave(filename = "PR_HE_plot.png",                      
         bg = "transparent", dpi = 600) 



# Barchart imd score distribution per course  
levels(studentInfo$imd_band)[3] <- "10-20%"
studentInfo %>%
  mutate(imd_band = as.factor(imd_band),
         final_result = as.factor(final_result),
         code_module = as.factor(code_module)) %>%
  group_by(code_module, imd_band, final_result) %>%
  summarise(count_final_result = n()) %>%
  mutate(count_man = sum(count_final_result)) %>%
  mutate(percent = count_final_result / count_man * 100) %>%
  ungroup() %>%
  ggplot(aes(x = imd_band,
             y = count_final_result,
             group = final_result)) +
  geom_bar(aes(fill = final_result), 
           stat = "identity") +
  geom_text(aes(label = sprintf("%0.0f%%", percent)),
            position = position_stack(vjust = 0.5), check_overlap = TRUE, size=2.0) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "grey85"),
        plot.background = element_rect(fill = "transparent", color = NA),
        strip.background = element_rect(fill = "grey75", colour = NA),
        legend.background = element_rect(fill = "transparent", color = NA)) +
  scale_fill_manual(values = c("#E68A99","#A3B9AA")) +  
  ggtitle("Student Pass Rate by IMD Band") +                     
  xlab("IMD Band") +                                  
  ylab("Student count") +
  labs(fill = "Final result") +
  facet_grid(.~code_module) +
  ggsave(filename = "PR_IMD_plot.png",                      
         bg = "transparent", dpi = 600) 



# Barchart previous assessment attempt distribution per course
studentInfo %>%
  mutate(num_of_prev_attempts = as.factor(num_of_prev_attempts),
         final_result = as.factor(final_result),
         code_module = as.factor(code_module)) %>%
  group_by(code_module, num_of_prev_attempts, final_result) %>%
  summarise(count_final_result = n()) %>%
  mutate(count_man = sum(count_final_result)) %>%
  mutate(percent = count_final_result / count_man * 100) %>%
  ungroup() %>%
  ggplot(aes(x = num_of_prev_attempts,
             y = count_final_result,
             group = final_result)) +
  geom_bar(aes(fill = final_result), 
           stat = "identity") +
  geom_text(aes(label = sprintf("%0.0f%%", percent)),
            position = position_stack(vjust = 0.5), check_overlap = TRUE, size=3.0) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "grey85"),
        plot.background = element_rect(fill = "transparent", color = NA),
        strip.background = element_rect(fill = "grey75", colour = NA),
        legend.background = element_rect(fill = "transparent", color = NA)) +
  scale_fill_manual(values = c("#E68A99","#A3B9AA")) +  
  ggtitle("Student Pass Rate by Number of Previous Attempts") +                     
  xlab("Previous Attempts") +                                  
  ylab("Student count") +
  labs(fill = "Final result") +
  facet_grid(.~code_module) +
  ggsave(filename = "PR_PrevAttempts_plot.png",                      
         bg = "transparent", dpi = 600) 


# Barchart credits distribution per course
ggplot(data = studentInfo,aes(x = final_result,                                               
                              y = studied_credits,
                              fill = factor(final_result, levels = c("Fail", "Pass")))) +
  geom_boxplot() +                                                                  # Create a boxplot 
  scale_y_continuous(limits=c(0, 450)) +                                            # Determine scale cut-off values
  theme(plot.title = element_text(hjust = 0.5),                                     # Add visual polish
        panel.background = element_rect(fill = "grey85"),
        plot.background = element_rect(fill = "transparent", color = NA),
        strip.background = element_rect(fill = "grey75", colour = NA),
        legend.background = element_rect(fill = "transparent", color = NA)) +
  scale_fill_manual(values = c("#E68A99","#A3B9AA")) +  
  ggtitle("Student Pass Rate by Studied Credits") +                     
  xlab("Final result") +                                  
  ylab("Studied credits") +
  labs(fill = "Final result") +
  facet_grid(.~code_module) +
  ggsave(filename = "PR_credits_plot.png",                      
         bg = "transparent", dpi = 600) 


# Compute mean credits per course
mean(studentInfo$studied_credits[(studentInfo$final_result == "Fail") & (studentInfo$code_module == "BBB")])
mean(studentInfo$studied_credits[(studentInfo$final_result == "Pass") & (studentInfo$code_module == "BBB")])

mean(studentInfo$studied_credits[(studentInfo$final_result == "Fail") & (studentInfo$code_module == "DDD")])
mean(studentInfo$studied_credits[(studentInfo$final_result == "Pass") & (studentInfo$code_module == "DDD")])

mean(studentInfo$studied_credits[(studentInfo$final_result == "Fail") & (studentInfo$code_module == "FFF")])
mean(studentInfo$studied_credits[(studentInfo$final_result == "Pass") & (studentInfo$code_module == "FFF")])


# Barchart disability distribution per course
studentInfo %>%
  mutate(disability = as.factor(disability),
         final_result = as.factor(final_result),
         code_module = as.factor(code_module)) %>%
  group_by(code_module, disability, final_result) %>%
  summarise(count_final_result = n()) %>%
  mutate(count_man = sum(count_final_result)) %>%
  mutate(percent = count_final_result / count_man * 100) %>%
  ungroup() %>%
  ggplot(aes(x = disability,
             y = count_final_result,
             group = final_result)) +
  geom_bar(aes(fill = final_result), 
           stat = "identity") +
  geom_text(aes(label = sprintf("%0.0f%%", percent)),
            position = position_stack(vjust = 0.5), check_overlap = TRUE) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "grey85"),
        plot.background = element_rect(fill = "transparent", color = NA),
        strip.background = element_rect(fill = "grey75", colour = NA),
        legend.background = element_rect(fill = "transparent", color = NA)) +
  scale_fill_manual(values = c("#E68A99","#A3B9AA")) +  
  ggtitle("Student Pass Rate by Disability") +                     
  xlab("Disability") +                                  
  ylab("Student count") +
  labs(fill = "Final result") +
  facet_grid(.~code_module) +
  ggsave(filename = "PR_Disability_plot.png",                      
         bg = "transparent", dpi = 600) 


studentVle_fr <- merge(studentVle, studentInfo[,c(1:3,12)], by.x = c(1,2,3)) # Merge studentVle and studentInfo dataframes

# Students' average number of clicks in the VLE over time
studentVle_fr[,-c(4)] %>%                                                   # Use the new df without site_id variable
  mutate(date = as.integer(date),                                           # Make sure variables have appropriate types
         sum_click = as.integer(sum_click),
         code_module = as.factor(code_module),
         final_result = as.factor(final_result),
         code_presentation = as.factor(code_presentation)) %>%
  distinct(code_module, code_presentation,id_student, sum_click, date, final_result) %>% # Remove duplicate rows
  group_by(code_module,code_presentation, final_result, date) %>%                        # Group df
  summarise(avg_clicks = mean(sum_click)) %>%                                            # Compute avg clicks summary statistic
  ungroup() %>%
  ggplot(mapping = aes(x = date, y = avg_clicks, group = final_result, color = final_result)) + # Plot linechart
  geom_line(size = 1.0) +
  theme(plot.title = element_text(hjust = 0.5),                                                 # Visual polish
        panel.background = element_rect(fill = "grey85"),
        plot.background = element_rect(fill = "transparent", color = NA),
        strip.background = element_rect(fill = "grey75", colour = NA),
        legend.background = element_rect(fill = "transparent", color = NA)) +
  scale_color_manual(values = c("#E68A99","#A3B9AA")) +  
  ggtitle("Students' Average Clicks over Time") +                     
  xlab("Date") +                                  
  ylab("Average clicks") +
  labs(color = "Final result") +
  facet_grid(code_module ~ code_presentation) +
  ggsave(filename = "Clicks_Time_plot.png",                      
       bg = "transparent", dpi = 600) 



# Scatterplot sum of clicks vs active days
studentVle_fr[,-c(4)] %>%
  mutate(date = as.integer(date),
         sum_click = as.integer(sum_click),
         code_module = as.factor(code_module),
         final_result = as.factor(final_result),
         id_student = as.factor(id_student),
         code_presentation = as.factor(code_presentation)) %>%
  distinct(code_module, code_presentation,id_student, sum_click, final_result) %>%
  group_by(code_module,code_presentation, id_student) %>%
  mutate(sum_clicks = sum(sum_click)) %>%
  mutate(active_days = n())%>%
  ungroup() %>%
  distinct(code_module, code_presentation,id_student,active_days, sum_clicks, final_result) %>%
  ggplot(mapping = aes(x = sum_clicks, y = active_days, color = final_result)) +
  geom_point(alpha = 0.2) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "grey85"),
        plot.background = element_rect(fill = "transparent", color = NA),
        strip.background = element_rect(fill = "grey75", colour = NA),
        legend.background = element_rect(fill = "transparent", color = NA)) +
  scale_color_manual(values = c("#E68A99","#A3B9AA")) +  
  ggtitle("Students' Active Days by Sum of Clicks") +                     
  xlab("Sum of clicks") +                                  
  ylab("Active days") +
  labs(color = "Final result") +
  facet_grid(code_module ~ code_presentation) +
  ggsave(filename = "Days_Clicks_plot.png",                      
         bg = "transparent", dpi = 600) 



  
studentAssessment_fr <- merge(studentAssessment, assessments[,c(1:5)], by.x = 1)
studentAssessment_fr <- merge(studentAssessment_fr, studentInfo[,c(1:3,12)], by.x = c(2,6,7))

# Assessment score over time
studentAssessment_fr[!is.na(studentAssessment_fr$score),-c(4,5,6,8)] %>%
  mutate(date = as.integer(date),
         score = as.integer(score),
         code_module = as.factor(code_module),
         final_result = as.factor(final_result),
         code_presentation = as.factor(code_presentation)) %>%
  distinct(code_module, code_presentation,id_student, score, date, final_result) %>%
  group_by(code_module,code_presentation, final_result, date) %>%
  summarise(avg_score = mean(score)) %>%
  ungroup() %>%
  ggplot(mapping = aes(x = date, y = avg_score, group = final_result, color = final_result)) +
  geom_line(size = 1.2) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "grey85"),
        plot.background = element_rect(fill = "transparent", color = NA),
        strip.background = element_rect(fill = "grey75", colour = NA),
        legend.background = element_rect(fill = "transparent", color = NA)) +
  scale_color_manual(values = c("#E68A99","#A3B9AA")) +  
  ggtitle("Assessment Score Over Time") +                     
  xlab("Date") +                                  
  ylab("Average score") +
  labs(color = "Final result") +
  facet_grid(code_module ~ code_presentation) +
  ggsave(filename = "Score_Date_plot.png",                      
         bg = "transparent", dpi = 600) 


studentAssessment_fr$submission_delay <- studentAssessment_fr$date_submitted - studentAssessment_fr$date
studentAssessment_fr <- studentAssessment_fr[studentAssessment_fr$assessment_type != "Exam",]

# Results by submission delay
studentAssessment_fr[!is.na(studentAssessment_fr$score),-c(4,6,8)] %>%
  mutate(date = as.integer(date),
         date_submitted = as.integer(date_submitted),
         submission_delay = as.integer(submission_delay),
         score = as.integer(score),
         code_module = as.factor(code_module),
         final_result = as.factor(final_result),
         code_presentation = as.factor(code_presentation)) %>%
  distinct(code_module, code_presentation,id_student, score, date, date_submitted, submission_delay, final_result) %>%
  group_by(code_module,code_presentation, final_result, id_student) %>%
  summarise_at(c("submission_delay","score"),mean) %>%
  ungroup() %>%
  ggplot(mapping = aes(x = submission_delay, y = score, color = final_result)) +
  geom_point(alpha = 0.2) +
  scale_color_manual(values = c("#E68A99","#A3B9AA")) +  
  ggtitle(as.character("Results by Submission Delay"))+
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_grid(code_module ~ code_presentation)













## RESULTS: MODEL PERFORMANCE VISUALIZATIONS =============================================================

library("readxl")
library("reshape2")
library("cowplot")

# Import excel sheets
Performance_lst <- lapply(1:6, function(i) read_excel("Model Performance Sheets.xlsx", sheet = i))

# Make a list of dataframes, each dataframe corresponding to the accuracy or recall results for one of the three courses
Performance_dfs <- list()

for (i in 1:length(Performance_lst)){
  Performance_dfs[[i]] <- data.frame(Performance_lst[i])                                      # Turn tibbles into dataframes
  
  names(Performance_dfs[[i]])[1] <- "Decile"                                                  # Rename first column to "Decile"
  Performance_dfs[[i]]$Decile <- c(0:10)                                                      # Make "Decile" variable numerical
  
  Performance_dfs[[i]] <- melt(Performance_dfs[[i]], id.var = "Decile")                       # Melt variables together for ggplot convenience
  Performance_dfs[[i]]$variable<-factor(Performance_dfs[[i]]$variable,
                                        levels=c("DT","RF","LR","SVM","NB", "MLP", "LSTM", "Baseline"))   # Reorder variables for plotting purposes later on
}

# Give a name to each dataframe in the list
names(Performance_dfs) <- c("Course BBB Accuracy","Course BBB Recall",
                            "Course DDD Accuracy","Course DDD Recall",
                            "Course FFF Accuracy","Course FFF Recall")


# Make a plot showing the performance of each model in terms of accuracy / recall for each course
perf_plots <- list()

for (i in 1:length(Performance_dfs)){
  perf_plots[[i]] <- ggplot(data=Performance_dfs[[i]], aes(x=Decile, y = value, colour = variable, linetype =variable)) +  # Specify dataframe and variables for line chart
    geom_point(size = 2) +                                                                                                 # Size of markers in plot
    geom_line(size = 1.5) +                                                                                                # Size of lines in plot
    scale_colour_manual("",                                                                                                # Set Colors for each line
                        breaks = c("DT", "RF", "LR","SVM","NB","MLP", "LSTM", "Baseline"),
                        values = c("#00798c", "#d1495b", "#edae49", "#66a182", "#706072", "#cc8664", "black", "#625F63")) +
    scale_linetype_manual(values=c("solid","solid","solid","solid","solid","solid","solid", "dashed"), guide = FALSE)+     # Make sure baseline is of dashed linetype
    ggtitle(names(Performance_dfs)[i]) +                                                                                   # Plot title
    xlab("Decile") +                                                                                                       # X-label
    ylab(tail(strsplit(names(Performance_dfs)[i], split=" ")[[1]],1)) +                                                    # Y-label: grab last word from the list element name which is either accuracy or recall
    theme(plot.title = element_text(hjust = 0.5),                                                                          # Style of plot
          panel.background = element_rect(fill = "grey85"),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.background = element_rect(fill = "transparent", color = NA),
          text = element_text(size=8),
          plot.margin = unit(c(6,4,6,1), "pt")) +
    scale_x_continuous(breaks = seq(0, 10, by = 1)) +                                                                     # Scale and intervals x-label
    scale_y_continuous(breaks = seq(0, 1, by = 0.1))                                                                      # Scale and intervals y-label
}
perf_plots[[1]]
# Create a grid with the accuracy and recall plots for the specific courses side-by-side and export them
file_names <- c("BBB_Performance.png",0, "DDD_Performance.png",0, "FFF_Performance.png")

for (i in c(1,3,5)){
  duo_plot <- plot_grid( perf_plots[[i]] + theme(legend.position="none"),
                         perf_plots[[i+1]] + theme(legend.position="none"),
                         align = 'vh',
                         nrow = 1,
                         ncol = 2)
?scale_linetype_manual  
  # Extract the legend of the accuracy plot and make it horizontal
  legend <- get_legend(perf_plots[[i]] +
                         guides(colour=guide_legend(nrow=1, byrow=TRUE), shape=guide_legend(nrow=1, byrow=TRUE)) +
                         theme(legend.direction = "horizontal",legend.justification="center" ,legend.box.just = "bottom"))
  
  # Add the shared legend to the grid
  p <- plot_grid(duo_plot, legend, align = "h", axis = "t", rel_heights = c(4,.3), nrow = 2)
  
  save_plot(file_names[i],plot = p, bg = "transparent")
}











