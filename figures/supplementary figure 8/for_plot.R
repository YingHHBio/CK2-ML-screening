
setwd("C:\\Users\\Localadmin_weikaixi\\OneDrive - University of Helsinki\\Private\\ying\\TSanalysis")
# Load the necessary packages
library(ggplot2)
library(plotly)
library(reshape2)

# Read your CSV file
df <- read.csv('svm_plot_input.csv')

# Change Tanimoto_threshold to factor
df$Tanimoto_threshold <- factor(df$Tanimoto_threshold)

# Split df into two dataframes for Dropout_test and Random_control
df_dropout <- df[, c("Tanimoto_threshold", "Dropout_test", "SEM1", "Proportion_of_removed_compounds")]
df_random <- df[, c("Tanimoto_threshold", "Random_control", "SEM2", "Proportion_of_removed_compounds")]

# Rename the columns to a common format
colnames(df_dropout) <- c("Tanimoto_threshold", "Test", "SEM", "Proportion_of_removed_compounds")
colnames(df_random) <- c("Tanimoto_threshold", "Test", "SEM", "Proportion_of_removed_compounds")

# Add a new column to each dataframe to indicate the test type
df_dropout$Type <- "Dropout_test"
df_random$Type <- "Random_control"

# Combine the dataframes
df_combined <- rbind(df_dropout, df_random)
fix(df_combined)
# # Create the initial bar plot
# p <- ggplot(df_combined, aes(x = Tanimoto_threshold, y = Test, fill = Type)) + 
#   geom_bar(stat = "identity", position = position_dodge()) +
#   geom_errorbar(aes(ymin = Test - SEM, ymax = Test + SEM), width = 0.2, position = position_dodge(0.9)) +
#   scale_fill_manual(values = c("Dropout_test" = "blue", "Random_control" = "red")) + 
#   ylab("Spearman coefficient") + xlab("Tanimoto_threshold")


# # Create a secondary y-axis that is a transformation of the primary one
# sec_axis_trans <- ~ . / max(df_combined$Test) * max(df_combined$Proportion_of_removed_compounds)
# 
# p <- ggplot(df_combined, aes(x = as.numeric(Tanimoto_threshold))) + 
#   geom_bar(aes(y = Test, fill = Type), stat = "identity", position = position_dodge()) +
#   geom_errorbar(aes(ymin = Test - SEM, ymax = Test + SEM), width = 0.2, position = position_dodge(0.9)) +
#   geom_point(aes(y = Proportion_of_removed_compounds / max(Proportion_of_removed_compounds) * max(Test), color = Type), size = 3, position = position_dodge(0.9)) +
#   geom_line(aes(y = Proportion_of_removed_compounds / max(Proportion_of_removed_compounds) * max(Test), color = Type, group = Type), position = position_dodge(0.9)) +
#   scale_color_manual(values = c("Dropout_test" = "blue", "Random_control" = "red")) +
#   scale_fill_manual(values = c("Dropout_test" = "blue", "Random_control" = "red")) + 
#   ylab("Spearman coefficient") + xlab("Tanimoto threshold") + 
#   scale_y_continuous(sec.axis = sec_axis(sec_axis_trans, name = "Proportion of removed compounds"))
# 
# print(p)

# Calculate unique values for the line plot
fix(df_combined)
df_line <- unique(df_combined[, c("Tanimoto_threshold", "Proportion_of_removed_compounds")])
fix(df_line)
# Create a secondary y-axis that is a transformation of the primary one
sec_axis_trans <- ~ . / max(df_combined$Test) * max(df_combined$Proportion_of_removed_compounds)
fix(sec_axis_trans)
p <- ggplot(df_combined, aes(x = as.numeric(Tanimoto_threshold))) + 
  geom_bar(aes(y = Test, fill = Type), stat = "identity", position = position_dodge()) +
  geom_errorbar(data = df_combined %>% filter(Type == "Random_control"),
                aes(ymin = Test - SEM, ymax = Test + SEM), width = 0.2, position = position_dodge(0.9)) +
  geom_line(data = df_line, aes(y = Proportion_of_removed_compounds / max(df_combined$Proportion_of_removed_compounds) * max(df_combined$Test), group = 1), 
            color = "black", size = 1) +
  scale_fill_manual(values = c("Dropout_test" = "steelblue", "Random_control" = "darkorange")) + 
  ylab("Spearman coefficient") + xlab("Tanimoto threshold") + 
  
  scale_y_continuous(sec.axis = sec_axis(sec_axis_trans, name = "Proportion of removed compounds")) +
  theme_bw() +
  theme(panel.grid = element_blank())

print(p)
pdf(file=paste0("SVM",".pdf"), width=11, height=5)
print(p)
dev.off()















