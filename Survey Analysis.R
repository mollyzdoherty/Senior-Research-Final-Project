# Import libraries
library(ggplot2)
library(caret)

# Import survey rating results for supposed creators
supposed <- read.csv("SurveyRatingsSupposedCreators.csv", header = TRUE, sep = ",")

# Import survey rating results for actual creators
actual <- read.csv("SurveyRatingsActualCreators.csv", header = TRUE, sep = ",")

# Create data frames for image ratings

supposed <- data.frame(supposed)
View(supposed)

actual <- data.frame(actual)
View(actual)

# Remove entries where image has possibly been seen before
supposed <- supposed[supposed$Creator != "I believe I have seen this image before",]
actual <- actual[actual$Creator != "I believe I have seen this image before",]

# Levels of experience
experience <- c("None", "Beginner", "Intermediate", "Advanced","Professional")

# Accuracy percentage for each level of experience
accuracy <- c(0.802, 0.884, 0.688, 0.833,0) 

# Create data frame for accuracy and levels of experience
exp <- data.frame(experience, accuracy) 
View(exp)

# Order experience levels
exp$experience <- factor(exp$experience, levels=c("None","Beginner","Intermediate","Advanced","Professional"))

# Remove "Professional" experience due to lack of entries
exp <- exp[exp$experience != "Professional",]

# Graph to display accuracy between levels of artistic experience
ggplot(exp, aes(x=experience, y=accuracy, fill=experience)) + 
  geom_col(show.legend = FALSE, position = "dodge") + geom_text(aes(label = accuracy), vjust = 1.5,
  position = position_dodge(width = 0.9)) + ggtitle("Accuracy of Levels of Artistic Experience") +
  labs(x = "Artistic Experience", y = "Accuracy Percentage") + theme_classic()

# Boxplots for comparing ratings between supposed creators

# Boxplot for comparison of beauty between supposed creators
ggplot(supposed, aes(Creator, Beauty, fill = Creator)) + 
  geom_boxplot(show.legend = FALSE) + 
  ggtitle("Comparison of Beauty between Supposed Creators") +
  labs(x = "Creator", y = "Rating") +
  theme_classic() 

# Boxplot for comparison of emotion between supposed creators
ggplot(supposed, aes(Creator, Emotion, fill = Creator)) + 
  geom_boxplot(show.legend = FALSE) + 
  ggtitle("Comparison of Emotion between Supposed Creators") +
  labs(x = "Creator", y = "Rating") +
  theme_classic() 

# Boxplot for comparison of effort between supposed creators
ggplot(supposed, aes(Creator, Effort, fill = Creator)) + 
  geom_boxplot(show.legend = FALSE) + 
  ggtitle("Comparison of Effort between Supposed Creators") +
  labs(x = "Creator", y = "Rating") +
  theme_classic() 

# Boxplot for comparison of worth between supposed creators
ggplot(supposed, aes(Creator, Worth, fill = Creator)) + 
  geom_boxplot(show.legend = FALSE) + 
  ggtitle("Comparison of Worth between Supposed Creators") +
  labs(x = "Creator", y = "Rating") +
  theme_classic() 

# Boxplots for comparing ratings between actual creators

# Boxplot for comparison of beauty between actual creators
ggplot(actual, aes(Creator, Beauty, fill = Creator)) + 
  geom_boxplot(show.legend = FALSE) + 
  ggtitle("Comparison of Beauty between Actual Creators") +
  labs(x = "Creator", y = "Rating") +
  theme_classic() 

# Boxplot for comparison of emotion between actual creators
ggplot(actual, aes(Creator, Emotion, fill = Creator)) + 
  geom_boxplot(show.legend = FALSE) + 
  ggtitle("Comparison of Emotion between Actual Creators") +
  labs(x = "Creator", y = "Rating") +
  theme_classic() 

# Boxplot for comparison of effort between actual creators
ggplot(actual, aes(Creator, Effort, fill = Creator)) + 
  geom_boxplot(show.legend = FALSE) + 
  ggtitle("Comparison of Effort between Actual Creators") +
  labs(x = "Creator", y = "Rating") +
  theme_classic() 

# Boxplot for comparison of worth between actual creators
ggplot(actual, aes(Creator, Worth, fill = Creator)) + 
  geom_boxplot(show.legend = FALSE) + 
  ggtitle("Comparison of Worth between Actual Creators") +
  labs(x = "Creator", y = "Rating") +
  theme_classic() 

# Create visually appealing confusion matrix
# Modified code from Cybernetic on stackoverflow
# https://stackoverflow.com/questions/23891140/r-how-to-visualize-confusion-matrix-using-the-caret-package

cm$table <- matrix(c(114, 21, 37, 128), nrow = 2, byrow = TRUE)

rownames(cm$table) <- c("AI-generated", "Human-made")
colnames(cm$table) <- c("AI-generated", "Human-made")

draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
  
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, 'AI-generated', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, 'Human-made', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, 'AI-generated', cex=1.2, srt=90)
  text(140, 335, 'Human-made', cex=1.2, srt=90)
  
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
}  

draw_confusion_matrix(cm)