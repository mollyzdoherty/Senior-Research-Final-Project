library(ggplot2)

experience <- c("None", "Beginner", "Intermediate", "Advanced", "Professional") 
accuracy <- c(0.10, 0.20, 0.30, 0.40, 0.50) 
creator <- c("AI-generated","Human-made","AI-generated","Human-made","AI-generated")
beauty <- c(1,2,3,4,5)
emotion <- c(1,2,3,4,5)
effort <- c(1,2,3,4,5)
worth <- c(1,2,3,4,5)
df <- data.frame(experience, accuracy, creator, beauty, emotion, effort, worth) 
View(df)

df$experience <- factor(df$experience, levels=c("None","Beginner","Intermediate","Advanced","Professional"))
ggplot(df, aes(x=experience, y=accuracy, fill=experience)) + 
  geom_col(show.legend = FALSE) +
  ggtitle("Accuracy of Levels of Artistic Experience") +
  labs(x = "Artistic Experience", y = "Accuracy Percentage") + theme_classic()

ggplot(df, aes(creator, beauty, fill = creator)) + 
  geom_boxplot(show.legend = FALSE) + 
  ggtitle("Comparison of Beauty between Creators") +
  labs(x = "Creator", y = "Rating") +
  theme_classic() 

ggplot(df, aes(creator, emotion, fill = creator)) + 
  geom_boxplot(show.legend = FALSE) + 
  ggtitle("Comparison of Emotion between Creators") +
  labs(x = "Creator", y = "Rating") +
  theme_classic() 

ggplot(df, aes(creator, effort, fill = creator)) + 
  geom_boxplot(show.legend = FALSE) + 
  ggtitle("Comparison of Effort between Creators") +
  labs(x = "Creator", y = "Rating") +
  theme_classic() 

ggplot(df, aes(creator, worth, fill = creator)) + 
  geom_boxplot(show.legend = FALSE) + 
  ggtitle("Comparison of Worth between Creators") +
  labs(x = "Creator", y = "Rating") +
  theme_classic() 