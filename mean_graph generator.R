# Calculate and plot the mean of RTs in both set and money conditions, for self / other(partner)
# Ratings for snacks in Task A / Task B

# Codes marked *Optional are not necessary to run

# change the working directory to the current computer
setwd("/Users/lingwei/Desktop/Lab_Candidate_Assignment")

# read the keys to observe data meaning (*Optional)
keyA <- read.csv("TaskA_Key.csv", stringsAsFactors = FALSE)
keyB <- read.csv("TaskB_Key.csv", stringsAsFactors = FALSE)

# list files in the data file
filenames <- list.files("data")

# create A and B result pointers
resultA <- c()
resultB <- c()

# read one B file to observe the data (*Optional)
sampleB <- read.csv(paste("data/", filenames[2], sep = ''), header = TRUE, stringsAsFactors = FALSE)
str(sampleB)

# read Task A data and Task B data into dataframe
for (files in filenames){
        file <- read.csv(paste("data/", files, sep = ''), header = TRUE, stringsAsFactors = FALSE)
        
        # add ID column 
        file$ID <- rep(substr(files, 1, 3), length(file[, 1]))
        if (grepl("_a_", files)){
                resultA <- rbind(resultA, file)
        }
        else{
                resultB <- rbind(resultB, file)
        }
}

# create a function data.summary to return a matrix of mean and sd
# data is a vector, var is a factor

data.summary <- function(data, var){
        
        mean <- unlist(tapply(as.numeric(data)[!(is.na(as.numeric(data)))], var, mean))
        sd <- unlist(tapply(as.numeric(data)[!(is.na(as.numeric(data)))], var, sd))
        data.sum <- data.frame(condition = c("self", "other"),mean, sd)
        return(data.sum)
        
}

# calculate mean RT for each participant for choice 1 for Set Condition
# select RT as not "n/a" and for Option 1 contains c(2,3,6,12)

RT.set <- data.summary(resultB[resultB$Choice.1 %in% c(2,3,6,12) & resultB$Computer.Response == 0, "Choice.1.RT"], 
                       resultB[resultB$Choice.1 %in% c(2,3,6,12) & resultB$Computer.Response == 0,"Choosing.For"])

RT.money <- data.summary(resultB[resultB$Choice.1 %in% c(0.5, 1, 1.25, 1.5, 1.75) & resultB$Computer.Response == 0, "Choice.1.RT"],
                         resultB[resultB$Choice.1 %in% c(0.5, 1, 1.25, 1.5, 1.75) & resultB$Computer.Response == 0,"Choosing.For"])


# create a function making.plot() to plot and save plots, with settled parameters
making.plot <- function(m_data, m_title, m_fill, m_filename, m_x, m_y){
        library(ggplot2)
        png(filename = m_filename)
        plot<- ggplot(m_data, aes(x=condition, y=mean)) + theme(axis.text=element_text(size=12), 
                                                                axis.title = element_text(size = 14), 
                                                                title = element_text(size = 16, face = "bold")) +
                geom_bar(stat="identity", color="black", width = 0.3, fill = m_fill,
                         position=position_dodge()) +
                geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width = 0.2) + 
                labs(title=m_title, x=m_x, y = m_y)
        print(plot)
        print(c(m_data, m_title, m_fill, m_filename, m_x, m_y))
        dev.off()
}

# create bar graph for RT for money stimuli
making.plot(m_data = RT.money, m_filename = "RT.Money.png", m_fill = c("red", "blue"), 
            m_title = "RT in Money Condition for Choice 1", m_x="condition", m_y = "RT")

making.plot(m_data = RT.set, m_filename = "RT.Set.png", m_fill = c("red", "blue"), 
            m_title = "RT in Set Condition for Choice 1", m_x = "condition", m_y = "RT")


# calculate mean and sd for preference rating in Taks A, Task B (self) and Task B (other)

rating.TaskB <- data.summary(resultB[grepl(".jpg", resultB$Choice.2) & resultB$Computer.Response == 0, "Decision.Rating"], 
                             resultB[ grepl(".jpg", resultB$Choice.2) & resultB$Computer.Response == 0,"Choosing.For"])

rating.TaskA <- data.frame(condition = "Task A", mean = mean(resultA$Rating, na.rm = TRUE), 
                           sd = sd(resultA$Rating, na.rm = TRUE) )

rating <- rbind(rating.TaskA, rating.TaskB)

# create bar graph for ratings of Task A and Task B for self / other
making.plot(m_data = rating, m_filename = "Rating.png", m_fill = c("black","red", "blue"), 
            m_title = "Rating in Task A, Task B for self, Tasl B for other", m_x = "condition", m_y = "rating")

