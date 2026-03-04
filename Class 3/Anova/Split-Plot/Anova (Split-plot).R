###    Code writen by Yousuf Islam   ###

#Load the library 
library(agricolae)

# Load the dataset 
data<-read.csv(file.choose())
head(data)

#Anova
model<- with(data, sp.plot(Rep, Date, Trt,   HI ))

## LSD test 
gla<- model$gl.a
glb<- model$gl.b
Ea<- model$Ea
Eb<- model$Eb

#output
out1<- with(data, LSD.test(  HI,Date, gla,Ea, console= T))
out2<- with(data, LSD.test(  HI,Trt, glb,Eb, console= T))

#  Step 1:Create interaction variable
data$DateTrt <- interaction(data$Date, data$Trt)
# Step 2: Run LSD test on interaction
out3 <- with(data, LSD.test(  HI, DateTrt, glb, Eb, console = TRUE))


 ##  Extract and round only numeric columns

# For out1
df1 <- out1$groups
df1$  HI <- round(df1$  HI, 2)

# For out2
df2 <- out2$groups
df2$  HI <- round(df2$  HI, 2)

# For out3
df3 <- out3$groups
df3$  HI <- round(df3$  HI, 2)

# Step 1: Add source label
df1$Source <- "Date"
df2$Source <- "Treatment"
df3$Source <- "Date × Treatment"

# Step 2: Move rownames to a column
df1$Level <- rownames(df1)
df2$Level <- rownames(df2)
df3$Level <- rownames(df3)

# Step 3: Reorder columns
df1 <- df1[, c("Source", "Level", "HI", "groups")]
df2 <- df2[, c("Source", "Level", "HI", "groups")]
df3 <- df3[, c("Source", "Level", "HI", "groups")]

# Step 4: Combine into one data frame
combined_df <- rbind(df1, df2, df3)

# Step 5: Rename columns
colnames(combined_df) <- c("Source", "Treatment_Level", "  HI", "Group")

# Step 6: Export to CSV
write.csv(combined_df, "LSD_combined_  HI1.csv", row.names = FALSE)


   