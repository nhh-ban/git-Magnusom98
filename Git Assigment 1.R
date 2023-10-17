# Problem 2

# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Define the file name
data_file <- "suites_dw_Table1.txt"

# Read all lines from the file
all_lines <- readLines(data_file)

# Identify the index of the separator line
separator_index <- which(grepl("-----+----+----", all_lines))

# Extract column names from the line just above the separator
col_names <- strsplit(all_lines[separator_index - 1], "\\|")[[1]]
col_names <- gsub("^[[:space:]]+|[[:space:]]+$", "", col_names)  # Remove leading/trailing whitespaces

# Read the actual data starting from the line just below the separator
data_lines <- all_lines[(separator_index + 1):length(all_lines)]

# Combine data lines into a single string, separating columns with ","
data_text <- paste(gsub("\\|", ",", data_lines), collapse = "\n")

# Read the data text into a data frame
data_df <- read.csv(text = data_text, header = FALSE, stringsAsFactors = FALSE)

# Assign column names to the data frame
names(data_df) <- col_names

# Handle missing data, etc. according to requirements
data_df <- data_df %>%
  mutate(across(everything(), ~na_if(.,""))) %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(where(is.factor), as.numeric))

# Problem 3

#Possible Approach
#Histogram: Create a histogram of linear diameters (a_26) or stellar mass (log_lk) of the galaxies. 
#If smaller objects are under-represented, the histogram should show fewer galaxies with small diameters or lower mass.

#Scatter Plot: Create a scatter plot with distance (D) on the x-axis
#and another property such as diameter or mass on the y-axis. 
#If smaller galaxies are under-represented, we might see a paucity of points
#in the area of the plot representing small sizes/masses.

#Likely Explanation
#If smaller objects appear to be under-represented, it might be due to the limits of our detection capabilities. 
#Smaller, less luminous galaxies are harder to detect at larger distances than bigger, more luminous ones. 
#So, this could induce a bias in our sample where only the larger objects are included at the further distances,
#which could mislead interpretations regarding the population of galaxies in the universe.
#This is supported by the histogram, showing a leftward shift, showing an underepresentation of smaller objects.
#A more centered spike would be expected.

# Histogram of linear diameters
p1 <- ggplot(data_df, aes(x = a_26)) +
  geom_histogram(binwidth=0.5, fill="blue", color="black", alpha=0.7) +
  labs(title = "Histogram of Linear Diameters",
       x = "Linear Diameter (a_26, kpc)",
       y = "Frequency") +
  theme_minimal()

# Scatter plot of distance vs. linear diameter
p2 <- ggplot(data_df, aes(x = D, y = a_26)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter plot of Distance vs. Linear Diameter",
       x = "Distance (D, Mpc)",
       y = "Linear Diameter (a_26, kpc)") +
  theme_minimal()

# Display the plots
print(p1)
print(p2)
