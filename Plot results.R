
######visualization for anitibiotic contamination probability at different monitoring provinces(Figure 2 in the manuscript)##############################
#########################################
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(readxl)

#################################################################################
# Create the plot
# Load the data
data <- read_excel("AntibioticsProb.xlsx",sheet="Sheet3")

# Convert the antibiotic pollution probabilities to percentages
data$prob_anti <- data$prob_anti * 100
data$Total_Antibiotic_Pollution_Probability <- data$Total_Antibiotic_Pollution_Probability * 100

# Create a color palette with as many distinct colors as there are production provinces
num_colors <- length(unique(data$PRODP))
color_palette <- colorRampPalette(brewer.pal(6, "Set3"))(num_colors)
p <- ggplot(data, aes(x = reorder(MonitP, -Total_Antibiotic_Pollution_Probability), y = prob_anti, fill = PRODP)) +
 geom_bar(stat = "identity", position = "stack", width = 0.7) +
 geom_text(data = data %>% group_by(MonitP) %>% slice(1), aes(y = Total_Antibiotic_Pollution_Probability, label = sprintf("%.1f%%", Total_Antibiotic_Pollution_Probability)), hjust = 0, size = 2) +
 scale_y_continuous(name = "Percentage of Antibiotic Contamination in the Total Contaminated Eggs.") +
 scale_x_discrete(name = "Consumption Regions") +
 labs(fill = "Production Regions") +
 coord_flip() +
 theme_minimal(base_size = 15) + 
 theme(
  plot.title = element_text(hjust = 0.5, vjust = 0),
  plot.title.position = "panel",
  legend.text = element_text(size = 8),##change the size of legend text
  legend.position = "right", # Moving the legend to the left
  legend.direction = "vertical", # Making the legend vertical
  legend.box.just = "top", # Aligning the legend with the top of the plot
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.text = element_text(size = 12)
 ) +
 scale_fill_manual(values = color_palette)
print(p) 

# Save and display the plot
ggsave("AntibioticProb.png", plot = p, width = 10, height = 8, dpi =600)

###############################################################################################################################################
#############################visualization for antibiotic contamination caused HI at different monitoring provinces from each production province####
#(Figure 6A in the manuscript)
# Load the data
data <- read_excel("HIatMfromP_311.xlsx")
colnames(data)<-c("MonitP","Prod","THIatM","HIfromPtoM")


# Create a color palette with as many distinct colors as there are production provinces
num_colors <- length(unique(data$Prod))
color_palette <- colorRampPalette(brewer.pal(6, "Set3"))(num_colors)

# Create the plot
p <- ggplot(data, aes(x = reorder(MonitP, -THIatM), y = HIfromPtoM, fill = Prod)) +
 geom_bar(stat = "identity", position = "stack", width = 0.7) +
 geom_text(data = data %>% group_by(MonitP) %>% slice(1), aes(y = THIatM, label = sprintf("%.4f", THIatM)), hjust = -0.1, size = 2) +
 scale_y_continuous(name = "Hazard Index") +
 scale_x_discrete(name = "Consumption Regions") +
 labs( fill = "Production Regions") +
 coord_flip() +
 theme_minimal(base_size = 10) +
 theme(
  plot.title = element_text(hjust = 0.5, vjust = 0),
  plot.title.position = "panel",
  legend.position = "right", # Moving the legend to the left
  legend.direction = "vertical", # Making the legend vertical
  legend.text = element_text(size = 8),##change the size of legend text
  legend.box.just = "top", # Aligning the legend with the top of the plot
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.text = element_text(size = 10)
 ) +
 scale_fill_manual(values = color_palette)

print(p)
# Save and display the plot
ggsave("HIfromPtoM.png", plot = p, width = 10, height = 8, dpi =600)
print(p)


########################################################################################################
######visualization for antibiotic concentration at different monitoring provinces##############################
#(Figure 3B in the manuscript)
########################################################################################################
library(readxl)
library(ggplot2)
library(reshape2)
install.packages("pheatmap")
library(pheatmap)
library(viridis)
# Reading the data from an Excel file
data <- read_excel("Concentration level.xlsx", sheet = "Sheet3")
#####################################
# Aggregating the data to get mean concentrations
agg_data <- data %>%
 group_by(MonitP, Hazard) %>%
 summarise(Mean_Concentration = mean(Concentration, na.rm = TRUE)) %>%
 ungroup()

# Creating the heatmap
q<-ggplot(agg_data, aes(x = Hazard, y = MonitP, fill = Mean_Concentration)) +
 geom_tile() +
 geom_text(aes(label = sprintf("%.2f", Mean_Concentration)), vjust = 1, size = 3) +
 scale_fill_viridis(direction = -1, option = "D") +
 theme_minimal() +
 theme(axis.text.x = element_text(angle = 45, hjust = 1),
       panel.grid.major = element_blank(), 
       panel.grid.minor = element_blank(),
       panel.background = element_blank()) +
 labs(fill = "Median Concentration (ug/kg)", x = "Antibiotic Residues", y = "Consumption Regions")
########################################
print(q)
ggsave("concentration.png", plot = q, width = 10, height = 8, dpi =600)



####################################################################################################################
######visualization for each antibiotic contamination probability at different monitoring provinces##############################
#(Figure 3A in the manuscript)
########################################################################################################
# Load required libraries
library(readxl)
library(ggplot2)
library(reshape2)
install.packages("scales")
library(scales)

# Read the data from Excel file
data <- read_excel("ProbForEachAnti.xlsx",sheet = "Sheet2")

# Convert 'contProb' to percentage
data$contProb <- data$contProb * 100

# Reshape the data for ggplot2
data_melted <- melt(data, id.vars = c("Monitoring Province", "Hazard"))

# Create the heatmap
q<-ggplot(data_melted, aes(x = Hazard, y = `Monitoring Province`, fill = value)) +
 geom_tile() + 
 geom_text(aes(label = sprintf("%.2f%%", value)), vjust = 1, size = 3) +
 scale_fill_gradient(low = "light blue", high = "blue") +
 theme_minimal() +
 theme(axis.text.x = element_text(angle = 45, hjust = 1),
       panel.grid.major = element_blank(), 
       panel.grid.minor = element_blank(),
       panel.background = element_blank()) +
 labs(fill = "Contamination Probability (%)", x = "Antibiotic Residues", y = "Consumption Regions")
########################################
print(q)
ggsave("CPorb.png", plot = q, width = 10, height = 8, dpi =600)



####################################################################################################################
#######visualize the HI for monitoring province based on different antibiotics
#(Figure in the appendix of manuscript)
#############################################################################################################
# Load the data
data <- read_excel("HIandCHQatM_130.xlsx")
colnames(data)<-c("MonitP","Hazard","CHQatMonitP","HI")


# Create a color palette with as many distinct colors as there are production provinces
num_colors <- length(unique(data$Hazard))
color_palette <- colorRampPalette(brewer.pal(8, "Set3"))(num_colors)


# Create the plot
p <- ggplot(data, aes(x = reorder(MonitP, -HI), y = CHQatMonitP, fill = Hazard)) +
 geom_bar(stat = "identity", position = "stack", width = 0.5) +
 geom_text(data = data %>% group_by(MonitP) %>% slice(1), aes(y = HI, label = sprintf("%.4f", HI)), hjust = 0, size = 3) +
 scale_y_continuous(name = "Hazard Index") +
 scale_x_discrete(name = "Consumption Regions") +
 labs(fill = "Antibiotic Residues") +
 coord_flip() +
 theme_minimal(base_size = 15) +
 theme(
  plot.title = element_text(hjust = 0.5, vjust = 0), # Adjusting the position of the title
  plot.title.position = "panel", # Moving the title inside the panel
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.text = element_text(size = 12),
  legend.position = "bottom",
  legend.box = "horizontal",
  legend.title = element_text(size = 10),
  legend.text = element_text(size = 10)
 ) +
 scale_fill_manual(values = color_palette)
p <- p + 
 geom_bar(stat = "identity", position = "stack", width = 0.5) + # Adjust the width of the bars
 theme(
  axis.text = element_text(size = 10), # Adjust the text size
  legend.text = element_text(size = 8)
 )
# Display the plot
print(p)
ggsave("HIatMonitP.png", plot = p, width = 10, height = 8, dpi =600)

####################################################################################################
#chord graph for HIfromPtoM
#(Figure 6B in the manuscript)
#########################################################################################################
library(readxl)
library(dplyr)
library(circlize)
library(RColorBrewer)

# Step 1: Load the data
data <- read_xlsx("HIatMfromP_311.xlsx", sheet = "Sheet3")

# Step 2: Aggregate the data
data_aggregated <- data %>%
 group_by(ProdP, MonitP) %>%
 summarise(HIfromPtoM = sum(HIfromPtoM), .groups = 'drop')



# Assuming you have a way to create 'flow_matrix' from 'data_aggregated'
# For the sake of example, this step is skipped here
# Ensure your 'flow_matrix' preparation logic is in place
# Create a matrix for the chord diagram
provinces <- unique(c(data_aggregated$ProdP, data_aggregated$MonitP))
matrix_size <- length(provinces)
flow_matrix <- matrix(0, nrow = matrix_size, ncol = matrix_size, dimnames = list(provinces, provinces))

for(i in 1:nrow(data_aggregated)) {
 row <- data_aggregated[i, ]
 flow_matrix[rownames(flow_matrix) == row$ProdP, colnames(flow_matrix) == row$MonitP] <- row$HIfromPtoM
}
# Prepare the color palette
num_colors <- length(unique(data$ProdP))
color_palette <- colorRampPalette(brewer.pal(min(num_colors, 9), "Set3"))(num_colors)

# Assign colors to each ProdP
prod_colors <- setNames(color_palette, unique(data$ProdP))
# Clear any existing plots
circos.clear()
circos.par("start.degree" = 90, "points.overflow.warning" = FALSE)
par(mar = c(5, 5, 5, 5))

# Draw the chord diagram with adjusted settings and apply the color mapping
chordDiagram(flow_matrix, transparency = 0.01, grid.col = prod_colors[rownames(flow_matrix)],
             annotationTrack = "grid", preAllocateTracks = list(track.height = 0.2))

# Adjust label positions
circos.track(track.index = 1, panel.fun = function(x, y) {
 xlim = get.cell.meta.data("xlim")
 ylim = get.cell.meta.data("ylim")
 sector.index = get.cell.meta.data("sector.index")
 label_y_pos = ylim[1] + 0.7  # Adjusted for visual fit
 circos.text(mean(xlim), label_y_pos, sector.index, facing = "clockwise", niceFacing = TRUE,
             adj = c(0.5, 0.5), cex = 1, font = 1)  # Font size and style adjusted
}, bg.border = NA)


######################################################################################################################
#######visualize the HI caused by antibiotics contaminated eggs from each production province##############################
#(Figure 5 in the manuscript)
######################################################################################################################
# Load the data
data <- read_excel("HICausedfromP_311.xlsx",sheet = "Sheet2") 
colnames(data)<-c("ProdP","Hazard","HQcausedbyProdP","HI")


# Create a color palette with as many distinct colors as there are production provinces
num_colors <- length(unique(data$Hazard))
color_palette <- colorRampPalette(brewer.pal(8, "Set3"))(num_colors)

# Create the plot
p <- ggplot(data, aes(x = reorder(ProdP, -HI), y = HQcausedbyProdP, fill = Hazard)) +
 geom_bar(stat = "identity", position = "stack", width = 0.5) +
 geom_text(data = data %>% group_by(ProdP) %>% slice(1), aes(y = HI, label = sprintf("%.4f", HI)), hjust = 0, size = 3) +
 scale_y_continuous(name = "Hazard Index") +
 scale_x_discrete(name = "Production Regions") +
 labs( fill = "Antibiotic Residues") +
 coord_flip() +
 theme_minimal(base_size = 15) +
 theme(
  plot.title = element_text(hjust = 0.5, vjust = 0,size=10), # Adjusting the position of the title
  plot.title.position = "panel", # Moving the title inside the panel
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.text = element_text(size = 10),
  legend.position = "bottom",
  legend.box = "horizontal",
  legend.title = element_text(size = 10),
  legend.text = element_text(size = 10)
 ) +
 scale_fill_manual(values = color_palette)
p <- p + 
 geom_bar(stat = "identity", position = "stack", width = 0.5) + # Adjust the width of the bars
 theme(
  axis.text = element_text(size = 10), # Adjust the text size
  legend.text = element_text(size = 8)
 )
# Display the plot
print(p)
ggsave("HIfromProdP.png", plot = p, width = 10, height = 8, dpi =600)

######################################################################################################################
#######################
#######visualize the consumption data at each province##############################
#(Figure A1 in the manuscript)
# Load the data
data <- read_excel("consumption data.xlsx") 
colnames(data)<-c("MonitoringRegion","Consumption")

# Calculate rankings (higher consumption gets a lower rank number)
data$Rank <- rank(-data$Consumption)
# Generate the bar plot with ordered bars based on rank
p <- ggplot(data, aes(x = reorder(MonitoringRegion, Rank), y = Consumption)) +
 geom_bar(stat = "identity", fill = "skyblue") +
 coord_flip() +
 geom_text(aes(label = round(Consumption, 2)), hjust = -0.1) +
 xlab("Consumption Regions") +
 ylab("Average Egg Consumption (g/day)") +
 theme(
  panel.background = element_blank(), 
  axis.line = element_line(colour = "black"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()
 ) +
 scale_y_continuous(expand = c(0, 0), limits = c(0, 60))  # Set x-axis limits

print(p)
ggsave("Consumption.png", plot = p, width = 10, height = 8, dpi =600)

