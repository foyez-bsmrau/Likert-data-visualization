library(dplyr)
library(tidyr)
library(ggplot2)
library(likert)
library(gridExtra)

# Set the folder path where your CSV files are located
dataFolder <- "D:/Likert_visualization"

# Load your cleaned CSV files
cul <- read.csv(paste0(dataFolder, "/Cul_separation.csv"))
supp <- read.csv(paste0(dataFolder, "/Supp_separation.csv"))

# Drop the 'Distance from Sea' column if present
cul <- cul %>% select(-contains("Distance"))
supp <- supp %>% select(-contains("Distance"))

#Rename the column
cul_eco <- cul %>%
  rename("Group"= Group, "Perceived importance of tourism" = Perceived.importance.of.tourism,
         "Perceived importance of recreational" = Perceived.importance.of.recreational, "Perceived importance of aesthetics " = Perceived.importance.of.aesthetics,
         "Perceived importance of education"=Perceived.importance.of.education,
         "Perceived importance of research"=Perceived.importance.of.research)

supp_eco <- supp %>%
  rename("Group"= Group, "Perceived importance of habitat for animals" = Perceived.importance.of.habitat.for.animals,
         "Perceived importance of habitat for birds" = Perceived.importance.of.habitat.for.birds, "Perceived importance of habitat for fisheries " = Perceived.importance.of.habitat.for.fisheries)


# Define Likert scale labels
labels <- c("Not important", "Low important", "Moderate important", "High important", "Very high important")

# Convert numeric to factors with labels
cul_survey <- cul_eco %>%
  mutate(across(where(is.numeric), ~ factor(., levels = 1:5, labels = labels))) %>%
  as.data.frame()

supp_survey <- supp_eco %>%
  mutate(across(where(is.numeric), ~ factor(., levels = 1:5, labels = labels))) %>%
  as.data.frame()

# Generate Likert objects
likert_cul <- likert(cul_survey[, 2:6], grouping = cul_survey$Group)
likert_supp <- likert(supp_survey[, 2:4], grouping = supp_survey$Group)

# Create plots with no legend
plot_cul <- plot(likert_cul, wrap = 60, text.size = 4.6) +
  ggtitle("(a)") +
  theme(
    axis.text = element_text(size = 14, face = "bold"),         # Axis tick labels
    axis.title.x = element_text(size = 14, face = "bold"),      # X-axis title
    axis.title.y = element_text(size = 14, face = "bold"),      # Y-axis title
    strip.text = element_text(size = 14, face = "bold"),        # Facet strip titles
    plot.title = element_text(size = 18, face = "bold"),        # Plot title
    legend.title = element_text(size = 15, face = "bold"),      # Legend title
    legend.position = "none",legend.text=element_text(size=10,face='bold')
  )

plot_supp <- plot(likert_supp, wrap = 60, text.size = 4.6, face = "bold") +
  ggtitle("(b)") +
  theme(
    axis.text = element_text(size = 14, face = "bold"),         # Axis tick labels
    axis.title.x = element_text(size = 14, face = "bold"),      # X-axis title
    axis.title.y = element_text(size = 14, face = "bold"),      # Y-axis title
    strip.text = element_text(size = 14, face = "bold"),        # Facet strip titles
    plot.title = element_text(size = 18, face = "bold"),        # Plot title
    legend.title = element_text(size = 15, face = "bold"),      # Legend title
    legend.position = "none",legend.text=element_text(size=10,face='bold')
  )

# Extract legend from one of the plots

get_legend <- function(a.gplot) {
  tmp <- ggplotGrob(
    a.gplot + theme(
      legend.position = "bottom",
      legend.text = element_text(size = 15, face = "bold"),        # Adjust font size of legend labels
      legend.title = element_text(size = 15, face = "bold"),  # Optional: for title if any
    )
  )
  legend <- tmp$grobs[which(sapply(tmp$grobs, function(x) x$name) == "guide-box")][[1]]
  return(legend)
}

common_legend <- get_legend(plot(likert_supp))


# Arrange plots and legend
combined_plot <- grid.arrange(
  arrangeGrob(plot_cul, plot_supp, ncol = 2),
  common_legend,
  nrow = 2,
  heights = c(10, 1.5)
)

# Save output at 600 DPI
ggsave(filename = file.path(dataFolder, "Likert_plot600dpi.png"),
       plot = combined_plot,
       width = 18, height = 9, dpi = 600, units = "in")