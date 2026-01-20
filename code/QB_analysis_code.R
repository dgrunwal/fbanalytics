# ============================================
# QB Rank vs Passing Completions Correlation
# ============================================

# Load required libraries
library(ggplot2)
library(dplyr)

# Clear console
cat("\014")

# Read the CSV file
qb_data <- read.csv("FantasyPros_Fantasy_Football_Statistics_QB.csv")

# Display first few rows to understand the data
print("First few rows of data:")
head(qb_data)

# Display column names
print("\nColumn names:")
colnames(qb_data)

# ============================================
# Data Preparation - FIXED
# ============================================

# Create clean dataset - remove rows where CMP or Rank is 0 or missing
qb_clean <- qb_data %>%
  filter(!is.na(Rank) & !is.na(CMP)) %>%
  filter(CMP > 0)  # Only QBs with actual completions

print(paste("\nRows after cleaning:", nrow(qb_clean)))

# YDS is character, convert to numeric (remove commas)
qb_clean$YDS <- as.numeric(gsub(",", "", qb_clean$YDS))

# ============================================
# Create Correlation Plot
# ============================================

# Main scatter plot with trend line
p1 <- ggplot(qb_clean, aes(x = Rank, y = CMP)) +
  geom_point(aes(size = ATT, color = TD), alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
  labs(
    title = "QB Rank vs Passing Completions",
    subtitle = "2024 Fantasy Football Season",
    x = "QB Rank",
    y = "Passing Completions",
    size = "Attempts",
    color = "Touchdowns"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.position = "right"
  )

print(p1)

# ============================================
# Calculate Correlation Coefficient
# ============================================

correlation <- cor(qb_clean$Rank, qb_clean$CMP, use = "complete.obs")
print(paste("\nCorrelation coefficient (Rank vs Completions):", round(correlation, 4)))

# ============================================
# Linear Regression Model
# ============================================

model <- lm(CMP ~ Rank, data = qb_clean)
print("\nLinear Regression Summary:")
print(summary(model))

# ============================================
# Simple Version - Without ggrepel (if not installed)
# ============================================

# Identify top 10 and bottom 10 QBs
top_bottom <- qb_clean %>%
  arrange(Rank) %>%
  mutate(Group = case_when(
    Rank <= 10 ~ "Top 10",
    Rank >= max(Rank) - 9 ~ "Bottom 10",
    TRUE ~ "Middle"
  ))

p2 <- ggplot(top_bottom, aes(x = Rank, y = CMP)) +
  geom_point(aes(color = Group), size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "darkblue", alpha = 0.3) +
  labs(
    title = "QB Rank vs Passing Completions",
    subtitle = "Highlighting Top 10 and Bottom 10 QBs",
    x = "QB Rank",
    y = "Passing Completions",
    color = "Group"
  ) +
  scale_color_manual(values = c("Top 10" = "green", "Middle" = "gray", "Bottom 10" = "red")) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom"
  )

print(p2)

# ============================================
# Multiple Correlation Analysis
# ============================================

# Correlation matrix for multiple variables
cor_vars <- qb_clean %>%
  select(Rank, CMP, ATT, YDS, TD, INT) %>%
  cor(use = "complete.obs")

print("\nCorrelation Matrix:")
print(round(cor_vars, 3))

# Visualize correlation with completions
cor_with_cmp <- cor_vars[, "CMP"]
cor_df <- data.frame(
  Variable = names(cor_with_cmp),
  Correlation = cor_with_cmp
) %>%
  filter(Variable != "CMP") %>%
  arrange(desc(abs(Correlation)))

p3 <- ggplot(cor_df, aes(x = reorder(Variable, Correlation), y = Correlation, fill = Correlation)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Correlation, 3)), hjust = -0.1, size = 4) +
  coord_flip() +
  scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = 0) +
  labs(
    title = "Correlation with Passing Completions",
    x = "Variable",
    y = "Correlation Coefficient"
  ) +
  theme_minimal() +
  ylim(-1, 1)

print(p3)

# ============================================
# Completion Rate Analysis
# ============================================

# Calculate completion percentage (already in data as PCT)
qb_clean <- qb_clean %>%
  mutate(CMP_PCT = PCT)

p4 <- ggplot(qb_clean, aes(x = Rank, y = CMP_PCT)) +
  geom_point(aes(color = CMP), size = 3, alpha = 0.7) +
  geom_smooth(method = "loess", se = TRUE, color = "blue") +
  labs(
    title = "QB Rank vs Completion Percentage",
    x = "QB Rank",
    y = "Completion Percentage (%)",
    color = "Total\nCompletions"
  ) +
  theme_minimal() +
  scale_color_gradient(low = "lightblue", high = "darkblue")

print(p4)

# ============================================
# Additional Analysis: Fantasy Points vs Completions
# ============================================

p5 <- ggplot(qb_clean, aes(x = CMP, y = FPTS)) +
  geom_point(aes(color = Rank, size = TD), alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "purple") +
  labs(
    title = "Passing Completions vs Fantasy Points",
    x = "Passing Completions",
    y = "Fantasy Points",
    color = "Rank",
    size = "Touchdowns"
  ) +
  theme_minimal() +
  scale_color_gradient(low = "green", high = "red")

print(p5)

# ============================================
# Save Plots
# ============================================

ggsave("qb_rank_completions.png", plot = p1, width = 12, height = 8, dpi = 300)
ggsave("qb_rank_completions_grouped.png", plot = p2, width = 12, height = 8, dpi = 300)
ggsave("correlation_analysis.png", plot = p3, width = 10, height = 6, dpi = 300)
ggsave("completion_percentage.png", plot = p4, width = 12, height = 8, dpi = 300)
ggsave("completions_vs_fpts.png", plot = p5, width = 12, height = 8, dpi = 300)

print("\n✓ All plots created and saved successfully!")
print(paste("✓ Correlation between Rank and Completions:", round(correlation, 4)))
print("✓ Files saved in current working directory")

# ============================================
# Summary Statistics
# ============================================

print("\n=== Summary Statistics ===")
print(paste("Total QBs analyzed:", nrow(qb_clean)))
print(paste("Average Completions:", round(mean(qb_clean$CMP), 2)))
print(paste("Median Completions:", median(qb_clean$CMP)))
print(paste("Top QB Completions:", max(qb_clean$CMP)))
print(paste("Lowest QB Completions:", min(qb_clean$CMP)))

