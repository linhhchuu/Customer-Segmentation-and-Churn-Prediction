library(conflicted)
library(tidyverse)
conflict_prefer_all("dplyr", quiet = TRUE)
library(here)
library(scales)
library(hrbrthemes)
library(beyonce)
library(parsnip)
library(ggrepel)
library(kableExtra)
library(gt)
library(psych)
library(broom)
library(MASS)
library(ggplot2)
library(reshape2)
library(psy)
library("FactoMineR")
library("factoextra")
library(janitor)
library(treemapify)

# Format ------------------------------------------------------------------
select <- dplyr::select

theme_set(theme_ipsum(base_family = "poppins"))
pal <- beyonce_palette(66)
SMALL_FONT_SIZE <- 14

theme_colors_variable <- function(color="#C7C8CC") {
  theme(
    panel.background = element_rect(fill = color, color = color),
    plot.background = element_rect(fill = color, color = color)
  )
}
# The code below is a function I use to format my tables. It relies on the
# gt() package. 
lv_tab_style <- function(gt_object) {
  gt_object |> 
    fmt_number(decimals = 0, use_seps = TRUE) |> 
    tab_style(
      style = list(
        cell_borders(sides = c("top", "bottom"), color = "#cfcfcf")
      ),
      locations = list(cells_body(), cells_stub())
    ) |> 
    tab_style(
      style = list(
        cell_text(color = "#000000")
      ),
      locations = cells_body()
    ) |> 
    tab_style(
      style = list(
        cell_borders(sides = "top", weight = px(3), color = "black"),
        cell_text(weight = "bold", color = "black")
      ),
      locations = list(cells_stub(), cells_column_labels())
    ) |> 
    tab_style(
      style = list(
        cell_borders(sides = "bottom", weight = px(1), color = "black")
      ),
      locations = list(cells_stub(), cells_column_labels())
    ) |> 
    cols_width(
      everything() ~ px(70)
    ) |> 
    tab_options(
      table.align = "left",
      table_body.hlines.color = "#000000", 
      table_body.border.top.style = "#000000", 
      heading.border.bottom.color = "#000000", 
      heading.border.bottom.width = px(1),
      column_labels.border.bottom.color = "#000000",
      column_labels.border.bottom.width = px(1),
      column_labels.font.size = px(12),
      table_body.border.bottom.color = "#000000",
      table_body.border.bottom.width = px(2),
      table.font.names = "poppins"
    )
}
# Read data ---------------------------------------------------------------

data <- read_csv(here("Documents/11. USC MSBA/14. MKT 512 Customer Insights and Analysis/Assignments/IA2 Crews Cup/crews-cup-data.csv"))
data$tenure <- 2024 - data$origination

commit <- data[, c("a1", "a2", "a3", "n1", "n2", "n3", "e1", "e2", "e3", "f1", "f2", "f3", "h1", "h2", "h3")]

num_var <- data %>% select(-id, -origination, -treatment)

# EDA ---------------------------------------------------------------------
num_var |> 
  gather(Variable, value) |> 
  group_by(Variable) |> 
  summarise(
    N = n(),
    Median = median(value),
    Mean = mean(value),
    SD = sd(value),
    Min = min(value),
    Max = max(value)
  ) |> 
  gt() |> 
  lv_tab_style() |> 
  fmt_number(
    columns = c("Mean", "SD"),
    decimals = 2
  ) |> 
  cols_width(
    Variable ~ px(100),
    Median:Max ~ px(100)
  )

# Create the bar plot
ggplot(data, aes(x = treatment)) +
  geom_bar() +
  coord_flip() +
  geom_text(stat = "count", aes(label = ..count..), hjust = -0.2) +
  labs(title = "Number of Customers per Treatment Type", x = "Treatment Type", y = "Number of Customers")

# Correlation (ChatGPT)

# Calculate the correlation matrix
cor_matrix <- cor(num_var, use = "complete.obs")

# Melt the correlation matrix for ggplot2
cor_melted <- melt(cor_matrix)

# Create the heatmap
ggplot(cor_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, 
                       limit = c(-1, 1), name = "Correlation") +
  geom_text(aes(label = round(value, 2)), color = "black") +
  labs(title = "Correlation Heatmap", x = "Variables", y = "Variables")

# Correlation for survey
commit_cor <- commit %>% 
  cor()

commit_cor %>% 
  reshape2::melt() %>% 
  mutate(
    fill = case_when(
      value >= 0.5 ~ scales::col_numeric(palette = c("white", "blue"), domain = c(0.5, 1))(value),
      TRUE ~ "white"
    )
  ) %>% 
  ggplot(aes(x = Var1, y = Var2, fill = fill)) +
  geom_tile(
    color = "grey70"
  ) +
  geom_text(
    aes(label = number(value, .001)),
    color = "black",
    fontface = "bold"
  ) +
  scale_fill_identity() +
  labs(
    title = "Brand Equity Variable Correlations",
    x = "",
    y = "",
    caption = "Color intensity increases with correlation strength (>= 0.5)"
  )
# EFA ---------------------------------------------------------------------
ev <- eigen(cor(commit)) # get eigenvalues
ev$values

scree(commit, pc=FALSE)  # Use pc=FALSE for factor analysis
psy::scree.plot(commit)


# Scree plot and Parallel Analysis
fa.parallel(commit, fa = "fa")

# EFA statistics
facts <- tibble(f = 1:8) %>% 
  mutate(
    fct = map(f, ~factanal(commit, factors = .x, scores = "regression")),
    tidied = map(fct, tidy),
    glanced = map(fct, glance)
  )

facts %>% 
  unnest(glanced) %>% 
  select(n.factors, total.variance, statistic, p.value, df) %>% 
  gt() |> 
  lv_tab_style() |> 
  cols_width(
    total.variance ~ px(100),
    statistic ~ px(100)
  ) |> 
  fmt_number(
    columns = where(is.numeric),
    decimals = 3
  )

# Factor loadings
facts %>% 
  filter(f == 3) %>% 
  unnest(tidied) %>% 
  select(starts_with("fl"), variable) %>% 
  gather(factor, loading, -variable) %>% 
  mutate(factor = str_replace(factor, "fl", "Factor ")) %>% 
  ggplot(aes(x=factor, y=fct_rev(variable), fill=loading)) +
  geom_tile(
    show.legend = FALSE,
    color = "grey50"
  ) +
  geom_text(
    aes(label = number(loading, .001)),
    size = 5
  ) +
  scale_fill_gradient(low = "white", high = "#ff9999") +
  theme(
    axis.text = element_text(face = "bold")
  ) +
  labs(
    title = "Factor Loadings",
    y="", x="Factor"
  )

# EFA diagrams
fa_3 <- fa(commit, 
           nfactors = 3, 
           rotate = "Varimax", 
           max.iter = 100, 
           scores = "regression")
colnames(fa_3$loadings) <- c("Factor 1", "Factor 2", "Factor 3")

fa.diagram(fa_3, simple = FALSE, rsize = 0.35, main = "EFA—Varimax Rotation", cex = 1.2, digits = 2)

# Uniqueness
facts %>% 
  filter(f == 3) %>% 
  unnest(tidied) %>% 
  select(variable, uniqueness) %>% 
  arrange(desc(uniqueness)) |> 
  gt() |> 
  lv_tab_style() |> 
  fmt_number(
    columns = uniqueness,
    decimals = 3
  ) |> 
  cols_width(
    variable ~ px(150),
    uniqueness ~ px(150)
  )

# Rotated Factors
f_obl <- fa(commit, nfactors = 3, rotate = "oblimin")

d <- data.frame(unclass(f_obl$loadings))

d <- as_tibble(d) %>% 
  mutate(variable = row.names(d)) %>% 
  relocate(variable, .before = MR1)

d %>% 
  pivot_longer(
    !variable,
    names_to = "factor",
    values_to = "value"
  ) %>%
  mutate(
    factor = str_replace(factor, "MR", "Factor ")
  ) %>% 
  ggplot(aes(x=factor, y=fct_rev(variable), fill=value)) +
  geom_tile(
    show.legend = FALSE,
    color = "grey50"
  ) +
  geom_text(
    aes(label = number(value, .001)),
    size = 5
  ) +
  scale_fill_gradient(low = "white", high = "grey70") +
  theme(
    axis.text = element_text(face = "bold")
  ) +
  labs(
    title = "Factor Loadings",
    y="", x="Factor"
  )

# EFA diagram rotated
fa_3_pmx <- fa(commit, 
               nfactors = 3, 
               rotate = "Promax", 
               max.iter = 100, 
               scores = "regression")
colnames(fa_3_pmx$loadings) <- c("Factor 1", "Factor 2", "Factor 3")

fa.diagram(fa_3_pmx, simple = FALSE, rsize = 0.35, main = "EFA—Promax Rotation", cex = 1.2, digits = 2)

# Factor assignment
f3 <- factanal(commit, factors = 3, scores = "regression")

f3$scores %>% 
  as_tibble() %>% 
  rename(
    calculative = `Factor1`,
    normative = `Factor2`,
    affective = `Factor3`
  ) %>% 
  cor() %>% 
  reshape2::melt() %>% 
  ggplot(aes(x=Var1, y=Var2, fill=value)) +
  geom_tile(show.legend = F, color = "grey80") +
  geom_text(aes(label = number(value, .01)), fontface = "bold") +
  scale_fill_gradient(low = "white", high = "grey70") +
  theme(
    axis.text = element_text(face = "bold")
  ) +
  labs(
    title = "Factor Correlations"
  )

# Alpha
affect <- commit %>% 
  select(a1, a2, a3)

norm <- commit %>% 
  select(n1, n2, n3)

econ <- commit %>% 
  select(e1, e2, e3)

forc <- commit %>% 
  select(f1, f2, f3)

habit <- commit %>% 
  select(h1, h2, h3)

affect_alpha <- psych::alpha(affect)
norm_alpha <- psych::alpha(norm)
econ_alpha <- psych::alpha(econ)
forc_alpha <- psych::alpha(forc)
habit_alpha <- psych::alpha(habit)

affect_alpha
norm_alpha
econ_alpha
forc_alpha
habit_alpha

# Combine data ------------------------------------------------------------
# Combine factor to original df (without intent, treatment)
factor_data <- data %>%
  select(classes, tenure, contribution, csat) %>%
  mutate(
    affective = rowMeans(select(data, a1, a2, a3)),
    normative = rowMeans(select(data, n1, n2, n3)),
    economic = rowMeans(select(data, e1, e2, e3)),
    forced = rowMeans(select(data, f1, f2, f3)),
    habitual = rowMeans(select(data, h1, h2, h3))
  )

# factor_data <- transmute(
#   data,
#   affective = rowMeans(select(data, a1, a2, a3), na.rm = TRUE),
#   normative = rowMeans(select(data, n1, n2, n3), na.rm = TRUE),
#   economic = rowMeans(select(data, e1, e2, e3), na.rm = TRUE),
#   forced = rowMeans(select(data, f1, f2, f3), na.rm = TRUE),
#   habitual = rowMeans(select(data, h1, h2, h3), na.rm = TRUE)
# )

factor_data |> 
  gather(Variable, value) |> 
  group_by(Variable) |> 
  summarise(
    N = n(),
    Median = median(value),
    Mean = mean(value),
    SD = sd(value),
    Min = min(value),
    Max = max(value)
  ) |> 
  gt() |> 
  lv_tab_style() |> 
  fmt_number(
    columns = c("Mean", "SD"),
    decimals = 2
  ) |> 
  cols_width(
    Variable ~ px(100),
    Median:Max ~ px(100)
  )

# Reshape and plot the histogram
factor_data %>%
  gather(variable, value) %>%
  ggplot(aes(x = value)) +
  geom_histogram(color = "white", fill = pal[5]) +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Histogram of Selected Variables in new_data", x = "Value", y = "Frequency")

base <- factor_data %>% 
  gather(key = "metric", value = "score")

a1 <- aov(score ~ metric, data = base)
hsd_1 <- TukeyHSD(a1)
hsd_tidy <- tidy(hsd_1)


hsd_tidy |> 
  mutate(
    Significance = ifelse(adj.p.value < 0.05, "Significant", "Not Significant"),
    Significance = factor(Significance)
  )  |> 
  ggplot(aes(y=contrast, color = Significance)) +
  geom_segment(aes(xend = conf.low, x = conf.high, yend=contrast), show.legend = FALSE) +
  geom_point(aes(x=estimate), size = 3) +
  geom_text(aes(x=estimate, label = round(estimate, 2)), fontface = "bold", nudge_y = .4, color = "grey60") +
  geom_vline(xintercept = 0, color = pal[2], alpha = 0.8, linetype = "dashed") +
  scale_color_manual(values = c("grey80", "#FF2244")) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(linetype = "dotted", linewidth = .5),
    panel.grid.major.x = element_line(linetype = "dotted", linewidth = .4)
  ) +
  labs(
    title = "Tukey's HSD"
  )
# Clustering --------------------------------------------------------------
## Prepare data for clustering
# cluster_data <- data %>% select(-id, -origination, -treatment)

# PCA
pca_data <- factor_data |>
  prcomp(scale. = TRUE)

pct_exp <- pca_data |>
  tidy(matrix = "eigenvalues") |>
  pluck("cumulative", 2)

pca_tidy <- pca_data |>
  tidy(matrix = "rotation")

pca_augment <- pca_data |>
  augment(data)

# Function to plot PCA
pca_plot <- function(data, title, pce) {
  ggplot(data, aes(x=PC1, y=PC2)) +
    geom_segment(
      xend = 0,
      yend = 0,
      arrow = arrow(ends = "first", length = unit(6, "pt"))
    ) +
    geom_label_repel(aes(label = column)) +
    labs(
      title = title,
      caption = str_glue("{percent(pce, 1)} of data explained")
    )
}

pca_tidy |>
  pivot_wider(
    names_from = "PC",
    names_prefix = "PC",
    values_from = "value"
  ) |>
  pca_plot(title = "PCA of All Data", pct_exp)

pca <- PCA(factor_data, graph = TRUE)

# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(pca, choice = "var", axes = 1:2)

## Clustering with K-means
pca_df <- pca$ind$coord[, 1:2] |>
  as_tibble() |>
  janitor::clean_names()

pca_df_aug <- data |>
  bind_cols(pca_df)

set.seed(2121)
kclusts <- tibble(k = 1:6) |> 
  mutate(
    clust = map(k, ~ kmeans(pca_df, centers = .x, nstart = 25)),
    tidied = map(clust, tidy),
    glanced = map(clust, glance),
    augmented = map(clust, augment, pca_df_aug)
  )

pct_exp <- pca$eig |> 
  as_tibble() |> 
  janitor::clean_names() |> 
  pluck("cumulative_percentage_of_variance", 2)

kclusts |> 
  unnest(glanced) |> 
  ggplot(aes(x=k, y=tot.withinss)) +
  geom_line(aes(group=1)) +
  geom_point(size = 4) +
  labs(
    title = "Scree Plot",
    subtitle = "Using first two factor dimensions",
    caption = str_glue("{percent(pct_exp / 100, 1)} of data explained")
  )

# Cluster center
kclusts |> 
  unnest(augmented) |> 
  ggplot(aes(x = dim_1, y = dim_2)) +
  geom_point(aes(color = .cluster), alpha = 0.15, size = 1, show.legend = FALSE) +
  facet_wrap(~ k, nrow = 2) +
  scale_color_viridis_d() +
  geom_point(data = kclusts |> unnest(tidied), size = 5, shape = 13, color = "#FFFFFF", show.legend = FALSE) +
  labs(
    title = "Cluster Centers",
    caption = str_glue("{percent(pct_exp / 100, 1)} of data explained")
  )

# 3 clusters
k3 <- kclusts |> 
  unnest(augmented) |> 
  filter(k == 3)

K3_SCALE <- k3 |> 
  group_by(.cluster) |> 
  summarise(n = n(), .groups = "drop") |> 
  mutate(p = prop.table(n))

plot_core <- k3 |> 
  group_by(.cluster) |> 
  summarise(
    N = n(),
    Satisfaction = mean(csat), 
    Intent = mean(intent)) |> 
  gather(metric, score, -.cluster, -N) |> 
  mutate(
    label = str_glue("K{.cluster}\n(n={N})")
  ) |> 
  ggplot(aes(x=score, y=fct_rev(label), fill=label)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label = round(score,2)), hjust=0, nudge_x = 0.02, fontface="bold") +
  scale_fill_viridis_d() +
  theme(
    axis.text.x = element_blank(),
    strip.text = element_text(face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  facet_wrap(~ metric, scales = "free_x") +
  labs(x="", y="Cluster")

plot_core +
  scale_x_continuous(expand = expansion(mult = c(0, 0.5)))

# Segment share
k3 |> 
  group_by(cluster = .cluster) |> 
  count() |> 
  ungroup() |> 
  mutate(
    p = prop.table(n),
    label = str_glue("K{cluster}\n{percent(p, 1)}")
  ) |> 
  ggplot(aes(area = n, fill=n, label=label)) +
  treemapify::geom_treemap(show.legend = FALSE) +
  treemapify::geom_treemap_text(color = "white") +
  theme(
    plot.margin = margin(0, 0, 0, 0, "pt")
  )

# # Behavioral
# k3 |> 
#   select(.cluster, contribution, classes, tenure) |> 
#   mutate(across(where(is.numeric), \(x) as.numeric(scale(x)))) |> 
#   gather(variable, score, -.cluster) |> 
#   group_by(.cluster, variable) |> 
#   summarise(mu = mean(score)) |> 
#   ggplot(aes(x=variable, y=mu, color = .cluster, shape=.cluster, group = .cluster)) +
#   geom_line(linewidth=1.2, show.legend = FALSE) +
#   geom_point(size = 3) +
#   scale_color_viridis_d() +
#   labs(
#     title = "Behavioral Differences",
#     subtitle = "Scaled Data by Cluster",
#     x="", y=""
#   )

# Extract the cluster labels for 3 clusters
k3_assign <- kclusts %>%
  filter(k == 3) %>%        # Filter for k = 3 solution
  pull(clust) %>%          # Pull out the clustering result
  pluck(1) %>%              # Extract the first clustering object
  .$cluster                 # Get the cluster assignments
## Using Factor Data
# Add the cluster labels back to the original dataset
cluster_factor_data <- factor_data %>%
  mutate(Cluster = k3_assign)  # Add a new column 'Cluster'

# Assuming your data frame is named cluster_factor_data
summary_table <- cluster_factor_data %>%
  group_by(Cluster) %>%
  summarise(across(everything(), list(mean = mean), na.rm = TRUE))

styled_table <- summary_table %>%
  gt() |>
  lv_tab_style() |>
  cols_width(
    everything() ~ px(100)
  ) |> 
  fmt_number(
    columns = where(is.numeric),
    decimals = 2
  )
styled_table

# Add everything back to data table
export_data <- data %>%
  bind_cols(factor_data %>% select(affective, normative, economic, forced, habitual)) %>%
  mutate(cluster = k3_assign)

# Export the data to a CSV file
write.csv(export_data, "crews-cup-lr.csv", row.names = FALSE)