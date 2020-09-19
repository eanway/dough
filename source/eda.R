# exploratory data analysis for dough data
# 9/19/2020
# ECA

if(!require(pacman)) install.packages("pacman"); library(pacman)

# Import ####
p_load(
  readr, 
  here
)

df_dough <- read_rds(here::here("working", "out", "df_dough_standard.rds"))


# Analyze ####
p_load(
  dplyr
)

summary(df_dough)

df_dough_complete <- df_dough %>%
  filter(
    !is.na(rating)
  )

# df_dough_full <- df_dough_complete %>%
#   select(
#     where(~!anyNA(.))
#   )

# Plot ####
p_load(
  ggplot2
)

ggplot(stack(df_dough_complete), aes(x = ind, y = values)) + 
  geom_boxplot() + 
  facet_wrap(~ind, scales = "free")

ggplot(stack(df_dough_complete), aes(x = values)) + 
  geom_histogram() + 
  facet_wrap(~ind, scales = "free")


# Remove Outliers ####
df_dough_inliers <- df_dough_complete %>%
  filter(
    !batch_number == 2
  )


# Correlation ####
df_dough_in <- df_dough_inliers %>%
  select(
    -batch_number, -rating
  )

df_dough_out <- df_dough_inliers %>%
  select(
    rating
  )

df_cor <- cor(df_dough_in, df_dough_out, use = "complete.obs")

pairs(df_dough_inliers)


# PCA ####
p_load(
  tidyr, 
  ggfortify, 
  factoextra
)

df_dough_pca <- df_dough_inliers %>%
  select(
    -batch_number
  ) %>%
  drop_na()

pca_res <- prcomp(select(df_dough_pca, -rating), scale. = TRUE)

autoplot(pca_res, data = df_dough_pca, loadings = TRUE, loadings.label = TRUE, colour = "rating")

# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/
fviz_eig(pca_res)

quanti.coord <- cor(select(df_dough_pca, rating), pca_res$x)

pca_res %>%
  fviz_pca_var() %>%
  fviz_add(quanti.coord, color = "blue", geom = "arrow")

# Kmeans ####
kmeans_res <- kmeans(df_dough_pca, 2)

autoplot(kmeans_res, data = df_dough_pca, size = "rating")


# Bench time per loaf ####
ggplot(df_dough_inliers, aes(x = units, y = bench_duration_mins)) + 
  geom_point(aes(size = bench_time_per_loaf))

ggplot(df_dough_inliers, aes(x = units, y = bench_time_per_loaf)) + 
  geom_point(aes(size = rating))

plot_v_rating <- function(df, x_var) {
  ggplot(df, aes(x = {{x_var}}, y = rating)) + 
    geom_point() + 
    geom_smooth()
}

plot_v_rating(df_dough_inliers, hydration)

plot_v_rating(df_dough_inliers, units)

plot_v_rating(df_dough_inliers, auto_duration_mins)

plot_v_rating(df_dough_inliers, bench_duration_mins)

plot_v_rating(df_dough_inliers, bench_time_per_loaf)

plot_v_rating(df_dough_inliers, bake_duration_mins)
