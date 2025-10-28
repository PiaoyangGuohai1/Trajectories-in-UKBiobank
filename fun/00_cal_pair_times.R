calculate_pair_times <- function(data_long, diag_key, diag_other) {
  
  data_long_sp <- data_long[disease %in% c(diag_key, unlist(diag_other)), 
                            .(eid, disease, diagnosis_date)]
  
  
  dt_wide <- dcast(data_long_sp, eid ~ disease, value.var = "diagnosis_date")
  
  setnames(dt_wide, old = diag_key, new = "diag_key")
  
  result_dt <- data.table(
    disease = character(length(diag_other)),
    mean_diff_years = numeric(length(diag_other)),
    count = integer(length(diag_other))
  )
  result_dt[, disease := diag_other]
  
  for (i in seq_along(diag_other)) {
    d <- diag_other[i]
    if (!d %in% colnames(dt_wide)) {
      warning(sprintf("Disease '%s' not found in data_long; skipped.", d))
      result_dt[i, `:=`(
        mean_diff_years = NA_real_,
        count = 0L
      )]
      next
    }
    dt_sub <- dt_wide[!is.na(diag_key) & !is.na(get(d)), 
                      .(diag_key, diag_other_date = get(d))]
    
    pair_count <- nrow(dt_sub)
    if (pair_count == 0) {
      result_dt[i, `:=`(
        mean_diff_years = NA_real_,
        count = 0L
      )]
      next
    }
    dt_sub[, diff_years := as.numeric((diag_other_date - diag_key) / 365.25)]
    avg_diff <- mean(dt_sub$diff_years)
    
    result_dt[i, `:=`(
      mean_diff_years = avg_diff,
      count = pair_count
    )]
  }
 
  
  result_dt[, group := toupper(substr(disease, 1, 1))]
  result_dt <- result_dt[group %in% c("E", "I", "N")]
  
  if(min(result_dt$mean_diff_years) > -10){
    result_dt <- bind_rows(result_dt, data.table(disease = "min_add",
                                                 mean_diff_years = -10,
                                                 count = 0, 
                                                 group = "I"))
  }
  
  if(max(result_dt$mean_diff_years) < 10){
    result_dt <- bind_rows(result_dt, data.table(disease = "max_add",
                                                 mean_diff_years = 10,
                                                 count = 0, 
                                                 group = "I"))
  }
  
  return(result_dt)
}

my_density_plot <- function(results = results, disease, bw = 0.08, top_n = 10){
  
  library(paletteer)
  library(ggplot2)
  library(ggrepel)
  
  my_d_palette <- paletteer_d("ggsci::nrc_npg")[c(1, 3, 4)]
  
  dens_list <- lapply(unique(results$group), function(g) {
    df_g <- subset(results, group == g)
    dens <- density(
      df_g$mean_diff_years,
      weights = df_g$count / sum(df_g$count),
      bw      = bw
    )
    data.frame(
      x     = dens$x,
      y     = dens$y,
      group = g
    )
  })
  dens_df <- do.call(rbind, dens_list)
  
  label_df <- results
  label_df$y <- mapply(function(x0, g) {
    dens_g <- dens_df[dens_df$group == g, ]
    approx(dens_g$x, dens_g$y, xout = x0)$y
  }, label_df$mean_diff_years, label_df$group)
  
  label_top <- label_df[order(-label_df$count), ][1:top_n, ]
  
  my_d_palette <- paletteer_d("ggsci::nrc_npg")[c(1, 3, 4)]
  
  p <- ggplot(results,
         aes(x = mean_diff_years,
             weight = count,
             color=group,
             fill   = group)) +
    geom_density(
      position = "stack",
      alpha    = 0.7,
      bw       = bw,
      linewidth = 0.1,
      outline.type = "full"
    ) +
    geom_text_repel(
      data = label_top,
      aes(x     = mean_diff_years,
          y     = y,
          label = disease),
      size = 3
    ) +
    coord_cartesian(xlim = c(-10, 10), ylim = c(0, 2)) +
    scale_fill_manual(values = my_d_palette) +
    labs(
      x = paste0("Mean Difference in Years", "(", disease,")"),
      y = NULL
    ) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  return(p)
}
