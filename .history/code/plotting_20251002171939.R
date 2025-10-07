###############################################################################
# Topic: Estimating Trump's Tariff Impact on Global Welfare
# Goal: Create functions for plotting results
# Keywords: Tariffs, Global Welfare, Trade Policy, International Trade
# Autor: Nícolas de Moura (<nicolasgoulartdemoura@gmail.com>)
# Date: 2025-10-02
###############################################################################

# Define color palette
color_palette <- c("#CC0000", "#46647B", "#507867", "#F1D998", "#EF5B78", "#8E806F", "#973B74", "#CAAF49", "#B4B4B4", "#5C5C5C", "#3498DB", "#E74C3C", "#2ECC71", "#F39C12", "#9B59B6")

###############################################################################
# Functions for plotting scatterplots
###############################################################################

# Function to create styled scatterplots
gg_scatterplot <- function(data,
                          x_var,
                          y_var,
                          color_var = NULL,
                          size_var = NULL,
                          title = NULL,
                          subtitle = NULL,
                          x_label = "",
                          y_label = "",
                          out_dir = "figures",
                          out_name = NULL,
                          width = 16,
                          height = 9,
                          point_color = "#CC0000",
                          point_size = 2.5,
                          point_alpha = 0.7,
                          xlim = NULL,
                          ylim = NULL,
                          v_lines = NULL,
                          h_lines = NULL,
                          show_trend = FALSE,
                          trend_color = "#2c3e50",
                          legend = TRUE,
                          overwrite = TRUE,
                          verbose = TRUE) {
    
    # Base plot
    if (!is.null(color_var) && !is.null(size_var)) {
        p <- ggplot(data, aes_string(x = x_var, y = y_var, color = color_var, size = size_var))
    } else if (!is.null(color_var)) {
        p <- ggplot(data, aes_string(x = x_var, y = y_var, color = color_var))
    } else if (!is.null(size_var)) {
        p <- ggplot(data, aes_string(x = x_var, y = y_var, size = size_var))
    } else {
        p <- ggplot(data, aes_string(x = x_var, y = y_var))
    }
    
    # Add points
    if (is.null(color_var) && is.null(size_var)) {
        p <- p + geom_point(color = point_color, size = point_size, alpha = point_alpha)
    } else {
        p <- p + geom_point(alpha = point_alpha)
    }
    
    # Add trend line if requested
    if (show_trend) {
        p <- p + geom_smooth(method = "lm", se = FALSE, color = trend_color, 
                            linewidth = 1, linetype = "dashed")
    }
    
    # Add baseline
    p <- p + geom_hline(yintercept = 0, color = "#2c3e50", size = 0.5) +
             geom_vline(xintercept = 0, color = "#2c3e50", size = 0.5)
    
    # Add vertical lines if provided
    if (!is.null(v_lines)) {
        if (is.list(v_lines) && all(sapply(v_lines, is.list))) {
            for (vl in v_lines) {
                p <- p + geom_vline(xintercept = vl$value,
                                   linetype = ifelse(is.null(vl$linetype), "dashed", vl$linetype),
                                   color = ifelse(is.null(vl$color), "#7f8c8d", vl$color),
                                   size = ifelse(is.null(vl$size), 0.8, vl$size))
            }
        } else {
            for (vl in v_lines) {
                p <- p + geom_vline(xintercept = vl, linetype = "dashed", 
                                   color = "#7f8c8d", size = 0.8)
            }
        }
    }

    # Add horizontal lines if provided
    if (!is.null(h_lines)) {
        if (is.list(h_lines) && all(sapply(h_lines, is.list))) {
            for (hl in h_lines) {
                p <- p + geom_hline(yintercept = hl$value,
                                   linetype = ifelse(is.null(hl$linetype), "dashed", hl$linetype),
                                   color = ifelse(is.null(hl$color), "#7f8c8d", hl$color),
                                   size = ifelse(is.null(hl$size), 0.8, hl$size))
            }
        } else {
            for (hl in h_lines) {
                p <- p + geom_hline(yintercept = hl, linetype = "dashed", 
                                   color = "#7f8c8d", size = 0.8)
            }
        }
    }
    
    # Add color scale if color variable is used
    if (!is.null(color_var)) {
        if (is.factor(data[[color_var]]) || is.character(data[[color_var]])) {
            p <- p + scale_color_manual(values = color_palette, 
                                       guide = if (legend) guide_legend(title = color_var) else "none")
        } else {
            p <- p + scale_color_gradient(low = "#B4B4B4", high = "#CC0000",
                                         guide = if (legend) guide_colorbar(title = color_var) else "none")
        }
    }
    
    # Apply style
    p <- p + 
        labs(
            title = title,
            subtitle = subtitle,
            x = x_label,
            y = y_label
        ) +
        theme_minimal() +
        theme(
            # Text styling
            plot.title = element_text(size = 16, face = "bold", color = "#2c3e50", 
                                    margin = margin(b = 10)),
            plot.subtitle = element_text(size = 12, color = "#7f8c8d", 
                                       margin = margin(b = 20)),
            axis.title = element_text(size = 18, color = "#2c3e50", face = "bold"),
            axis.text = element_text(size = 18, color = "#2c3e50"),
            legend.title = element_text(size = 18, color = "#2c3e50", face = "bold"),
            legend.text = element_text(size = 18, color = "#2c3e50"),
            
            # Clean background - no gridlines
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "white", color = NA),
            plot.background = element_rect(fill = "white", color = NA),
            
            # Margins
            plot.margin = margin(20, 20, 20, 20)
        )
    
    # Apply axis limits
    if (!is.null(xlim)) {
        p <- p + scale_x_continuous(labels = scales::comma, limits = xlim, 
                                   expand = expansion(mult = c(0.02, 0.02)))
    } else {
        p <- p + scale_x_continuous(labels = scales::comma, 
                                   expand = expansion(mult = c(0.02, 0.02)))
    }
    
    if (!is.null(ylim)) {
        p <- p + scale_y_continuous(labels = scales::comma, limits = ylim, 
                                   expand = expansion(mult = c(0.02, 0.02)))
    } else {
        p <- p + scale_y_continuous(labels = scales::comma, 
                                   expand = expansion(mult = c(0.02, 0.02)))
    }

    # Save the plot
    if (!is.null(out_name)) {
        if (!dir.exists(out_dir)) {
            dir.create(out_dir, recursive = TRUE)
        }
        filename <- file.path(out_dir, paste0(out_name, ".png"))
        if (overwrite || !file.exists(filename)) {
            ggsave(filename, plot = p, width = width, height = height, dpi = 300, bg = "white")
            if (verbose) {
                cat("Plot saved to:", filename, "\n")
            }
        } else if (verbose) {
            cat("File exists and overwrite is FALSE:", filename, "\n")
        }
    }
    
    return(p)
}