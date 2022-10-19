## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  dpi=300,
  comment = "#>"
)

## ----prepare-data-------------------------------------------------------------
library(grid)
library(forestploter)

# Read provided sample example data
dt <- read.csv(system.file("extdata", "example_data.csv", package = "forestploter"))

# Keep needed columns
dt <- dt[,1:6]

# indent the subgroup if there is a number in the placebo column
dt$Subgroup <- ifelse(is.na(dt$Placebo), 
                      dt$Subgroup,
                      paste0("   ", dt$Subgroup))

# NA to blank or NA will be transformed to carachter.
dt$Treatment <- ifelse(is.na(dt$Treatment), "", dt$Treatment)
dt$Placebo <- ifelse(is.na(dt$Placebo), "", dt$Placebo)
dt$se <- (log(dt$hi) - log(dt$est))/1.96

# Add blank column for the forest plot to display CI.
# Adjust the column width with space. 
dt$` ` <- paste(rep(" ", 20), collapse = " ")

# Create confidence interval column to display
dt$`HR (95% CI)` <- ifelse(is.na(dt$se), "",
                             sprintf("%.2f (%.2f to %.2f)",
                                     dt$est, dt$low, dt$hi))
head(dt)

## ----simple-plot, out.width="80%", fig.width  = 8, fig.height = 6.5-----------
p <- forest(dt[,c(1:3, 8:9)],
            est = dt$est,
            lower = dt$low, 
            upper = dt$hi,
            sizes = dt$se,
            ci_column = 4,
            ref_line = 1,
            arrow_lab = c("Placebo Better", "Treatment Better"),
            xlim = c(0, 4),
            ticks_at = c(0.5, 1, 2, 3),
            footnote = "This is the demo data. Please feel free to change\nanything you want.")

# Print plot
plot(p)


## ----simple-plot-theme, out.width="80%", fig.width  = 7, fig.height = 6-------
dt_tmp <- rbind(dt[-1, ], dt[1, ])
dt_tmp[nrow(dt_tmp), 1] <- "Overall"
dt_tmp <- dt_tmp[1:11, ]
# Define theme

tm <- forest_theme(base_size = 10,
                   # Confidence interval point shape, line type/color/width
                   ci_pch = 15,
                   ci_col = "#762a83",
                   ci_fill = "black",
                   ci_alpha = 0.8,
                   ci_lty = 1,
                   ci_lwd = 1.5,
                   ci_Theight = 0.2, # Set an T end at the end of CI 
                   # Reference line width/type/color
                   refline_lwd = 1,
                   refline_lty = "dashed",
                   refline_col = "grey20",
                   # Vertical line width/type/color
                   vertline_lwd = 1,
                   vertline_lty = "dashed",
                   vertline_col = "grey20",
                   # Change summary color for filling and borders
                   summary_fill = "#4575b4",
                   summary_col = "#4575b4",
                   # Footnote font size/face/color
                   footnote_cex = 0.6,
                   footnote_fontface = "italic",
                   footnote_col = "blue")


pt <- forest(dt_tmp[,c(1:3, 8:9)],
            est = dt_tmp$est,
            lower = dt_tmp$low, 
            upper = dt_tmp$hi,
            sizes = dt_tmp$se,
            is_summary = c(rep(FALSE, nrow(dt_tmp)-1), TRUE),
            ci_column = 4,
            ref_line = 1,
            arrow_lab = c("Placebo Better", "Treatment Better"),
            xlim = c(0, 4),
            ticks_at = c(0.5, 1, 2, 3),
            footnote = "This is the demo data. Please feel free to change\nanything you want.",
            theme = tm)

# Print plot
plot(pt)


## ----edit-plot, out.width="80%", fig.width  = 8, fig.height = 7.5-------------
# Change text color in row 3
g <- edit_plot(p, row = 3, gp = gpar(col = "red", fontface = "italic"))

# Change color of the CI
g <- edit_plot(g,
               row = c(3, 6, 11, 13),
               col = 4,
               which = "ci",
               gp = gpar(col = "green"))

# Bold grouping text
g <- edit_plot(g,
               row = c(2, 5, 10, 13, 17, 20),
               gp = gpar(fontface = "bold"))



# Edit background of row 5
g <- edit_plot(g, row = 5, which = "background",
               gp = gpar(fill = "darkolivegreen1"))

# Insert text at top
g <- insert_text(g,
                 text = "Treatment group",
                 col = 2:3,
                 part = "header",
                 gp = gpar(fontface = "bold"))

# Add underline at the bottom of the header
g <- add_underline(g, part = "header")


# Insert text
g <- insert_text(g,
                 text = "This is a long text. Age and gender summarised above.\nBMI is next",
                 row = 10,
                 just = "left",
                 gp = gpar(cex = 0.6, col = "green", fontface = "italic"))

plot(g)


## ----text-justification, out.width="80%", fig.width  = 7, fig.height = 3------
dt <- dt[1:4, ]

# Header center and content right
tm <- forest_theme(core=list(fg_params=list(hjust = 1, x = 0.9),
                             bg_params=list(fill = c("#edf8e9", "#c7e9c0", "#a1d99b"))),
                   colhead=list(fg_params=list(hjust=0.5, x=0.5)))

p <- forest(dt[,c(1:3, 8:9)],
            est = dt$est,
            lower = dt$low, 
            upper = dt$hi,
            sizes = dt$se,
            ci_column = 4,
            title = "Header center content right",
            theme = tm)

# Print plot
plot(p)

# Mixed justification
tm <- forest_theme(core=list(fg_params=list(hjust=c(1, 0, 0, 0.5),
                                            x=c(0.9, 0.1, 0, 0.5)),
                             bg_params=list(fill = c("#f6eff7", "#d0d1e6", "#a6bddb", "#67a9cf"))),
                   colhead=list(fg_params=list(hjust=c(1, 0, 0, 0, 0.5),
                                               x=c(0.9, 0.1, 0, 0, 0.5))))

p <- forest(dt[,c(1:3, 8:9)],
            est = dt$est,
            lower = dt$low, 
            upper = dt$hi,
            sizes = dt$se,
            ci_column = 4,
            title = "Mixed justification",
            theme = tm)
plot(p)


## ----multiple-group, out.width="80%", fig.width  = 8, fig.height = 8----------
dt <- read.csv(system.file("extdata", "example_data.csv", package = "forestploter"))
# indent the subgroup if there is a number in the placebo column
dt$Subgroup <- ifelse(is.na(dt$Placebo), 
                      dt$Subgroup,
                      paste0("   ", dt$Subgroup))

# NA to blank or NA will be transformed to carachter.
dt$n1 <- ifelse(is.na(dt$Treatment), "", dt$Treatment)
dt$n2 <- ifelse(is.na(dt$Placebo), "", dt$Placebo)

# Add two blank column for CI
dt$`CVD outcome` <- paste(rep(" ", 20), collapse = " ")
dt$`COPD outcome` <- paste(rep(" ", 20), collapse = " ")

# Set-up theme
tm <- forest_theme(base_size = 10,
                   refline_lty = "solid",
                   ci_pch = c(15, 18),
                   ci_col = c("#377eb8", "#4daf4a"),
                   footnote_col = "blue",
                   legend_name = "Group",
                   legend_value = c("Trt 1", "Trt 2"),
                   vertline_lty = c("dashed", "dotted"),
                   vertline_col = c("#d6604d", "#bababa"))

p <- forest(dt[,c(1, 19, 21, 20, 22)],
            est = list(dt$est_gp1,
                       dt$est_gp2,
                       dt$est_gp3,
                       dt$est_gp4),
            lower = list(dt$low_gp1,
                         dt$low_gp2,
                         dt$low_gp3,
                         dt$low_gp4), 
            upper = list(dt$hi_gp1,
                         dt$hi_gp2,
                         dt$hi_gp3,
                         dt$hi_gp4),
            ci_column = c(3, 5),
            ref_line = 1,
            vert_line = c(0.5, 2),
            nudge_y = 0.2,
            theme = tm)

plot(p)

## ----multiple-param, out.width="70%", fig.width  = 10, fig.height = 8---------

dt$`HR (95% CI)` <- ifelse(is.na(dt$est_gp1), "",
                             sprintf("%.2f (%.2f to %.2f)",
                                     dt$est_gp1, dt$low_gp1, dt$hi_gp1))
dt$`Beta (95% CI)` <- ifelse(is.na(dt$est_gp2), "",
                             sprintf("%.2f (%.2f to %.2f)",
                                     dt$est_gp2, dt$low_gp2, dt$hi_gp2))

tm <- forest_theme(arrow_type = "closed",
                   arrow_label_just = "end")

p <- forest(dt[,c(1, 21, 23, 22, 24)],
            est = list(dt$est_gp1,
                       dt$est_gp2),
            lower = list(dt$low_gp1,
                         dt$low_gp2), 
            upper = list(dt$hi_gp1,
                         dt$hi_gp2),
            ci_column = c(2, 4),
            ref_line = c(1, 0),
            vert_line = list(c(0.3, 1.4), c(0.6, 2)),
            x_trans = c("log", "none"),
            arrow_lab = list(c("L1", "R1"), c("L2", "R2")),
            xlim = list(c(0, 3), c(-1, 3)),
            ticks_at = list(c(0.1, 0.5, 1, 2.5), c(-1, 0, 2)),
            xlab = c("OR", "Beta"),
            nudge_y = 0.2,
            theme = tm)

plot(p)

## ----eval=FALSE---------------------------------------------------------------
#  # Base method
#  png('rplot.png', res = 300, width = 7.5, height = 7.5, units = "in")
#  p
#  dev.off()
#  
#  # ggsave function
#  ggplot2::ggsave(filename = "rplot.png", plot = p,
#                  dpi = 300,
#                  width = 7.5, height = 7.5, units = "in")

## ----eval=FALSE---------------------------------------------------------------
#  # Get width and height
#  p_wh <- get_wh(plot = p, unit = "in")
#  png('rplot.png', res = 300, width = p_wh[1], height = p_wh[2], units = "in")
#  p
#  dev.off()
#  
#  # Or get scale
#  get_scale <- function(plot,
#                        width_wanted,
#                        height_wanted,
#                        unit = "in"){
#    h <- convertHeight(sum(plot$heights), unit, TRUE)
#    w <- convertWidth(sum(plot$widths), unit, TRUE)
#    max(c(w/width_wanted,  h/height_wanted))
#  }
#  p_sc <- get_scale(plot = p, width_wanted = 6, height_wanted = 4, unit = "in")
#  ggplot2::ggsave(filename = "rplot.png",
#                  plot = p,
#                  dpi = 300,
#                  width = 6,
#                  height = 4,
#                  units = "in",
#                  scale = p_sc)

