## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  dpi = 300,
  comment = "#>"
)

## ----prepare-data-------------------------------------------------------------
library(grid)
library(forestploter)

# Read provided sample example data
dt <- read.csv(system.file("extdata", "example_data.csv", package = "forestploter"))

# Keep needed columns
dt <- dt[, 1:6]

# Indent the subgroup if there is a number in the placebo column
dt$Subgroup <- ifelse(is.na(dt$Placebo), 
                      dt$Subgroup,
                      paste0("   ", dt$Subgroup))

# Replace NA with blank or NA will be transformed to character
dt$Treatment <- ifelse(is.na(dt$Treatment), "", dt$Treatment)
dt$Placebo <- ifelse(is.na(dt$Placebo), "", dt$Placebo)
dt$se <- (log(dt$hi) - log(dt$est)) / 1.96

# Add a blank column for the forest plot to display CI
# Adjust the column width with spaces; increase the number of spaces below 
# to provide a larger area for drawing the CI
dt$` ` <- paste(rep(" ", 20), collapse = " ")

# Create a confidence interval column to display
dt$`HR (95% CI)` <- ifelse(is.na(dt$se), "",
                             sprintf("%.2f (%.2f to %.2f)",
                                     dt$est, dt$low, dt$hi))
head(dt)

## ----simple-plot, out.width="80%", fig.width = 8, fig.height = 6--------------
p <- forest(dt[, c(1:3, 8:9)],
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

## ----simple-plot-theme, out.width="80%", fig.width = 7, fig.height = 3.3------
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
                   ci_Theight = 0.2, # Set a T end at the end of CI 
                   # Reference line width/type/color
                   refline_gp = gpar(lwd = 1, lty = "dashed", col = "grey20"),
                   # Vertical line width/type/color
                   vertline_lwd = 1,
                   vertline_lty = "dashed",
                   vertline_col = "grey20",
                   # Change summary color for filling and borders
                   summary_fill = "#4575b4",
                   summary_col = "#4575b4",
                   # Footnote font size/face/color
                   footnote_gp = gpar(cex = 0.6, fontface = "italic", col = "blue"))

pt <- forest(dt_tmp[, c(1:3, 8:9)],
             est = dt_tmp$est,
             lower = dt_tmp$low, 
             upper = dt_tmp$hi,
             sizes = dt_tmp$se,
             is_summary = c(rep(FALSE, nrow(dt_tmp) - 1), TRUE),
             ci_column = 4,
             ref_line = 1,
             arrow_lab = c("Placebo Better", "Treatment Better"),
             xlim = c(0, 4),
             ticks_at = c(0.5, 1, 2, 3),
             footnote = "This is the demo data. Please feel free to change\nanything you want.",
             theme = tm)

# Print plot
plot(pt)

## ----text-justification, out.width="80%", fig.width = 7, fig.height = 2-------
dt <- dt[1:4, ]

# Header center and content right
tm <- forest_theme(core = list(fg_params = list(hjust = 1, x = 0.9),
                               bg_params = list(fill = c("#edf8e9", "#c7e9c0", "#a1d99b"))),
                   colhead = list(fg_params = list(hjust = 0.5, x = 0.5)))

p <- forest(dt[, c(1:3, 8:9)],
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
tm <- forest_theme(core = list(fg_params = list(hjust = c(1, 0, 0, 0.5),
                                                x = c(0.9, 0.1, 0, 0.5)),
                               bg_params = list(fill = c("#f6eff7", "#d0d1e6", "#a6bddb", "#67a9cf"))),
                   colhead = list(fg_params = list(hjust = c(1, 0, 0, 0, 0.5),
                                                   x = c(0.9, 0.1, 0, 0, 0.5))))

p <- forest(dt[, c(1:3, 8:9)],
            est = dt$est,
            lower = dt$low, 
            upper = dt$hi,
            sizes = dt$se,
            ci_column = 4,
            title = "Mixed justification",
            theme = tm)
plot(p)

## ----text-parsing, out.width="80%", fig.width = 7, fig.height = 2-------------
# Check out the `plotmath` function for math expression.
dt <- data.frame(
  Study = c("Study ~1^a", "Study ~2^b", "NO[2]"),
  low = c(0.2, -0.03, 1.11),
  est = c(0.71, 0.35, 1.79),
  hi = c(1.22, 0.74, 2.47)
)

dt$SMD <- sprintf("%.2f (%.2f, %.2f)", dt$est, dt$low, dt$hi)
dt$` ` <- paste(rep(" ", 20), collapse = " ")

fig_dt <- dt[, c(1, 5:6)]

# Get a matrix of which row and columns to parse
parse_mat <- matrix(FALSE, 
                    nrow = nrow(fig_dt),
                    ncol = ncol(fig_dt))

# Here we want to parse the first column only, you can amend this to whatever you want.
parse_mat[, 1] <- TRUE  

# Remove this if you don't want to parse the column head.
tm <- forest_theme(colhead = list(fg_params = list(parse = TRUE)), 
                   core = list(fg_params = list(parse = parse_mat)))

p <- forest(fig_dt,
            est = dt$est,
            lower = dt$low,
            upper = dt$hi,
            ci_column = 3,
            theme = tm)

# Add customized footnote.
# Due to the limitation of the textGrob, passing a parsed text with linebreak 
# has some issues. We use a different approach here.
txt <- "<sup>a</sup> This is study A<br><sup>b</sup> This is study B"

add_grob(p, 
         row = 4, 
         col = 1:2,
         order = "background",
         gb_fn = gridtext::richtext_grob,
         text = txt,
         gp = gpar(fontsize = 8),
         hjust = 0, vjust = 1, halign = 0, valign = 1,
         x = unit(0, "npc"), y = unit(1, "npc"))

## ----multiple-group, out.width="80%", fig.width = 8, fig.height = 5-----------
dt <- read.csv(system.file("extdata", "example_data.csv", package = "forestploter"))
dt <- dt[1:7, ]
# Indent the subgroup if there is a number in the placebo column
dt$Subgroup <- ifelse(is.na(dt$Placebo), 
                      dt$Subgroup,
                      paste0("   ", dt$Subgroup))

# Replace NA with blank or NA will be transformed to character
dt$n1 <- ifelse(is.na(dt$Treatment), "", dt$Treatment)
dt$n2 <- ifelse(is.na(dt$Placebo), "", dt$Placebo)

# Add two blank columns for CI
dt$`CVD outcome` <- paste(rep(" ", 20), collapse = " ")
dt$`COPD outcome` <- paste(rep(" ", 20), collapse = " ")

# Generate point estimation and 95% CI. Paste two CIs together and separate by line break.
dt$ci1 <- paste(sprintf("%.1f (%.1f, %.1f)", dt$est_gp1, dt$low_gp1, dt$hi_gp1),
                sprintf("%.1f (%.1f, %.1f)", dt$est_gp3, dt$low_gp3, dt$hi_gp3),
                sep = "\n")
dt$ci1[grepl("NA", dt$ci1)] <- "" # Any NA to blank

dt$ci2 <- paste(sprintf("%.1f (%.1f, %.1f)", dt$est_gp2, dt$low_gp2, dt$hi_gp2),
                sprintf("%.1f (%.1f, %.1f)", dt$est_gp4, dt$low_gp4, dt$hi_gp4),
                sep = "\n")
dt$ci2[grepl("NA", dt$ci2)] <- ""

# Set-up theme
tm <- forest_theme(base_size = 10,
                   refline_lty = "solid",
                   ci_pch = c(15, 18),
                   ci_col = c("#377eb8", "#4daf4a"),
                   footnote_gp = gpar(col = "blue"),
                   legend_name = "Group",
                   legend_value = c("Trt 1", "Trt 2"),
                   vertline_lty = c("dashed", "dotted"),
                   vertline_col = c("#d6604d", "#bababa"),
                   # Table cell padding, width 4 and heights 3
                   core = list(padding = unit(c(4, 3), "mm")))

p <- forest(dt[, c(1, 19, 23, 21, 20, 24, 22)],
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
            ci_column = c(4, 7),
            ref_line = 1,
            vert_line = c(0.5, 2),
            nudge_y = 0.4,
            theme = tm)

plot(p)

## ----multiple-param, out.width="70%", fig.width = 10, fig.height = 6.5--------
dt$`HR (95% CI)` <- ifelse(is.na(dt$est_gp1), "",
                             sprintf("%.2f (%.2f to %.2f)",
                                     dt$est_gp1, dt$low_gp1, dt$hi_gp1))
dt$`Beta (95% CI)` <- ifelse(is.na(dt$est_gp2), "",
                             sprintf("%.2f (%.2f to %.2f)",
                                     dt$est_gp2, dt$low_gp2, dt$hi_gp2))

tm <- forest_theme(arrow_type = "closed",
                   arrow_label_just = "end")

p <- forest(dt[, c(1, 21, 23, 22, 24)],
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

## ----custom-ci, out.width="70%", fig.width = 3, fig.height = 3----------------
# Function to calculate Box plot values
box_func <- function(x){
  iqr <- IQR(x)
  q3 <- quantile(x, probs = c(0.25, 0.5, 0.75), names = FALSE)
  c("min" = q3[1] - 1.5 * iqr, "q1" = q3[1], "med" = q3[2],
    "q3" = q3[3], "max" = q3[3] + 1.5 * iqr)
}
# Prepare data
val <- split(ToothGrowth$len, list(ToothGrowth$supp, ToothGrowth$dose))
val <- lapply(val, box_func)

dat <- do.call(rbind, val)
dat <- data.frame(Dose = row.names(dat),
                  dat, row.names = NULL)

dat$Box <- paste(rep(" ", 20), collapse = " ")

# Draw a single group box plot
tm <- forest_theme(ci_Theight = 0.2)

p <- forest(dat[, c(1, 7)],
            est = dat$med,
            lower = dat$min,
            upper = dat$max,
            # sizes = sizes,
            fn_ci = make_boxplot,
            ci_column = 2,
            lowhinge = dat$q1, 
            uphinge = dat$q3,
            hinge_height = 0.2,
            # values of the lowhinge and uphinge will be used as row values
            index_args = c("lowhinge", "uphinge"), 
            gp_box = gpar(fill = "black", alpha = 0.4),
            theme = tm
)
p

## ----eval=FALSE---------------------------------------------------------------
# # Base method
# png('rplot.png', res = 300, width = 7.5, height = 7.5, units = "in")
# p
# dev.off()
# 
# # ggsave function
# ggplot2::ggsave(filename = "rplot.png", plot = p,
#                 dpi = 300,
#                 width = 7.5, height = 7.5, units = "in")

## ----eval=FALSE---------------------------------------------------------------
# # Get width and height
# p_wh <- get_wh(plot = p, unit = "in")
# png('rplot.png', res = 300, width = p_wh[1], height = p_wh[2], units = "in")
# p
# dev.off()
# 
# # Or get scale
# get_scale <- function(plot,
#                       width_wanted,
#                       height_wanted,
#                       unit = "in"){
#   h <- convertHeight(sum(plot$heights), unit, TRUE)
#   w <- convertWidth(sum(plot$widths), unit, TRUE)
#   max(c(w / width_wanted,  h / height_wanted))
# }
# p_sc <- get_scale(plot = p, width_wanted = 6, height_wanted = 4, unit = "in")
# ggplot2::ggsave(filename = "rplot.png",
#                 plot = p,
#                 dpi = 300,
#                 width = 6,
#                 height = 4,
#                 units = "in",
#                 scale = p_sc)

