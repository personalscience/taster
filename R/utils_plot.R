
#' @title Personal Science theme
#' @description Use for ensuring a consistent look and feel in plots
#' @import ggplot2
#' @export
psi_theme <- function() {
  return(theme(text = element_text(# family = "Montserrat",
  face = "bold", size = 15),
  axis.text.x = element_text(size = 15, angle = 90, hjust = 1),
  legend.title = element_blank())
  )
}

#' @title Plot a glucose dataframe comparing several meals
#' @description Plot of a valid CGM file.
#' @param food_times_df dataframe of a valid dataframe in food_times_df format.
#' @param title string to display on ggplot
#' @param subtitle string to display for subtitle
#' @param smooth smooth lines (default = FALSE)
#' @param combine combine lines into one average (default = FALSE)
#' @param legend_var a food_times_df column name to be used for the plot legend
#' @import dplyr
#' @import ggplot2
#' @export
#' @return ggplot object
plot_compare_glucose <- function(food_times_df,
                                 combine = FALSE,
                                 smooth = FALSE,
                                 title = "Name",
                                 subtitle = "foodname",
                                 legend_var = "date_ch") {

  foods_to_show <- food_times_df

  g <- if(combine){
    foods_to_show %>% cgmr::combined_food_times_df() %>%
      ggplot() +
      geom_line(aes(x=`t`,y=`value`,color=.data[[{{legend_var }}]]), size = 1) +
      geom_smooth(inherit.aes= FALSE,
                  aes(x=t,y=ave),
                  method = "loess",
                  size = 4)

  } else {

    foods_to_show %>%
      ggplot(aes(x=`t`,y=`value`,color=.data[[{{legend_var }}]]))  +
      geom_point( size = 3) +
      if(smooth) geom_smooth(method = "loess", aes(fill=`date_ch`)) else geom_line(size=2)
  }

  g +
    psi_theme() +
    annotate(geom="rect",  # draw a light box around the portion of the graph during "expected experiment time"
             xmin=0,
             xmax=120, # we typical measure two hours out
             ymin=-Inf,
             ymax=Inf,
             color = "lightgrey",
             alpha=0.2) +
    labs(title = title, subtitle = subtitle,
         x = "minutes", y = "mg/dL")
}

#' @title Plot a glucose dataframe
#' @description Plot of a valid CGM file.
#' @param glucose_raw dataframe of a valid CGM data stream
#' @param title string to display on ggplot
#' @param subtitle string to display on ggplot
#' @import dplyr
#' @import ggplot2
#' @export
#' @return ggplot object
plot_glucose <- function(glucose_raw, title = "Name", subtitle = "") {

  g_df <- glucose_raw %>% filter(!is.na(value)) # remove NA values (usually because they're from a strip)

  earliest <- min(g_df[["time"]])
  latest <- max(g_df[["time"]])

  lowest <- min(g_df[["value"]])
  highest <- max(g_df[["value"]])

  #auc = cgmr::auc_calc(g_df,as.numeric(difftime(latest,earliest,units="mins")))
  g = ggplot(data = g_df, aes(x=time, y = value) )
  gp <- g +
    psi_theme() +
    geom_line(color = "red")  +
    geom_point(size = 2) +
    labs(title = title, x = "", y = "mg/mL",
         subtitle = subtitle)  +
    scaled_axis(g_df) +
    #scale_x_datetime(date_breaks = "1 day", date_labels = "%a %b-%d") +
    coord_cartesian(ylim = c(lowest, highest))

  return(gp)
}

#' Adjust x axis depending on time scale of glucose data frame
#' @param glucose_raw dataframe of a valid Libreview glucose table
#' @import ggplot2
#' @return scale_x_datetime object, to be added to glucose plot
scaled_axis <- function(glucose_raw) {
  time_length <- max(glucose_raw$time) - min(glucose_raw$time)
  if (as.numeric(time_length, units = "days") > 1)
    return({
      if (as.numeric(time_length, units = "weeks") > 4) {
        scale_x_datetime(date_breaks = "1 week", date_labels = "%a %b-%d", timezone = Sys.timezone())
      }
      else scale_x_datetime(date_breaks = "1 day", date_labels = "%a %b-%d", timezone = Sys.timezone())
    })
  else return(scale_x_datetime(date_breaks = "15 min", date_labels = "%b-%d %H:%M", timezone = Sys.timezone()))
}

