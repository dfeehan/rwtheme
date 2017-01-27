
## TODO - Sibling (1 yr) and Sibling (5 yr) colors are
##        currently too similar to one another...


#' tie definition color scale
#'
#' @rdname scale_tiedefn
#' @export
scale_tiedefn_values <- function() {

    ## see http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
    cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                    "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

    values <- c("Acquaintance"=cbbPalette[4],
                "Meal"=cbbPalette[6],
                "Blended"=cbbPalette[8],
                "Sibling"=cbbPalette[2],
                ## NB: by default, Sibling is
                ## the same as Sibling (5 yr),
                ## so they should have the same
                ## color
                "Sibling (5 yr)"=cbbPalette[2],
                "Sibling (1 yr)"=cbbPalette[5])

    return(values)

}


#' tie definition color scale
#'
#' @rdname scale_tiedefn
#' @export
scale_color_tiedefn <- function(...) {

    scale_colour_manual(name="tie\ndefinition", 
                        values=scale_tiedefn_values(), 
                        ...)
}

#' tie definition linetype scale
#'
#' @rdname scale_tiedefn
#' @export
scale_linetype_tiedefn <- function(...) {

    scale_linetype_manual(name="tie\ndefinition", 
                          values=c("Acquaintance"="dashed",
                                   "Meal"="solid",
                                   "Sibling"="dotted"),
                          ...)
}

#' tie definition color scale
#'
#' @rdname scale_tiedefn
#' @export
scale_fill_tiedefn <- function(...) {

    scale_fill_manual(name="tie\ndefinition", 
                      values=scale_tiedefn_values(), 
                      ...)
}

#' sex color scale
#'
#' @rdname scale_sex
#' @export
scale_sex_values <- function() {

    ## see http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
    cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                    "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

    values <- c("Male"=cbbPalette[3],
                "male"=cbbPalette[3],
                "Female"=cbbPalette[7],
                "female"=cbbPalette[7])

    return(values)

}

#' sex color scale
#'
#' @rdname scale_sex
#' @export
scale_color_sex <- function(...) {

    scale_colour_manual(name="sex", 
                        values=scale_sex_values(), 
                        ...)
}

#' sex color scale
#'
#' @rdname scale_sex
#' @export
scale_fill_sex <- function(...) {

    scale_fill_manual(name="sex", 
                      values=scale_sex_values(), 
                      ...)
}

#' theme for Rwanda plots
#'
#'
#' see the source for ggplot2::theme_bw() to see how we're doing this
#' @export
theme_rwanda <- function(base_size = 12, base_family="") {
    theme_bw(base_size = base_size, base_family=base_family) %+replace%
        theme(panel.grid.minor=element_blank())
}

#' this function is taken from this thread:
#' https://groups.google.com/forum/#!topic/ggplot2/a_xhMoQyxZ4
#'
#' it puts axis labels in scientific notation
#' @export
fancy_scientific <- function(l) { 
    # turn in to character string in scientific notation 
    l <- format(l, scientific = TRUE) 
    # quote the part before the exponent to keep all the digits 
    l <- gsub("^(.*)e", "'\\1'e", l) 

    # remove the + in the exponent, if there is one
    l <- gsub("\\+", "", l)

    # turn the 'e+' into plotmath format 
    l <- gsub("e", "%*%10^", l) 
    # return this as an expression 
    parse(text=l) 
}



## rwanda_se_theme <- function(base_size=12, base_family="",...) {
##   modifyList(theme_bw(base_size=base_size, base_family=base_family),
##              list( panel.grid.major = theme_blank(),
##                    panel.grid.minor = theme_blank()))
## }

## see https://github.com/hadley/ggplot2/wiki/Themes
##theme_minimal_light <- function (base_size = 12, base_family = "", ...){
##  modifyList (theme_minimal (base_size = base_size, base_family = base_family),
##              list (axis.ticks = theme_segment (colour = "grey50"),
##                    axis.text.x = theme_text (colour = "grey33"),
##                    axis.text.y = theme_text (colour = "grey33")))
##}




