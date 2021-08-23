# #' @title Plot analyses tips
# #'
# #' @description Plotting the results of the analyses per tips
# #'
# #' @param results the results of an analyses per group (typically from \code{\link{analyses.group}}).
# #' @param what which results to plot. By default everything is plotted (e.g. \code{c("projections", "rejections")}).
# #' @param col a colour vector (for the levels).
# #' @param group optional, either a list factors for grouping the data or a phylogenetic tree.
# #' @param ... any plotting options to be passed to \code{\link{graphics}{boxplot}}

# #' 
# #' @examples
# #'
# #' @seealso
# #' 
# #' @author Thomas Guillerme
# #' @export
# plot.analyses.tip <- function(results, what, col, group, ...) {
    
#     ## Get the graphical options
#     box_args <- plot.dots(...)

#     ## Grouping
#     if(missing(group)) {
#         group <- list(rownames(results[[1]]))
#         names(group) <- "data"
#     }

#     ## Missing colours
#     if(missing(col)) {
#         if(length(group) == 1) {
#             col <- "white"    
#         } else {
#             col <- grDevices::rainbow(length(group))
#         }
#     }
#     box_args$col <- col

#     ## Missing what
#     if(missing(what)) {
#         what <- names(results)
#     }

#     ## Transform the results into a table for boxplotting
#     table <- list()
#     for(one_result in 1:length(what)) {
#         table[[one_result]] <- lapply(group, function(one_group, one_result) c(one_result[one_group, ]), one_result = results[what][[one_result]])
#         names(table[[one_result]]) <- names(group)
#     }

#     ## Plot the results
#     par(mfrow = c(length(what),1))
#     for(one_stat in 1:length(what)) {
#         ## Setting the box arguments
#         one_box <- box_args
#         one_box$ylab <- what[one_stat]

#         ## Setting the box data
#         selected_data <- table[[one_stat]]
#         one_box$x <- selected_data

#         ## Removing the labels
#         if(one_stat != length(what)) {
#             one_box$xaxt <- "n"
#         }
#         ## Removing the title
#         if(one_stat != 1) {
#             one_box$main <- NULL
#         }

#         ## Plotting the box
#         do.call(boxplot, one_box)
#     }
#     return(invisible())
# }


# # install.packages(c("phyloseq", "dplyr"))
# # install_github("YuLab-SMU/ggtreeExtra")
# # install_github("YuLab-SMU/ggtree")
# # library(phyloseq)
# # library(dplyr)

# # data("GlobalPatterns")
# # GP <- GlobalPatterns
# # GP <- prune_taxa(taxa_sums(GP) > 600, GP)
# # sample_data(GP)$human <- get_variable(GP, "SampleType") %in%
# #                               c("Feces", "Skin")
# # mergedGP <- merge_samples(GP, "SampleType")
# # mergedGP <- rarefy_even_depth(mergedGP,rngseed=394582)
# # mergedGP <- tax_glom(mergedGP,"Order")

# # melt_simple <- psmelt(mergedGP) %>%
# #                filter(Abundance < 120) %>%
# #                select(OTU, val=Abundance)

# # p <- ggtree(mergedGP, layout="fan", open.angle=10) + 
# #      geom_tippoint(mapping=aes(color=Phylum), 
# #                    size=1.5,
# #                    show.legend=FALSE)
# # p <- rotate_tree(p, -90)

# # p <- p +
# #      geom_fruit(
# #          data=melt_simple,
# #          geom=geom_boxplot,
# #          mapping = aes(
# #                      y=OTU,
# #                      x=val,
# #                      group=label,
# #                      fill=Phylum,
# #                    ),
# #          size=.2,
# #          outlier.size=0.5,
# #          outlier.stroke=0.08,
# #          outlier.shape=21,
# #          axis.params=list(
# #                          axis       = "x",
# #                          text.size  = 1.8,
# #                          hjust      = 1,
# #                          vjust      = 0.5,
# #                          nbreak     = 3,
# #                      ),
# #          grid.params=list()
# #      ) 
     
# # p <- p +
# #      scale_fill_discrete(
# #          name="Phyla",
# #          guide=guide_legend(keywidth=0.8, keyheight=0.8, ncol=1)
# #      ) +
# #      theme(
# #          legend.title=element_text(size=9), # The title of legend 
# #          legend.text=element_text(size=7) # The label text of legend, the sizes should be adjust with dpi.
# #      )
# # p
# # http://yulab-smu.top/treedata-book/chapter10.html