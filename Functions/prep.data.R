## Function for preparing the data
#@param level which level to prepare it for (0 is the whole tree)
#@param lvl.inc which levels to include (e.g. 1 and 2 for the super orders and the orders)
#@param clades the clade levels list
#@param space the space
#@param dim the dimensions to include
#@param consensus the consensus tree
#@param trees the list of trees
#@param min the minimum of species to include by level (default is 15)
prep.data <- function(level, lvl.inc, clades, space, dim, consensus, trees, min = 15, verbose) {

  ## Selecting the right level(s)
  if(level == 0) {
    list_selected <- list(rownames(space))
  } else {
    ## Get the correct levels
    select_levels <- levels(clades[, level])
    ## Remove the empty levels
    if(any(empty <- select_levels == "")) {
      select_levels <- select_levels[-empty]
    }
    ## Separate the data by level
    get.levels <- function(one_level, level, clades) {
      rownames(clades)[which(clades[, level] == one_level)]
    }
    list_selected <- lapply(as.list(select_levels), get.levels, level = level, clades = clades)
    names(list_selected) <- select_levels
    ## Remove elements 
    if(length(to_small <- which(unlist(lapply(list_selected, length)) <= min)) > 0) {
      list_selected <- list_selected[-to_small]
    }
  }

  ## Match the data and the trees
  match.data.trees <- function(species, tree, trees, verbose) {
    if(verbose) message(".", appendLF = FALSE)
    included <- tree$tip.label %in% species
    tree_out <- drop.tip(tree, tip = tree$tip.label[!included])
    trees_out <- lapply(trees_list, drop.tip, tip = tree$tip.label[!included])
    class(trees_out) <- "multiPhylo"
    return(list(species = tree_out$tip.label, tree = tree_out, trees = trees_out))
  }

  ## Get all the trees
  if(verbose) message("Separating the trees:", appendLF = FALSE)
  all_species_trees <- lapply(list_selected, match.data.trees, consensus, trees, verbose)
  if(verbose) message("Done.", appendLF = TRUE)

  ## Finding clades by levels
  get.level.list <- function(one_list, clades, lvl.inc, min, verbose) {

    ## Separate the data by levels
    get.levels <- function(data, min) {      
      ## Separate the data by levels
      levels_list <- lapply(as.list(levels(data[,1])), function(level, data) which(data == level), data = data)
      ## Add the species names
      levels_list <- lapply(levels_list, function(one_level, data) {names(one_level) <- rownames(data)[one_level]; return(one_level)}, data = data)
      names(levels_list) <- levels(data[,1])

      ## Remove "" clade
      if(any(empty <- which(names(levels_list) == ""))) {
        levels_list <- levels_list[-empty]
      }

      ## Remove lists to small
      if(length(to_small <- which(unlist(lapply(levels_list, length)) < min)) > 0) {
        levels_list <- levels_list[-to_small]
      }
      return(levels_list)
    }

    if(verbose) message(".", appendLF = FALSE)
    levels_out <- list()
    for(i in 1:length(lvl.inc)) {
      ## Get the levels for this set of species
      data <- clades[one_list$species, lvl.inc[i], drop = FALSE]
      levels_out[[i]] <- get.levels(data, min)
    }
    return(levels_out)
  }

  ## Getting all the clades levels
  if(verbose) message("Preparing the levels:", appendLF = FALSE)
  levels_list <- lapply(all_species_trees, get.level.list, clades, lvl.inc, min, verbose)
  if(verbose) message("Done.", appendLF = TRUE)

  ## Preparing the traitspaces
  get.traitspace <- function(species, levels, dim, space, verbose){
    if(verbose) message(".", appendLF = FALSE)
    ## Getting the subspace
    sub_space <- as.data.frame(space[species$species, dim])
    
    ## Adding the levels
    level_columns <- replicate(length(levels), rep("", nrow(sub_space)))
    rownames(level_columns) <- rownames(sub_space)
    
    ## Filling in the levels
    for(i in 1:length(levels)) {
      go_through_levels <- levels[[i]]
      while(length(go_through_levels) != 0) {
        level_columns[names(go_through_levels[[1]]), i] <- names(go_through_levels)[[1]]
        go_through_levels <- go_through_levels[-1]
      }
      ## Adding the level columns
      sub_space[, ncol(sub_space)+1] <- as.factor(level_columns[,i])
      colnames(sub_space)[ncol(sub_space)] <- paste0("level", i)
    }

    ## Adding the animal column
    sub_space[, ncol(sub_space)+1] <- rownames(sub_space)
    colnames(sub_space)[ncol(sub_space)] <- "animal"

    return(sub_space)
  }

  ## Getting all the trait spaces
  if(verbose) message("Preparing the spaces:", appendLF = FALSE)
  all_trait_spaces <- mapply(get.traitspace, all_species_trees, levels_list, MoreArgs = list(dim = dim, space = space, verbose = verbose), SIMPLIFY = FALSE)
  if(verbose) message("Done.", appendLF = TRUE)

  ## Return the list of datasets
  combine.everything <- function(space, trees, groups, dim) {
    return(list("space"          = space,
                "consensus_tree" = trees$tree,
                "trees_list"     = trees$trees,
                "levels"         = groups,
                "dimensions"     = dim))
  }

  return(mapply(combine.everything,
                    space    = all_trait_spaces,
                    trees    = all_species_trees,
                    groups   = levels_list,
                    MoreArgs = list(dim = dim),
                    SIMPLIFY = FALSE))
}