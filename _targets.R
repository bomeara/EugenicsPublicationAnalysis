library(targets)
library(crew)

tar_option_set(
	packages = c("ggplot2", "tidyverse", "readxl", "magrittr", "tarchetypes", "stringr", "igraph", "ggplot2", "viridis", "tidygraph", "ggraph","ngramr", "Matrix", "waffle", "gganimate", "animation", "gridExtra")
	#controller = crew_controller_local(workers = 8)
)

source("_functions.R")

# This for caching the raw data so the repo does not need to worry about sharing proprietary data from Web of Science.
prerun <- FALSE
if(prerun) {
	library(magrittr)
	library(tidyverse)
	library(readxl)
	all_papers_separated_raw <- load_files()
	saveRDS(all_papers_separated_raw, file="data/all_papers_separated.RDS")
}

list(
 # tar_target(all_papers_separated, load_files()), # If I had access to the raw data, this would be the target.
  tar_target(all_papers_separated, readRDS("data/all_papers_separated.RDS")), #Since I don't have access to the raw data in this public repo, this is the target.
  tar_target(all_papers, merge_eugenics_journals(all_papers_separated)),
  tar_target(array_of_all, populate_array(all_papers)),
  tar_target(array_of_counts, get_array_of_counts(array_of_all)),
  tar_target(array_of_papers, get_array_of_papers(array_of_all)),
  tar_target(array_omeara, array_of_counts[which(grepl('meara, bc', dimnames(array_of_counts)[[1]])),as.character(2001:2023),]),
  tar_target(array_fisher_ra, array_of_counts[which(grepl('fisher, ra', dimnames(array_of_counts)[[1]])),,]),
  tar_target(author_connections, get_authors_across_journals(array_of_counts)),
  tar_target(authors_per_journal, get_unique_authors_per_journal(array_of_papers)),
  tar_target(authors_multiple_per_journal, get_count_of_authors_publishing_at_least_twice_in_same_journal(array_of_counts)),
  tar_target(pairwise_author_matrix, get_pairwise_author_matrix(author_connections, authors_per_journal, authors_multiple_per_journal)),
  tar_target(pairwise_author_matrices_1946_1968, get_pairwise_author_matrix_year_window(all_papers, min_year=1946, max_year=1968)),
  tar_target(pairwise_author_matrices_1946_1955, get_pairwise_author_matrix_year_window(all_papers, min_year=1946, max_year=1955)),
  tar_target(pairwise_author_matrices_1956_1965, get_pairwise_author_matrix_year_window(all_papers, min_year=1956, max_year=1966)),

  tar_target(heatmap_1946_1968, make_heatmap(pairwise_author_matrices_1946_1968$pairwise_author_matrix_percentage)),
  tar_target(heatmap_1946_1955, make_heatmap(pairwise_author_matrices_1946_1955$pairwise_author_matrix_percentage)),

  tar_target(heatmap_1956_1964, make_heatmap(pairwise_author_matrices_1956_1965$pairwise_author_matrix_percentage)),

  tar_target(heatmap_1946_1968_counts, make_heatmap(pairwise_author_matrices_1946_1968$pairwise_author_matrix_counts)),

  tar_target(author_counts_per_journal_per_year, get_authors_per_journal_per_year(array_of_counts)),
  tar_target(author_counts_plot, plot_author_counts_per_journal_per_year(author_counts_per_journal_per_year)),
  tar_target(all_papers_1946_1968, subset_years(all_papers, min_year=1946, max_year=1968)),
  tar_target(array_of_counts_1946_1968, get_array_of_counts(populate_array(all_papers_1946_1968))),
  tar_target(author_counts_per_journal_per_year_1946_1968, get_authors_per_journal_per_year(array_of_counts_1946_1968)),
  tar_target(author_counts_plot_1946_1968, plot_author_counts_per_journal_per_year(author_counts_per_journal_per_year_1946_1968)),
  tar_target(coauthor_array_two_plus_papers, get_coauthor_array(all_papers)),
  #tar_target(data_for_waffles_1946_2022, get_data_for_animated_waffles(all_papers, min_year=1946, max_year=2022)),
  tar_target(data_for_waffles_1946_1968, get_data_for_animated_waffles(all_papers, min_year=1946, max_year=1968)),
  #tar_target(data_for_waffles_1946_2000, get_data_for_animated_waffles(all_papers, min_year=1946, max_year=2000)),

  #tar_target(amnat_waffle, make_waffle_animated_plots(data_for_waffles_1946_1968, focal_journal="American Naturalist")),
  tar_target(waffle_plot, make_waffle_animated_plot_grid(data_for_waffles_1946_1968)),
  #tar_target(waffle_plot_long, make_waffle_animated_plot_grid(data_for_waffles_1946_2000, file_name="waffle_plot_long.gif")),

  tar_target(eugenics_authors, authors_eugenics(array_of_counts)),
  tar_target(presidents, c(get_ssb_presidents(), get_asn_presidents(), get_sse_presidents())),
  tar_target(coauthor_network,create_igraph_network(coauthor_array_two_plus_papers, presidents=presidents, eugenics_authors=eugenics_authors)),
  tar_target(network_animation, plot_interesting_network4(coauthor_network)),
  tar_target(eugenics_ngram_plot, plot_ngram_counts(c("eugenics", "systematics", "phylogeny"))),
  tar_target(summary_of_journals, CreateSummaryTable(all_papers, authors_per_journal)),
  tar_target(network_fixed_all_years, plot_interesting_network_all_years(coauthor_network))


  )

# maybe do by time bin