load_files <- function() {
	all_papers <- data.frame()
	input_files <- list.files(path = "data", pattern = "*.xls", full.names = TRUE)	
#	for (i in 1:3) {

	for (i in 1:length(input_files)) {
		print(input_files[i])
		local_file <- as.data.frame(readxl::read_excel(input_files[i]))
		file_name_no_path <- strsplit(input_files[i], "/")[[1]][2]
		local_file$Journal <- strsplit(file_name_no_path, "_")[[1]][1]
		local_file$SourceFileName <- file_name_no_path
		for (col_index in sequence(ncol(local_file))) {
			local_file[,col_index] <- as.character(local_file[,col_index])
		}
		all_papers <- dplyr::bind_rows(all_papers, local_file)
	}
	all_papers$`Journal Original` <- all_papers$Journal
	
	all_papers$Journal <- stringr::str_trim(gsub("  ", " ", gsub("([A-Z])", " \\1", all_papers$Journal, perl = TRUE)))
	all_papers$Journal <- gsub("Systematic Zoology", "Systematic Biology", all_papers$Journal)
	
	all_papers %<>% select(c(`Authors`, `Author Full Names`, `Article Title`, `Author Keywords`, `Keywords Plus`, `Abstract`, `Addresses`, `Affiliations`, `Email Addresses`, `Researcher Ids`, `ORCIDs`, `Cited References`, `Cited Reference Count`, `Times Cited, WoS Core`, `Times Cited, All Databases`, `Publication Date`, `Publication Year`, `Volume`, `Issue`, `Start Page`, `End Page`, `DOI`, `DOI Link`, `WoS Categories`, `Web of Science Index`, `Research Areas`, `Pubmed Id`, `UT (Unique WOS ID)`, `Web of Science Record`, `Journal`, `Journal Original`, `SourceFileName`))
	all_papers$Year <- as.numeric(all_papers$`Publication Year`)
	print("done loading")
	print(dim(all_papers))
	return( all_papers)
}

merge_eugenics_journals <- function(all_papers) {
	all_papers$Journal[grepl("eugenics", all_papers$Journal, ignore.case=TRUE)] <- "Eugenics journals"	
	return(all_papers)
}

# To simplify
prune_journals <- function(all_papers, journals_to_prune="Genetics|Ecology") {
	all_papers <- subset(all_papers, !grepl(journals_to_prune, all_papers$Journal, ignore.case=TRUE))
	return(all_papers)
}

subset_years <- function(all_papers, min_year=1900, max_year=2023) {
	all_papers <- subset(all_papers, Year >= min_year)
	all_papers <- subset(all_papers, Year <= max_year)
	return(all_papers)
}


# Note: use all_papers$Authors as the `Author Full Names` column is not always populated with full names
extract_authors_one_string <- function(name_string) {
	return(stringr::str_trim(strsplit(tolower(name_string), ";")[[1]]))
}

extract_authors_all_strings <- function(names_vector) {
	return(unname(sapply(names_vector, extract_authors_one_string)))
}

# Will get only those authors who have published at least twice
get_coauthor_array <- function(all_papers) {
	all_authors <- unlist(extract_authors_all_strings(all_papers$Authors))
	all_authors_counts <- table(all_authors)
	all_authors_with_multiple_papers <- names(all_authors_counts[all_authors_counts > 1])
	all_authors_with_multiple_papers <- all_authors_with_multiple_papers[!grepl("anonymous", all_authors_with_multiple_papers)]
	all_authors_with_multiple_papers <- all_authors_with_multiple_papers[!is.na(all_authors_with_multiple_papers)]
	
	coauthor_array <- Matrix(nrow=length(all_authors_with_multiple_papers), ncol=length(all_authors_with_multiple_papers), data=FALSE, sparse=TRUE)
	dimnames(coauthor_array) <- list(all_authors_with_multiple_papers, all_authors_with_multiple_papers)
	for(paper_index in sequence(nrow(all_papers))) {
		authors <- extract_authors_one_string(all_papers$Authors[paper_index])
		authors <- authors[!is.na(authors)]
		authors <- authors[authors %in% all_authors_with_multiple_papers]
		cat("  ", paper_index, "/", nrow(all_papers), " number authors: ", length(authors), '\r')
		for (author1 in authors) {
			for (author2 in authors) {
				coauthor_array[author1, author2] <- TRUE
			}
		}
	}
	return(coauthor_array)
}

populate_array <- function(all_papers) {
	all_authors <- sort(unique(unlist(extract_authors_all_strings(all_papers$Authors))))
	all_authors <- all_authors[!grepl("anonymous", all_authors)]
	all_years <- sort(unique(all_papers$`Publication Year`))
	all_journals <- sort(unique(all_papers$Journal))
	array_of_counts <- array(0, dim=c(length(all_authors), length(all_years), length(all_journals)))
	array_of_papers <- array("", dim=c(length(all_authors), length(all_years), length(all_journals)))
	dimnames(array_of_counts) <- list(all_authors, all_years, all_journals)
	dimnames(array_of_papers) <- list(all_authors, all_years, all_journals)
	for(paper_index in sequence(nrow(all_papers))) {
		cat("  ", paper_index, '\r')
		authors <- extract_authors_one_string(all_papers$Authors[paper_index])
		authors <- authors[!grepl("anonymous", authors)]
		authors <- authors[!is.na(authors)]

		year <- all_papers$`Publication Year`[paper_index]
		journal <- all_papers$Journal[paper_index]
		for (author in authors) {
			array_of_counts[author, year, journal] <- array_of_counts[author, year, journal] + 1
			array_of_papers[author, year, journal] <- stringr::str_trim(paste(array_of_papers[author, year, journal], paper_index, sep=" "))
		}
	
		
		cat(paper_index, '\r')
		
	}
	return(list(by_counts=array_of_counts, by_paper=array_of_papers))
}

authors_eugenics <- function(array_of_counts) {
	eugenics_journals <- array_of_counts[,,which(grepl('eugenics', dimnames(array_of_counts)[[3]], ignore.case = TRUE))]
	eugenics_totals <- rowSums(eugenics_journals)
	authors_anytime_eugenics <- names(eugenics_totals[eugenics_totals > 0])
	post_1945 <- which(as.numeric(dimnames(array_of_counts)[[2]]) > 1945)
	authors_after_1945_eugenics_counts <- rowSums(eugenics_journals[,post_1945])
	authors_after_1945_eugenics <- names(authors_after_1945_eugenics_counts[authors_after_1945_eugenics_counts > 0])
	return(list(anytime_eugenics=authors_anytime_eugenics, still_active_after_1945_eugenics=authors_after_1945_eugenics, only_less_than_equal_1945 = setdiff(authors_anytime_eugenics, authors_after_1945_eugenics)))
}

get_authors_across_journals <- function(array_of_counts) {
	edges_journals_df <- data.frame(from=character(), to=character(), eugenic_type=character())
	array_of_counts_summed_years <- apply(array_of_counts, c(1,3), sum)
	for(row_index in sequence(nrow(array_of_counts_summed_years))) {
		cat("  ", row_index, '\r')
		focal_row <- array_of_counts_summed_years[row_index,]
		focal_row <- subset(focal_row, focal_row > 0)	
		if(length(focal_row)>1) {
			# eugenic_status <- "none"
			# if(dimnames(array_of_counts_summed_years)[[1]][row_index] %in% eugenics_authors$still_active_after_1945_eugenics) {
			# 	eugenic_status <- "after_1945"
			# }
			# if(dimnames(array_of_counts_summed_years)[[1]][row_index] %in% eugenics_authors$only_less_than_equal_1945) {
			# 	eugenic_status <- "only_before_1945"
			# }
			combination_result <- combn(names(focal_row),2)
			edges_journals_df <- dplyr::bind_rows(edges_journals_df, data.frame(from=combination_result[1,], to=combination_result[2,], author=rownames(array_of_counts_summed_years)[row_index]))
		}
	}
	return(edges_journals_df)
}

get_array_of_counts <- function(array_of_all) {
	return(array_of_all$by_counts)
}

get_array_of_papers <- function(array_of_all) {
	return(array_of_all$by_paper)
}

# get_total_authors_per_journal <- function(array_of_papers) {
# 	counts <- rep(0, length(dimnames(array_of_papers)[[3]]))
# 	names(counts) <- dimnames(array_of_papers)[[3]]
# 	for(journal_index in sequence(length(counts))) {
# 		focal_journal <- array_of_papers[,,journal_index]
# 		some_papers <- which(focal_journal != "", arr.ind=TRUE)
# 		counts[journal_index] <- length(unique(some_papers[,1]))
# 	}
# 	return(counts)
	
# }

get_pairwise_author_matrix <- function(author_connections, authors_per_journal, authors_multiple_per_journal) {
	all_journals <- unique(c(author_connections$from, author_connections$to))
	matrix_of_authors <- matrix(0, nrow=length(all_journals), ncol=length(all_journals))
	dimnames(matrix_of_authors) <- list(all_journals, all_journals)
	for(row_index in sequence(nrow(author_connections))) {
		from <- author_connections$from[row_index]
		to <- author_connections$to[row_index]
		matrix_of_authors[from, to] <- matrix_of_authors[from, to] + 1
		matrix_of_authors[to, from] <- matrix_of_authors[to, from] + 1
	}
	for(journal_index in sequence(length(authors_multiple_per_journal))) {
		journal_name <- names(authors_multiple_per_journal)[journal_index]
		matrix_of_authors[journal_name, journal_name] <- authors_multiple_per_journal[journal_name]
	}
	return(matrix_of_authors)
}

get_pairwise_author_matrix_year_window <- function(all_papers, min_year, max_year) {
	#all_papers$Year <- as.numeric(all_papers$`Publication Year`)
	all_papers <- subset(all_papers, Year >= min_year & Year <= max_year)
	array_of_all <- populate_array(all_papers)
	array_of_counts <- get_array_of_counts(array_of_all)
	array_of_papers <- get_array_of_papers(array_of_all)
	authors_per_journal <- get_unique_authors_per_journal(array_of_papers)
	authors_multiple_per_journal <- get_count_of_authors_publishing_at_least_twice_in_same_journal(array_of_counts)
	author_connections <- get_authors_across_journals(array_of_counts)	
	pairwise_author_matrix_counts <- get_pairwise_author_matrix(author_connections, authors_per_journal, authors_multiple_per_journal)
	pairwise_author_matrix_percentage <- get_percentage_of_authors_publishing_in_other_journals(pairwise_author_matrix_counts, authors_per_journal)
	return(list(pairwise_author_matrix_counts=pairwise_author_matrix_counts, pairwise_author_matrix_percentage=pairwise_author_matrix_percentage))
}

get_percentage_of_authors_publishing_in_other_journals <- function(pairwise_author_matrix, authors_per_journal) {
	result <- pairwise_author_matrix
	for (journal in names(authors_per_journal)) {
		result[journal,] <- 100*pairwise_author_matrix[journal,]/authors_per_journal[journal]
	}	
	return(result)
}

get_count_of_authors_publishing_at_least_twice_in_same_journal <- function(array_of_counts) {
	all_journals <- rep(0, length(dimnames(array_of_counts)[[3]]))
	names(all_journals) <- dimnames(array_of_counts)[[3]]
	for(journal_index in sequence(length(all_journals))) {
		focal_journal <- array_of_counts[,,journal_index]
		some_papers <- which(focal_journal > 1, arr.ind=TRUE)
		all_journals[journal_index] <- length(unique(some_papers[,1]))
	}
	return(all_journals)
}

get_journals <- function() {
	return(c( "Systematic Biology", "American Naturalist", "Evolution", "Ecology", "Genetics", "Molecular Biology And Evolution", "Molecular Phylogenetics And Evolution", "Systematic Botany", "Trends In Ecology And Evolution" , "Copeia", "American Midland Naturalist", "Social Biology","Annals Of Human Genetics", "Biodemography And Social Biology", "Eugenics Quarterly", "Eugenics Review", "Annals of Eugenics", "Journal of Biosocial Science", "Eugenics journals"))
}

get_ssb_presidents <- function() {
	return(c(
		"baum, da",
		"o'meara, bc",
		"kubatko, ls",
		"edwards, ej",
		"ane, c",
		"magallon, sa",
		"harmon, lj",
		"yoder, ad",
		"lewis, po",
		"sites, jw",
		"knowles, ll",
		"sullivan, jm",
		"mindell, dp",
		"crandall, ka",
		"huelsenbeck, jp",
		"penny, d",
		"simon, c",
		"edwards, sv",
		"soltis, ps",
		"cannatella, dc",
		"sanderson, mj",
		"kellogg, ea",
		"de queiroz, k",
		"maddison, wp",
		"funk, va",
		"miyamoto, mm",
		"mabee, pm",
		"savage, jm",
		"donoghue, mj",
		"hillis, dm",
		"cracraft, j",
		"novacek, mj",
		"fink, wl",
		"powell, ja",
		"hoffman, rl",
		"hoffman, rl",
		"wiley, eo",
		"nelson, gj",
		"hull, dl",
		"hull, dl",
		"slater, ja",
		"slater, ja",
		"olson, ec",
		"olson, ec",
		"johnston, rf",
		"johnston, rf",
		"rosen, de",
		"rosen, de",
		"downs, t",
		"downs, t",
		"ross, hh",
		"newell, nd", 
		"inger, rf",
		"corliss, jo",
		"michener, cd",
		"illg, pl",
		"usinger, rl",
		"mayr, e",
		"smitch, hm",
		"pennak, rw",
		"simpson, gg",
		"sabrosky, cw",
		"blackwelder, re",
		"follett, wi",
		"hyman, lh",
		"emerson, ae",
		"moore, rc",
		"wharton, gw",
		"klauber, lm",
		"stunkard, hw",
		"hungerford, hb",
		"romer, as",
		"hubbs, cl",
		"petrunkevitch, a",
		"schmidt, wl",
		"schmidt, wl"
	))	
}

get_asn_presidents <- function() {
	return(c(
		"servedio, mr",
		"bronstein, jl",
		"brodie, ed",
		"kalisz, s",
		"whitlock, mc",
		"strauss, sy",
		"donohue, k",
		"mcpeek, ma",
		"ketterson, ed",
		"price, td",
		"schluter, d",
		"arnold, sj",
		"ricklefs, re",
		"losos, jb",
		"kingsolver, jg",
		"thompson, jn",
		"holt, rd",
		"power, me",
		"travis, j",
		"grossberg, rk",
		"simberloff, d",
		"schmitt, j",
		"thomson, jd",
		"polis, ga",
		"grant, pr",
		"colwell, rk",
		"vermeij, gj",
		"wilbur, hm",
		"slatkin, m",
		"futuyma, dj",
		"huey, rb",
		"hendrick, pw",
		"brown, jh",
		"ehrman, l",
		"wake, db",
		"eisner, t",
		"anderson, ww",
		"antonovics, j",
		"slobodkin, lb",
		"sokal, rr",
		"raven, ph",
		"lewontin, rc",
		"spiess, eb",
		"whittaker, rh",
		"spofford, j",
		"michener, cd",
		"giles, nh",
		"levene, h",
		"patrick, r",
		"allard, rw",
		"carson, hl",
		"moore, ja",
		"lewis, h",
		"wallace, b",
		"stebbins, gl",
		"shultz, j",
		"pittendrigh, cs",
		"rollins, rc",
		"glass, hb",
		"tyler, a",
		"brink, ra",
		"mayr, e",
		"bates, m",
		"dunn, lc",
		"sears, pb",
		"hutchinson, ge",
		"no one",
		"harvey, en",
		"thimann, kv",
		"demerec, m",
		"stadler, lj",
		"wright, s",
		"mangelsdorf, pc",
		"dobzhansky, t",
		"sonneborn, tm",
		"burkholder, pr",
		"lashley, ks",
		"metz, cw",
		"sinnott, ew",
		"cole, f",
		"muller, hj",
		"cleland, re",
		"cort, ww",
		"bodine, jh",
		"lewis, if",
		"yerkes, rm",
		"tennent, dh",
		"allen, ce",
		"merriam, jc",
		"shull, af",
		"livingston, be",
		"gortner, ra",
		"holmes, sj",
		"blakeslee, af",
		"parker, gh",
		"donaldson, hh",
		"mcclung, ce",
		"harris, ja",
		"merriam, ch",
		"howell, wh",
		"emerson, ra",
		"wheeler, wh",
		"davis, bm",
		"loeb, j",
		"east, em",
		"castle, we",
		"schull, gh",
		"pearl, r",
		"lillie, fr",
		"clarke, sf",
		"harrison, rg",
		"conklin, eg",
		"jennings, hs",
		"macdougal, dt",
		"morgan, th",
		"penhallow, dp",
		"mcmurrich, jp",
		"davenport, cb",
		"mark, el",
		"trelease, w",
		"cattell, jm",
		"sedgwick, wt",
		"wilson, eb",
		"farlow, wg",
		"bowditch, hp",
		"whitman, co",
		"scott, wb",
		"cope, ed",
		"minot, cs",
		"chittenden, rh",
		"osborn, hf",
		"rice, wn",
		"martin, hn",
		"goodale, gl",
		"allen, h",
		"allen, h",
		"gilbert, gk",
		"gilbert, gk",
		"hyatt, a"
	))	
}

# Note that some of the first initials are uncertain, as https://www.evolutionsociety.org/file.php?file=sitefiles/Handbook%20files/SSE_Past_Officers_1-13-2022.pdf does not list them.
get_sse_presidents <- function() {
	return(c(
		"zamudio, kr",
		"galloway, lf",
		"delph, lf",
		"shaw, rg",
		"rausher, md",
		"hoekstra, he",
		"otto, sp",
		"hughes, al",
		"fairbairn, dj",
		"noor, ma",
		"lenski, re",
		"edwards, sv",
		"coyne, ja",
		"orr, ha",
		"moritz, c",
		"schmitt, j",
		"waller, dm",
		"waller, dm",
		"schluter, d",
		"schaal, ba",
		"hillis, dm",
		"moran, na",
		"barton, nh",
		"lynch, m",
		"charlesworth, b",
		"arnold, sj",
		"lande, r",
		"templeton, ar",
		"endler, ja",
		"avise, jc",
		"felsenstein, j",
		"west-eberhard, mj",
		"antonovics, j",
		"gould, f",
		"williams, gc",
		"smith, jm"
	))	
}

make_heatmap <- function(pairwise_author_matrix, transform_diag=FALSE, journals=get_journals()) {
	

	if(transform_diag) {
		diag(pairwise_author_matrix) <- 0
	}
	transformed_df <- data.frame(from=character(), to=character(), value=numeric())
	for (row_index in sequence(nrow(pairwise_author_matrix))) {
		for (col_index in sequence(ncol(pairwise_author_matrix))) {
			transformed_df <- dplyr::bind_rows(transformed_df, data.frame(authors_in=rownames(pairwise_author_matrix)[row_index], also_in=colnames(pairwise_author_matrix)[col_index], value=pairwise_author_matrix[row_index,col_index]))
		}
	}
	
	#transformed_df$authors_in <- factor(transformed_df$authors_in, levels=unique(transformed_df$authors_in))
	#transformed_df$also_in <- factor(transformed_df$also_in, levels=rev(unique(transformed_df$also_in)))


	transformed_df$authors_in <- factor(transformed_df$authors_in, levels=rev(journals))
	transformed_df$also_in <- factor(transformed_df$also_in, levels=journals)
	myplot <- ggplot(data=transformed_df, aes(x=also_in, y=authors_in, fill=value)) + geom_tile() + scale_fill_viridis(discrete=FALSE, option = "G", direction=-1) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme(axis.text.y = element_text(angle = 0, hjust = 1)) + geom_text(aes(label = round(value), colour =ifelse(value<10, "white", "black"))) +     scale_colour_manual(values=c("white", "black")) +theme(legend.position="none") + labs(x="Who wrote another paper in:", y="Percent of authors writing papers in:")  +  theme(text = element_text(family = "Optima"))  + scale_x_discrete(position="top")
	return(myplot)	
}

get_authors_per_journal_per_year <- function(array_of_counts) {
	author_counts <- colSums(array_of_counts>0, dims=c(1))
	return(as.data.frame(author_counts))
}

plot_author_counts_per_journal_per_year <- function(author_counts_per_journal_per_year, journals=get_journals()) {
	author_counts_per_journal_per_year$Year <- as.numeric(rownames(author_counts_per_journal_per_year))
	pivoted <- tidyr::pivot_longer(as.data.frame(author_counts_per_journal_per_year), cols=-Year, names_to="Journal", values_to="Authors")
	pivoted$Journal <- factor(pivoted$Journal, levels=journals)
	pivoted <- subset(pivoted, Authors>0) # remove years with no pubs

	myplot <- ggplot(data=pivoted, aes(x=Year, y=Authors, colour=Journal, group=Journal)) + geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x="Year", y="Number of authors") + facet_wrap(~Journal) +  theme(text = element_text(family = "Optima"))

	return(myplot)
}

pairwise_to_graph_input <- function(pairwise_author_matrix) {
	connections <- data.frame(from=character(), to=character(), weight=numeric())	
	for (row_index in sequence(nrow(pairwise_author_matrix))) {
		for (col_index in sequence(ncol(pairwise_author_matrix))) {
			connections <- dplyr::bind_rows(connections, data.frame(from=rownames(pairwise_author_matrix)[row_index], to=colnames(pairwise_author_matrix)[col_index], weight=pairwise_author_matrix[row_index,col_index]))
		}
	}
	return(connections)
}

get_graph <- function(pairwise_author_matrix) {
	connections <- pairwise_to_graph_input(pairwise_author_matrix)
	g <- graph_from_data_frame(connections, directed=FALSE)
	return(g)
}

plot_graph <- function(pairwise_author_matrix) {
	#g <- get_graph(pairwise_author_matrix)
	g <- as_tbl_graph(pairwise_author_matrix)
	#ggt<-ggraph(g, layout = 'fr', weights=exp(weight)) + geom_edge_link() +  theme_graph() + geom_node_text(aes(label = name), colour = 'purple', vjust = 0.4) +   geom_edge_link(aes(colour = weight)) 
	#return(ggt)
	layout <- layout_with_kk(g, weights=1/E(g)$weight)

	return( plot(g, layout = layout,  edge.width=10*E(g)$weight/max(E(g)$weight)))
}

plot_ngram_counts <- function(terms) {
	ng  <- ngram(terms, year_start = 1800)
	myplot <- ggplot(ng, aes(x = Year, y = Frequency, colour = Phrase)) + geom_line()	
	return(myplot)
}

get_unique_authors_per_journal <- function(array_of_counts) {
	journals <- dimnames(array_of_counts)[[3]]
	counts_per_journal <- rep(0, length(journals))
	names(counts_per_journal) <- journals
	for (journal in journals) {
		journal_counts <- array_of_counts[,,journal, drop=FALSE]
		authors_publishing <- apply(journal_counts, 1, function(x) {any(x>0)})
		counts_per_journal[journal] <- sum(authors_publishing)
	}
	return(counts_per_journal)
}

get_data_for_animated_waffles <- function(all_papers, journals=get_journals(), min_year=1946, max_year=1968) {
	results <- data.frame()
	
	bounds_all_papers <- subset(all_papers, Year >= min_year & Year <= max_year)
	bounds_array_of_all <- populate_array(bounds_all_papers)
	bounds_array_of_counts <- get_array_of_counts(bounds_array_of_all)
	bounds_authors_per_journal <- get_unique_authors_per_journal(bounds_array_of_counts)
	
	for(year_width in sequence(max_year-min_year)) {
		window_all_papers <- subset(all_papers, Year >= min_year & Year <= min_year+year_width)
		window_array_of_all <- populate_array(window_all_papers)
		window_array_of_counts <- get_array_of_counts(window_array_of_all)
		window_authors_per_journal <- get_unique_authors_per_journal(window_array_of_counts)
		window_authors_multiple_per_journal <- get_count_of_authors_publishing_at_least_twice_in_same_journal(window_array_of_counts)
		window_author_connections <- get_authors_across_journals(window_array_of_counts)	
		window_pairwise_author_matrix <- get_pairwise_author_matrix(window_author_connections, window_authors_per_journal, window_authors_per_journal)
		for (focal_journal in journals) {
			other_journals <- journals[journals!=focal_journal]
			for (non_focal_journal in other_journals) {
				intersection_count <- 0
				try({intersection_count <- window_pairwise_author_matrix[focal_journal, non_focal_journal]}, silent=TRUE)
				total_authors <- sum(bounds_authors_per_journal[c(focal_journal, non_focal_journal)], na.rm=TRUE)
				authors_this_window <- sum(window_authors_per_journal[c(focal_journal, non_focal_journal)], na.rm=TRUE)-intersection_count
				local_df <- data.frame(focal=focal_journal, non_focal=non_focal_journal, min_year=min_year, max_year=min_year+year_width, category = c(paste(focal_journal, "alone"), paste(focal_journal, "and", non_focal_journal), paste(non_focal_journal, "alone"), "future_authors"),counts=c(window_authors_per_journal[focal_journal]-intersection_count, intersection_count=intersection_count, sum(0, window_authors_per_journal[non_focal_journal], na.rm=TRUE)-intersection_count, total_authors-authors_this_window))
				results <- dplyr::bind_rows(results, local_df)
			}
			# now do with itself
			total_authors <- bounds_authors_per_journal[focal_journal]
			local_df <- data.frame(focal=focal_journal, non_focal=focal_journal, min_year=min_year, max_year=min_year+year_width, category = c(paste(focal_journal, "alone"), paste(focal_journal, "and", non_focal_journal), paste(non_focal_journal, "alone"), "future_authors"),counts=c(0,0,0, total_authors))
			results <- dplyr::bind_rows(results, local_df)
			
		}
	}
	return(results)
}

make_waffle_animated_plots <- function(waffles_data, focal_journal="American Naturalist", non_focal_journals=c("Eugenics journals", "American Naturalist", "Evolution", "Systematic Biology", "Ecology", "Genetics", "Copeia", "American Midland Naturalist")) {
	non_focal_journals <- setdiff(non_focal_journals, focal_journal)
	waffles_data <- subset(waffles_data, focal == focal_journal)
	waffles_data <- waffles_data[which(waffles_data$non_focal %in% non_focal_journals),]
	waffles_data$category <- gsub("future_authors", "", waffles_data$category)
	waffles_data$category[grepl(" and ", waffles_data$category)] <- "Both journals"
	waffles_data$category[grepl(paste(non_focal_journals, collapse="|"), waffles_data$category)] <- "Other journal alone"
	waffles_data$category <- factor(waffles_data$category, levels=c(paste0(focal_journal, " alone"), "Both journals", "Other journal alone", ""))
	waffles_data$Year <- waffles_data$max_year
	file_name <- paste0(gsub(" ", "_", focal_journal), '.gif')
	saveGIF({
		for (focal_year in c(unique(sort(waffles_data$Year)), replicate(10, max(waffles_data$Year)))) { # replicate so it pauses on last slide
			myplot <- ggplot(subset(waffles_data, Year==focal_year), aes(fill=category, values=counts, group=Year)) + geom_waffle(n_rows=20, size=0.1, na.rm=TRUE, colour = "white") + coord_equal() + theme_void() + facet_wrap(~non_focal) + labs(title=paste0(focal_journal, " authors in ", min(waffles_data$min_year), " to ", focal_year)) + scale_fill_manual(name = NULL,
                    values = c("lightblue", "purple", "pink", "white"), labels=levels(waffles_data$category))
			print(myplot)
		}
	}, movie.name=file_name, ani.height=9*100, ani.width=16*100, interval=0.3)
	return(file_name)
}

make_waffle_animated_plot_grid <- function(waffles_data, focal_journals=c("American Naturalist", "Evolution", "Systematic Biology"), non_focal_journals=c("American Naturalist", "Evolution", "Systematic Biology", "Eugenics journals", "Ecology", "Genetics", "Copeia", "American Midland Naturalist"), file_name="waffle_grid.gif") {
	waffles_data$Year <- waffles_data$max_year
	waffles_data <- subset(waffles_data, waffles_data$focal %in% focal_journals)
	waffles_data <- subset(waffles_data, waffles_data$non_focal %in% non_focal_journals)
	waffles_data$focal <- factor(waffles_data$focal, levels=focal_journals)
	waffles_data$non_focal <- factor(waffles_data$non_focal, levels=non_focal_journals)
	waffles_data$category <- gsub("future_authors", "", waffles_data$category)
	waffles_data$category[paste(waffles_data$focal, "alone")==waffles_data$category] <-  "Focal alone"
	waffles_data$category[paste(waffles_data$non_focal, "alone")==waffles_data$category] <-  "Other journal alone"
	waffles_data$category[paste(waffles_data$focal, "and", waffles_data$non_focal)==waffles_data$category] <- "Both journals"
	waffles_data$counts[is.na(waffles_data$counts)] <- 0

	waffles_data$category <- factor(waffles_data$category, levels=c("Focal alone", "Both journals", "Other journal alone", ""))
	saveGIF({
		for (focal_year in c(unique(sort(waffles_data$Year)),rep(max(waffles_data$Year), 5))) { # replicate so it pauses on last slide
		#for (focal_year in range(waffles_data$Year)) { # replicate so it pauses on last slide

			print(focal_year)
			waffles_data_local <- subset(waffles_data, Year==focal_year)
			#print(dim(waffles_data_local))
			#waffles_data_local %<>% complete(category, focal, non_focal, Year, fill=list(counts=0))
			#print(dim(waffles_data_local))
			#print("plotting")
			#waffles_data_local$counts[waffles_data_local$category=="" & waffles_data_local$counts==0] <-10
			myplot <- ggplot(waffles_data_local, aes(fill=category, values=counts, group=Year)) + geom_waffle(n_rows=20, size=0.1, na.rm=TRUE, colour = "white") + coord_equal() + theme_void() + scale_fill_manual(name = NULL,values = c("lightblue", "purple", "pink", "white"), labels=levels(waffles_data_local$category)) + facet_grid(rows=vars(non_focal), cols=vars(focal)) + labs(title=paste0("Authors from ", min(waffles_data$min_year), " to ", focal_year)) + theme(legend.position="bottom") + theme(text = element_text(family = "Optima", size=100)) 
			print(myplot)
		}
		print("done")
	}, movie.name=file_name, ani.height=2*600, ani.width=9*600, loop=1, interval=0.3)
	return(file_name)
}

create_igraph_network <- function(coauthor_array_two_plus_papers, presidents=c(get_ssb_presidents(), get_asn_presidents(), get_sse_presidents()), eugenics_authors=authors_eugenics(array_of_counts)) {
	g <- graph_from_adjacency_matrix(coauthor_array_two_plus_papers, diag=FALSE, mode="undirected")	

	
	presidents <- unique(presidents)
	presidents_in_eugenics_any_time <- presidents[presidents %in% eugenics_authors$anytime_eugenics]
	presidents_in_eugenics_after_1945 <- presidents[presidents %in% eugenics_authors$still_active_after_1945_eugenics]
	
	g <- set_vertex_attr(g, "node_type", value="regular, no eugenics")
	g <- set_vertex_attr(g, "eugenics_anytime", value=FALSE)
	g <- set_vertex_attr(g, "eugenics_anytime", index=V(g)[V(g)$name %in% eugenics_authors$anytime_eugenics], value=TRUE)
	g <- set_vertex_attr(g, "node_type", index=V(g)[V(g)$name %in% eugenics_authors$anytime_eugenics], value="regular, eugenics anytime")


	g <- set_vertex_attr(g, "eugenics_after_1945", value=FALSE)
	g <- set_vertex_attr(g, "eugenics_after_1945", index=V(g)[V(g)$name %in% eugenics_authors$still_active_after_1945_eugenics], value=TRUE)
	g <- set_vertex_attr(g, "node_type", index=V(g)[V(g)$name %in% eugenics_authors$still_active_after_1945_eugenics], value="regular, eugenics after 1945")

	
	g <- set_vertex_attr(g, "president", value=FALSE)
	g <- set_vertex_attr(g, "president", index=V(g)[V(g)$name %in% presidents], value=TRUE)
	g <- set_vertex_attr(g, "node_type", index=V(g)[V(g)$name %in% presidents], value="president, no eugenics")
	
	g <- set_vertex_attr(g, "color", value="gray")
	g <- set_vertex_attr(g, "color", index=V(g)[V(g)$eugenics_anytime==TRUE], value="#a6dba0")
	g <- set_vertex_attr(g, "color", index=V(g)[V(g)$eugenics_after_1945==TRUE], value="#008837")
	g <- set_vertex_attr(g, "color", index=V(g)[V(g)$president==TRUE], value="#fdb863")
	g <- set_vertex_attr(g, "color", index=V(g)[V(g)$eugenics_anytime==TRUE & V(g)$president==TRUE], value="#c2a5cf")
	g <- set_vertex_attr(g, "node_type", index=V(g)[V(g)$eugenics_anytime==TRUE & V(g)$president==TRUE], value="president, eugenics anytime")

	g <- set_vertex_attr(g, "color", index=V(g)[V(g)$eugenics_after_1945==TRUE & V(g)$president==TRUE], value="#7b3294")
	g <- set_vertex_attr(g, "node_type", index=V(g)[V(g)$eugenics_after_1945==TRUE & V(g)$president==TRUE], value="president, eugenics after 1945")

	return(g)
}

prune_isolated_clusters <- function(network, network_clumped, min_size=3) {
	# from https://stackoverflow.com/questions/51271730/how-to-remove-small-communities-using-igraph-in-r
	small <-  which(table(network_clumped$membership) < min_size)
	to_keep <- V(network)[!(network_clumped$membership %in% small)]
	return(induced_subgraph(network, to_keep))
}

plot_network <- function(network) {
	plot(network,  layout = layout_with_fr, vertex.shape = "none", edge.arrow.size = 0, vertex.label=NA)
}

plot_interesting_network <- function(network, file="network.pdf") {
	pdf(file=file, width=100, height=100)
	to_keep <- V(network)[V(network)$color!="gray"]
	plot(induced_subgraph(network, to_keep),  layout = layout_with_fr, edge.arrow.size = 0, vertex.label=NA, vertex.size=1, niter=50000)
	dev.off()
}

plot_interesting_network2 <- function(network, file_name="network.gif", ssb=get_ssb_presidents(), sse=get_sse_presidents(), asn=get_asn_presidents()) {
	# pdf(file=file, width=100, height=100)
	# to_keep <- V(network)[V(network)$president==TRUE]
	# ego_result <- make_ego_graph(network, order=2, nodes=to_keep)
	# to_keep2 <- unique(names(unlist(lapply(ego_result, V))))
	# pruned <- induced_subgraph(network, to_keep2)
	# isolated <- which(degree(pruned)==0)
	# pruned2 = delete.vertices(pruned, isolated)
	# plot(pruned2,  layout = layout_with_kk, edge.arrow.size = 0, vertex.label=NA, vertex.size=1, niter=50000)
	# dev.off()
	
	presidents <- c()
	for (i in sequence(min(length(ssb), length(sse), length(asn)))) {
		presidents <- c(presidents, ssb[i], sse[i], asn[i])
	}
	presidents <- unique(presidents)
	presidents <- presidents[presidents %in% V(network)$name]
	saveGIF({
		for (president in presidents) {
			cat(president, " ", which(president %in% presidents), "\r")
			ego_result <- make_ego_graph(network, order=1, nodes=president)
			if(length(V(ego_result[[1]]))>2) {
				to_keep <- unique(names(unlist(lapply(ego_result, V))))
				pruned <- induced_subgraph(network, to_keep)
				par(mfcol=c(1,2))
				plot(pruned,  layout = layout_with_fr, edge.arrow.size = 0, vertex.label=NA, vertex.size=10, niter=50000)
				plot(table(V(pruned)$node_type))
			}
		}
		
	}, movie.name=file_name, ani.height=1000, ani.width=2000, loop=1, interval=0.3)
}


layout(matrix(c(1,2,3,4,5,6,7,8,9,rep(10,9)), 3,6, byrow=FALSE))


plot_interesting_network3 <- function(network, file_name="network.gif", ssb=get_ssb_presidents(), sse=get_sse_presidents(), asn=get_asn_presidents()) {
	# pdf(file=file, width=100, height=100)
	# to_keep <- V(network)[V(network)$president==TRUE]
	# ego_result <- make_ego_graph(network, order=2, nodes=to_keep)
	# to_keep2 <- unique(names(unlist(lapply(ego_result, V))))
	# pruned <- induced_subgraph(network, to_keep2)
	# isolated <- which(degree(pruned)==0)
	# pruned2 = delete.vertices(pruned, isolated)
	# plot(pruned2,  layout = layout_with_kk, edge.arrow.size = 0, vertex.label=NA, vertex.size=1, niter=50000)
	# dev.off()
	
	presidents_max_length <- max(length(ssb), length(sse), length(asn))
	president_df <- data.frame(year=seq(from=2023, by=-1, length.out=presidents_max_length), ssb=NA, sse=NA, asn=NA, percent_eugenics_after_1945=NA, percent_eugenics_president_after_1945=NA)
	president_df$ssb[1:length(ssb)] <- ssb
	president_df$sse[1:length(sse)] <- sse
	president_df$asn[1:length(asn)] <- asn
	president_df <- subset(president_df, year>=1960)
	window_size <- 3
	quartzFonts(optima = c("Optima Regular", "Optima Bold", "Optima Italic", 
        "Optima Bold Italic"))
	
	saveGIF({
		for (start_window in rev(sequence(nrow(president_df)-window_size))) {
			focal_presidents <- c(president_df[start_window:(start_window+window_size-1), "ssb"], president_df[start_window:(start_window+window_size-1), "sse"], president_df[start_window:(start_window+window_size-1), "asn"])
			focal_presidents <- unique(focal_presidents)
			focal_presidents <- focal_presidents[focal_presidents %in% V(network)$name]
			ego_result <- make_ego_graph(network, order=1, nodes=focal_presidents)
			all_vertices <- unique(names(unlist(lapply(ego_result, V))))
			president_df$percent_eugenics_after_1945[start_window] <- 100*sum(V(network)[all_vertices]$eugenics_after_1945)/length(all_vertices)
			president_df$percent_eugenics_president_after_1945[start_window] <- 100*sum(V(network)[focal_presidents]$eugenics_after_1945)/length(focal_presidents)

			
			layout(matrix(c(sequence(3*window_size),rep((3*window_size)+1,9)), 3,window_size+3, byrow=FALSE))
			par(family = 'optima')

			for (i in sequence(length(focal_presidents))) {
				to_keep <- unique(names(V(ego_result[[i]])))
				pruned <- induced_subgraph(network, to_keep)
				focal_president_society <- toupper(paste(unique(colnames(president_df)[which(president_df[start_window:(start_window+window_size-1),]==focal_presidents[i], arr.ind=TRUE)[,2]]), collapse=", "))
				plot(pruned,  layout = layout_with_fr, edge.arrow.size = 0, vertex.label=NA, vertex.size=10, niter=50000, main=ifelse(focal_presidents[i]=="o'meara, bc", "O'Meara (SSB)", focal_president_society))
			}
			for (i in sequence(window_size*3-length(focal_presidents))) {
				plot(1,1, type="n", axes=FALSE, xlab="", ylab="") # placeholder	
			}
			par(mar=c(5.1, 9.1, 10.1, 2.1))
			plot(x=president_df$year, y=president_df$percent_eugenics_after_1945, type="l", ylim=c(0,34), xlab="Year at end of window", ylab="Percent", cex.axis=2.5, cex.lab=2.5, cex.main=2.5, col="#008837", lwd=8, bty="n", main=paste0("Percent of active SSB, SSE, or ASN presidents (purple) or their coauthors (green)\nwho have published in a \"Eugenics\" journal after 1945. \n(Uses a ", window_size, " year sliding window)"))
			lines(x=president_df$year, y=president_df$percent_eugenics_president_after_1945, col="#7b3294", lwd=4)
		}
		
	}, movie.name=file_name, ani.height=800, ani.width=16*1000/9, loop=1, interval=0.3)
}


plot_interesting_network4 <- function(network, file_name="network.gif", ssb=get_ssb_presidents(), sse=get_sse_presidents(), asn=get_asn_presidents()) {
	# pdf(file=file, width=100, height=100)
	# to_keep <- V(network)[V(network)$president==TRUE]
	# ego_result <- make_ego_graph(network, order=2, nodes=to_keep)
	# to_keep2 <- unique(names(unlist(lapply(ego_result, V))))
	# pruned <- induced_subgraph(network, to_keep2)
	# isolated <- which(degree(pruned)==0)
	# pruned2 = delete.vertices(pruned, isolated)
	# plot(pruned2,  layout = layout_with_kk, edge.arrow.size = 0, vertex.label=NA, vertex.size=1, niter=50000)
	# dev.off()
	
	presidents_max_length <- max(length(ssb), length(sse), length(asn))
	president_df <- data.frame(year=seq(from=2023, by=-1, length.out=presidents_max_length), ssb=NA, sse=NA, asn=NA, percent_eugenics_after_1945=NA, percent_eugenics_president_after_1945=NA)
	president_df$ssb[1:length(ssb)] <- ssb
	president_df$sse[1:length(sse)] <- sse
	president_df$asn[1:length(asn)] <- asn
	president_df <- subset(president_df, year>=1960)
	window_size <- 3
	quartzFonts(optima = c("Optima Regular", "Optima Bold", "Optima Italic", 
        "Optima Bold Italic"))
	
	saveGIF({
		for (start_window in rev(sequence(nrow(president_df)-window_size))) {
			focal_presidents <- c(president_df[start_window:(start_window+window_size-1), "ssb"], president_df[start_window:(start_window+window_size-1), "sse"], president_df[start_window:(start_window+window_size-1), "asn"])
			focal_presidents <- unique(focal_presidents)
			focal_presidents <- focal_presidents[focal_presidents %in% V(network)$name]
			ego_result <- make_ego_graph(network, order=1, nodes=focal_presidents)
			all_vertices <- unique(names(unlist(lapply(ego_result, V))))
			president_df$percent_eugenics_after_1945[start_window] <- 100*sum(V(network)[all_vertices]$eugenics_after_1945)/length(all_vertices)
			president_df$percent_eugenics_president_after_1945[start_window] <- 100*sum(V(network)[focal_presidents]$eugenics_after_1945)/length(focal_presidents)

			
			par(mfcol=c(1,2))
			par(family = 'optima')

			pruned <- induced_subgraph(network, all_vertices)
			plot(pruned,  layout = layout_with_fr, edge.arrow.size = 0, vertex.label=NA, vertex.size=ifelse(V(pruned)$color=="gray", 2, 10), niter=50000, main=paste0("Presidents and coauthors ", president_df[(start_window+window_size-1), "year"], " to ", president_df[start_window, "year"]), cex.main=3)


			par(mar=c(5.1, 9.1, 10.1, 2.1))
			plot(x=president_df$year, y=president_df$percent_eugenics_after_1945, type="l", ylim=c(0,34), xlab="Year at end of window", ylab="Percent", cex.axis=1.5, cex.lab=1.5, cex.main=1.5, col="#008837", lwd=8, bty="n", main=paste0("Percent of active SSB, SSE, or ASN presidents (purple) or their coauthors (green)\nwho have published in a \"Eugenics\" journal after 1945. \n(Uses a ", window_size, " year sliding window)"))
			lines(x=president_df$year, y=president_df$percent_eugenics_president_after_1945, col="#7b3294", lwd=4)
		}
		
	}, movie.name=file_name, ani.height=800, ani.width=16*1000/9, loop=1, interval=0.3)
}


plot_interesting_network_all_years <- function(network, ssb=get_ssb_presidents(), sse=get_sse_presidents(), asn=get_asn_presidents()) {
	# pdf(file=file, width=100, height=100)
	# to_keep <- V(network)[V(network)$president==TRUE]
	# ego_result <- make_ego_graph(network, order=2, nodes=to_keep)
	# to_keep2 <- unique(names(unlist(lapply(ego_result, V))))
	# pruned <- induced_subgraph(network, to_keep2)
	# isolated <- which(degree(pruned)==0)
	# pruned2 = delete.vertices(pruned, isolated)
	# plot(pruned2,  layout = layout_with_kk, edge.arrow.size = 0, vertex.label=NA, vertex.size=1, niter=50000)
	# dev.off()
	
	presidents_max_length <- max(length(ssb), length(sse), length(asn))
	president_df <- data.frame(year=seq(from=2023, by=-1, length.out=presidents_max_length), ssb=NA, sse=NA, asn=NA, percent_eugenics_after_1945=NA, percent_eugenics_president_after_1945=NA)
	president_df$ssb[1:length(ssb)] <- ssb
	president_df$sse[1:length(sse)] <- sse
	president_df$asn[1:length(asn)] <- asn
	president_df <- subset(president_df, year>=1960)
	quartzFonts(optima = c("Optima Regular", "Optima Bold", "Optima Italic", 
        "Optima Bold Italic"))


	focal_presidents <- c(president_df[, "ssb"], president_df[, "sse"], president_df[, "asn"])
	focal_presidents <- unique(focal_presidents)
	focal_presidents <- focal_presidents[focal_presidents %in% V(network)$name]
	ego_result <- make_ego_graph(network, order=1, nodes=focal_presidents)
	



	all_vertices <- unique(names(unlist(lapply(ego_result, V))))

	
	par(mfcol=c(1,1))
	par(family = 'optima')

	pruned <- induced_subgraph(network, all_vertices)
	
	components <- igraph::clusters(pruned, mode="weak")
	biggest_cluster_id <- which.max(components$csize)
	vert_ids <- V(pruned)[components$membership == biggest_cluster_id]
	pruned2 <- igraph::induced_subgraph(pruned, vert_ids)

	
	
	jpeg(file="network.jpg", width=16*1000/9, height=9*1000/9)
	E(pruned)$color <- rgb(0,0,0, .1)
	plot(pruned,  layout = layout_nicely, edge.arrow.size = 0, vertex.label=NA, vertex.size=ifelse(V(pruned)$color=="gray", 1, 5), niter=50000, main=paste0("Presidents and coauthors, 1946-2023"), cex.main=3)
	dev.off()
	
		
}

CreateSummaryTable <- function(all_papers, authors_per_journal) {
	result <- as.data.frame(t(t(table(all_papers$Journal)))	)
	result <- result[,-2]
	colnames(result) <- c("Journal", "PaperCount")
	result$MinYear <- NA
	result$MaxYear <- NA
	result$AuthorCount <- NA
	for (journal_index in sequence(nrow(result))) {
		journal <- result$Journal[journal_index]
		result$MinYear[journal_index] <- min(as.numeric(all_papers[all_papers$Journal==journal, "Year"]))
		result$MaxYear[journal_index] <- max(as.numeric(all_papers[all_papers$Journal==journal, "Year"]))
		result$AuthorCount[journal_index] <- authors_per_journal[journal]
	}
	return(result)
	
	
}