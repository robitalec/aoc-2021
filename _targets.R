library(targets)

source('R/packages.R')

c(
	tar_target(
		rmds,
		dir('days', '.Rmd', full.names = TRUE)
	),
	tar_target(
		rmd,
		rmds,
		pattern = map(rmds),
		format = 'file'
	),
	tar_target(
		rendered,
		render(
			rmd,
			output_dir = 'docs',
			output_format = 'github_document'
		),
		pattern = map(rmd)
	)
)
