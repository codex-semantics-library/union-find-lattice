##########################################################################
#  This file is part of the Codex semantics library                      #
#    (Union-find lattice subcomponent).                                  #
#                                                                        #
#  Copyright (C) 2026                                                    #
#    CEA (Commissariat à l'énergie atomique et aux énergies              #
#         alternatives)                                                  #
#                                                                        #
#  you can redistribute it and/or modify it under the terms of the GNU   #
#  Lesser General Public License as published by the Free Software       #
#  Foundation, version 2.1.                                              #
#                                                                        #
#  It is distributed in the hope that it will be useful,                 #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of        #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         #
#  GNU Lesser General Public License for more details.                   #
#                                                                        #
#  See the GNU Lesser General Public License version 3.0                 #
#  for more details (enclosed in the file LICENSE).                      #
#                                                                        #
##########################################################################

# set to ON/OFF to toggle ANSI escape sequences
COLOR = ON

# Uncomment to show commands
# VERBOSE = TRUE

# padding for help on targets
# should be > than the longest target
HELP_PADDING = 15

GRAPHS = join_time_* meet_time_* build_total_time_* incl_time_* triple_join_time_* triple_meet_time_* join_sum_* meet_sum_*
GRAPH_PATHS = $(addprefix graphs/,$(GRAPHS))

# ==================================================
# Make code and variable setting
# ==================================================

ifeq ($(COLOR),ON)
	color_yellow = \033[93;1m
	color_orange = \033[33m
	color_red    = \033[31m
	color_green  = \033[32m
	color_blue   = \033[34;1m
	color_reset  = \033[0m
endif

define print
	@echo "$(color_yellow)$(1)$(color_reset)"
endef

# =================================================
# Default target
# =================================================

default: build
.PHONY: default

# =================================================
# Special Targets
# =================================================

.PHONY: build
build: ## Compile the project
	dune build

.PHONY: test
test: build ## Run the tests
	./_build/default/tests/test.exe

.PHONY: bench-graph
bench-graph: build ## Run the benchmarks to generate the graph
	./_build/default/benchmarks/main.exe

.PHONY: bench-table
bench-table: build ## Run the benchmarks to generate the table
	./_build/default/benchmarks/main.exe table

.PHONY: bench ## run the benchmarks for both the graph and table
bench: bench-graph bench-table

.PHONY: table
table: build ## Print the benchmarking table
	./_build/default/benchmarks/graph/graph.exe table

.PHONY: graph
graph: build ## Generate the graphs
	./_build/default/benchmarks/graph/graph.exe
# cp $(wildcard $(GRAPH_PATHS)) /home/dorian/Documents/LaTeX/union-find-lattice/graphs/

.PHONY: gen_stats
gen_stats: build ## Show random generation stats
	./_build/default/benchmarks/main.exe gen_stats

.PHONY: configs
configs: build ## Short sample of various run conditions
	./_build/default/benchmarks/main.exe configs

.PHONY: help
help: ## Show this help
	@echo "$(color_yellow)make:$(color_reset) list of useful targets:"
	@egrep -h '\s##\s' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "  $(color_blue)%-$(HELP_PADDING)s$(color_reset) %s\n", $$1, $$2}'

UPDATED_EXTENSIONS = *.mli *.ml *.mll *.mly dune dune-project Dockerfile*
FIND_EXCLUDE = -not -path './_build/*'
FIND_EXCLUDE += -not -path './benchmarks/dynarray.ml'
FIND_EXCLUDE += -not -path './graph/gnuplot.ml*'
FIND_EXCLUDE += -not -path './union-find-lattice/memcad/*'

.PHONY: headers
headers: ## update headers
	find . $(FIND_EXCLUDE) -type f \( -name makefile $(patsubst %, -o -name "%", $(UPDATED_EXTENSIONS)) \) \
	-exec headache -c headers/config.txt -h headers/text.txt {} \;
