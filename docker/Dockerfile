FROM rocker/verse:4.0.0

RUN apt-get update && apt-get install -y \
  build-essential \
  libglpk40 \
  tmux \
  libxml2-dev \
  libcurl4-openssl-dev \
  libssl-dev \ 
  nano \
  less

# R packages
## CRAN
#RUN R -e "install.packages('tidyverse',dependencies=TRUE)" 
RUN R -e "install.packages(c('janitor', 'ggdag', 'here', 'cowplot', 'gridExtra', 'pheatmap', 'princurve', 'scico', 'patchwork'), dependencies = TRUE)"
RUN R -e "install.packages(c('ggsignif', 'broom', 'ggrepel', 'readxl', 'platetools'), dependencies = TRUE)"
RUN R -e "install.packages(c('reshape2'), dependencies = TRUE)"

## Bioconductor
RUN R -e "install.packages('BiocManager')"
RUN R -e "BiocManager::install('splots',dependencies=TRUE)"
RUN R -e "BiocManager::install('synergyfinder',dependencies=TRUE)" #fixs
RUN R -e "BiocManager::install('PharmacoGx',dependencies=TRUE)" #fix

## devtools and dependencies
### installing dependencies for ggrastr
RUN apt-get install -y \
  libgtk2.0-dev \
  libcairo2-dev \
   xvfb \
   xauth \
   xfonts-base \
   libxt-dev
RUN R -e "devtools::install_github('VPetukhov/ggrastr', build_vignettes = FALSE)"
