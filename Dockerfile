# TEMPLATE DOCKERFILE FOR CCAO SHINY APPS

# This Dockerfile serves as a minimal template for CCAO shiny applications
# It is used in conjunction with GitHub Actions to create Docker images
# which are stored in CCAO's GitHub container registry and later deployed to
# ShinyProxy. The build here has five general steps:

# 1) SETUP. Create the necessary folders and permissions to run the app
# 2) DEPENDENCIES. Install linux backend libraries needed by certain R libs
# 3) R PACKAGE INSTALL. Download the R packages needed by the application
# 4) COPY CODE. Copy the source code of the app into the appropriate folder(s)
# 5) LABELLING. Create metadata for the container image

# The order of these steps is VERY important for efficient build times
# To avoid "cache busting" or the need to rebuild the entire image, you want
# to order things from least to change (at the top of the script) to most
# like to change (at the bottom of the script)


### SETUP ###

# Use the shiny image as a base
FROM rocker/r-ver:4.3.1

# Arguments that get passed to apt to install linux dependencies. Formatted as
# strings with libraries separated by a space. If an app is missing linux
# dependencies, add them to EXTRA_APT_DEPS. APT_DEPS lists the dependencies for
# all database-connected shiny apps
ARG APT_DEPS="libcairo2-dev libcurl4-gnutls-dev libssl-dev libxt-dev tar tdsodbc unixodbc unixodbc-dev wget"
ARG EXTRA_APT_DEPS="gdal-bin libgdal-dev libudunits2-dev pandoc"

# Create and set the default working dir for our application. This is where
# code will live and execute from
WORKDIR /app/

# Declare port 3838 as the port for inbound connections to shiny
EXPOSE 3838

# Set the command that runs at startup. This tells our app to run app.R
# when the container is launched
CMD ["R", "-e", "shiny::runApp('/app/app.R')"]


### DEPENDENCIES ###

# Install R linux dependencies and utilities. These are needed by some common
# R libraries as backends. You may have to add to this list if certain packages
# require linux libraries (for example, the R package sf requires libgdal-dev)
RUN apt-get update && apt-get install --no-install-recommends -y \
    $(echo $APT_DEPS) \
    $(echo $EXTRA_APT_DEPS) \
    && apt-get clean && apt-get autoremove --purge -y \
    && rm -rf /var/lib/apt/lists/* /tmp/*

# Install ODBC drivers for MS SQL. These are used to establish a connection
# to CCAO's existing SQL server
RUN wget --no-verbose https://packages.microsoft.com/debian/9/prod/pool/main/m/msodbcsql17/msodbcsql17_17.4.2.1-1_amd64.deb -O /tmp/msodbc.deb \
    && ACCEPT_EULA=Y apt-get install /tmp/msodbc.deb


### R PACKAGE INSTALL ###

# Copy ONLY the files needed to install dependencies, since these are unlikely
# to change. Copying all of our files would bust the cache if we had updated
# any of our code. Cache will bust when renv.lock (the package manifest) is
# altered, meaning adding new packages requires a full cache rebuild
COPY .Rprofile renv.lock /app/
COPY renv/activate.R /app/renv/activate.R

# Docker containers run as root by default, which is extremely unsafe from a
# security perspective. Always use the minimum possible privileges when running
# an application. In this case, create a user and group, both named shiny,
# and run the container with that user after chowning the working directory
# This step is completed here because recursively chowning the renv library
# takes a very long time, better to install packages as the user shiny
RUN useradd --no-log-init --user-group -mr shiny && chown -R shiny:shiny /app/
USER shiny

# Install R packages necessary for the reporting scripts
# The renv package is used to version lock to specific R packages
# renv.lock contains a list of all packages needed to run the application,
# and it will install all these packages by running the command renv::restore()
RUN R -e 'renv::settings$use.cache(FALSE); renv::restore()'


### COPY CODE ###

# Copy configuration files from our repo to their expected locations. These
# files point ODBC to the correct drivers and ensure that Shiny starts on
# port 3838, respectively.
COPY config/odbcinst.ini /etc/
COPY config/Rprofile.site /usr/local/lib/R/etc/

# Copy all files and subdirectories from our repository into the folder /app/
# This command ignores the files listed in .dockerignore
COPY --chown=shiny:shiny . /app/


### LABELLING ###

# Build arguments used to label the container, these variables are predefined
# as part of GitHub Actions. They get passed to the container as build-args
# in the Actions workflow. These arguments only exist when building the
# container
ARG VCS_NAME
ARG VCS_URL
ARG VCS_REF
ARG VCS_REF_SHORT
ARG VCS_VER
ARG VCS_ID
ARG VCS_NAMESPACE

# Environmental variables that are passed to the container. These variables
# exist inside each app and can be called from R. They are used to create a
# version number in the application UI as well as link to GitHub  
ENV VCS_NAME=$VCS_NAME
ENV VCS_URL=$VCS_URL
ENV VCS_REF=$VCS_REF
ENV VCS_REF_SHORT=$VCS_REF_SHORT
ENV VCS_VER=$VCS_VER
ENV VCS_ID=$VCS_ID
ENV VCS_NAMESPACE=$VCS_NAMESPACE

# Create labels for the container. These are standardized labels defined by
# label-schema.org. Many applications look for these labels in order to display
# information about a container
LABEL maintainer "Dan Snow <daniel.snow@cookcountyil.gov>"
LABEL com.centurylinklabs.watchtower.enable="true"
LABEL org.opencontainers.image.title=$VCS_NAME
LABEL org.opencontainers.image.source=$VCS_URL
LABEL org.opencontainers.image.revision=$VCS_REF
LABEL org.opencontainers.image.version=$VCS_VER
