sudo apt-get -y install python3-pip
python3 -m pip --no-cache-dir install radian
install2.r --error --skipinstalled --ncpus -1 \
    languageserver \
    httpgd \
    jsonlite \
    devtools \
    usethis \
    renv \
    yaml \
&& rm -rf /tmp/downloaded_packages
