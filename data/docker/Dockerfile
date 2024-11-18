FROM jupyter/minimal-notebook:notebook-7.0.6

ENV BRIGHTWAY2_DIR=/home/$NB_USER/data
ENV BRIGHTWAY2_DOCKER=1
ENV BRIGHTWAY2_OUTPUT_DIR=/home/$NB_USER/output
ENV XDG_CACHE_HOME="/home/${NB_USER}/.cache/"

USER $NB_USER

RUN mkdir -p /home/$NB_USER/data \
    && mkdir -p /home/$NB_USER/notebooks \
    && mkdir -p /home/$NB_USER/output \
    && fix-permissions "/home/${NB_USER}/data" \
    && fix-permissions "/home/${NB_USER}/notebooks" \
    && fix-permissions "/home/${NB_USER}/output" \
    && npm install prettier

# keep in sync with requirements.txt
# allow to update the image if the source repo is updated
#ADD https://api.github.com/repos/ccomb/brightway2-io/git/refs/tags/ccomb-7 bw2-io.json
ADD https://api.github.com/repos/brightway-lca/brightway2-io/git/refs/tags/0.9.dev41 bw2-io.json
ADD https://api.github.com/repos/brightway-lca/brightway2-parameters/git/refs/tags/1.1.0 bw2-parameters.json
ADD https://api.github.com/repos/brightway-lca/brightway2-data/git/refs/tags/4.0.dev60 bw2-data.json
ADD https://api.github.com/repos/brightway-lca/brightway2-calc/git/refs/tags/2.0.dev23 bw2-calc.json
ADD https://api.github.com/repos/brightway-lca/brightway2-analyzer/git/refs/tags/0.11.7 bw2-analyzer.json
ADD https://api.github.com/repos/brightway-lca/bw_projects/git/refs/tags/v2.1.0 bw_projects.json

COPY requirements.txt .

USER root
RUN apt update \
    && apt install -y gosu vim \
    && pip install -r /home/$NB_USER/requirements.txt \
    && rm /home/$NB_USER/requirements.txt \
    && git config --global --add safe.directory /home/$NB_USER/ecobalyse \
    && if [ "$(dpkg --print-architecture)" = "amd64" ]; then pip install  pypardiso==0.4; ldconfig; fi;


RUN curl -L -o elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz \
    && gunzip elm.gz \
    && chmod +x elm \
    && mv elm /usr/local/bin/

COPY simapro-biosphere.json /opt/conda/lib/python3.11/site-packages/bw2io/data/
COPY entrypoint.sh /

VOLUME /home/$NB_USER
WORKDIR /home/$NB_USER/notebooks
ENTRYPOINT ["/entrypoint.sh"]
