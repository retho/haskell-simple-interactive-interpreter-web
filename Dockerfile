FROM terrorjack/asterius:200830

VOLUME /workspace
WORKDIR /workspace
EXPOSE 5000

ARG hpack_version=0.34.2
RUN cabal v2-update && cabal v2-install hpack -j --constraint="hpack == $hpack_version" --installdir=/bin --install-method=copy --overwrite-policy=always --ghc-options='-optl-static -optl-pthread'

RUN npm i -g serve
RUN npm i -g nodemon

ARG force_update
RUN ahc-cabal new-update
