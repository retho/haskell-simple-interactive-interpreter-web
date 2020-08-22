FROM terrorjack/asterius:200702

RUN ahc-cabal v1-update

RUN cabal v2-update
RUN cabal v2-install hpack -j --constraint='hpack == 0.34.2' --installdir=/bin --install-method=copy

RUN npm i -g serve
EXPOSE 5000
