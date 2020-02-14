FROM haskell as dev
WORKDIR /usr/local/src
#COPY stack.yaml stack.yaml.lock budget.cabal ./
#RUN stack setup --system-ghc
#RUN stack build --install-ghc --system-ghc --dependencies-only
#COPY . /usr/local/src
#RUN stack build --install-ghc --system-ghc
#VOLUME ["/usr/local/src/.stack-work"]
