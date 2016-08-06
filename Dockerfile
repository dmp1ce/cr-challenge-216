FROM haskell
MAINTAINER /u/dmp1ce

# Install Yesod requriements
# http://www.yesodweb.com/book/haskell
RUN stack setup && stack build yesod persistent-sqlite yesod-static esqueleto
