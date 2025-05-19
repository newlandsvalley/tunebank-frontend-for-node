# tunebank-frontend-for-node


This is the browser frontend code for [tunebank-node](https://github.com/newlandsvalley/tunebank-node) - i.e. it is a (small) rewrite of [tunebank-frontend](https://github.com/newlandsvalley/tunebank-frontend) allowing it to operate against the node backend rather than against the `musicrest` server whilst attempting to keep the look-and-feel just about the same.

## To Build

    npm run build

## To Test

    fire up a tunebank-node development server make sure its test suite has been run and then:

    npm run test

## Key Differences from the original tunebank-frontend

  * Tunes in search URLs are identified uniquely by the tune title rather than the combination of title and rhythm.
  * The tunes page no longer supports download of tunes in `postscript` or `pdf` format. (postscript is little used and pdf downloads can better be managed by printing and saving as pdf).
  * Search URLs are no longer required to supply search terms in lower-case.  For example, search terms of `title`, `rhythm`, `origin`, `source` etc. can all be supplied using any combination of case.  This means that tune titles in tune lists are correctly capitalised.
  * It is no longer possible to search by the tune notes in the ABC itself.  This facility rarely worked.
  * Any user may be made into an administrator (at the discretion of the owner of tunebank-node).  This is invisible to most users but can allow some users to see extra options previously available only to the user named `Administrator`.



