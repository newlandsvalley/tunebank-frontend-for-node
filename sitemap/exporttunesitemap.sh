#!/bin/bash

EXPECTED_ARGS=2

if [ $# -ne $EXPECTED_ARGS ]
then
  echo "Usage: `basename $0` {domain} {genre}"
  exit $E_BADARGS
fi

domain=$1
genre=$2

psql -U john tunedbtest --command \
"\COPY (SELECT 'https://${domain}/#/genre/${genre}/tune/' || replace (title, ' ', '%20')
      FROM tunes
      wHERE genre = '${genre}'
    ) 
to '/home/john/services/tunebank-node/sitemap/${genre}sitemap.txt';"


