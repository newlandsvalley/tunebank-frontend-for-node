# tunebank-frontend-for-node

Tradtunedb.org.uk has now moved to a new domain - try it here - [tunebank.org.uk](https://tunebank.org.uk).

This is the browser frontend code for the new domain. It operates against a backend of [tunebank-node](https://github.com/newlandsvalley/tunebank-node) - i.e. it is a (small) rewrite of [tunebank-frontend](https://github.com/newlandsvalley/tunebank-frontend) whilst attempting to keep the look-and-feel just about the same. The frontend for the older domain operated against the `musicrest` server both of which have now been deprecated. 

## To Build

```
    npm run build
```
## To Test

    fire up a tunebank-node development server make sure its test suite has been run and then:
```
    npm run test
```
## SSL, CORS and a Reverse-Proxy

We want communication between `tunebank-frontend-for-node` and `tunebank-node` (the backend) to be as simple and seemless as possible.  In particular, we want the frontend to use SSL and to be configured to use HTTPS and still use HTTP to communicate to the backend. If we do this, we don't then need the CORS response headers from the backend at all.

By far the most straightforward way to achieve this is to use a reverse-proxy.  This can be used to make browsers think that the backend is co-hosted alongside the frontend, and so that both also appear to use HTTPS.  In our case, we use `nginx` as a reverse proxy and configure our service to have an extra location route describing the backend which we'll call `tunebank` and which passes on traffic to the actual backend server on port 8080. Here's the addition we need to nginx configuration in a development environment:

```
location /tunebank/ {
     proxy_pass http://localhost:8080/;
     proxy_http_version 1.1;
     proxy_set_header Upgrade $http_upgrade;
     proxy_set_header Connection 'upgrade';
     proxy_set_header Host $host;
     proxy_cache_bypass $http_upgrade;
     }
```

All we then need to do is to change the baseURL reference in `index.html` to point to this location instead of the actual backend.  So for the development environment this is:

```
<script>
   localStorage.setItem('baseURL', 'https://localhost/tunebank')
</script>
```

and for a production service at `mydomain.com` we would have:

```
location /tunebank/ {
     proxy_pass http://mydomain.com:myport/tunebank;
     ...
     }
```
and:

```
<script>
   localStorage.setItem('baseURL', 'https://mydomain.com/tunebank')
</script>
```

## Sitemap Generation

Because of the dynamic nature of the site, there is no permanent sitemap - it will grow organically as more tunes are added.  However, it is possible to generate sitemaps for your production server that represent a single point in time by querying the backend database used by `tunebank-node`. The sitemap folder holds a bash script that can be run against the underlying PSQL database to generate a set of tune sitemaps - one for each of the five genres.

You should first edit the script and replace the postgres user name and database name with your own names. You should also edit the output file path to suit your own environment. Then run the script - it requires two parameters - firstly the domain name of your server and finally the genre of music for the tunes for which you wish to generate a map.

Finally you can copy these sitemap files to your production server and will then be able to submit them to `Google Search Console`.

## Key Differences from the original tunebank-frontend

  * Tunes in search URLs are identified uniquely by the tune title rather than the combination of title and rhythm.
  * The tunes page no longer supports download of tunes in `postscript` or `pdf` format. (postscript is little used and pdf downloads can better be managed by printing and saving as pdf).
  * Search URLs are no longer required to supply search terms in lower-case.  For example, search terms of `title`, `rhythm`, `origin`, `source` etc. can all be supplied using any combination of case.  This means that tune titles in tune lists are correctly capitalised.
  * It is no longer possible to search by the tune notes in the ABC itself.  This facility rarely worked.
  * Any user may be made into an administrator (at the discretion of the owner of tunebank-node).  This is invisible to most users but can allow some users to see extra options previously available only to the user named `Administrator`.
  * User email addresses are unique. The original tunebank-frontend allows different users to share the same address.  This is problematic in that it makes a _change password_ feature problematic to implement. Now, we do have _change/forgot password_ and _forgot user_.









