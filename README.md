gs.chainsearch
================

![Current Version](https://img.shields.io/badge/version-0.0.1-blue)
![GitHub
stars](https://img.shields.io/github/stars/timothyslau/gs.chainsearch)
![GitHub
forks](https://img.shields.io/github/forks/timothyslau/gs.chainsearch)

This package provides methods to perform a forward chaining search via
[Google Scholar](https://scholar.google.com/). It accomplishes this by
scraping publications that cite a cornerstone publication via the “Cited
By” search feature. The primary purpose of this package is to enable
researchers to produce comprehensive literature reviews.

The general strategy is as follows:

1.  Identify a cornerstone publication.
2.  Scrape the search results for citing publications and save the raw
    html.
3.  Parse and combine key metadata (publication details) from the raw
    html.
4.  Process metadata (e.g., remove duplicates).

Contributions are more than welcome! See
[CONTRIBUTING](.github/CONTRIBUTING.md) for guidance.

### Package Data

By default, this package tracks files within the R user package cache
(see `tools::R_user_dir`). This can be modified to any mounted directory
via `storage_update`. Each cornerstone publication is given a dedicated
subdirectory within that. The structure is as follows:

    ├── /<storage>/
      ├── proxy_table.csv
      ├── proxy_blacklist.csv
      ├── <publication_id>/
        ├── pages/
          ├── page1.html
          ├── page2.html
          ├── ...
        ├── meta_raw.csv
        ├── meta_final.csv

- `proxy_table.csv`: A table of available proxies. The table includes
  `ip`, `port`, and metadata for each proxy.
- `proxy_blacklist.csv`: A table of proxies that are marked as
  “blacklisted”, either manually via `blacklist_ip` or automatically via
  `save_gs_page(..., auto_cycle_ip = TRUE)`. The table includes `ip`,
  `port`, and `method` (either “manual” or “automatic”).
- `<publication_id>/`: Dedicated storage for a cornerstone publication.
- `pages/`: A subdirectory used as storage for raw HTML files that are
  scraped.
- `meta_raw.csv`: A table of raw metadata extracted from raw HTML pages.
- `meta_final.csv`: A table of processed metadata.

### Proxy Cycling

A proxy cycling procedure is implemented internally in order to
gracefully recover from IP bans issued by Google. At the beginning of a
working session, a fresh list of public proxies is fetched from
[Geonode](https://geonode.com/free-proxy-list/). This list is randomly
sampled during the scraping process. Each time an IP ban is detected,
the culprit IP is blacklisted and subsequent scrapes will not consider
it.

### Shiny App

This package includes a shiny interface accessible by running
`gs.chainsearch::app_run()`.

#### Session Settings

Via the `Session Settings` interface, the user is able to select the
storage directory, indicate the cornerstone publication, and manage
package files.

#### Proxy Settings

Via the `Proxy Settings` interface, the user is able to browse and
refresh the proxy list, manually set the active proxy, blacklist
individual proxies, and view proxy logs.

#### Scrape

Via the `Scrape` interface, the user is able to control and monitor the
active scraping job.

#### Results

Via the `Results` interface, the user is able to view and modify
publication metadata extracted from the scraped HTML.
