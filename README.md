# drought-monitor

Weekly U.S. Drought Monitor tracking for the Washington Columbia Basin potato
counties. Figures rebuild themselves every Thursday and are served as stable
image URLs that the lab website embeds directly.

Garnica Lab, WSU Department of Plant Pathology.

---

## Image URLs

Once the first run finishes, these are permanent:

```
https://garnicalab.github.io/drought-monitor/drought-basin-current.png
https://garnicalab.github.io/drought-monitor/drought-basin-timeseries.png
https://garnicalab.github.io/drought-monitor/drought-basin-dsci.png
```

And the standalone dashboard:

```
https://garnicalab.github.io/drought-monitor/
```

Paste those image URLs into WordPress Image blocks. The files behind them change
every Thursday; the URLs never do. Nothing to maintain.


---

## Schedule

`.github/workflows/update-figures.yml` runs Thursdays at 16:00 UTC, which is
9 a.m. Pacific.

---

## Data

U.S. Drought Monitor, a joint product of the National Drought Mitigation Center
at the University of Nebraska-Lincoln, the U.S. Department of Agriculture, and
the National Oceanic and Atmospheric Administration. Free to use with credit.
<https://droughtmonitor.unl.edu/>

Weekly records are written to `docs/drought-basin-weekly.csv` and
`docs/drought-basin-by-county.csv` on every run, so the full time series is
always available for analysis.
