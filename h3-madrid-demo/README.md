# H3 Geospatial Indexing Demo – From Lat/Lon to Hexagons and Neighbourhoods

This repository contains a self-contained, didactic demo of how to go from
**raw latitude/longitude coordinates** to:

- **H3 hexagonal indices** at multiple resolutions.
- **Administrative context** (neighbourhoods and districts in Madrid).
- **Geospatial visualisations** such as choropleth maps and hexagonal heatmaps.

The core of the project is a Jupyter notebook that walks through the entire
pipeline step by step, using synthetic data and official open data for Madrid.

---

## Background and motivation

This work originated from a real customer project where we needed to build an
**anomaly detection model for bird sightings**.

The problem:

- The dataset was extremely poor in terms of metadata.
- For each observation we had little more than:
  - `latitude`
  - `longitude`
  - a timestamp
- There were **no explicit country / region / city / neighbourhood labels**.

To detect *spatial outliers* – suspicious bird records in unlikely locations – we
needed a way to:

1. Turn raw coordinates into a **discrete spatial index**.
2. Aggregate and compare observations over **consistent spatial units**.
3. Optionally map those units back to **human-readable areas**.

This led to adopting **H3**, Uber’s hexagonal hierarchical geospatial indexing
system. This repository generalises that idea into a reusable demo that can be
applied to other use cases, not just birds.

---

## What is H3?

[H3](https://h3geo.org/) is an open-source geospatial indexing system that:

- Divides the Earth’s surface into **hexagonal cells** (plus a few pentagons).
- Assigns each cell a unique **H3 index** (a string).
- Supports **resolutions** from 0 (very large hexagons) to 15 (very small ones).
- Provides a **hierarchy**: each cell at resolution `r` is subdivided into
  multiple children at resolution `r+1`.

Why hexagons?

- Hexagons have more uniform adjacency than squares on a sphere.
- They are well-suited for aggregations, neighbourhood queries, and smooth
  heatmaps.
- H3 comes with a rich API: index conversions, neighbourhood (k-ring),
  polyfill for arbitrary polygons, etc.

In this project we use H3 to:

- Convert each `(lat, lon)` pair into H3 indices at different resolutions.
- Aggregate counts per hexagon.
- Visualise those aggregations as **hexagonal heatmaps**.
- Use H3 cells as a bridge between raw coordinates and **Madrid neighbourhoods**.

---

## Project structure

```text
.
├─ notebooks/
│  ├─ 01_h3_spain_madrid_demo.ipynb   # main teaching notebook
├─ data/
│  ├─ raw/
│  │  ├─ spain_h3_demo_points.csv     # synthetic Spain points (lat/lon + city)
│  │  ├─ BARRIOS.*                    # Madrid neighbourhood shapefile
│  │  └─ (other raw inputs if needed)
│  └─ processed/
│     ├─ spain_h3_demo_points_enriched.csv   # raw + H3 indices
│     └─ madrid_h3_points_with_barrios.csv   # Madrid + H3 + barrio + distrito
├─ src/
│  ├─ h3_utils.py                     # optional helpers (wrappers, plotting)
│  └─ __init__.py
├─ figures/
│  ├─ fig01_scatter_spain.png         # raw points over Spain
│  ├─ fig02_madrid_barrio_heatmap.png # choropleth by neighbourhood
│  └─ fig03_madrid_h3_hex_heatmap.png # hex heatmap (H3 grid)
├─ env/
│  └─ environment.yml                 # or requirements.txt
├─ README.md
├─ .gitignore
└─ LICENSE
