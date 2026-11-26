# chesterismay.com

Source for [chesterismay.com](https://chesterismay.com), the personal site of
Dr. Chester Ismay: consulting, workshops, talks, and a portfolio of books,
packages, and analyses. It is a [Quarto](https://quarto.org) website.

## How the site is built

- The pages are the `.qmd` files listed under `render:` in `_quarto.yml`.
  Static files (data files, the NBA over/under pages, and others) are served
  only when they match a `resources:` glob there.
- Every push to `master` runs `.github/workflows/quarto-netlify.yaml`. It
  renders the site and deploys the same build to Netlify (chesterismay.com) and
  to GitHub Pages (ismayc.github.io, kept because other repos read data files
  from it).
- `docs/` is the render output. It is not tracked in git, except for the data
  files and PDFs listed in `.gitignore`, which course material may fetch by
  raw GitHub URL.
- Two post-render scripts in `scripts/` tidy the sitemap and the cookie banner
  text.

To preview locally:

```bash
quarto preview
```

## Scheduled jobs

| Workflow | What it does |
| --- | --- |
| `nba-over-under.yaml` | Daily backup build of the current NBA over/under season page (a Mac mini builds it first). |
| `nba-player-finder.yaml` | Weekly roster refresh for the NBA Player Finder. |
| `pages-proxy-rules.yaml` | Weekly refresh of the rules that serve the other GitHub Pages sites under chesterismay.com. |
| `link-check.yaml` | Weekly check of every link on the main pages. |
| `deploy-shinyapp.yml` | Daily deploy of the over/under points calculator to shinyapps.io. |

## License

- **Code** (R, Python, shell, JavaScript, CSS, and workflow files) is under the
  [MIT License](LICENSE).
- **Writing, images, and analysis pages** are under
  [Creative Commons Attribution 4.0](LICENSE-content.md) (CC BY 4.0).
- **Excluded from both:** third-party data files (such as `dem_score.csv`,
  `pnwflights23.xlsx`, and `spotify_dataset.csv`), which keep their original
  sources' terms; book covers, which belong to their publishers; logos and
  trademarks; and photos of people.
