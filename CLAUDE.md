# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

`bk-advisors.github.io` is a **single Quarto website** for the BK Advisors insights blog, served by **GitHub Pages from the repo root of `main`**. There is no CI and no build step on GitHub's side: Quarto renders the site **in place** (`output-dir: .` in [_quarto.yml](_quarto.yml)) and the rendered `.html` / `site_libs/` / `search.json` / `_freeze/` are **committed on purpose** — Pages serves them directly. Publishing = render locally, commit, push.

This replaced an older dual system (a hand-coded `index.html` card grid + a separate Quarto project under `blog/`). That legacy build is preserved in the git tag **`legacy-v1`** (and branch `legacy-v1-snapshot`); it is not in the working tree.

## Architecture (the important parts)

- **The landing page is auto-generated.** [index.qmd](index.qmd) is just a Quarto `listing:` (grid, category cloud, RSS feed). It discovers every `posts/*/index.qmd` automatically. **Never hand-edit a post list** — adding a post folder is all that's needed.
- **One post = one folder:** `posts/<slug>/index.qmd` with its images in `posts/<slug>/images/`. Clean URL: `/posts/<slug>/`.
- **Shared defaults** for all posts live in [posts/_metadata.yml](posts/_metadata.yml) (author, html format, `freeze`). Post front-matter only needs `title`, `date`, `categories`, `image`, `description` (override a default by repeating the key).
- **Template:** [posts/_post-template.qmd](posts/_post-template.qmd). The `_` prefix means Quarto ignores it (not rendered, not listed).
- **Theme/branding:** [theme/brand-light.scss](theme/brand-light.scss) (Bootstrap `cosmo` + brand palette `#0d6efd` primary / `#e5001c` accent / Inter font) and [theme/styles.css](theme/styles.css) (small non-Sass tweaks).
- **`render:` allowlist** in [_quarto.yml](_quarto.yml) is `index.qmd` + `posts/*/index.qmd` only — so `README.md`, `CLAUDE.md`, `lab/`, and `sandbox/` are deliberately **not** part of the website and are left byte-untouched (they remain standalone and keep serving at their existing URLs).

## Adding / updating a post (the whole workflow)

```
mkdir -p posts/<slug>/images
cp posts/_post-template.qmd posts/<slug>/index.qmd
#   ...edit front-matter + paste body; put images in posts/<slug>/images/...
quarto render posts/<slug>/index.qmd      # or `quarto render` for the whole site
quarto preview                            # eyeball the homepage card + the post
git add posts/<slug>/ index.html posts/ search.json listings.json index.xml sitemap.xml site_libs/
git commit && git push
```

The homepage grid, category cloud, search index, and RSS feed regenerate automatically. A non-technical version of this is in **[How-to-Add-a-Blog-Post.md](How-to-Add-a-Blog-Post.md)** / `.docx`.

Controlled category vocabulary (keep tags to this set): `Health Financing`, `Global Health`, `Political Economy`, `Tax & Domestic Resource Mobilisation`, `Vaccines & Immunisation`, `Maternal & Child Health`, `Grants Management`, `Trade & Supply Chains`, `Data & Methods`.

## R-backed posts & `freeze`

Most posts are pure Markdown (no R, render instantly, no R install needed). Only **`posts/health-financing/`** executes R: it pulls a sibling child doc `eac-health-exp-line.Rmd` (a `highcharter` chart) reading `posts/health-financing/data/eac-govt-health-exp.csv`.

- `execute: freeze: auto` + the committed `_freeze/posts/health-financing/index/` mean this chart is **replayed from cache** on render — so the site rebuilds **without R** as long as that post's `index.qmd` content is unchanged.
- If you edit `posts/health-financing/index.qmd` or its child/data, Quarto re-executes it: you then need **R on PATH** (R 4.4.1 is installed at `C:\Program Files\R\R-4.4.1\bin`, not on PATH by default — prepend it for that render) with packages `highcharter, tidyverse, readxl`. Commit the regenerated `_freeze/` afterward.

## Gotchas

- **`.nojekyll` is mandatory** (empty file at root). Without it GitHub's Jekyll strips `site_libs/` and all CSS/JS breaks.
- **Committed render output is intentional.** `.gitignore` only excludes `/.quarto/` (project cache) and R cruft — do **not** add `*.html` / `site_libs/` / `_freeze/` to it.
- **Old shared links** `https://bk-advisors.github.io/blog/posts/<slug>.html` are kept alive by 5 redirect stubs in [blog/posts/](blog/posts/) (meta-refresh → `/posts/<slug>/`). Don't "clean up" the `blog/` folder — these stubs are the only thing in it and are load-bearing for inbound links.
- **`posts/why-african-govts-struggle/`** is `draft: true` (unfinished — contains placeholder sections). It renders but is excluded from the homepage/search until the `draft:` line is removed.
- Render with **Quarto ≥ 1.8** (developed on 1.8.27). Since there's no CI, the committed HTML is whatever the local machine produced.
