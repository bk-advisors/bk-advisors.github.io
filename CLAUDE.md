# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

`bk-advisors.github.io` is a **single Quarto website** for the BK Advisors insights blog. There is no CI: Quarto renders the whole site into **`docs/`** (`output-dir: docs` in [_quarto.yml](_quarto.yml)) and **`docs/` is committed** — GitHub Pages serves it directly. Publishing = render locally, commit, push.

**One-time GitHub setting (required):** repo **Settings → Pages → Build and deployment → Source: "Deploy from a branch", Branch: `main`, Folder: `/docs`**. (`output-dir: .` was tried first but breaks `quarto preview`/RStudio with *"Source and destination cannot be the same"*; `/docs` is the supported no-CI path.) `_freeze/` and `.quarto/` stay at the repo root (project cache); only `/.quarto/` is gitignored.

This replaced an older dual system (a hand-coded `index.html` card grid + a separate Quarto project under `blog/`). That legacy build is preserved in the git tag **`legacy-v1`** (and branch `legacy-v1-snapshot`); it is not in the working tree.

## Architecture (the important parts)

- **The landing page is auto-generated.** [index.qmd](index.qmd) is just a Quarto `listing:` (grid, category cloud, RSS feed). It discovers every `posts/*/index.qmd` automatically. **Never hand-edit a post list** — adding a post folder is all that's needed.
- **One post = one folder:** `posts/<slug>/index.qmd` with its images in `posts/<slug>/images/`. Clean URL: `/posts/<slug>/`.
- **Shared defaults** for all posts live in [posts/_metadata.yml](posts/_metadata.yml) (author, html format, `freeze`). Post front-matter only needs `title`, `date`, `categories`, `image`, `description` (override a default by repeating the key).
- **Template:** [posts/_post-template.qmd](posts/_post-template.qmd). The `_` prefix means Quarto ignores it (not rendered, not listed).
- **Theme/branding:** [theme/brand-light.scss](theme/brand-light.scss) (Bootstrap `cosmo` + brand palette `#0d6efd` primary / `#e5001c` accent / Inter font) and [theme/styles.css](theme/styles.css) (small non-Sass tweaks).
- **`render:` allowlist** in [_quarto.yml](_quarto.yml) is `index.qmd` + `posts/*/index.qmd` only — `README.md` / `CLAUDE.md` / `sandbox/` are never rendered. `assets/`, the legacy redirect stubs in `blog/posts/`, and the standalone `lab/` project are copied verbatim into `docs/` via the `resources:` list, so their URLs (e.g. `/lab/displacement-effect/`) keep working under `/docs`.

## Adding / updating a post (the whole workflow)

```
mkdir -p posts/<slug>/images
cp posts/_post-template.qmd posts/<slug>/index.qmd
#   ...edit front-matter + paste body; put images in posts/<slug>/images/...
quarto render                             # or `quarto preview` while writing
git add posts/<slug>/ docs/               # commit BOTH the source folder and the rebuilt docs/
git commit && git push
```

The homepage grid, category cloud, search index, and RSS feed regenerate automatically. A non-technical version of this is in **[How-to-Add-a-Blog-Post.md](How-to-Add-a-Blog-Post.md)** / `.docx`.

Controlled category vocabulary (keep tags to this set): `Health Financing`, `Global Health`, `Political Economy`, `Tax & Domestic Resource Mobilisation`, `Vaccines & Immunisation`, `Maternal & Child Health`, `Grants Management`, `Trade & Supply Chains`, `Data & Methods`.

## R-backed posts & `freeze`

Most posts are pure Markdown (no R, render instantly, no R install needed). Only **`posts/health-financing/`** executes R: it pulls a sibling child doc `eac-health-exp-line.Rmd` (a `highcharter` chart) reading `posts/health-financing/data/eac-govt-health-exp.csv`.

- `execute: freeze: auto` + the committed `_freeze/posts/health-financing/index/` mean this chart is **replayed from cache** on render — so the site rebuilds **without R** as long as that post's `index.qmd` content is unchanged.
- If you edit `posts/health-financing/index.qmd` or its child/data, Quarto re-executes it: you then need **R on PATH** (R 4.4.1 is installed at `C:\Program Files\R\R-4.4.1\bin`, not on PATH by default — prepend it for that render) with packages `highcharter, tidyverse, readxl`. Commit the regenerated `_freeze/` afterward.

## Gotchas

- **`.nojekyll` is mandatory.** Quarto auto-creates `docs/.nojekyll`; without it GitHub's Jekyll strips `site_libs/` and all CSS/JS breaks. (A root `.nojekyll` is also kept and copied via `resources:` as a belt-and-braces.)
- **The committed `docs/` is the deployed site.** Never hand-edit files in `docs/` — they are overwritten on every render. Edit sources at the repo root (`*.qmd`, `theme/`, `assets/`, `blog/posts/` stubs) and re-render. `.gitignore` only excludes `/.quarto/` and R cruft.
- **Old shared links** `https://bk-advisors.github.io/blog/posts/<slug>.html` are kept alive by 5 redirect stubs in [blog/posts/](blog/posts/) (meta-refresh → `/posts/<slug>/`); `resources:` copies them into `docs/blog/posts/`. Don't delete the root `blog/` folder — those stubs are load-bearing for inbound links.
- **`posts/why-african-govts-struggle/`** is `draft: true` (unfinished — contains placeholder sections). It renders but is excluded from the homepage/search until the `draft:` line is removed.
- Render with **Quarto ≥ 1.8** (developed on 1.8.27). Since there's no CI, the committed HTML is whatever the local machine produced.
