% How to Add a Blog Post — BK Advisors Insights
% A simple, non-technical guide
% Updated May 2026

# What changed (and why it's now easy)

The blog used to need two separate jobs every time you published: rendering the
article **and** hand-editing the home page to add a card for it (in several
places). That is gone.

Now the home page builds **itself** from your posts. To publish, you create one
folder, paste your article into one file, run one command, and push. The home
page card, the category filter, the search box, and the email/RSS feed all
update automatically. No more touching the home page by hand.

A post with no charts (all your LinkedIn articles) needs **no special software**
beyond Quarto and Git.

\newpage

# The 4-step workflow

Do everything from the project folder:
`bk-advisors.github.io`

## Step 1 — Create the post folder

Pick a short "slug" (lowercase words joined by hyphens) — this becomes the web
address, e.g. `the-matthew-effect` → `bk-advisors.github.io/posts/the-matthew-effect/`.

Copy the template into a new folder. In a terminal:

```
mkdir posts\the-matthew-effect
mkdir posts\the-matthew-effect\images
copy posts\_post-template.qmd posts\the-matthew-effect\index.qmd
```

**Success looks like:** a new folder `posts\the-matthew-effect\` containing
`index.qmd` and an empty `images` folder.

## Step 2 — Paste your article and fill in the details

Open `posts\the-matthew-effect\index.qmd` in any text editor.

At the very top, between the two `---` lines, fill in the five fields (keep the
quotation marks):

```
---
title: "The Matthew Effect"
date: "2025-09-30"
categories: [Political Economy]
image: "images/cover.jpg"
description: "One sentence that will appear on the home-page card."
---
```

Then **delete the instructional comment block** and **paste your LinkedIn
article text** below it. Drop your cover picture into the post's `images`
folder and make sure the `image:` line points at its file name.

See *Front-matter at a glance* and the *Formatting cheatsheet* on the next page.

## Step 3 — Build the site

In the terminal, run:

```
quarto render
```

**Success looks like:** it lists each post and ends with
`Output created: index.html`, with no red error text.

> Tip: while writing, use `quarto preview` instead — it opens the site in your
> browser and refreshes as you save, so you can see the post and its home-page
> card before publishing. Press `Ctrl + C` to stop the preview.

## Step 4 — Publish

```
git add .
git commit -m "Add post: The Matthew Effect"
git push
```

Your post is live at `bk-advisors.github.io` within a minute or two. The home
page, categories, search, and feed have already updated themselves.

\newpage

# Front-matter at a glance

| Field | What to put | Example |
|---|---|---|
| `title` | The article headline, in quotes | `"The Matthew Effect"` |
| `date` | Publish date, `YYYY-MM-DD`, in quotes | `"2025-09-30"` |
| `categories` | One or more tags from the list below, in `[ ]` | `[Political Economy, Global Health]` |
| `image` | Cover picture for the card; file must be in `images/` | `"images/cover.jpg"` |
| `description` | One sentence shown on the card and in search | `"Why advantage compounds."` |

**Use only these category names** (spelling matters):
Health Financing · Global Health · Political Economy ·
Tax & Domestic Resource Mobilisation · Vaccines & Immunisation ·
Maternal & Child Health · Grants Management · Trade & Supply Chains ·
Data & Methods

The author is automatically "Matthew Kuch". For a guest author, add a line
`author: "Guest Name"` to the front-matter.

# Formatting cheatsheet (LinkedIn → web)

LinkedIn pasted text often loses its formatting. Fix it with these simple rules:

| You want | Type this |
|---|---|
| A new paragraph | Leave a **blank line** between paragraphs |
| A section heading | `## My Heading` |
| A bullet list | `- first point` (one per line) |
| A numbered list | `1. first point` |
| A quote / callout | `> the quoted sentence` |
| A link | `[the words people see](https://the-address)` |
| A picture | `![caption](images/the-file.jpg)` — put the file in `images/` |
| A real dollar sign | `\$50` (a plain `$` can turn text into maths) |

\newpage

# Before you publish — quick checklist

- [ ] The five front-matter fields are filled in and still wrapped in quotes.
- [ ] The instructional comment block has been deleted.
- [ ] The cover image and any in-article images are inside the post's
      `images/` folder, and the file names match exactly (capitals count).
- [ ] You ran `quarto preview` and the post looks right, **and** a card for it
      appears on the home page.
- [ ] Dollar amounts are written `\$`.
- [ ] You ran `quarto render`, then `git add . / commit / push`.

# If something looks wrong

**A picture doesn't show up.** The file must be in this post's own `images/`
folder, and the name in the text must match the file exactly, including
capital letters and the extension (`.jpg` vs `.png`).

**The post isn't on the home page.** Two usual causes: (1) the front-matter is
missing a closing `---` or a quote mark — re-check Step 2; (2) the front-matter
has a line `draft: true` — delete that line to publish it.

**An error mentioning R or "knitr".** Only the older "Cost of Staying Alive"
post uses R for its chart. Your LinkedIn articles don't use R, so this should
not happen for new posts. If you edited that specific post, it needs R to be
available — ask for help re-rendering it.

**Still stuck.** Run `quarto preview`; the error message in the terminal
usually points at the file and the line number. The full technical notes are
in `CLAUDE.md` in the project folder.
