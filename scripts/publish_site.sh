#!/bin/sh
# render and publish the vignette site (supplementary material) to gh-pages.
#
# tracked deliberately: the repository's ignore policy excludes shell scripts
# by default, with a negation for this file recorded in .gitignore, because the
# publish path is part of the compendium's reproducibility claims (it syncs the
# transcript copy the site serves, renders, and deploys).
#
# usage, from anywhere:
#   sh scripts/publish_site.sh            # sync, render, commit gh-pages, push
#   sh scripts/publish_site.sh --no-push  # everything except the push
set -e
cd "$(dirname "$0")/.."

# 1. the site serves its own copy of the analysis transcript; sync it
cp analysis/run_all.md vignettes/run_all.md
rm -rf vignettes/run_all_files
cp -R analysis/run_all_files vignettes/run_all_files

# 2. render
quarto render vignettes

# 3. deploy _site to the gh-pages branch via a temporary worktree
tmp="$(mktemp -d)"
git worktree add "$tmp" gh-pages
rsync -a --delete --exclude .git vignettes/_site/ "$tmp"/
touch "$tmp/.nojekyll"
(
  cd "$tmp"
  git add -A
  if git diff --cached --quiet; then
    echo "site unchanged; nothing to commit"
  else
    git commit -m "Rebuild site $(date +%Y-%m-%d)"
    if [ "${1:-}" = "--no-push" ]; then
      echo "committed to gh-pages; push skipped (--no-push)"
    else
      git push origin gh-pages
    fi
  fi
)
git worktree remove "$tmp" --force
echo "done: site rendered from vignettes/ and deployed to gh-pages"
