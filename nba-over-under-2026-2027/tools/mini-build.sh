#!/bin/zsh
# Daily over/under page build on the Mac mini, with GitHub Actions as the
# backup. Does the same steps as .github/workflows/nba-over-under.yaml:
# page build, restyle, push, deploy dispatch, Python cross-check. The R and Python
# calculations are a pinned release of github.com/ismayc/ouanalytics, tested there.
#
# If any step before the push fails, it dispatches the GitHub workflow, so
# the page still updates (and a real fault shows up as a red GitHub run).
# A Python cross-check difference does not hold back the page: it opens a
# GitHub issue instead.
#
# Run by launchd at 2:00 AM Phoenix (com.chesterismay.nba-over-under-build).
# Set BUILD_DRY_RUN=1 to run every step except the commit, push, dispatch,
# and issue.
#
# The mini's home is /Users/Chester; never write the MacBook's home here.

# A reset of the repo replaces files, this one included, so run a copy
if [[ -z ${NBA_BUILD_SELF:-} ]]; then
  SELF_COPY="$(mktemp -t nba-build).sh"
  cp "$0" "$SELF_COPY"
  NBA_BUILD_SELF=1 exec /bin/zsh "$SELF_COPY" "$@"
fi

set -uo pipefail

SLUG="ismayc/ismayc.github.io"
REPO_DIR="${NBA_BUILD_DIR:-$HOME/repos/ismayc.github.io}"
PROJECT="nba-over-under-2026-2027"
DRY_RUN="${BUILD_DRY_RUN:-0}"
# The ouanalytics release (change here and in the workflow to upgrade)
OUANALYTICS_REF="${OUANALYTICS_REF:-v0.1.1}"
# Branch to build from; only a test of a change to this script sets it
BRANCH="${NBA_BUILD_BRANCH:-master}"
# Same dates as check-date in the workflow
FIRST_BUILD_DATE="2026-10-21"
CUTOFF_DATE="2027-04-12"

export PATH="/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"
export RETICULATE_PYTHON="$HOME/.virtualenvs/nba-over-under/bin/python"
export R_REMOTES_NO_ERRORS_FROM_WARNINGS=true

log() { print -r -- "$(date '+%F %T') $*"; }

fallback_to_github() {
  log "FALLBACK: $1"
  if [[ "$DRY_RUN" == 1 ]]; then
    log "dry run: not dispatching the GitHub workflow"
  else
    gh workflow run nba-over-under.yaml --repo "$SLUG" --ref master \
      && log "dispatched the GitHub workflow"
  fi
  exit 1
}

# One build at a time
LOCK="$HOME/Library/Caches/nba-over-under-build.lock"
mkdir -p "${LOCK:h}"
if ! mkdir "$LOCK" 2>/dev/null; then
  log "another build holds $LOCK; stopping"
  exit 0
fi
trap 'rmdir "$LOCK" 2>/dev/null' EXIT

CURRENT_DATE="$(date -u +%Y-%m-%d)"
if [[ "$CURRENT_DATE" < "$FIRST_BUILD_DATE" ]]; then
  log "no games played yet; nothing to do"; exit 0
fi
if [[ ! "$CURRENT_DATE" < "$CUTOFF_DATE" ]]; then
  log "past the season cutoff; nothing to do"; exit 0
fi

cd "$REPO_DIR" || fallback_to_github "no clone at $REPO_DIR"
log "start; repo $REPO_DIR"

git fetch -q origin "$BRANCH" || fallback_to_github "git fetch failed"
git reset -q --hard "origin/$BRANCH" && git clean -fdq || fallback_to_github "git reset failed"
START_SHA="$(git rev-parse HEAD)"
log "at $START_SHA"

if [[ ! -f "$PROJECT/rds/gs_picks_raw.rds" ]]; then
  log "no picks saved yet; nothing to do"; exit 0
fi

log "installing ouanalytics $OUANALYTICS_REF from GitHub"
export OUANALYTICS_REF
Rscript --vanilla -e 'remotes::install_github(paste0("ismayc/ouanalytics@", Sys.getenv("OUANALYTICS_REF")), subdir = "r", upgrade = "never", force = TRUE)' >/dev/null 2>&1 \
  || fallback_to_github "installing ouanalytics $OUANALYTICS_REF failed"
Rscript --vanilla -e 'cat("ouanalytics", as.character(packageVersion("ouanalytics")), "\n")' \
  || fallback_to_github "ouanalytics is not installed"

log "building the page"
Rscript "$PROJECT/make_plot_html.R" || fallback_to_github "page build failed"

log "restyling"
if [[ -f 2027-nba-rooting-guide.html ]]; then
  python3 nba-over-under-META/site-style/restyle_season_pages.py guide 2027 \
    || fallback_to_github "restyle guide failed"
fi
if [[ -f 2027-nba-scenario-explorer.html ]]; then
  python3 nba-over-under-META/site-style/restyle_season_pages.py explorer 2027 \
    || fallback_to_github "restyle explorer failed"
fi
python3 nba-over-under-META/site-style/restyle_season_pages.py season 2027 \
  || fallback_to_github "restyle season failed"

if [[ "$DRY_RUN" == 1 ]]; then
  log "dry run: built and restyled; skipping commit, push, deploy"
  git status --short | head -15
else
  git add -A
  if git diff --cached --quiet; then
    log "no changes to commit"
  else
    # [skip ci] keeps this push from starting a second build on GitHub
    git commit -q -m "Mini NBA page build [skip ci]" || fallback_to_github "commit failed"
    git fetch -q origin master || fallback_to_github "git fetch before push failed"
    if [[ "$(git rev-parse origin/master)" != "$START_SHA" ]]; then
      log "origin/master moved during the build; rebasing"
      git rebase -q origin/master \
        || { git rebase --abort; fallback_to_github "rebase conflict"; }
    fi
    git push -q origin master || fallback_to_github "push failed"
    log "pushed $(git rev-parse --short HEAD)"
    # A push with [skip ci] does not start the site deploy, so ask for it
    gh workflow run quarto-netlify.yaml --repo "$SLUG" --ref master \
      && log "dispatched the site deploy"
  fi
fi

# The page is out. Recompute today's tables in Python and compare.
log "Python cross-check"
CHECK_LOG="$(mktemp -t nba-crosscheck)"
(
  cd "$PROJECT" \
    && uvx --quiet --from "git+https://github.com/ismayc/ouanalytics@${OUANALYTICS_REF}#subdirectory=python" \
         ouanalytics-check "check-export/pool" "check-export/seeding"
) >"$CHECK_LOG" 2>&1
CHECK_STATUS=$?
tail -15 "$CHECK_LOG"
if [[ $CHECK_STATUS -ne 0 ]]; then
  log "CROSS-CHECK DIFFERENCE (exit $CHECK_STATUS)"
  if [[ "$DRY_RUN" == 1 ]]; then
    log "dry run: not opening an issue"
  else
    {
      print "The Python (polars) recomputation disagrees with R, or its tests failed."
      print "The page was already published. Date (UTC): $CURRENT_DATE. Commit: $(git rev-parse --short HEAD)."
      print
      print '```'
      tail -60 "$CHECK_LOG"
      print '```'
    } > "$CHECK_LOG.md"
    gh issue create --repo "$SLUG" \
      --title "Over/under cross-check: Python and R disagree ($CURRENT_DATE)" \
      --body-file "$CHECK_LOG.md" && log "opened an issue"
  fi
  exit 2
fi
log "done: cross-check matches"
