#!/usr/bin/env bash
#
# Assembles the files requested by the Biometrics editorial office for the
# final (accepted) version of BIOM2025402P.R2.
#
# Everything it writes lands in submissions/2026-08_Biometrics-final; the
# sources it reads from stay where they are in the repository. Run it from the
# repository root, or via `make submission`, after main.pdf and
# supplemental.pdf have been built.
#
# Outputs:
#   fig1.svg ... fig5.svg      print-quality figures, one file per figure,
#                              named to match their citation order, fonts
#                              embedded
#   main.pdf                   the Main Document (fonts embedded)
#   supplemental.pdf           the Supplementary Materials (fonts embedded)
#   latex-source/              self-contained LaTeX sources for the main
#                              manuscript (no supplementary material)
#   latex-source.zip           the same, zipped
#   code-and-data.zip          R scripts and data for the Open Materials Badge
#   figure-captions.txt        the captions as UTF-8, to paste into ScholarOne
#
# By default the two ~64 MB posterior draw files are left out of
# code-and-data.zip to keep it uploadable; pass --with-draws to include them.

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$ROOT"

OUT="submissions/2026-08_Biometrics-final"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

WITH_DRAWS=0
[[ "${1:-}" == "--with-draws" ]] && WITH_DRAWS=1

# Figures, in the order they are first cited in main.tex. The Biometrics
# guidelines ask for one file per figure, named fig1, fig2, ...
FIGURES=(challenges regions_diag sim-results CIS_final CIS_vary)

need() { command -v "$1" >/dev/null || { echo "missing required tool: $1" >&2; exit 1; }; }
need gs; need pdflatex; need zip; need pdfinfo; need inkscape

# Ghostscript pass that re-embeds every font. R's pdf() device references the
# base-14 Helvetica/Symbol rather than embedding them, and the journal requires
# embedded fonts; this substitutes the metrically identical URW faces and
# embeds them without otherwise altering the page.
embed_fonts() {
  gs -q -dNOPAUSE -dBATCH -dSAFER -sDEVICE=pdfwrite \
     -dEmbedAllFonts=true -dSubsetFonts=true \
     -dPDFSETTINGS=/prepress -dAutoRotatePages=/None \
     -o "$2" "$1"
}

rm -rf "$OUT"
mkdir -p "$OUT/latex-source"

echo "==> figures"
i=0
for f in "${FIGURES[@]}"; do
  i=$((i + 1))
  svg="figures/output/$f.svg"
  [[ -f "$svg" ]] || { echo "missing $svg -- run 'make all' first" >&2; exit 1; }
  # SVG is the one vector format that can carry these plots' semi-transparency,
  # so the whole figure stays geometry. Guard against that quietly regressing:
  # an <image> element or a base64 payload would mean part of the page got
  # flattened to a bitmap somewhere in the pipeline.
  if grep -q '<image' "$svg" || grep -q 'base64' "$svg"; then
    echo "$svg contains raster data; the figure is no longer fully vector" >&2
    exit 1
  fi
  cp "$svg" "$OUT/fig$i.svg"
  cp "$svg" "$OUT/latex-source/fig$i.svg"
  printf '    fig%d.svg  <- %s\n' "$i" "$svg"
done

echo "==> manuscript PDFs"
embed_fonts main.pdf "$OUT/main.pdf"
embed_fonts supplemental.pdf "$OUT/supplemental.pdf"
for p in main supplemental; do
  a=$(pdfinfo "$p.pdf" | awk '/^Pages:/ {print $2}')
  b=$(pdfinfo "$OUT/$p.pdf" | awk '/^Pages:/ {print $2}')
  [[ "$a" == "$b" ]] || { echo "page count changed for $p.pdf ($a -> $b)" >&2; exit 1; }
  printf '    %-16s %s pages\n' "$p.pdf" "$b"
done

echo "==> LaTeX source bundle"
# Same main.tex, with the \includesvg paths pointed at the fig1..fig5 files
# that accompany it, so the bundle is self-contained.
sed -e 's|figures/output/challenges|fig1|' \
    -e 's|figures/output/regions_diag|fig2|' \
    -e 's|figures/output/sim-results|fig3|' \
    -e 's|figures/output/CIS_final|fig4|' \
    -e 's|figures/output/CIS_vary|fig5|' \
    main.tex > "$OUT/latex-source/main.tex"
grep -q 'figures/output' "$OUT/latex-source/main.tex" && {
  echo "a figure path was not rewritten" >&2; exit 1; }

cp biom.cls biom.bst endrotfloat.sty references.bib "$OUT/latex-source/"
cp latex.out/main.bbl "$OUT/latex-source/main.bbl"

# Confirm the bundle really is self-contained by building it somewhere else.
# The svg package converts the figures during the run, which needs shell-escape.
cp -r "$OUT/latex-source" "$WORK/build"
(
  cd "$WORK/build"
  for _ in 1 2; do
    pdflatex -shell-escape -interaction=nonstopmode -halt-on-error main.tex >/dev/null
  done
) || { echo "the LaTeX source bundle does not compile" >&2; exit 1; }
built=$(pdfinfo "$WORK/build/main.pdf" | awk '/^Pages:/ {print $2}')
expected=$(pdfinfo "$OUT/main.pdf" | awk '/^Pages:/ {print $2}')
[[ "$built" == "$expected" ]] || {
  echo "source bundle builds $built pages, expected $expected" >&2; exit 1; }
echo "    compiles cleanly: $built pages"

(cd "$OUT" && zip -qr latex-source.zip latex-source)

echo "==> code and data archive"
PKG="$WORK/code-and-data"
mkdir -p "$PKG/figures/R" "$PKG/data"
cp figures/R/*.R "$PKG/figures/R/"
cp -r data/. "$PKG/data/"
if [[ "$WITH_DRAWS" -eq 0 ]]; then
  rm -f "$PKG/data/STATS17701/draws.rds" "$PKG/data/STATS18744/draws.rds"
fi
cp submissions/README-code-and-data.md "$PKG/README.md"
(cd "$WORK" && zip -qr "$ROOT/$OUT/code-and-data.zip" code-and-data)

# The figure captions, as UTF-8, for pasting into ScholarOne's caption boxes.
cp submissions/figure-captions.txt "$OUT/figure-captions.txt"

echo
echo "==> $OUT"
ls -la "$OUT" | tail -n +2
