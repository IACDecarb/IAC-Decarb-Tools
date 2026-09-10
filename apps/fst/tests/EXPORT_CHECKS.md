# Image-export regression checks

Run `Rscript tests/preview_exports.R` with the existing FST packages, then open
`http://127.0.0.1:8771`. Optional arguments are app directory, port, and a directory
for captured PNGs. Add `dense` as the fourth argument to test a synthetic facility
with 16 equipment labels and a tall on-screen diagram. This is a local test
harness, not the deployed application.

The harness preloads the repository's example workbook, applies the same
Content-Security-Policy as both websites (including no `blob:` image permission),
and saves each generated PNG to the printed output directory. Download links
still run normally. PNG signature checks reject non-image responses.

1. Export **Baseline** and **With Savings** for all three Sankey tabs. Open the
   files and confirm colored links, nodes, labels, and the correct chart title.
   Savings diagrams must show their savings flows rather than the baseline diagram.
2. Keep the default width 1000 / height 500 first. Confirm all three diagrams
   download as 5000 × 2500 PNGs with legible labels. Repeat using the dense fixture:
   long labels should wrap, crowded labels should separate, and no label should
   be clipped. The selected dimensions control the layout; raster resolution is
   up to 5×, as with the old server screenshot. Larger images use a lower multiplier
   to stay within 32 million pixels and 20000 pixels per edge.
   Set energy to width 1200 / height 750 and confirm a 6000 × 3750 PNG. Label font
   size is 14 logical pixels normally, with a 12-pixel minimum for crowded
   layouts, independent of the on-screen chart dimensions. If even that cannot
   fit, the exporter asks for more width or height rather than clipping labels.
3. Change a facility name, units, percentage selection, and drag a Sankey node.
   Export again and confirm the visible state and labels are represented.
4. Enter a product name and quantity (for example, Test Product and 100), enable
   all three intensity outputs, and calculate. Export energy, cost, and emissions
   with their camera icons. Repeat after **Calculate Modified Product Intensity**.
   Check that the comparison panels, fuel legend, and labels appear in the PNGs.
5. With an intensity plot visible, click the test harness's **Test original Plotly
   PNG path**. Under the production policy, the old path should report failure;
   the replacement camera must still download a valid PNG.
6. Try a 20000 × 20000 Sankey image and then a blank width. Each must show an
   actionable error and create no PNG. Restore a valid size and confirm retry works.
7. In the normal app (without preloaded data), try a Sankey download before loading
   a workbook. It should ask for inputs rather than downloading an empty file.

Run the dependency-free layout checks with an existing Node.js installation:
`node --test tests/test_sankey_layout.cjs`. They cover dense and scaled diagrams,
wrapping, label separation, insufficient space, and raster memory limits.
Run `Rscript tests/test_export_wiring.R` to check the app wiring and unchanged
500/1000 dimension defaults without loading the app packages.

The exporter uses SVG data URLs for canvas rendering. Blob URLs are used only
for the final download and are released after a delay. No Chrome/Chromote,
Pandoc, external image service, or new R/Python/JavaScript package is needed for
the image downloads. The old webshot screenshot could contain Chrome's file-not-found
page if the server browser could not access R's temporary HTML file. This path
has been removed.

After local checks, use the normal workbook upload on the DEV website and repeat
the exports. Repeat on official only after the reviewed migration and IT deployment.
