const test = require('node:test');
const assert = require('node:assert/strict');
const { arrange, wrapLabel, rasterSize } = require('../www/fst-sankey-layout.js');
const measure = text => Array.from(text).length * 7;
function dense(scale = 1) {
  return [
    {x: 0, y: 0, width: 30, height: 940 * scale, text: 'Total Energy (145,557.8)'},
    {x: 250, y: 0, width: 30, height: 250 * scale, text: 'Fuel (41,622)'},
    {x: 250, y: 270 * scale, width: 30, height: 670 * scale, text: 'Electricity (103,935.8)'},
    ...Array.from({length: 16}, (_, i) => ({x: 500, y: (i < 9 ? i * 92 : 830 + (i - 9) * 14) * scale,
      width: 30, height: (i < 9 ? 85 : 5) * scale,
      text: ['Flexo Press with Inline Cutter – Packaging Production Line 14 (30,186.2)', 'Air Cooled Chillers (1,440)',
        'Downstream Tunnel Dryer 11 (3,926.6)', 'HVAC Pumps (1,879)'][i % 4]}))
  ];
}
function checkLabels(layout, size) {
  for (const label of layout.labels) {
    assert.ok(label.x >= 0 && label.x + label.width <= size.width);
    assert.ok(label.y >= layout.plot.y - 1e-8);
    assert.ok(label.y + label.height <= size.height - 12 + 1e-8);
    assert.ok(label.lines.every(line => measure(line) <= label.width));
    assert.equal(label.lines.join('').replace(/\s/g, ''), label.text.replace(/\s/g, ''));
  }
  for (const col of new Set(layout.labels.map(label => label.column))) {
    const labels = layout.labels.filter(label => label.column === col);
    for (let i = 1; i < labels.length; i++) {
      assert.ok(labels[i].y >= labels[i - 1].y + labels[i - 1].height + 2 - 1e-8);
    }
  }
}
test('dense labels retain a readable pixel size, wrap and stay separated', () => {
  const size = {width: 1000, height: 500};
  const layout = arrange(dense(), size, 14, measure, true);
  checkLabels(layout, size);
  assert.ok(layout.labels.every(label => label.fontSize === 14));
  assert.ok(layout.labels.some(label => label.lines.length > 1));
  assert.ok(layout.labels.some(label => Math.abs(label.y + label.height / 2 - label.center) > 2));
});
test('a taller on-screen chart cannot shrink exported labels', () => {
  const size = {width: 1000, height: 500};
  const normal = arrange(dense(), size, 14, measure, true);
  const tall = arrange(dense(3), size, 14, measure, true);
  normal.labels.forEach((label, i) => {
    assert.equal(tall.labels[i].fontSize, 14);
    assert.deepEqual(tall.labels[i].lines, label.lines);
    assert.ok(Math.abs(tall.labels[i].y - label.y) < 1e-8);
  });
});
test('insufficient image height reports a size adjustment rather than shrinking text', () => {
  assert.throws(() => arrange(dense(), {width: 1000, height: 300}, 14, measure, true), /height to at least/);
});
test('default dimensions restore 5x resolution and larger images stay within canvas limits', () => {
  assert.deepEqual(rasterSize({width: 1000, height: 500}, 32000000), {width: 5000, height: 2500, scale: 5});
  assert.deepEqual(rasterSize({width: 1200, height: 750}, 32000000), {width: 6000, height: 3750, scale: 5});
  for (const size of [{width: 1600, height: 1000}, {width: 20000, height: 500}, {width: 5000, height: 5000}]) {
    const png = rasterSize(size, 32000000);
    assert.ok(png.width * png.height <= 32000000);
    assert.ok(png.width <= 20000 && png.height <= 20000);
    assert.ok(png.width >= size.width && png.height >= size.height);
  }
});
test('insufficient width and invalid font sizes fail with useful messages', () => {
  const nodes = dense().concat({x: 35, y: 0, width: 30, height: 50, text: 'Extra column'});
  assert.throws(() => arrange(nodes, {width: 750, height: 1000}, 22, measure, true), /image width/);
  for (const font of [0, 11, 49, NaN]) {
    assert.throws(() => arrange(dense(), {width: 1600, height: 1200}, font, measure, true), /12 and 48/);
  }
});
test('Unicode and unbroken long equipment names are retained without clipping', () => {
  const text = 'ÉquipementTrèsLongSansEspaces – Café CO₂e (1,234.5)';
  const lines = wrapLabel(text, 170, measure);
  assert.ok(lines.every(line => measure(line) <= 170));
  assert.equal(lines.join('').replace(/\s/g, ''), text.replace(/\s/g, ''));
});
