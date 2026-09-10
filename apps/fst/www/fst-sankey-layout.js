/* Layout exported Sankey labels in logical image pixels, independently of the
 * geometry's scale. The same pure calculation is exercised by the Node tests. */
(function (root, factory) {
  if (typeof module === "object" && module.exports) module.exports = factory();
  else root.FSTSankeyLayout = factory();
}(typeof window === "object" ? window : globalThis, function () {
  "use strict";

  function wrapLabel(text, width, measure) {
    var words = text.trim().split(/\s+/), lines = [], line = "";
    words.forEach(function (word) {
      var candidate = line ? line + " " + word : word;
      if (measure(candidate) <= width) { line = candidate; return; }
      if (line) { lines.push(line); line = ""; }
      Array.from(word).forEach(function (letter) {
        if (line && measure(line + letter) > width) { lines.push(line); line = ""; }
        line += letter;
      });
    });
    if (line) lines.push(line);
    return lines;
  }

  function arrange(nodes, size, fontSize, measure, hasTitle) {
    if (!nodes.length) throw new Error("Load a Sankey diagram before downloading.");
    if (!Number.isFinite(fontSize) || fontSize < 12 || fontSize > 48) {
      throw new Error("Choose a label text size between 12 and 48 pixels.");
    }
    var left = Math.min.apply(null, nodes.map(function (n) { return n.x; }));
    var top = Math.min.apply(null, nodes.map(function (n) { return n.y; }));
    var right = Math.max.apply(null, nodes.map(function (n) { return n.x + n.width; }));
    var bottom = Math.max.apply(null, nodes.map(function (n) { return n.y + n.height; }));
    if (!(right > left && bottom > top)) throw new Error("Wait for the Sankey diagram to finish rendering.");

    var columns = [];
    nodes.slice().sort(function (a, b) { return a.x - b.x; }).forEach(function (node) {
      var column = columns[columns.length - 1];
      if (!column || Math.abs(node.x - column.x) > 1) {
        column = {x: node.x, nodes: []};
        columns.push(column);
      }
      column.nodes.push(node);
    });
    var padding = 12, gap = 8, lineHeight = fontSize * 1.2, labelGap = 2;
    var titleHeight = hasTitle ? Math.max(36, fontSize + 20) : 0;
    var lastColumn = columns[columns.length - 1];
    // Wrap the outer labels before taking space away from intermediate columns.
    var maxReserve = size.width * 0.3;
    var labelReserve = Math.min(maxReserve, 160);
    var availableHeight = size.height - padding * 2 - titleHeight;
    while (labelReserve < maxReserve) {
      var needed = lastColumn.nodes.reduce(function (total, node) {
        return total + wrapLabel(node.text, labelReserve - gap, measure).length * lineHeight + labelGap;
      }, -labelGap);
      if (needed <= availableHeight) break;
      labelReserve = Math.min(maxReserve, labelReserve + 20);
    }
    var plot = {x: padding, y: padding + titleHeight,
      width: size.width - padding * 2 - labelReserve, height: size.height - padding * 2 - titleHeight};
    var sx = plot.width / (right - left), sy = plot.height / (bottom - top);
    var labels = [];

    columns.forEach(function (column, columnIndex) {
      var nextX = columnIndex < columns.length - 1 ?
        plot.x + (columns[columnIndex + 1].x - left) * sx : size.width - padding + gap;
      var laneLeft = Math.max.apply(null, column.nodes.map(function (n) {
        return plot.x + (n.x + n.width - left) * sx + gap;
      }));
      var laneWidth = nextX - laneLeft - gap;
      if (laneWidth < fontSize * 4) {
        throw new Error("The labels need more horizontal space. Increase the image width.");
      }
      var lane = column.nodes.map(function (node) {
        var center = plot.y + (node.y + node.height / 2 - top) * sy;
        var lines = wrapLabel(node.text, laneWidth, measure);
        return {text: node.text, lines: lines, x: laneLeft, width: laneWidth,
          height: lines.length * lineHeight, center: center,
          nodeRight: plot.x + (node.x + node.width - left) * sx,
          column: columnIndex, fontSize: fontSize};
      }).sort(function (a, b) { return a.center - b.center; });
      var required = lane.reduce(function (total, label) { return total + label.height; }, 0) +
        Math.max(0, lane.length - 1) * labelGap;
      if (required > plot.height) {
        var suggested = Math.ceil((required + titleHeight + padding * 2) / 100) * 100;
        throw new Error("The labels need more vertical space. Increase the image height to at least " +
          suggested + " pixels.");
      }
      var cursor = plot.y;
      lane.forEach(function (label) {
        label.y = Math.max(cursor, label.center - label.height / 2);
        cursor = label.y + label.height + labelGap;
      });
      cursor = plot.y + plot.height;
      for (var i = lane.length - 1; i >= 0; i--) {
        lane[i].y = Math.min(lane[i].y, cursor - lane[i].height);
        cursor = lane[i].y - labelGap;
      }
      labels = labels.concat(lane);
    });
    return {bounds: {x: left, y: top, width: right - left, height: bottom - top},
      plot: plot, labels: labels, lineHeight: lineHeight, titleHeight: titleHeight};
  }
  function rasterSize(size, maxPixels) {
    // Restore the old export's 5x detail while capping large canvas allocations.
    var scale = Math.max(1, Math.min(5, Math.floor(Math.sqrt(maxPixels / (size.width * size.height))),
      Math.floor(20000 / Math.max(size.width, size.height))));
    return {width: size.width * scale, height: size.height * scale, scale: scale};
  }
  return {arrange: arrange, wrapLabel: wrapLabel, rasterSize: rasterSize};
}));
