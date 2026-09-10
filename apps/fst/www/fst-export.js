/* Export rendered charts without a server browser or blob: image sources.
 * Both FST hosts allow data: images in their Content-Security-Policy.
 * Blob URLs are used only for the final download, never as image sources.
 */
(function () {
  "use strict";

  var SVG_NS = "http://www.w3.org/2000/svg";
  var MAX_PIXELS = 32000000;
  var pending = new WeakSet();
  var sankeys = {
    downloadPNG_e: {
      plots: ["sankey_energy", "sankey_energy_new"],
      titles: ["output_text_e", "output_text_e_new"],
      width: "width_e", height: "height_e", filename: "Energy_Flow"
    },
    downloadPNG_ec: {
      plots: ["sankey_energy_costs", "sankey_energy_costs_new"],
      titles: ["output_text_ec", "output_text_ec_new"],
      width: "width_ec", height: "height_ec", filename: "Energy_Cost_Flow"
    },
    downloadPNG: {
      plots: ["sankey", "sankey_new"],
      titles: ["output_text", "output_text_new"],
      width: "width", height: "height", filename: "CO2_Flow"
    }
  };

  function dimensions(width, height) {
    width = Number(width);
    height = Number(height);
    if (!Number.isInteger(width) || !Number.isInteger(height) || width < 1 || height < 1) {
      throw new Error("Enter a positive whole number for image width and height.");
    }
    if (width > 20000 || height > 20000 || width * height > MAX_PIXELS) {
      throw new Error("This image is too large. Reduce the width or height to use at most 32 million pixels (for example, 8000 by 4000).");
    }
    return {width: width, height: height};
  }

  function notify(message, type) {
    if (window.Shiny && window.Shiny.notifications) {
      window.Shiny.notifications.show({
        id: "fst-image-export", html: message, type: type || "default",
        duration: type === "error" ? null : 5, closeButton: true
      });
    }
  }

  function dateSuffix() {
    var now = new Date();
    return now.getFullYear() + "-" + String(now.getMonth() + 1).padStart(2, "0") +
      "-" + String(now.getDate()).padStart(2, "0");
  }

  function savePng(blob, filename) {
    if (!blob || blob.type !== "image/png" || !blob.size) {
      throw new Error("The browser could not create this image. Try a smaller image size.");
    }
    var url = URL.createObjectURL(blob);
    var link = document.createElement("a");
    link.href = url;
    link.download = filename + "_" + dateSuffix() + ".png";
    document.body.appendChild(link);
    link.click();
    link.remove();
    // Allow the browser time to consume the download before releasing its URL.
    window.setTimeout(function () { URL.revokeObjectURL(url); }, 60000);
  }

  function svgToPng(svgUrl, size) {
    // Plotly's default PNG path loads an SVG blob URL, which the hosts block.
    // Its SVG export is a data URL and can be drawn under the existing policy.
    if (!/^data:image\/svg\+xml[;,]/i.test(svgUrl)) {
      return Promise.reject(new Error("The chart did not produce a valid SVG image."));
    }
    return new Promise(function (resolve, reject) {
      var image = new Image();
      var canvas = document.createElement("canvas");
      var timer = window.setTimeout(function () {
        finish(new Error("Image export took too long. Try a smaller image size."));
      }, 20000);
      var done = false;
      function finish(error, blob) {
        if (done) return;
        done = true;
        window.clearTimeout(timer);
        image.onload = image.onerror = null;
        canvas.width = canvas.height = 0;
        if (error) reject(error); else resolve(blob);
      }
      image.onerror = function () {
        finish(new Error("The browser could not render this chart as an image."));
      };
      image.onload = function () {
        try {
          canvas.width = size.width;
          canvas.height = size.height;
          var context = canvas.getContext("2d");
          if (!context) throw new Error("The browser could not allocate this image. Try a smaller size.");
          context.fillStyle = "white";
          context.fillRect(0, 0, size.width, size.height);
          context.drawImage(image, 0, 0, size.width, size.height);
          canvas.toBlob(function (blob) {
            finish(blob ? null : new Error("Image creation failed. Try a smaller size."), blob);
          }, "image/png");
        } catch (error) { finish(error); }
      };
      image.src = svgUrl;
    });
  }

  function styledClone(svg) {
    var clone = svg.cloneNode(true);
    var originals = [svg].concat(Array.from(svg.querySelectorAll("*")));
    var copies = [clone].concat(Array.from(clone.querySelectorAll("*")));
    var properties = [
      "fill", "fill-opacity", "stroke", "stroke-width", "stroke-opacity",
      "stroke-dasharray", "opacity", "font-family", "font-size", "font-weight",
      "font-style", "text-anchor", "dominant-baseline", "visibility", "display"
    ];
    originals.forEach(function (element, index) {
      var style = window.getComputedStyle(element);
      properties.forEach(function (property) {
        copies[index].style.setProperty(property, style.getPropertyValue(property));
      });
    });
    return clone;
  }

  function sankeyNodes(svg) {
    var rootMatrix = svg.getScreenCTM();
    if (!rootMatrix) throw new Error("Wait for the Sankey diagram to finish rendering.");
    return Array.from(svg.querySelectorAll(".node")).map(function (node) {
      var rect = node.querySelector("rect"), text = node.querySelector("text");
      if (!rect || !text) throw new Error("The Sankey diagram has an incomplete node. Please recalculate it.");
      var box = rect.getBBox();
      var matrix = rootMatrix.inverse().multiply(rect.getScreenCTM());
      var points = [[box.x, box.y], [box.x + box.width, box.y],
        [box.x, box.y + box.height], [box.x + box.width, box.y + box.height]].map(function (xy) {
        var point = svg.createSVGPoint();
        point.x = xy[0]; point.y = xy[1];
        return point.matrixTransform(matrix);
      });
      var x = Math.min.apply(null, points.map(function (p) { return p.x; }));
      var y = Math.min.apply(null, points.map(function (p) { return p.y; }));
      return {x: x, y: y, text: text.textContent,
        width: Math.max.apply(null, points.map(function (p) { return p.x; })) - x,
        height: Math.max.apply(null, points.map(function (p) { return p.y; })) - y};
    });
  }

  function sankeySvg(svg, title, size, fontSize) {
    var measure = document.createElement("canvas").getContext("2d");
    measure.font = fontSize + "px Arial";
    var layout = window.FSTSankeyLayout.arrange(sankeyNodes(svg), size, fontSize,
      function (text) { return measure.measureText(text).width; }, Boolean(title));
    var root = document.createElementNS(SVG_NS, "svg");
    root.setAttribute("xmlns", SVG_NS);
    root.setAttribute("width", size.width);
    root.setAttribute("height", size.height);
    root.setAttribute("viewBox", "0 0 " + size.width + " " + size.height);
    if (title) {
      var titleFont = 20;
      var heading = document.createElementNS(SVG_NS, "text");
      heading.setAttribute("x", size.width / 2);
      heading.setAttribute("y", 10 + titleFont);
      heading.setAttribute("text-anchor", "middle");
      heading.setAttribute("font-family", "Arial, sans-serif");
      heading.setAttribute("font-size", titleFont);
      heading.setAttribute("fill", "#222");
      heading.textContent = title;
      measure.font = titleFont + "px Arial";
      if (measure.measureText(title).width > size.width - 24) {
        heading.setAttribute("textLength", size.width - 24);
        heading.setAttribute("lengthAdjust", "spacingAndGlyphs");
      }
      root.appendChild(heading);
    }
    // Scale only the flows and nodes. Labels live in the outer SVG, so their
    // readable layout size does not depend on screen size or vertical scaling.
    var clone = styledClone(svg);
    clone.removeAttribute("id");
    clone.querySelectorAll(".node text").forEach(function (text) { text.remove(); });
    clone.setAttribute("x", layout.plot.x);
    clone.setAttribute("y", layout.plot.y);
    clone.setAttribute("width", layout.plot.width);
    clone.setAttribute("height", layout.plot.height);
    clone.setAttribute("viewBox", [layout.bounds.x, layout.bounds.y, layout.bounds.width, layout.bounds.height].join(" "));
    clone.setAttribute("preserveAspectRatio", "none");
    root.appendChild(clone);

    layout.labels.forEach(function (label) {
      var center = label.y + label.height / 2;
      if (Math.abs(center - label.center) > 2) {
        var leader = document.createElementNS(SVG_NS, "path");
        leader.setAttribute("d", "M" + label.nodeRight + "," + label.center +
          " L" + (label.x - 4) + "," + center + " H" + (label.x - 1));
        leader.setAttribute("fill", "none");
        leader.setAttribute("stroke", "#555");
        leader.setAttribute("stroke-width", "1");
        root.appendChild(leader);
      }
      var text = document.createElementNS(SVG_NS, "text");
      text.setAttribute("font-family", "Arial, sans-serif");
      text.setAttribute("font-size", fontSize);
      text.setAttribute("fill", "#111");
      text.setAttribute("stroke", "white");
      text.setAttribute("stroke-width", "3");
      text.setAttribute("stroke-linejoin", "round");
      text.setAttribute("paint-order", "stroke fill");
      text.setAttribute("aria-label", label.text);
      label.lines.forEach(function (line, index) {
        var span = document.createElementNS(SVG_NS, "tspan");
        span.setAttribute("x", label.x);
        span.setAttribute("y", label.y + fontSize + index * layout.lineHeight);
        span.textContent = line;
        text.appendChild(span);
      });
      root.appendChild(text);
    });
    return "data:image/svg+xml;charset=utf-8," + encodeURIComponent(new XMLSerializer().serializeToString(root));
  }

  async function runExport(owner, create) {
    if (pending.has(owner)) return;
    pending.add(owner);
    owner.setAttribute("aria-busy", "true");
    try {
      await create();
      notify("Image prepared. Check your browser downloads.");
    } catch (error) {
      console.error("FST image export:", error);
      // Keep error messages text-only even if an upstream library reports HTML.
      var message = document.createElement("span");
      message.textContent = error.message || "Image export failed. Please try again.";
      notify(message.innerHTML, "error");
    } finally {
      pending.delete(owner);
      owner.removeAttribute("aria-busy");
    }
  }

  function downloadSankey(button) {
    return runExport(button, async function () {
      var config = sankeys[button.id];
      var size = dimensions(document.getElementById(config.width).value, document.getElementById(config.height).value);
      var index = config.plots.findIndex(function (id) {
        var plot = document.getElementById(id);
        return plot && plot.getClientRects().length > 0;
      });
      var plot = document.getElementById(config.plots[index]);
      var svg = plot && plot.querySelector("svg");
      if (!svg || !svg.querySelector(".node") || plot.classList.contains("recalculating") ||
          plot.classList.contains("shiny-output-error")) {
        throw new Error("Load facility inputs and wait for the Sankey diagram before downloading.");
      }
      var title = document.getElementById(config.titles[index]);
      var url;
      // Keep labels readable even in deep savings diagrams: use 14px normally,
      // with a 12px floor when the selected layout is crowded.
      for (var fontSize = 14; fontSize >= 12; fontSize--) {
        try {
          url = sankeySvg(svg, title ? title.textContent.trim() : "", size, fontSize);
          break;
        } catch (error) {
          if (fontSize === 12 || !/^The labels need more /.test(error.message)) throw error;
        }
      }
      var rasterSize = window.FSTSankeyLayout.rasterSize(size, MAX_PIXELS);
      savePng(await svgToPng(url, rasterSize), config.filename);
    });
  }

  function downloadPlotly(plot, filename) {
    return runExport(plot, async function () {
      if (!window.Plotly || plot.classList.contains("recalculating") ||
          plot.classList.contains("shiny-output-error")) {
        throw new Error("Wait for the product intensity chart to finish calculating before downloading.");
      }
      var bounds = plot.getBoundingClientRect();
      var size = dimensions(Math.round(bounds.width), Math.round(bounds.height));
      var url = await new Promise(function (resolve, reject) {
        var timer = window.setTimeout(function () {
          reject(new Error("Image export took too long. Please recalculate the chart and try again."));
        }, 20000);
        Promise.resolve().then(function () {
          return window.Plotly.toImage(plot, {format: "svg", width: size.width, height: size.height});
        }).then(resolve, reject).finally(function () { window.clearTimeout(timer); });
      });
      savePng(await svgToPng(url, dimensions(size.width * 2, size.height * 2)), filename);
    });
  }

  document.addEventListener("click", function (event) {
    var button = event.target.closest("button[data-fst-export]");
    if (button && sankeys[button.id]) {
      event.preventDefault();
      downloadSankey(button);
    }
  });

  window.FSTExport = {downloadPlotly: downloadPlotly};
}());
