// MAMC BioStat - renders formulas offline with KaTeX whenever Shiny updates content
(function () {
  function typeset(el) {
    if (!window.renderMathInElement) return;
    try {
      renderMathInElement(el || document.body, {
        delimiters: [
          { left: "\\[", right: "\\]", display: true },
          { left: "\\(", right: "\\)", display: false }
        ],
        throwOnError: false
      });
    } catch (e) { console.log("KaTeX:", e); }
  }
  document.addEventListener("DOMContentLoaded", function () { typeset(document.body); });
  $(document).on("shiny:value", function (ev) {
    setTimeout(function () { var el = document.getElementById(ev.target.id); if (el) typeset(el); }, 30);
  });
  // switch to a tab from the server
  Shiny && Shiny.addCustomMessageHandler && $(document).on("shiny:connected", function () {
    Shiny.addCustomMessageHandler("typeset", function (id) { typeset(document.getElementById(id)); });
  });
})();
