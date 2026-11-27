// Give each heading on a season page the level that its place in the page
// calls for. R Markdown turns the section headings that make up the tabs
// into tab links, so the headings left inside the tabs (h4 "Games through
// ...", for example) follow the page's h1 title with no h2 or h3 between
// them. A screen reader then reports a page outline with skipped levels.
// This sets aria-level from the headings that are still there: a heading is
// one level under the nearest enclosing section whose own heading remains,
// and level 2 when there is none. The tags (and so the look) do not change.
// It also hides the empty h1 that R Markdown leaves on the outer tab set.
(function () {
  var HEAD = /^H[1-6]$/;
  function ownHeading(section) {
    for (var c = section.firstElementChild; c; c = c.nextElementSibling) {
      if (HEAD.test(c.tagName)) return c;
    }
    return null;
  }
  function text(h) {
    var t = '';
    h.childNodes.forEach(function (n) {
      if (!(n.classList && n.classList.contains('ou-anchor'))) t += n.textContent;
    });
    return t.trim();
  }
  function levels() {
    var main = document.querySelector('.main-container');
    if (!main) return;
    main.querySelectorAll('h1, h2, h3, h4, h5, h6').forEach(function (h) {
      if (h.matches('.title, .subtitle, .date') || h.closest('#header')) return;
      if (!text(h)) { h.setAttribute('aria-hidden', 'true'); return; }
      var depth = 0;
      var own = h.parentElement;
      for (var s = own && own.parentElement; s && s !== main; s = s.parentElement) {
        if (!s.classList.contains('section')) continue;
        var sh = ownHeading(s);
        if (sh && text(sh)) depth++;
      }
      h.setAttribute('aria-level', String(2 + depth));
    });
  }
  // After R Markdown builds the tabs (an earlier jQuery ready handler)
  if (window.jQuery) jQuery(levels);
  window.addEventListener('load', levels);
})();
