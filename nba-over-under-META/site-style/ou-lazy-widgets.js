// Draw each chart and table only when its tab first opens.
// A season page holds about 19 htmlwidgets (plotly charts and DataTables), and
// all but one sit in tabs that are closed when the page opens. htmlwidgets
// draws every one of them at DOMContentLoaded, which took about 1.4 s of
// script time on the 2027 page (Lighthouse, November 27, 2026).
// restyle_season_pages.py puts this script right after htmlwidgets.js. All of
// the page's scripts are deferred, so this runs after the page is parsed and
// before htmlwidgets draws. It removes the binding class ("plotly",
// "datatables") from each widget in a closed tab, so htmlwidgets does not find
// it. When a tab opens, the class goes back and HTMLWidgets.staticRender()
// draws the widgets that are now visible (it skips ones already drawn).
(function () {
  // R Markdown builds the tabs later (on jQuery ready), so read the closed
  // tabs from the section structure: in a .tabset section of level N, the
  // child sections of level N+1 are its tabs, and the first one (or the one
  // marked .active) opens.
  function inClosedTab(el) {
    for (var s = el; s && s.parentElement; s = s.parentElement) {
      var p = s.parentElement;
      var m = p.className.match(/\blevel(\d)\b/);
      if (!m || !p.classList.contains('tabset') || !s.classList.contains('section')) continue;
      var tabClass = 'level' + (Number(m[1]) + 1);
      if (!s.classList.contains(tabClass)) continue;
      var tabs = [].filter.call(p.children, function (c) {
        return c.classList.contains('section') && c.classList.contains(tabClass);
      });
      var open = tabs.filter(function (c) { return c.classList.contains('active'); })[0] || tabs[0];
      if (s !== open) return true;
    }
    return false;
  }

  // A widget tied to a crosstalk filter must be drawn at once, so it hears
  // the filter from the start. No season page has one now.
  function linked(el) {
    var data = document.querySelector('script[data-for="' + el.id + '"]');
    return !!(data && /"group":"[^"]/.test(data.textContent));
  }

  var waiting = [];
  document.querySelectorAll('.html-widget').forEach(function (el) {
    var binding = el.classList[0];
    if (!binding || binding === 'html-widget' || !el.id || !inClosedTab(el) || linked(el)) return;
    el.classList.remove(binding);
    el.setAttribute('data-ou-lazy', binding);
    waiting.push(el);
  });
  if (!waiting.length || !window.jQuery) return;

  // Recolor a new chart (PLOTLY_FONT in restyle_season_pages.py) once
  // plotly has finished its first draw
  function restyleWhenDrawn(el, tries) {
    if (el._fullLayout) {
      if (window.ouRestylePlots) window.ouRestylePlots([el]);
    } else if (tries < 60) {
      setTimeout(function () { restyleWhenDrawn(el, tries + 1); }, 50);
    }
  }

  function wake() {
    var now = waiting.filter(function (el) { return el.offsetWidth > 0; });
    if (!now.length || !window.HTMLWidgets) return;
    waiting = waiting.filter(function (el) { return now.indexOf(el) < 0; });
    now.forEach(function (el) {
      el.classList.add(el.getAttribute('data-ou-lazy'));
      el.removeAttribute('data-ou-lazy');
    });
    window.HTMLWidgets.staticRender();
    now.forEach(function (el) {
      if (el.classList.contains('plotly')) restyleWhenDrawn(el, 0);
    });
  }
  jQuery(document).on('shown.bs.tab', wake);
  window.addEventListener('load', wake);
})();
