// Tables wider than the screen: give each its own sideways scroll box, a
// "Swipe sideways" note above it, and a shadow on the right edge until the
// reader reaches the last column. Styles are in ou-theme.css (.ou-scroll-*).
// restyle_season_pages.py inlines this into every over/under page.
(function () {
  function scroller(t) {
    for (var q = t.parentElement; q && q !== document.body; q = q.parentElement) {
      var ox = getComputedStyle(q).overflowX;
      if (ox === 'auto' || ox === 'scroll') return q;
    }
    return null;
  }
  function state(frame, box) {
    var over = box.scrollWidth > box.clientWidth + 4;
    frame.classList.toggle('is-over', over);
    frame.classList.toggle('at-end', box.scrollLeft + box.clientWidth >= box.scrollWidth - 4);
  }
  function framed(box) {
    if (box.parentElement.classList.contains('ou-scroll-frame')) return box.parentElement;
    var frame = document.createElement('div');
    frame.className = 'ou-scroll-frame';
    var hint = document.createElement('div');
    hint.className = 'ou-scroll-hint';
    hint.textContent = 'Swipe sideways to see the whole table';
    box.parentNode.insertBefore(frame, box);
    frame.appendChild(hint);
    frame.appendChild(box);
    box.addEventListener('scroll', function () { state(frame, box); }, { passive: true });
    return frame;
  }
  function run() {
    document.querySelectorAll('table').forEach(function (t) {
      if (!t.offsetWidth || t.closest('.ou-masthead, .dataTables_scrollHead')) return;
      var box = scroller(t);
      if (!box) {
        if (t.offsetWidth <= t.parentElement.clientWidth + 2) return;
        box = document.createElement('div');
        box.className = 'ou-scroll-box';
        t.parentNode.insertBefore(box, t);
        box.appendChild(t);
      }
      state(framed(box), box);
    });
  }
  var timer;
  function soon(ms) { clearTimeout(timer); timer = setTimeout(run, ms || 250); }
  window.addEventListener('load', function () { run(); setTimeout(run, 1500); });
  window.addEventListener('resize', function () { soon(200); });
  // A tab that opens shows tables that measured zero wide while hidden
  document.addEventListener('click', function () { soon(350); });
  if (window.jQuery) jQuery(document).on('shown.bs.tab draw.dt', function () { soon(50); });
})();
