// Every tab and heading on an over/under page has its own address.
// - #<id> opens each tab that holds it, outermost first, then scrolls to it
//   (2027-nba-over-under.html#by-game-number opens By team, then By game number)
// - clicking a tab puts its address in the URL, ready to copy or share
// - each heading gets a "#" link; clicking it also copies the address
(function () {
  var $ = window.jQuery;  // tabs need it; the playoffs page has headings only

  var style = document.createElement('style');
  style.textContent =
    '.ou-anchor { margin-left: .4em; font-weight: 400; font-size: .8em; ' +
    'color: #56627a; opacity: .35; text-decoration: none; }' +
    '.ou-anchor:hover, .ou-anchor:focus, :hover > .ou-anchor { opacity: 1; text-decoration: none; }' +
    '.ou-anchor.copied::after { content: " link copied"; font-size: .75em; }';
  document.head.appendChild(style);

  function slug(text) {
    return text.toLowerCase().replace(/[^a-z0-9]+/g, '-').replace(/^-|-$/g, '') || 'section';
  }

  // Headings in a section use the section's id; others get one from their text
  function addAnchors() {
    var used = {};
    document.querySelectorAll('h2, h3, h4, h5').forEach(function (h) {
      if (h.closest('.nav, .ou-masthead, header, #header, .modal') ||
          h.matches('.title, .subtitle, .author, .date') || h.querySelector('.ou-anchor')) return;
      if (!h.offsetParent && !h.closest('.tab-pane')) return;
      var sec = h.parentElement && h.parentElement.classList.contains('section') ? h.parentElement : null;
      var id = (sec && sec.id) || h.id;
      if (!id) {
        id = slug(h.textContent);
        while (document.getElementById(id) || used[id]) id += '-2';
        h.id = id;
      }
      used[id] = true;
      var a = document.createElement('a');
      a.className = 'ou-anchor';
      a.href = '#' + id;
      a.setAttribute('aria-label', 'Link to this section');
      a.textContent = '#';
      h.appendChild(a);
    });
  }

  function reveal(el) {
    if (!$) return;
    var panes = [];
    for (var p = el; p; p = p.parentElement) {
      if (p.classList && p.classList.contains('tab-pane')) panes.unshift(p);
    }
    panes.forEach(function (pane) { $('.nav-tabs a[href="#' + pane.id + '"]').tab('show'); });
  }

  function open() {
    var id = decodeURIComponent(location.hash.slice(1));
    var el = id && document.getElementById(id);
    if (!el) return;
    reveal(el);
    var tab = $ ? $('.nav-tabs a[href="#' + id + '"]') : [];
    if (tab.length) tab.tab('show');
    // After the tab transition, so the target has its height
    setTimeout(function () { (tab[0] || el).scrollIntoView(); }, 200);
  }

  window.addEventListener('load', function () { addAnchors(); open(); });
  window.addEventListener('hashchange', open);

  if ($) {
    $(document).on('click', '.nav-tabs a[data-toggle="tab"]', function () {
      var h = this.getAttribute('href');
      if (h && h.charAt(0) === '#' && h !== location.hash) history.replaceState(null, '', h);
    });
  }
  document.addEventListener('click', function (e) {
    var a = e.target.closest && e.target.closest('a.ou-anchor');
    if (!a) return;
    var url = location.href.split('#')[0] + a.getAttribute('href');
    if (navigator.clipboard) {
      navigator.clipboard.writeText(url).then(function () {
        a.classList.add('copied');
        setTimeout(function () { a.classList.remove('copied'); }, 1500);
      }, function () {});
    }
  });
})();
