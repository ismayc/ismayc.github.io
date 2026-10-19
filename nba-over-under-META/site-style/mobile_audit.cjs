// Phone-width audit of the over/under pages (and any other page): opens every
// tab pane at 390px and reports sideways page scroll, overlapping chart labels,
// labels past a chart's edge, text under 9px, and wide elements cut off;
// screenshots each flagged pane into OUTDIR.
// Known false positives: rotated column labels (preview similarity grid), a
// chart's "Final" label drawn twice, and plotly x-axis titles reported as
// "past the chart edge".
// Run: PW=<playwright module dir> node mobile_audit.cjs OUTDIR page.html...
// (Playwright 1.60.0 is in ~/.npm/_npx/705bc6b22212b352/node_modules/playwright.)
const { chromium } = require(process.env.PW);
const fs = require('fs');
(async () => {
  const [out, ...files] = process.argv.slice(2);
  const b = await chromium.launch();
  const report = [];
  for (const f of files) {
    const p = await b.newPage({ viewport: { width: 390, height: 844 } });
    const errs = []; p.on('pageerror', e => errs.push(e.message.slice(0, 120)));
    try { await p.goto(f.startsWith('http') ? f : 'file://' + f, { timeout: 120000 }); } catch (e) { report.push({ page: f, error: String(e).slice(0, 100) }); continue; }
    await p.waitForTimeout(2500);
    const panes = await p.evaluate(() => {
      // leaf tab panes, with their chain of tab hrefs
      const chain = (el) => { const c = []; for (let q = el; q; q = q.parentElement) if (q.classList && q.classList.contains('tab-pane') && q.id) c.unshift('#' + q.id); return c; };
      const leaves = [...document.querySelectorAll('.tab-pane[id]')].filter(t => !t.querySelector('.tab-pane'));
      return leaves.length ? leaves.map(chain) : [[]];
    });
    for (const ch of panes) {
      for (const h of ch) await p.evaluate(h => { const a = document.querySelector(`.nav-tabs a[href="${h}"]`); if (a && window.jQuery) jQuery(a).tab('show'); }, h);
      if (ch.length) await p.waitForTimeout(1300);
      const r = await p.evaluate((scope) => {
        const root = scope ? document.querySelector(scope) : document.body;
        const vis = (e) => { const r = e.getBoundingClientRect(); return r.width > 0 && r.height > 0; };
        const issues = [];
        const docW = document.documentElement.scrollWidth;
        if (docW > innerWidth + 2) issues.push(`page scrolls sideways: ${docW}px`);
        // overlapping text inside each svg
        root.querySelectorAll('svg').forEach((svg, si) => {
          if (!vis(svg)) return;
          const ts = [...svg.querySelectorAll('text')].filter(t => t.textContent.trim() && vis(t)).map(t => ({ t: t.textContent.trim().slice(0, 18), r: t.getBoundingClientRect() }));
          let n = 0; const ex = [];
          for (let i = 0; i < ts.length; i++) for (let j = i + 1; j < ts.length; j++) {
            const a = ts[i].r, c = ts[j].r;
            const ix = Math.min(a.right, c.right) - Math.max(a.left, c.left), iy = Math.min(a.bottom, c.bottom) - Math.max(a.top, c.top);
            if (ix > 2 && iy > 2 && ix * iy > 0.15 * Math.min(a.width * a.height, c.width * c.height)) { n++; if (ex.length < 3) ex.push(ts[i].t + ' / ' + ts[j].t); }
          }
          if (n) issues.push(`chart ${si}: ${n} overlapping labels (${ex.join('; ')})`);
          // text clipped by svg edge
          const sr = svg.getBoundingClientRect(); const cut = ts.filter(x => x.r.left < sr.left - 2 || x.r.right > sr.right + 2).length;
          if (cut) issues.push(`chart ${si}: ${cut} labels past the chart edge`);
        });
        // tiny text
        let tiny = 0; const tex = [];
        root.querySelectorAll('body *, *').forEach(e => { if (!e.childNodes.length) return; const own = [...e.childNodes].some(n => n.nodeType === 3 && n.textContent.trim()); if (!own || !vis(e)) return; const fs = parseFloat(getComputedStyle(e).fontSize); if (fs < 9) { tiny++; if (tex.length < 3) tex.push(e.textContent.trim().slice(0, 20) + '@' + fs); } });
        if (tiny) issues.push(`${tiny} text items under 9px (${tex.join('; ')})`);
        // elements wider than viewport not in a scroll box
        const wide = [...root.querySelectorAll('img, canvas, iframe, .html-widget, .plotly, pre')].filter(e => vis(e) && e.getBoundingClientRect().right > innerWidth + 2).filter(e => { for (let q = e.parentElement; q && q !== document.body; q = q.parentElement) { const ox = getComputedStyle(q).overflowX; if (ox === 'auto' || ox === 'scroll') return false; } return true; });
        if (wide.length) issues.push(`${wide.length} wide elements cut off (${wide.slice(0, 2).map(e => e.tagName + '.' + e.className.toString().slice(0, 20)).join(', ')})`);
        return issues;
      }, ch[ch.length - 1] || null);
      const name = f.split('/').pop().replace('.html', '') + (ch.length ? '__' + ch[ch.length - 1].slice(1) : '');
      if (r.length) {
        const el = ch.length ? await p.$(ch[ch.length - 1]) : null;
        try { if (el) await el.screenshot({ path: `${out}/${name}.png`, timeout: 15000 }); else await p.screenshot({ path: `${out}/${name}.png`, fullPage: true }); } catch (e) {}
      }
      report.push({ page: name, issues: r });
    }
    if (errs.length) report.push({ page: f.split('/').pop(), jsErrors: errs.slice(0, 3) });
    await p.close();
  }
  fs.writeFileSync(`${out}/report.json`, JSON.stringify(report, null, 1));
  report.filter(x => (x.issues && x.issues.length) || x.error || x.jsErrors).forEach(x => console.log(x.page, '\n   ', (x.issues || [x.error || x.jsErrors]).join('\n    ')));
  console.log('panes checked:', report.length);
  await b.close();
})();
