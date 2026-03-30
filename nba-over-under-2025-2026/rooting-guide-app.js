// P is injected by R as: const P=[...];
// Validate data on load
(function(){
  var errs=[];
  if(!D||!D.teams||!D.picks)errs.push('D.teams or D.picks is missing');
  if(!P||!P.length)errs.push('P (players array) is missing');
  for(var t in D.teams){
    if(!D.picks[t]){errs.push('No picks for team: '+t);continue}
    for(var i=0;i<P.length;i++){
      var pk=D.picks[t][P[i]];
      if(!pk||pk.length!==2||pk[0]===null||pk[1]===null)
        errs.push('Bad pick: '+t+' / '+P[i]+' = '+JSON.stringify(pk));
    }
  }
  if(errs.length){console.error('DATA VALIDATION ERRORS:',errs);
    var el=document.getElementById('panels');
    if(el) el.innerHTML='<div class="error-box">Data validation errors ('+errs.length+'):\n'+errs.slice(0,20).join('\n')+(errs.length>20?'\n... and '+(errs.length-20)+' more':'')+'</div>';
  }
})();

const SN={"Atlanta Hawks":"Hawks","Boston Celtics":"Celtics","Brooklyn Nets":"Nets","Charlotte Hornets":"Hornets","Chicago Bulls":"Bulls","Cleveland Cavaliers":"Cavs","Detroit Pistons":"Pistons","Indiana Pacers":"Pacers","Miami Heat":"Heat","Milwaukee Bucks":"Bucks","New York Knicks":"Knicks","Orlando Magic":"Magic","Philadelphia 76ers":"76ers","Toronto Raptors":"Raptors","Washington Wizards":"Wizards","Dallas Mavericks":"Mavs","Denver Nuggets":"Nuggets","Golden State Warriors":"Warriors","Houston Rockets":"Rockets","Los Angeles Clippers":"Clippers","Los Angeles Lakers":"Lakers","Memphis Grizzlies":"Grizzlies","Minnesota Timberwolves":"Wolves","New Orleans Pelicans":"Pelicans","Oklahoma City Thunder":"Thunder","Phoenix Suns":"Suns","Portland Trail Blazers":"Blazers","Sacramento Kings":"Kings","San Antonio Spurs":"Spurs","Utah Jazz":"Jazz"};
const sn=t=>SN[t]||t;
function teamTracking(t){const td=D.teams[t];return td.winPct>td.wp/82?'OVER':'UNDER'}
function getScore(p){
  let locked=0,pending=0,correct=0,wrong=0,undecided=[];
  for(const[t,td]of Object.entries(D.teams)){
    if(!D.picks[t]||!D.picks[t][p]){continue}
    const[wage,choice]=D.picks[t][p];
    if(td.det==='OVER'){choice==='OVER'?(locked+=wage,correct++):(locked-=wage,wrong++)}
    else if(td.det==='UNDER'){choice==='UNDER'?(locked+=wage,correct++):(locked-=wage,wrong++)}
    else{
      const tracking=teamTracking(t);
      const pts=tracking===choice?wage:-wage;
      pending+=pts;
      undecided.push({team:t,wage,choice,tracking,pts,wtg:td.wtg,ltg:td.ltg,rem:td.rem,w:td.w,l:td.l,wp:td.wp,pWins:td.pWins});
    }
  }
  return{locked,pending,total:locked+pending,correct,wrong,undecided};
}

// ── Schedule strength helpers ──────────────────────────────────────────────
function hasSched(){
  for(var t in D.teams){if(D.teams[t].sched&&D.teams[t].sched.length)return true}
  return false;
}

function oppWinPct(oppName){
  var o=D.teams[oppName];
  return o?o.winPct:0.5;
}

function isRestCandidate(oppName){
  // Opponent has clinched their over/under AND is a strong team (>= .585 win%)
  // These teams are likely playoff-bound with a comfortable seed and may rest starters late
  var o=D.teams[oppName];
  if(!o)return false;
  return o.det!=='not yet'&&o.winPct>=0.585;
}

function effWinPct(oppName){
  // Discount effective strength by 15% if opponent may rest stars
  var raw=oppWinPct(oppName);
  if(isRestCandidate(oppName)) return Math.max(0.15,raw*0.85);
  return raw;
}

function schedStrength(teamName){
  var td=D.teams[teamName];
  if(!td||!td.sched||!td.sched.length)return null;
  var sum=0,effSum=0,n=td.sched.length;
  var strong=0,mid=0,weak=0,restCount=0;
  td.sched.forEach(function(g){
    var wp=oppWinPct(g.opp);
    var ew=effWinPct(g.opp);
    sum+=wp;effSum+=ew;
    if(wp>=0.585)strong++;
    else if(wp>=0.415)mid++;
    else weak++;
    if(isRestCandidate(g.opp))restCount++;
  });
  return{
    avgOppWp:sum/n,
    avgEffWp:effSum/n,
    strong:strong,mid:mid,weak:weak,
    restCount:restCount,
    total:n
  };
}

function wpColor(wp){
  if(wp>=0.585)return'var(--under)';
  if(wp>=0.415)return'var(--gold)';
  return'var(--over)';
}

function wpLabel(wp){
  if(wp>=0.585)return'Strong';
  if(wp>=0.415)return'Mid';
  return'Weak';
}

// ── Tab renderers ──────────────────────────────────────────────────────────

function renderTab1(el,player){
  const sc=getScore(player);
  const teams=sc.undecided.sort((a,b)=>b.wage-a.wage);
  const decided=Object.values(D.teams).filter(t=>t.det!=='not yet').length;
  const undecidedCount=30-decided;
  let h='<div style="display:flex;gap:16px;margin-bottom:16px;flex-wrap:wrap">';
  h+='<div style="background:var(--card);border:1px solid var(--border);border-radius:10px;padding:14px 20px">';
  h+='<div style="font-size:11px;color:var(--t3)">Locked Points</div>';
  h+='<div style="font-size:24px;font-weight:800;font-family:JetBrains Mono;color:'+(sc.locked>=0?'var(--over)':'var(--under)')+'">'+(sc.locked>=0?'+':'')+sc.locked+'</div>';
  h+='<div style="font-size:11px;color:var(--t3)">'+sc.correct+'W / '+sc.wrong+'L of '+decided+' decided</div></div>';
  h+='<div style="background:var(--card);border:1px solid var(--border);border-radius:10px;padding:14px 20px">';
  h+='<div style="font-size:11px;color:var(--t3)">Projected Total</div>';
  h+='<div style="font-size:24px;font-weight:800;font-family:JetBrains Mono;color:'+(sc.total>=0?'var(--over)':'var(--under)')+'">'+(sc.total>=0?'+':'')+sc.total+'</div>';
  h+='<div style="font-size:11px;color:var(--t3)">'+undecidedCount+' teams still undecided</div></div></div>';

  h+='<div class="section-title">Root for these outcomes</div>';
  h+='<div style="display:flex;flex-wrap:wrap;gap:6px;margin-bottom:20px">';
  teams.forEach(function(t){
    var cls=t.choice==='OVER'?'chip-o':'chip-u';
    var bright=t.pts>0?'bright':'faded';
    h+='<span class="chip '+cls+' '+bright+'">'+sn(t.team)+' '+t.choice+' <span style="opacity:.6">('+t.wage+')</span></span>';
  });
  h+='</div>';

  h+='<div class="section-title">Team-by-team breakdown</div>';
  h+='<div style="display:grid;grid-template-columns:120px 60px 50px 50px 1fr 60px 60px;gap:8px;padding:4px 10px;font-size:10px;font-weight:700;color:var(--t3);text-transform:uppercase;letter-spacing:.5px"><span>Team</span><span>Pick</span><span>Record</span><span>Target</span><span>Progress</span><span>To Over</span><span>To Under</span></div>';
  teams.forEach(function(t){
    var onTrack=t.pts>0;
    var pct=t.rem>0?(t.choice==='OVER'?(t.wtg!==null?Math.max(0,1-t.wtg/t.rem):1):(t.ltg!==null?Math.max(0,1-t.ltg/t.rem):1)):0;
    var barCol=onTrack?'var(--over)':'var(--under)';
    h+='<div class="trow" style="grid-template-columns:120px 60px 50px 50px 1fr 60px 60px">';
    h+='<span class="team-name">'+sn(t.team)+'</span>';
    h+='<span><span class="pill-sm pill-'+t.choice.toLowerCase()+'">'+t.wage+' '+t.choice[0]+'</span></span>';
    h+='<span class="mono">'+t.w+'-'+t.l+'</span>';
    h+='<span class="mono" style="color:var(--t3)">'+t.wp+'</span>';
    h+='<div style="display:flex;align-items:center;gap:6px"><div class="prog-wrap"><div class="prog-fill" style="width:'+pct*100+'%;background:'+barCol+'"></div></div><span class="mono" style="font-size:10px;color:'+(onTrack?'var(--over)':'var(--under)')+'">'+(onTrack?'On track':'Behind')+'</span></div>';
    h+='<span class="mono" style="text-align:center;color:var(--t2)">'+(t.wtg!==null?t.wtg:'-')+'</span>';
    h+='<span class="mono" style="text-align:center;color:var(--t2)">'+(t.ltg!==null?t.ltg:'-')+'</span>';
    h+='</div>';
  });
  el.innerHTML=h;
}

function renderTab2(el){
  var hasGames=D.todaysGames&&D.todaysGames.length>0;
  if(!hasGames){el.innerHTML='<p style="color:var(--t3)">No games scheduled today.</p>';return}
  var h='<div class="section-title">Today\'s Games</div><p style="font-size:12px;color:var(--t3);margin-bottom:16px">Showing stakes for each player in undecided teams playing today.</p>';
  D.todaysGames.forEach(function(g){
    var away=g[0],home=g[1];
    var hT=D.teams[home],aT=D.teams[away];
    var hU=hT&&hT.det==='not yet',aU=aT&&aT.det==='not yet';
    if(!hU&&!aU)return;
    h+='<div class="game-card"><div class="game-matchup">'+sn(away)+' <span class="game-at">@</span> '+sn(home)+'</div>';
    h+='<div style="display:grid;grid-template-columns:1fr 1fr;gap:16px">';
    if(hU){
      var hClinch='';
      if(hT.wtg===1) hClinch+=' <span style="color:var(--gold)">WIN = CLINCH OVER</span>';
      if(hT.ltg===1) hClinch+=' <span style="color:var(--gold)">LOSS = CLINCH UNDER</span>';
      h+='<div><div style="font-size:11px;font-weight:700;color:var(--t2);margin-bottom:6px">'+sn(home)+' ('+hT.w+'-'+hT.l+') &middot; Target: '+hT.wp+'</div>';
      h+='<div style="font-size:10px;color:var(--t3);margin-bottom:6px">Wins to go over: '+(hT.wtg!==null?hT.wtg:'-')+' &middot; Losses to go under: '+(hT.ltg!==null?hT.ltg:'-')+hClinch+'</div>';
      P.forEach(function(p){var pks=D.picks[home];if(!pks||!pks[p])return;var pk=pks[p];h+='<div style="display:flex;align-items:center;gap:6px;padding:2px 0;font-size:12px"><span style="width:55px;color:var(--t2)">'+p+'</span><span class="pill-sm pill-'+pk[1].toLowerCase()+'">'+pk[0]+pk[1][0]+'</span><span style="color:var(--t3);font-size:11px">'+(pk[1]==='OVER'?'wants wins':'wants losses')+'</span></div>'});
      h+='</div>';
    }
    if(aU){
      var aClinch='';
      if(aT.wtg===1) aClinch+=' <span style="color:var(--gold)">WIN = CLINCH OVER</span>';
      if(aT.ltg===1) aClinch+=' <span style="color:var(--gold)">LOSS = CLINCH UNDER</span>';
      h+='<div><div style="font-size:11px;font-weight:700;color:var(--t2);margin-bottom:6px">'+sn(away)+' ('+aT.w+'-'+aT.l+') &middot; Target: '+aT.wp+'</div>';
      h+='<div style="font-size:10px;color:var(--t3);margin-bottom:6px">Wins to go over: '+(aT.wtg!==null?aT.wtg:'-')+' &middot; Losses to go under: '+(aT.ltg!==null?aT.ltg:'-')+aClinch+'</div>';
      P.forEach(function(p){var pks=D.picks[away];if(!pks||!pks[p])return;var pk=pks[p];h+='<div style="display:flex;align-items:center;gap:6px;padding:2px 0;font-size:12px"><span style="width:55px;color:var(--t2)">'+p+'</span><span class="pill-sm pill-'+pk[1].toLowerCase()+'">'+pk[0]+pk[1][0]+'</span><span style="color:var(--t3);font-size:11px">'+(pk[1]==='OVER'?'wants wins':'wants losses')+'</span></div>'});
      h+='</div>';
    }
    h+='</div></div>';
  });
  el.innerHTML=h;
}

function renderTab3(el){
  var h='<div class="section-title">Undecided Teams - Projected Pace vs Target</div><p style="font-size:12px;color:var(--t3);margin-bottom:16px">Sorted by how close their pace is to the target. Win% Needed = required rate in remaining games to go OVER.</p>';
  var undecided=[];
  var showSched=hasSched();
  for(var nm in D.teams){var t=D.teams[nm];if(t.det==='not yet'){var gap=t.pWins-t.wp;var wpn=(t.wtg!==null&&t.rem>0)?Math.round(t.wtg/t.rem*100):null;var ss=showSched?schedStrength(nm):null;undecided.push({name:nm,gap:gap,w:t.w,l:t.l,wp:t.wp,pWins:t.pWins,wpn:wpn,rem:t.rem,wtg:t.wtg,ltg:t.ltg,ss:ss})}}
  undecided.sort(function(a,b){
    var aw=a.wpn!==null?a.wpn:50, bw=b.wpn!==null?b.wpn:50;
    var da=Math.min(aw,100-aw), db=Math.min(bw,100-bw);
    return da-db;
  });

  // Columns widen if schedule data is available
  var cols=showSched?'140px 55px 50px 50px 80px 1fr 70px 80px':'140px 55px 50px 50px 80px 1fr 70px';
  h+='<div style="display:grid;grid-template-columns:'+cols+';gap:8px;padding:4px 10px;font-size:10px;font-weight:700;color:var(--t3);text-transform:uppercase;letter-spacing:.5px"><span>Team</span><span>Record</span><span>Target</span><span>Pace</span><span>Gap</span><span>OVER Likelihood</span><span>Win% Needed</span>';
  if(showSched)h+='<span>Opp Str</span>';
  h+='</div>';

  undecided.forEach(function(t){
    var g=t.gap,gc=g>0?'var(--over)':'var(--under)';
    var fill=t.wpn!==null?Math.max(0,Math.min(100,100-t.wpn)):50;
    var barCol=fill>=60?'var(--over)':fill>=40?'var(--gold)':'var(--under)';
    var dl='',dc='var(--t2)';
    if(t.wpn!==null){if(t.wpn>100){dl='Impossible';dc='var(--under)'}else if(t.wpn>80){dl=t.wpn+'%';dc='var(--under)'}else if(t.wpn>60){dl=t.wpn+'%';dc='var(--gold)'}else{dl=t.wpn+'%';dc='var(--over)'}}else{dl='Clinched'}
    h+='<div class="trow" style="grid-template-columns:'+cols+'">';
    h+='<span class="team-name">'+sn(t.name)+'</span>';
    h+='<span class="mono">'+t.w+'-'+t.l+'</span>';
    h+='<span class="mono" style="color:var(--t3)">'+t.wp+'</span>';
    h+='<span class="mono" style="color:'+gc+'">'+t.pWins+'</span>';
    h+='<span class="mono" style="color:'+gc+'">'+(g>0?'+':'')+g.toFixed(1)+'</span>';
    h+='<div style="display:flex;align-items:center"><div style="width:100%;height:14px;border-radius:3px;overflow:hidden;background:var(--border)"><div style="width:'+fill+'%;height:100%;background:'+barCol+';border-radius:3px;transition:width .3s"></div></div></div>';
    h+='<span class="mono" style="text-align:center;color:'+dc+';font-size:11px">'+dl+'</span>';
    if(showSched&&t.ss){
      var oC=wpColor(t.ss.avgEffWp);
      h+='<span class="mono" style="text-align:center;color:'+oC+';font-size:11px" title="Avg opp win%: '+(t.ss.avgOppWp*100).toFixed(1)+'% (eff: '+(t.ss.avgEffWp*100).toFixed(1)+'%)\n'+t.ss.strong+' strong / '+t.ss.mid+' mid / '+t.ss.weak+' weak'+'">.'+(Math.round(t.ss.avgEffWp*1000)+'').padStart(3,'0');
      if(t.ss.restCount>0)h+=' <span title="'+t.ss.restCount+' opponent(s) may rest starters" style="cursor:help">&#x1F4A4;</span>';
      h+='</span>';
    } else if(showSched){
      h+='<span class="mono" style="text-align:center;color:var(--t3)">-</span>';
    }
    h+='</div>';
  });

  // Razor-thin margins with adaptive toggle
  var maxRem=Math.max.apply(null,undecided.map(function(t){return t.rem||0}));
  var adaptiveThreshold=Math.max(1,Math.round(maxRem*0.08));
  var threshold=razorAdaptive?adaptiveThreshold:5;
  var close=undecided.filter(function(t){return Math.abs(t.gap)<threshold});
  h+='<div style="display:flex;align-items:center;gap:12px;margin-top:24px;margin-bottom:8px">';
  h+='<div class="section-title" style="margin:0">Razor-thin margins</div>';
  h+='<div style="display:flex;align-items:center;gap:8px;font-size:12px;color:var(--t2)">';
  h+='<button onclick="toggleRazor()" style="display:inline-flex;align-items:center;gap:6px;padding:4px 10px;border-radius:8px;border:1px solid var(--border);background:'+(razorAdaptive?'var(--card)':'transparent')+';cursor:pointer;font-size:11px;color:'+(razorAdaptive?'var(--t1)':'var(--t3)')+';font-weight:'+(razorAdaptive?'600':'400')+'">';
  h+='<span style="display:inline-block;width:28px;height:16px;border-radius:8px;background:'+(razorAdaptive?'var(--over)':'var(--border)')+';position:relative;transition:background .2s"><span style="display:block;width:12px;height:12px;border-radius:50%;background:#fff;position:absolute;top:2px;'+(razorAdaptive?'right:2px':'left:2px')+';transition:all .2s"></span></span>';
  h+='Adaptive</button>';
  var wUnit=function(n){return n===1?'win':'wins'};
  h+='<span style="font-size:11px;color:var(--t3)">'+(razorAdaptive?'Within '+threshold+' '+wUnit(threshold)+' of target ('+maxRem+' games remaining)':'Fixed: within 5 wins of target')+'</span>';
  h+='</div></div>';
  if(close.length){
    h+='<div style="display:flex;flex-wrap:wrap;gap:6px">';
    close.forEach(function(t){h+='<span class="chip chip-'+(t.gap>0?'o':'u')+'">'+sn(t.name)+': '+(t.gap>0?'+':'')+t.gap.toFixed(1)+' '+(t.gap>0?'over':'under')+' pace</span>'});
    h+='</div>';
  } else {
    h+='<p style="font-size:12px;color:var(--t3);margin-top:4px">No teams within the current threshold.</p>';
  }
  el.innerHTML=h;
}

function renderTab4(el,player){
  var others=P.filter(function(p){return p!==player});
  var rows=[];
  for(var team in D.teams){
    var td=D.teams[team];if(td.det!=='not yet')continue;
    if(!D.picks[team]||!D.picks[team][player])continue;
    var pk=D.picks[team][player],wage=pk[0],choice=pk[1];
    var od=others.map(function(p){var q=(D.picks[team]&&D.picks[team][p])||[0,'OVER'];return{player:p,wage:q[0],choice:q[1]}});
    var sd=od.filter(function(o){return o.choice===choice}).length;
    rows.push({team:team,wage:wage,choice:choice,sameDir:sd,oppositeDir:others.length-sd,tracking:teamTracking(team),w:td.w,l:td.l,wp:td.wp,othersData:od});
  }
  rows.sort(function(a,b){return b.oppositeDir-a.oppositeDir||b.wage-a.wage});
  var sc=getScore(player);
  var h='<div style="display:flex;gap:16px;margin-bottom:16px;flex-wrap:wrap">';
  h+='<div style="background:var(--card);border:1px solid var(--border);border-radius:10px;padding:14px 20px"><div style="font-size:11px;color:var(--t3)">Unique Picks</div><div style="font-size:24px;font-weight:800;font-family:JetBrains Mono;color:var(--purple)">'+rows.filter(function(r){return r.oppositeDir>=5}).length+'</div><div style="font-size:11px;color:var(--t3)">where 5+ others disagree</div></div>';
  h+='<div style="background:var(--card);border:1px solid var(--border);border-radius:10px;padding:14px 20px"><div style="font-size:11px;color:var(--t3)">Biggest Outlier</div><div style="font-size:16px;font-weight:700;color:var(--t1)">'+(rows.length?sn(rows[0].team):'-')+'</div><div style="font-size:11px;color:var(--t3)">'+(rows.length?rows[0].oppositeDir+'/'+(P.length-1)+' others disagree':'')+'</div></div></div>';
  h+='<div class="section-title">Undecided teams where '+player+' diverges from the group</div>';
  h+='<div style="display:grid;grid-template-columns:120px 55px 50px 55px 60px 1fr;gap:8px;padding:4px 10px;font-size:10px;font-weight:700;color:var(--t3);text-transform:uppercase;letter-spacing:.5px"><span>Team</span><span>'+player+'</span><span>Record</span><span>Tracking</span><span>Agree</span><span>Others\' Picks</span></div>';
  rows.forEach(function(r){
    h+='<div class="trow" style="grid-template-columns:120px 55px 50px 55px 60px 1fr">';
    h+='<span class="team-name">'+sn(r.team)+'</span>';
    h+='<span><span class="pill-sm pill-'+r.choice.toLowerCase()+'">'+r.wage+r.choice[0]+'</span></span>';
    h+='<span class="mono">'+r.w+'-'+r.l+'</span>';
    h+='<span class="pill-sm pill-'+r.tracking.toLowerCase()+'" style="font-size:9px">'+r.tracking+'</span>';
    h+='<span style="font-size:12px;color:'+(r.sameDir>=4?'var(--over)':r.sameDir<=2?'var(--under)':'var(--gold)')+'">'+r.sameDir+'/'+(P.length-1)+'</span>';
    h+='<div style="display:flex;flex-wrap:wrap;gap:3px">';
    r.othersData.forEach(function(o){
      var agree=o.choice===r.choice;
      h+='<span style="font-size:10px;padding:1px 5px;border-radius:6px;background:'+(agree?'rgba(16,185,129,.08)':'rgba(244,63,94,.08)')+';color:'+(agree?'var(--over)':'var(--under)')+'">'+o.player[0]+o.wage+o.choice[0]+'</span>';
    });
    h+='</div></div>';
  });
  el.innerHTML=h;
}

// ── Tab 5: Schedule Strength ───────────────────────────────────────────────
var expandedTeams={};
function toggleTeamSched(team){expandedTeams[team]=!expandedTeams[team];renderAll()}

function renderTab5(el){
  if(!hasSched()){
    el.innerHTML='<div class="error-box">Schedule data is not available. Make sure the R script includes remaining schedule info in D.teams[].sched.</div>';
    return;
  }

  var h='<div class="section-title">Remaining Schedule Strength</div>';
  h+='<p style="font-size:12px;color:var(--t3);margin-bottom:6px">Opponent strength based on current win%. Click a team to expand its remaining games.</p>';
  h+='<p style="font-size:11px;color:var(--t3);margin-bottom:16px">';
  h+='&#x1F4A4; = Opponent has clinched with a strong record and may rest starters. "Eff. Str." discounts these opponents by 15%.';
  h+='</p>';

  // Build rows for undecided teams
  var rows=[];
  for(var nm in D.teams){
    var t=D.teams[nm];
    if(t.det!=='not yet')continue;
    var ss=schedStrength(nm);
    if(!ss)continue;
    rows.push({name:nm,w:t.w,l:t.l,wp:t.wp,rem:t.rem,wtg:t.wtg,ltg:t.ltg,pWins:t.pWins,ss:ss,sched:t.sched||[]});
  }

  // Sort hardest schedule first
  rows.sort(function(a,b){return b.ss.avgEffWp-a.ss.avgEffWp});

  // Summary cards
  var hardest=rows[0],easiest=rows[rows.length-1];
  h+='<div style="display:flex;gap:16px;margin-bottom:20px;flex-wrap:wrap">';
  if(hardest){
    h+='<div style="background:var(--card);border:1px solid var(--border);border-radius:10px;padding:14px 20px">';
    h+='<div style="font-size:11px;color:var(--t3)">Hardest Remaining Schedule</div>';
    h+='<div style="font-size:18px;font-weight:700;color:var(--under)">'+sn(hardest.name)+'</div>';
    h+='<div style="font-size:11px;color:var(--t3)">Avg opp: .'+(Math.round(hardest.ss.avgOppWp*1000)+'').padStart(3,'0')+' | '+hardest.ss.strong+' strong opponents</div></div>';
  }
  if(easiest){
    h+='<div style="background:var(--card);border:1px solid var(--border);border-radius:10px;padding:14px 20px">';
    h+='<div style="font-size:11px;color:var(--t3)">Easiest Remaining Schedule</div>';
    h+='<div style="font-size:18px;font-weight:700;color:var(--over)">'+sn(easiest.name)+'</div>';
    h+='<div style="font-size:11px;color:var(--t3)">Avg opp: .'+(Math.round(easiest.ss.avgOppWp*1000)+'').padStart(3,'0')+' | '+easiest.ss.weak+' weak opponents</div></div>';
  }
  h+='</div>';

  // Table header
  h+='<div style="display:grid;grid-template-columns:30px 130px 55px 50px 70px 70px 60px 55px 55px;gap:8px;padding:4px 10px;font-size:10px;font-weight:700;color:var(--t3);text-transform:uppercase;letter-spacing:.5px">';
  h+='<span></span><span>Team</span><span>Record</span><span>Rem</span><span>Avg Opp WP</span><span>Eff. Str.</span><span>Strong</span><span>Mid</span><span>Weak</span></div>';

  rows.forEach(function(r){
    var isExpanded=!!expandedTeams[r.name];
    var oC=wpColor(r.ss.avgEffWp);
    h+='<div class="trow" style="grid-template-columns:30px 130px 55px 50px 70px 70px 60px 55px 55px;cursor:pointer" onclick="toggleTeamSched(\''+r.name.replace(/'/g,"\\'")+'\')">';
    h+='<span style="font-size:12px;transition:transform .2s;display:inline-block;transform:rotate('+(isExpanded?'90':'0')+'deg)">&#9654;</span>';
    h+='<span class="team-name">'+sn(r.name)+'</span>';
    h+='<span class="mono">'+r.w+'-'+r.l+'</span>';
    h+='<span class="mono">'+r.rem+'</span>';
    h+='<span class="mono" style="color:'+oC+'">.'+(Math.round(r.ss.avgOppWp*1000)+'').padStart(3,'0')+'</span>';
    h+='<span class="mono" style="color:'+oC+'">.'+(Math.round(r.ss.avgEffWp*1000)+'').padStart(3,'0');
    if(r.ss.restCount>0)h+=' &#x1F4A4;';
    h+='</span>';
    h+='<span class="mono" style="color:var(--under)">'+r.ss.strong+'</span>';
    h+='<span class="mono" style="color:var(--gold)">'+r.ss.mid+'</span>';
    h+='<span class="mono" style="color:var(--over)">'+r.ss.weak+'</span>';
    h+='</div>';

    // Expanded: individual games
    if(isExpanded&&r.sched.length){
      h+='<div style="margin:0 0 12px 40px;border-left:2px solid var(--border);padding-left:12px">';
      h+='<div style="display:grid;grid-template-columns:85px 30px 120px 65px 55px 70px;gap:6px;padding:4px 0;font-size:10px;font-weight:700;color:var(--t3);text-transform:uppercase;letter-spacing:.5px">';
      h+='<span>Date</span><span></span><span>Opponent</span><span>Opp Record</span><span>Opp WP</span><span>Note</span></div>';
      r.sched.forEach(function(g){
        var oWp=oppWinPct(g.opp);
        var oTd=D.teams[g.opp];
        var oRec=oTd?(oTd.w+'-'+oTd.l):'-';
        var rest=isRestCandidate(g.opp);
        var gc=wpColor(oWp);
        h+='<div style="display:grid;grid-template-columns:85px 30px 120px 65px 55px 70px;gap:6px;padding:3px 0;font-size:12px;border-bottom:1px solid var(--border)">';
        h+='<span class="mono" style="font-size:11px;color:var(--t3)">'+g.d+'</span>';
        h+='<span style="font-size:10px;color:var(--t3)">'+(g.home?'vs':'@')+'</span>';
        h+='<span class="team-name" style="font-size:12px">'+sn(g.opp)+'</span>';
        h+='<span class="mono" style="font-size:11px;color:var(--t2)">'+oRec+'</span>';
        h+='<span class="mono" style="color:'+gc+'">.'+(Math.round(oWp*1000)+'').padStart(3,'0')+'</span>';
        h+='<span style="font-size:10px">';
        if(rest)h+='<span style="color:var(--gold)" title="Clinched + strong record - may rest starters">&#x1F4A4; May rest</span>';
        else h+='<span style="color:var(--t3)">'+wpLabel(oWp)+'</span>';
        h+='</span></div>';
      });
      h+='</div>';
    }
  });

  el.innerHTML=h;
}

var razorAdaptive=true;
function toggleRazor(){razorAdaptive=!razorAdaptive;renderAll()}
var tabDefs=[
  {id:'rooting',label:'Rooting Guide',needsPlayer:true,render:renderTab1},
  {id:'tonight',label:"Tonight's Games",needsPlayer:false,render:renderTab2},
  {id:'pace',label:'Pace & Projections',needsPlayer:false,render:renderTab3},
  {id:'sched',label:'Schedule Strength',needsPlayer:false,render:renderTab5},
  {id:'field',label:'Player vs Field',needsPlayer:true,render:renderTab4}
];
var activeTab='rooting',activePlayer=P[0]||'Chester';
function renderAll(){
  document.getElementById('tabs').innerHTML=tabDefs.map(function(t){return '<div class="tab '+(t.id===activeTab?'active':'')+'" onclick="switchTab(\''+t.id+'\')">'+t.label+'</div>'}).join('');
  var panel=document.getElementById('panels');var def=tabDefs.filter(function(t){return t.id===activeTab})[0];
  var h='';
  if(def.needsPlayer){
    h+='<div class="player-row"><span class="label">Player</span>';
    P.forEach(function(p){var sc=getScore(p);h+='<button class="pp '+(p===activePlayer?'active':'')+'" onclick="switchPlayer(\''+p+'\')">'+p+' <span style="opacity:.6;font-family:JetBrains Mono;font-size:10px">('+(sc.total>=0?'+':'')+sc.total+')</span></button>'});
    h+='</div>';
  }
  h+='<div id="tab-content"></div>';panel.innerHTML=h;
  var content=document.getElementById('tab-content');
  try{
    if(def.needsPlayer) def.render(content,activePlayer); else def.render(content);
  }catch(e){
    content.innerHTML='<div class="error-box">Render error in '+def.id+':\n'+e.message+'\n\nStack: '+e.stack+'</div>';
    console.error(e);
  }
}
function switchTab(id){activeTab=id;renderAll()}
function switchPlayer(p){activePlayer=p;renderAll()}
try{renderAll()}catch(e){
  document.getElementById('tabs').innerHTML='<div class="tab active">Error</div>';
  document.getElementById('panels').innerHTML='<div class="error-box">Fatal render error:\n'+e.message+'\n\nStack: '+(e.stack||'')
    +'\n\nD.teams keys: '+Object.keys(D.teams||{}).join(', ')
    +'\n\nD.picks keys: '+Object.keys(D.picks||{}).join(', ')
    +'\n\nP: '+JSON.stringify(P)+'</div>';
  console.error(e);
}