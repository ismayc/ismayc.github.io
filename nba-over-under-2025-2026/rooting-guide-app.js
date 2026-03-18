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
  for(var nm in D.teams){var t=D.teams[nm];if(t.det==='not yet'){var gap=t.pWins-t.wp;var wpn=(t.wtg!==null&&t.rem>0)?Math.round(t.wtg/t.rem*100):null;undecided.push({name:nm,gap:gap,w:t.w,l:t.l,wp:t.wp,pWins:t.pWins,wpn:wpn,rem:t.rem,wtg:t.wtg,ltg:t.ltg})}}
  undecided.sort(function(a,b){
    // Distance from nearest extreme: min(wpn, 100-wpn). Lower = closer to clinching.
    var aw=a.wpn!==null?a.wpn:50, bw=b.wpn!==null?b.wpn:50;
    var da=Math.min(aw,100-aw), db=Math.min(bw,100-bw);
    return da-db;
  });
  h+='<div style="display:grid;grid-template-columns:140px 55px 50px 50px 80px 1fr 70px;gap:8px;padding:4px 10px;font-size:10px;font-weight:700;color:var(--t3);text-transform:uppercase;letter-spacing:.5px"><span>Team</span><span>Record</span><span>Target</span><span>Pace</span><span>Gap</span><span>OVER Likelihood</span><span>Win% Needed</span></div>';
  undecided.forEach(function(t){
    var g=t.gap,gc=g>0?'var(--over)':'var(--under)';
    // Single bar: fill = likelihood of going OVER = (100 - wpn), clamped 0-100
    var fill=t.wpn!==null?Math.max(0,Math.min(100,100-t.wpn)):50;
    var barCol=fill>=60?'var(--over)':fill>=40?'var(--gold)':'var(--under)';
    var dl='',dc='var(--t2)';
    if(t.wpn!==null){if(t.wpn>100){dl='Impossible';dc='var(--under)'}else if(t.wpn>80){dl=t.wpn+'%';dc='var(--under)'}else if(t.wpn>60){dl=t.wpn+'%';dc='var(--gold)'}else{dl=t.wpn+'%';dc='var(--over)'}}else{dl='Clinched'}
    h+='<div class="trow" style="grid-template-columns:140px 55px 50px 50px 80px 1fr 70px">';
    h+='<span class="team-name">'+sn(t.name)+'</span>';
    h+='<span class="mono">'+t.w+'-'+t.l+'</span>';
    h+='<span class="mono" style="color:var(--t3)">'+t.wp+'</span>';
    h+='<span class="mono" style="color:'+gc+'">'+t.pWins+'</span>';
    h+='<span class="mono" style="color:'+gc+'">'+(g>0?'+':'')+g.toFixed(1)+'</span>';
    h+='<div style="display:flex;align-items:center"><div style="width:100%;height:14px;border-radius:3px;overflow:hidden;background:var(--border)"><div style="width:'+fill+'%;height:100%;background:'+barCol+';border-radius:3px;transition:width .3s"></div></div></div>';
    h+='<span class="mono" style="text-align:center;color:'+dc+';font-size:11px">'+dl+'</span></div>';
  });
  var close=undecided.filter(function(t){return Math.abs(t.gap)<5});
  if(close.length){
    h+='<div class="section-title" style="margin-top:24px">Razor-thin margins (pace within 5 wins of target)</div><div style="display:flex;flex-wrap:wrap;gap:6px">';
    close.forEach(function(t){h+='<span class="chip chip-'+(t.gap>0?'o':'u')+'">'+sn(t.name)+': '+(t.gap>0?'+':'')+t.gap.toFixed(1)+' '+(t.gap>0?'over':'under')+' pace</span>'});
    h+='</div>';
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

var tabDefs=[
  {id:'rooting',label:'Rooting Guide',needsPlayer:true,render:renderTab1},
  {id:'tonight',label:"Tonight's Games",needsPlayer:false,render:renderTab2},
  {id:'pace',label:'Pace & Projections',needsPlayer:false,render:renderTab3},
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
