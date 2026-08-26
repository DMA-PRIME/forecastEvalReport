#' Peak-forecast carousel: three linked plots of the peak metrics
#'
#' Builds a self-contained SVG/JS widget showing, for the selected
#' location x season, three plots the viewer can page through:
#'   1. Peak Timing & Peak-to-Peak Magnitude (two-armed crosshair),
#'   2. Magnitude at Predicted Peak (vertical gap to the observed curve that
#'      same week),
#'   3. Magnitude at Observed Peak (vertical gap at the true peak week).
#' Each plot shows the observed curve, one forecast line + one dot per horizon,
#' and isolates a horizon's crosshair on hover. `window.peakGraphRedraw(label,
#' season)` repoints it from the section dropdowns.
#'
#' @param data Testing evaluation frame (`value`, `Observed`, `target_end_date`,
#'   `reference_date`, `horizon`, `location`) -- the source of the curves.
#' @param loc Location code(s) to include.
#' @param training.data.label Unused (kept for signature compatibility).
#' @param outcome Character outcome label for axis/legend text.
#' @param peakTrough.data Output of `calculating_peak_trough_PEAKPHASE()` -- the
#'   per-horizon peak points and metrics.
#' @param season_month Integer month the season starts (default 8 = August),
#'   matched to the calc so seasons align.
#'
#' @return Rendered HTML via [htmltools::HTML()], or an empty HTML on no data.
#' @keywords internal
#' @noRd
make_peak_phase_plot <- function(data, loc, training.data.label = NULL,
                                 outcome = NULL, peakTrough.data = NULL,
                                 season_month = 8) {

  if(is.null(peakTrough.data) || !is.data.frame(peakTrough.data) ||
     nrow(peakTrough.data) == 0) return(htmltools::HTML(""))
  if(is.null(data) || !is.data.frame(data) || nrow(data) == 0)
    return(htmltools::HTML(""))

  outcome_lab <- if(is.null(outcome) || is.na(outcome) || !nzchar(outcome))
    "Observed" else outcome
  iso  <- function(d) format(as.Date(d), "%Y-%m-%d")
  num1 <- function(x){ x <- suppressWarnings(as.numeric(x))
    if(length(x) == 0 || is.na(x[1])) NA else round(x[1], 2) }

#------------------------------------------------------------------------------#
# Prepare the series source (curves) -------------------------------------------
#------------------------------------------------------------------------------#

  D <- data
  D$reference_date  <- as.Date(D$reference_date)
  D$target_end_date <- as.Date(D$target_end_date)
  ref_month <- as.numeric(format(D$reference_date, "%m"))
  ref_year  <- as.numeric(format(D$reference_date, "%Y"))
  D$season  <- ifelse(ref_month < season_month,
                      paste0(ref_year - 1, "-", ref_year),
                      paste0(ref_year, "-", ref_year + 1))
  D$forecastValue <- D$value
  D$targetValue   <- D$Observed
  D <- D[D$location %in% loc, , drop = FALSE]

  # Display label per code (from peakTrough.data if it carries one)
  resolve_label <- function(cd){
    if("location_display" %in% names(peakTrough.data)){
      cand <- unique(peakTrough.data$location_display[peakTrough.data$location == cd])
      cand <- cand[!is.na(cand) & nzchar(cand)]
      if(length(cand)) return(cand[1])
    }
    as.character(cd)
  }

#------------------------------------------------------------------------------#
# Build the per (location x season) data object --------------------------------
#------------------------------------------------------------------------------#

  keyset <- unique(peakTrough.data[, c("location", "season")])
  pdata  <- list()

  for(r in seq_len(nrow(keyset))){
    cd <- keyset$location[r]; sn <- as.character(keyset$season[r])
    lab <- resolve_label(cd)

    dss <- D[D$location == cd & D$season == sn, , drop = FALSE]
    if(nrow(dss) == 0) next

    # Observed curve: one truth value per target week
    obs_df <- dss[!is.na(dss$targetValue), c("target_end_date", "targetValue")]
    obs_df <- obs_df[!duplicated(obs_df$target_end_date), , drop = FALSE]
    obs_df <- obs_df[order(obs_df$target_end_date), , drop = FALSE]
    if(nrow(obs_df) == 0) next
    obs_series <- lapply(seq_len(nrow(obs_df)), function(i)
      list(d = iso(obs_df$target_end_date[i]), v = num1(obs_df$targetValue[i])))

    pk <- peakTrough.data[peakTrough.data$location == cd &
                          as.character(peakTrough.data$season) == sn, , drop = FALSE]
    if(nrow(pk) == 0) next
    pk <- pk[order(-pk$horizon), , drop = FALSE]   # longest lead first

    obs_peak <- list(d = iso(pk$observedPeakDate[1]),
                     v = num1(pk$observedPeakValue[1]))

    horizons <- lapply(seq_len(nrow(pk)), function(i){
      h  <- pk$horizon[i]
      fs <- dss[dss$horizon == h & !is.na(dss$forecastValue),
                c("target_end_date", "forecastValue")]
      fs <- fs[!duplicated(fs$target_end_date), , drop = FALSE]
      fs <- fs[order(fs$target_end_date), , drop = FALSE]
      series <- lapply(seq_len(nrow(fs)), function(j)
        list(d = iso(fs$target_end_date[j]), v = num1(fs$forecastValue[j])))

      grab <- function(nm) if(nm %in% names(pk)) pk[[nm]][i] else NA
      list(
        name   = as.character(grab("horizonName")),
        series = series,
        ppD    = if(!is.na(grab("predictedPeakDate"))) iso(grab("predictedPeakDate")) else NA,
        ppV    = num1(grab("predictedPeakValue")),
        oap    = num1(grab("observedAtPredictedPeak")),
        pwV    = num1(grab("peakWeekForecastValue")),
        timing = num1(grab("predictedPeakTimingOff")),
        tLab   = as.character(grab("predictedPeakTimingLabel")),
        magOff = num1(grab("predictedPeakMagnitudeOff")),
        magAcc = num1(grab("predictedPeakAccuracy") * 100),
        sdOff  = num1(grab("sameDayMagnitudeOff")),
        sdAcc  = num1(grab("sameDayAccuracy") * 100),
        pwOff  = num1(grab("peakWeekMagnitudeOff")),
        pwAcc  = num1(grab("peakWeekAccuracy") * 100),
        pwEx   = isTRUE(as.logical(grab("peakWeekForecastExists"))))
    })

    pdata[[paste0(lab, "||", sn)]] <- list(
      label = lab, season = sn, outcome = outcome_lab,
      obs = obs_series, obsPeak = obs_peak, horizons = horizons)
  }

  if(length(pdata) == 0) return(htmltools::HTML(""))

  data_json <- jsonlite::toJSON(pdata, auto_unbox = TRUE, null = "null", na = "null")
  cid <- paste0("peakCarousel_", as.integer(stats::runif(1, 1, 1e7)))

#------------------------------------------------------------------------------#
# Self-contained carousel widget -----------------------------------------------
#------------------------------------------------------------------------------#

  htmltools::HTML(paste0('
<div id="', cid, '" class="peak-carousel" style="width:100%;max-width:960px;margin:0 auto;font-family:Arial,Helvetica,sans-serif;position:relative;">
  <div style="display:flex;align-items:center;justify-content:space-between;gap:8px;margin-bottom:4px;">
    <button class="pc-prev" type="button" aria-label="Previous plot" style="border:1px solid #ddd;background:#fff;border-radius:6px;width:32px;height:32px;cursor:pointer;font-size:15px;color:#522D80;">&#9664;</button>
    <div class="pc-title" style="flex:1 1 auto;text-align:center;font-size:17px;font-weight:700;color:#333;"></div>
    <button class="pc-next" type="button" aria-label="Next plot" style="border:1px solid #ddd;background:#fff;border-radius:6px;width:32px;height:32px;cursor:pointer;font-size:15px;color:#522D80;">&#9654;</button>
  </div>
  <div class="pc-readout" style="text-align:center;font-size:14px;font-weight:400;color:#888;line-height:1.5;margin:2px 0 8px;min-height:18px;"></div>
  <div class="pc-dots" style="text-align:center;margin-bottom:6px;"></div>
  <div class="pc-plot" style="position:relative;">
    <svg class="pc-svg" width="100%" viewBox="0 0 720 340" role="img"></svg>
    <div class="pc-float" style="display:none;position:absolute;top:8px;left:66px;min-width:186px;background:#fff;border:1px solid #cbcbcb;border-radius:6px;box-shadow:0 2px 10px rgba(0,0,0,0.16);font-size:12px;color:#333;z-index:6;"></div>
  </div>
  <div class="pc-legend" style="display:flex;flex-wrap:wrap;gap:6px 16px;justify-content:center;align-items:center;margin-top:8px;font-size:12px;color:#555;"></div>
</div>
<script>
(function(){
  var DATA = ', data_json, ';
  var root = document.getElementById("', cid, '");
  if(!root) return;
  var svg=root.querySelector(".pc-svg"), rdEl=root.querySelector(".pc-readout"),
      titleEl=root.querySelector(".pc-title"), dotsEl=root.querySelector(".pc-dots"),
      legEl=root.querySelector(".pc-legend"), floatEl=root.querySelector(".pc-float");
  var NS="http://www.w3.org/2000/svg";
  var keys=Object.keys(DATA);
  var MON=["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"];
  var PLOTS=[
    {t:"Peak Timing & Peak-to-Peak Magnitude", hint:"Hover a forecast peak to preview its gaps; Click it for all metrics."},
    {t:"Magnitude at Predicted Peak", hint:"Hover a forecast peak to preview the same-week gap; Click it for all metrics."},
    {t:"Magnitude at Observed Peak", hint:"Hover a dot to preview the peak-week gap; Click it for all metrics."}
  ];
  var state={ key:keys[0], plot:0, hidden:{}, hideObs:false };
  var W=720,H=340,mL=64,mR=18,mT=26,mB=48;

  function el(t,a){ var e=document.createElementNS(NS,t); for(var k in a) e.setAttribute(k,a[k]); return e; }
  function dnum(s){ return Math.round(new Date(s+"T00:00:00").getTime()/86400000); }
  function fmtDate(s){ return s; }
  function hx(a,b,t){ function p(h){return [parseInt(h.substr(1,2),16),parseInt(h.substr(3,2),16),parseInt(h.substr(5,2),16)];}
    var x=p(a),y=p(b),o="#"; for(var i=0;i<3;i++){var v=Math.round(x[i]+(y[i]-x[i])*t).toString(16); o+=(v.length<2?"0":"")+v;} return o; }
  function ramp(i,n){ return n<=1 ? "#6C63C9" : hx("#C9C4F2","#3C3489", i/(n-1)); }
  function fS(v){ if(v==null) return String.fromCharCode(8212); return (v>0?"+":"")+v; }
  function fI(v){ if(v==null) return String.fromCharCode(8212); return Math.round(v).toLocaleString(); }
  function pcnt(v){ return v==null ? String.fromCharCode(8212) : Math.round(v)+"%"; }

  var drag=null;
  floatEl.addEventListener("mousedown", function(e){
    if(e.target.classList && e.target.classList.contains("pc-fclose")) return;
    drag={ ox:e.clientX, oy:e.clientY, l:parseFloat(floatEl.style.left)||0, t:parseFloat(floatEl.style.top)||0 }; e.preventDefault();
  });
  document.addEventListener("mousemove", function(e){ if(!drag) return;
    floatEl.style.left=(drag.l+e.clientX-drag.ox)+"px"; floatEl.style.top=(drag.t+e.clientY-drag.oy)+"px"; });
  document.addEventListener("mouseup", function(){ drag=null; });

  function render(){
    var d=DATA[state.key]; svg.innerHTML=""; floatEl.style.display="none";
    if(!d){ rdEl.textContent=""; return; }
    var P=state.plot, locked=null;

    // Label layout constants. Declared here because labels are placed at
    // several points below and these must be assigned before the first one.
    var LPAD=7;
    // Matches the report background behind the SVG. The plate is meant to
    // be invisible: it only knocks out the guide lines behind a label.
    var PLATE_BG="#fafafa";
    // Boxes already occupied by something a label must not cover. Reset per
    // render; applyFocus works from a copy so transient labels do not stick.
    var placed=[];
    titleEl.textContent=PLOTS[P].t; rdEl.textContent="* "+PLOTS[P].hint;

    var xs=[], ys=[0], ymaxData=0;
    function push(v){ if(v!=null){ ys.push(v); if(v>ymaxData) ymaxData=v; } }
    d.obs.forEach(function(p){ xs.push(dnum(p.d)); push(p.v); });
    if(d.obsPeak && d.obsPeak.d!=null){ xs.push(dnum(d.obsPeak.d)); push(d.obsPeak.v); }
    d.horizons.forEach(function(h){ h.series.forEach(function(p){ xs.push(dnum(p.d)); push(p.v); });
      push(h.ppV); push(h.pwV); push(h.oap); });
    var x0=Math.min.apply(null,xs), x1=Math.max.apply(null,xs), ymax=(ymaxData*1.1)||1;
    function X(dn){ return mL+(x1===x0?0:(dn-x0)/(x1-x0))*(W-mL-mR); }
    function Y(v){ return (H-mB)-(v/ymax)*(H-mB-mT); }
    function XD(s){ return X(dnum(s)); }

    svg.appendChild(el("line",{x1:mL,y1:mT,x2:mL,y2:H-mB,stroke:"#ddd","stroke-width":1}));
    svg.appendChild(el("line",{x1:mL,y1:H-mB,x2:W-mR,y2:H-mB,stroke:"#ddd","stroke-width":1}));
    var yc=(mT+(H-mB))/2;
    var ylab=el("text",{x:15,y:yc,"font-size":12,fill:"#666","text-anchor":"middle",transform:"rotate(-90, 15, "+yc+")"}); ylab.textContent=d.outcome; svg.appendChild(ylab);
    [0,1,2,3,4].map(function(mk){return ymaxData*mk/4;}).forEach(function(v){ var yy=Y(v);
      svg.appendChild(el("line",{x1:mL-4,y1:yy,x2:mL,y2:yy,stroke:"#ddd","stroke-width":1}));
      var tt=el("text",{x:mL-7,y:yy+3,"font-size":10,fill:"#999","text-anchor":"end"}); tt.textContent=fI(v); svg.appendChild(tt); });
    var dts=d.obs.map(function(p){return p.d;}), nT=Math.min(5,dts.length);
    for(var k=0;k<nT;k++){ var di=Math.round(k*(dts.length-1)/Math.max(1,nT-1)), ds=dts[di], xx=XD(ds);
      svg.appendChild(el("line",{x1:xx,y1:H-mB,x2:xx,y2:H-mB+4,stroke:"#ddd","stroke-width":1}));
      var anc=(k===0)?"start":(k===(nT-1)?"end":"middle");
      var xt=el("text",{x:xx,y:H-mB+16,"font-size":10,fill:"#999","text-anchor":anc}); xt.textContent=fmtDate(ds); svg.appendChild(xt); }

    var opx=(d.obsPeak&&d.obsPeak.d!=null)?XD(d.obsPeak.d):null;
    var opy=(d.obsPeak&&d.obsPeak.v!=null)?Y(d.obsPeak.v):null;
    // Created here, attached after the series below so the focus guides
    // and labels paint on top of the observed curve and horizon lines
    var xh=el("g",{"pointer-events":"none"});
    function peakGuide(){ xh.appendChild(el("line",{x1:opx,y1:mT-4,x2:opx,y2:H-mB,stroke:"#e8e8e8","stroke-width":1,"stroke-dasharray":"3 5"})); }
    if(P===2 && opx!=null) peakGuide();

    var od=""; d.obs.forEach(function(p){ if(p.v==null) return; od+=(od===""?"M":"L")+XD(p.d).toFixed(1)+","+Y(p.v).toFixed(1); });
    if(od && !state.hideObs) svg.appendChild(el("path",{d:od,fill:"none",stroke:"#333","stroke-width":2.4}));

    var n=d.horizons.length, lineEls=[], dotEls=[];
    d.horizons.forEach(function(h,i){
      if(state.hidden[i]){ lineEls.push(null); dotEls.push(null); return; }
      var c=ramp(i,n), ld="";
      h.series.forEach(function(p){ if(p.v==null) return; ld+=(ld===""?"M":"L")+XD(p.d).toFixed(1)+","+Y(p.v).toFixed(1); });
      var lp=el("path",{d:ld,fill:"none",stroke:c,"stroke-width":1.6}); lp.style.transition="opacity .12s"; svg.appendChild(lp); lineEls.push(lp);
      var dx,dy; if(P===2){ dx=opx; dy=(h.pwV!=null)?Y(h.pwV):null; } else { dx=(h.ppD!=null)?XD(h.ppD):null; dy=(h.ppV!=null)?Y(h.ppV):null; }
      var dot=null;
      if(dx!=null && dy!=null){
        dot=el("circle",{cx:dx,cy:dy,r:5,fill:c}); dot.style.cursor="pointer"; svg.appendChild(dot);
        (function(idx){
          dot.addEventListener("mouseenter",function(){ if(locked==null) applyFocus(idx); });
          dot.addEventListener("mouseleave",function(){ if(locked==null) clearFocus(); });
          dot.addEventListener("click",function(e){ e.stopPropagation();
            if(locked===idx){ locked=null; hideFloat(); clearFocus(); }
            else { locked=idx; applyFocus(idx); showFloat(idx); } });
        })(i);
      }
      dotEls.push(dot);
    });

    if(P!==1 && !state.hideObs && opx!=null && opy!=null){
      svg.appendChild(el("circle",{cx:opx,cy:opy,r:5,fill:"#333"}));
      placed.push({l:opx-7,r:opx+7,t:opy-7,b:opy+7});
      var og=el("g",{}); svg.appendChild(og);
      var al=el("text",{x:opx+8,y:opy-6,"font-size":12,fill:"#444","text-anchor":"start"}); al.textContent="Observed Peak"; og.appendChild(al);
      fitLabel(al, opx+8, opy-6, "start", opx, 8, placed);
      plate(og, al, 12);
      leader(og, opx, opy, al, 6);
    }

    // Focus group goes on top of everything drawn above
    svg.appendChild(xh);

    // ---- Label placement ---------------------------------------------
    // Labels sit beside short segments that can land anywhere in the plot,
    // including hard against an edge. Each label is drawn at its preferred
    // spot, measured, flipped to the other side of its anchor point if that
    // side would spill, then clamped inside the plot rectangle so it is
    // never cut off by the SVG edge or pushed on top of an axis.
    function textWidth(t){
      var w=0;
      try { w=t.getComputedTextLength(); } catch(e){ w=0; }
      return w>0 ? w : (t.textContent||"").length*6.2;
    }
    function extent(x,w,anc){
      if(anc==="end")    return [x-w, x];
      if(anc==="middle") return [x-w/2, x+w/2];
      return [x, x+w];
    }
    // Box a label would occupy, matching the plate padding.
    function boxOf(x,y,w,anc){
      var e=extent(x,w,anc), padX=4, padY=2.5;
      return { l:e[0]-padX, r:e[1]+padX, t:y-8.5-padY, b:y+2.5+padY };
    }
    function overlaps(b,list){
      for(var i=0;i<list.length;i++){ var o=list[i];
        if(b.l<o.r && b.r>o.l && b.t<o.b && b.b>o.t) return true; }
      return false;
    }
    // px and dx are the feature the label points at and its offset from it.
    // When supplied, a label that would spill flips to the mirrored offset
    // on the far side of that feature instead of drifting away from it.
    // occ is the list of boxes already taken; the label steps vertically
    // until it finds a free slot, and its own box is added to that list.
    function fitLabel(t,x,y,anc,px,dx,occ){
      var w=textWidth(t), lo=mL+LPAD, hi=W-mR-LPAD, e=extent(x,w,anc);
      if(px!=null && dx!=null){
        if(e[1]>hi && anc==="start"){ anc="end"; x=px-dx; }
        else if(e[0]<lo && anc==="end"){ anc="start"; x=px+dx; }
        t.setAttribute("text-anchor",anc);
        e=extent(x,w,anc);
      }
      if(e[1]>hi){ x-=(e[1]-hi); e=extent(x,w,anc); }
      if(e[0]<lo){ x+=(lo-e[0]); }
      var yTop=mT+LPAD+9, yBot=H-mB-LPAD-3;
      if(y<yTop) y=yTop;
      if(y>yBot) y=yBot;
      if(occ && occ.length){
        // Step away from the collision, nearest offset first, alternating
        // sides so the label stays as close to its feature as it can.
        var steps=[0,-15,15,-30,30,-45,45,-60,60], pick=null;
        for(var si=0; si<steps.length; si++){
          var yy=y+steps[si];
          if(yy<yTop || yy>yBot) continue;
          if(!overlaps(boxOf(x,yy,w,anc),occ)){ pick=yy; break; }
        }
        if(pick!==null) y=pick;
      }
      if(occ) occ.push(boxOf(x,y,w,anc));
      t.setAttribute("x",x.toFixed(1));
      t.setAttribute("y",y.toFixed(1));
    }
    // A crosshair label lands on top of dashed guides and the observed
    // curve, so it gets an opaque rounded plate behind it. Inserted before
    // the text node so it paints underneath its own label.
    function plate(cg,t,fs){
      var w=textWidth(t), anc=t.getAttribute("text-anchor"),
          x=parseFloat(t.getAttribute("x")), y=parseFloat(t.getAttribute("y")),
          e=extent(x,w,anc), padX=4, padY=2.5,
          asc=(fs==null)?8.5:fs*0.74, hgt=(fs==null)?11:fs;
      cg.insertBefore(el("rect",{
        x:(e[0]-padX).toFixed(1), y:(y-asc-padY).toFixed(1),
        width:(w+padX*2).toFixed(1), height:(hgt+padY*2).toFixed(1),
        rx:2, fill:PLATE_BG}), t);
    }
    // Thin connector from a point to its label. Drawn from the edge of the
    // marker to the nearest edge of the label plate, and skipped entirely
    // when the two are already close enough for the pairing to be obvious.
    function leader(g,cx,cy,t,r){
      var w=textWidth(t), anc=t.getAttribute("text-anchor"),
          x=parseFloat(t.getAttribute("x")), y=parseFloat(t.getAttribute("y")),
          b=boxOf(x,y,w,anc);
      var tx=Math.max(b.l,Math.min(cx,b.r)), ty=Math.max(b.t,Math.min(cy,b.b));
      var vx=tx-cx, vy=ty-cy, len=Math.sqrt(vx*vx+vy*vy);
      if(len<r+8) return;
      var ux=vx/len, uy=vy/len;
      g.insertBefore(el("line",{
        x1:(cx+ux*r).toFixed(1), y1:(cy+uy*r).toFixed(1),
        x2:(tx-ux*1.5).toFixed(1), y2:(ty-uy*1.5).toFixed(1),
        stroke:"#9a9a9a","stroke-width":1}), g.firstChild);
    }
    function lab(cg,x,y,anc,txt,px,dx,occ){
      var t=el("text",{x:x,y:y,"font-size":11.5,fill:"#222","text-anchor":anc,"font-weight":700}); t.textContent=txt; cg.appendChild(t);
      fitLabel(t,x,y,anc,px,dx,occ);
      plate(cg,t);
    }
    function applyFocus(i){
      lineEls.forEach(function(l){ if(l) l.style.opacity=0; }); dotEls.forEach(function(dt,j){ if(dt) dt.style.opacity=(j===i)?1:0; });
      var h=d.horizons[i], cg=el("g",{});
      // Attach first so getComputedTextLength can measure the labels
      xh.innerHTML=""; xh.appendChild(cg);
      // Start from the static boxes, add the focused dot, then let each
      // label add its own so two labels never land on top of each other.
      var occ=placed.slice();
      var fx=(P===2)?opx:((h.ppD!=null)?XD(h.ppD):null);
      var fyd=(P===2)?((h.pwV!=null)?Y(h.pwV):null):((h.ppV!=null)?Y(h.ppV):null);
      if(fx!=null && fyd!=null) occ.push({l:fx-7,r:fx+7,t:fyd-7,b:fyd+7});
      if(P===0){
        var px=XD(h.ppD), py=Y(h.ppV);
        if(opx!=null && opy!=null){
          cg.appendChild(el("line",{x1:opx,y1:mT-4,x2:opx,y2:H-mB,stroke:"#e8e8e8","stroke-width":1,"stroke-dasharray":"3 5"}));
          cg.appendChild(el("line",{x1:mL,y1:opy,x2:W-mR,y2:opy,stroke:"#e8e8e8","stroke-width":1,"stroke-dasharray":"3 5"}));
          cg.appendChild(el("line",{x1:px,y1:py,x2:opx,y2:py,stroke:"#666","stroke-width":1.5,"stroke-dasharray":"4 4"}));
          cg.appendChild(el("line",{x1:px,y1:py,x2:px,y2:opy,stroke:"#666","stroke-width":1.5,"stroke-dasharray":"4 4"}));
          lab(cg,(px+opx)/2, py-9, "middle", fS(h.timing)+" wk", null, null, occ);
          lab(cg,px-9, (py+opy)/2, "end", fS(h.magOff), px, 9, occ);
        }
      } else if(P===1){
        var px2=XD(h.ppD), py2=Y(h.ppV), cy2=(h.oap!=null)?Y(h.oap):null;
        if(cy2!=null){
          cg.appendChild(el("line",{x1:px2,y1:py2,x2:px2,y2:cy2,stroke:"#666","stroke-width":1.5,"stroke-dasharray":"4 4"}));
          cg.appendChild(el("circle",{cx:px2,cy:cy2,r:4,fill:"none",stroke:"#333","stroke-width":1.5}));
          lab(cg,px2+9, (py2+cy2)/2, "start", fS(h.sdOff), px2, 9, occ);
        }
      } else {
        if(opx!=null && opy!=null && h.pwV!=null){ var fy=Y(h.pwV);
          cg.appendChild(el("line",{x1:opx,y1:mT-4,x2:opx,y2:H-mB,stroke:"#e8e8e8","stroke-width":1,"stroke-dasharray":"3 5"}));
          cg.appendChild(el("line",{x1:opx,y1:fy,x2:opx,y2:opy,stroke:"#666","stroke-width":1.5,"stroke-dasharray":"4 4"}));
          lab(cg,opx+11, (fy+opy)/2, "start", fS(h.pwOff), opx, 11, occ);
        }
      }
    }
    function clearFocus(){ lineEls.forEach(function(l){ if(l) l.style.opacity=1; }); dotEls.forEach(function(dt){ if(dt) dt.style.opacity=1; }); xh.innerHTML=""; if(P===2 && opx!=null) peakGuide(); }

    function showFloat(i){
      var h=d.horizons[i], rows="";
      function row(a,b){ return `<div><span style="color:#888;">${a}:</span> ${b}</div>`; }
      if(P===0){
        rows = row("Observed peak", fI(d.obsPeak.v)) + row("Forecasted peak", fI(h.ppV))
             + row("Timing", h.tLab || (fS(h.timing)+" wk"))
             + row("Magnitude", fS(h.magOff)) + row("% Accuracy (Magnitude)", pcnt(h.magAcc));
      } else if(P===1){
        rows = row("Observed (that week)", fI(h.oap)) + row("Forecasted peak", fI(h.ppV))
             + row("Magnitude", fS(h.sdOff)) + row("% Accuracy (Magnitude)", pcnt(h.sdAcc));
      } else {
        rows = h.pwEx
          ? row("Observed peak", fI(d.obsPeak.v)) + row("Forecast (peak week)", fI(h.pwV))
            + row("Magnitude", fS(h.pwOff)) + row("% Accuracy (Magnitude)", pcnt(h.pwAcc))
          : `<div>No forecast targeted the true peak week at this horizon.</div>`;
      }
      floatEl.innerHTML =
        `<div class="pc-fhead" style="cursor:move;padding:6px 10px;border-bottom:1px solid #eee;font-weight:700;display:flex;justify-content:space-between;align-items:center;gap:12px;"><span>${h.name}</span><span class="pc-fclose" style="cursor:pointer;color:#999;font-size:16px;line-height:1;">&times;</span></div>`+
        `<div style="padding:7px 10px;line-height:1.75;">`+rows+`</div>`;
      floatEl.style.display="";
      if(!floatEl.style.left){ floatEl.style.left="66px"; floatEl.style.top="8px"; }
      var cl=floatEl.querySelector(".pc-fclose");
      if(cl) cl.addEventListener("click",function(){ locked=null; hideFloat(); clearFocus(); });
    }
    function hideFloat(){ floatEl.style.display="none"; }

    function legItem(key,label,color,off){
      var op=off?0.4:1, deco=off?"line-through":"none";
      return `<span data-leg="${key}" style="display:inline-flex;align-items:center;cursor:pointer;opacity:${op};text-decoration:${deco};user-select:none;"><span style="display:inline-block;width:16px;height:3px;background:${color};margin-right:6px;"></span>${label}</span>`;
    }
    var leg = legItem("obs","Observed "+d.outcome,"#333",state.hideObs);
    d.horizons.forEach(function(h,i){ leg += legItem("h"+i, h.name, ramp(i,n), !!state.hidden[i]); });
    legEl.innerHTML = leg;
    Array.prototype.forEach.call(legEl.querySelectorAll("[data-leg]"), function(sp){
      sp.addEventListener("click", function(){ var key=sp.getAttribute("data-leg");
        if(key==="obs") state.hideObs=!state.hideObs; else { var ix=+key.slice(1); state.hidden[ix]=!state.hidden[ix]; }
        render(); });
    });

    dotsEl.innerHTML="";
    PLOTS.forEach(function(p,i){ var b=document.createElement("span");
      b.style.cssText="display:inline-block;width:9px;height:9px;border-radius:50%;margin:0 4px;cursor:pointer;background:"+(i===P?"#522D80":"#ccc")+";";
      b.addEventListener("click",function(){ state.plot=i; render(); }); dotsEl.appendChild(b); });
  }

  root.querySelector(".pc-prev").addEventListener("click",function(){ state.plot=(state.plot+2)%3; render(); });
  root.querySelector(".pc-next").addEventListener("click",function(){ state.plot=(state.plot+1)%3; render(); });
  window.peakGraphRedraw=function(label,season){ var k=label+"||"+season;
    if(!DATA[k]){ k=keys.filter(function(z){ return z.indexOf(label+"||")===0; })[0]||keys[0]; }
    if(k){ state.key=k; state.hidden={}; state.hideObs=false; render(); } };
  render();
})();
</script>
'))
}
