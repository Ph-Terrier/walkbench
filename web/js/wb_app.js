/*
 * Title: Interface of the walkbench web simulator, models M1 and M2
 * Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
 * Produced by: written by the orchestrating model of the web-simulator session of SIM-ACI
 *   (Claude Opus 5.5), on 2026-11-19, without a code-generation model. The drawing of the
 *   walker, of the phase portrait and of the step durations is adapted from
 *   feasibility/walker_demo.html, written on 2026-08-20 by Claude Fable 5, orchestrating model
 *   of the feasibility session.
 * Date: 2026-11-19
 * Description: live animation of M1 or M2 driven by the engine of the model (the engine of the
 *   reference runs), indicative estimates during the animation (wb_stream.js), a reference run
 *   of transient + 600 steps whose outcomes follow estimate_all() of walkbench
 *   (wb_estimators.js), and the R code that reruns the reference run in walkbench. The input
 *   is an ARFIMA series drawn with the generator of R (wb_input.js) or a series read from a
 *   local file, which stays in the browser. Texts in French and English (wb_i18n.js).
 *   Changes from the demonstration: one engine for the animation and the reference values;
 *   input series of 8192 values rescaled as a whole (the demonstration rescaled blocks of 3000
 *   values separately); durations cleared after a fall; model M2 with arced feet; fitting
 *   windows set by the visitor; reference run and R code; local file; two languages; system
 *   fonts. Second version of the same day: model M2 and export of R code, after the decisions
 *   of the author. Change of 2026-11-20 by the same model, after the remarks of the author on
 *   the first trial: the walker is drawn at the scale of the canvas height (its leg measured
 *   130 px before), the camera keeps the hip at 64% of the width (42% before), the canvas is 200
 *   to 270 px high (240 to 330 px before), the preset "Beyond sigma_max" takes 3 sigma_max (M1:
 *   0.006 rad before; M2: 0.0004; 4 sigma_max in a first change of the same day, then 3 sigma_max
 *   after the decision of the author) and the slider ends at 5 sigma_max (M1: 0.008 rad before;
 *   M2: 0.0005). The legend of the walker became a line of text under the canvas, the bars of the
 *   input stay under the ground, and the box of the phase portrait grows to twice its first
 *   size at most, so that frequent falls do not shrink the limit cycle. Also on 2026-11-20, after
 *   the decision of the author: the exported R code holds a commented line that installs
 *   walkbench from its public repository (no tag before the release of 0.7.0). Change of
 *   2026-11-21 by the same model, after the decision of the author relayed by the coordination
 *   session of SIM-ACI (Claude Fable 5.2): a file with a line that does not hold one number
 *   written with a decimal point is refused, and the message names the lines; the comment of
 *   the exported R code states the format of the file. Later that day, after a second decision
 *   of the author: a file whose values are all equal has its own message, and the status line
 *   of the reference run shows the seed of each attempt.
 * Dependencies: wb_i18n.js, wb_estimators.js, wb_input.js, wb_m1.js, wb_m2.js, wb_stream.js
 * Input: optional text file chosen by the visitor
 * Output: none (nothing leaves the browser)
 */
(function () {
  "use strict";
  const WB = window.WB;
  const el = {};
  ["cv", "cvA", "cvB", "cvD", "playBtn", "resetBtn", "speed", "speedVal", "sigma", "sigmaVal",
    "sigmaLabel", "sigmaHint", "dmem", "dVal", "stepCount", "fallCount", "durVal", "epsVal",
    "epsLabel", "alphaVal", "lamS", "lamL", "fallMsg", "langBtn", "modelM1", "modelM2",
    "srcArfima", "srcFile", "fileInput", "fileInfo", "seed", "ws0", "ws1", "wl0", "wl1",
    "windowsLabel", "windowsNote", "windowsPlan", "windowsError", "refText", "refBtn",
    "refStatus", "refTable", "divPanel", "rPanel", "rCode", "rCopy", "rCopyStatus", "sceneLegend"]
    .forEach(function (id) { el[id] = document.getElementById(id); });

  const MODEL_PER_SECOND = 2.9; // model time per second of animation at speed 1
  const N_KEEP = 600;           // kept steps of a reference run (300 strides, analysis plan)

  // Settings of each model: amplitudes of the analysis plan (calibration runs, section 4.5) and
  // fitting windows of table 5.1. The preset "Beyond sigma_max" takes 3 sigma_max (M1: 0.0039 rad;
  // M2: 0.0003), the value chosen by the author on 2026-11-20, and the slider ends at 5 sigma_max:
  // closer to sigma_max, falls from the fixed point take hundreds of steps
  // (tools/03_fall_rates.js, validation/fall_rates.md).
  const MODELS = {
    M1: {
      mod: WB.m1, r: 0, sigmaMax: 0.0195, sigmaStep: 0.00002, sigmaRef: 0.00234, sigmaBeyond: 0.0117,
      sigmaDigits: 5, win: { s: [0, 0.1], l: [3, 4] }, winStepS: 0.05, horizonMin: 4,
      strideUnits: false, theilerSteps: 2
    },
    M2: {
      mod: WB.m2, r: 0.45, sigmaMax: 0.0015, sigmaStep: 0.000005, sigmaRef: 0.00018, sigmaBeyond: 0.0009,
      sigmaDigits: 6, win: { s: [0, 0.5], l: [4, 10] }, winStepS: 0.1, horizonMin: 10,
      strideUnits: true, theilerSteps: 1
    }
  };

  const S = {
    model: "M1", paused: false, sigma: 0.00234, d: 0, source: "arfima", fileSeries: null,
    fileName: "series.txt", fileMsg: null, seed: 1, windowsS: [0, 0.1], windowsL: [3, 4],
    fp: null, nT: 20, fpCache: {}, engine: null, live: null, livePtr: 0, liveBlocks: 0,
    stream: null, durations: [], alpha: NaN, steps: 0, falls: 0, fallUntil: 0, x0: 0,
    trail: [], ticks: [], budget: 0, lastT: null, lastResults: null, running: false
  };
  const tmp4 = new Float64Array(4);
  function MD() { return MODELS[S.model]; }

  // ---------- storage and language ----------
  function storageGet(k) {
    try { return window.localStorage.getItem(k); } catch (e) { return null; }
  }
  function storageSet(k, v) {
    try { window.localStorage.setItem(k, v); } catch (e) { /* storage unavailable: no memory of the choice */ }
  }
  function initialLang() {
    const s = storageGet("walkbench_sim_lang");
    if (s === "fr" || s === "en") return s;
    const nl = String(navigator.language || "en").toLowerCase();
    return nl.indexOf("fr") === 0 ? "fr" : "en";
  }
  function mkey(base) {
    return base + (S.model === "M2" ? "_m2" : "_m1");
  }

  // ---------- number formats ----------
  function num(x, digits) {
    if (x === null || x === undefined || !Number.isFinite(x)) return "–";
    let s = x.toFixed(digits);
    if (WB.lang === "fr") s = s.replace(".", ",");
    return s;
  }
  function signed(x, digits) {
    if (!Number.isFinite(x)) return "–";
    return (x >= 0 ? "+" : "") + num(x, digits);
  }
  function sep() {
    return WB.lang === "fr" ? " ; " : "; ";
  }

  // ---------- model, fixed point, inputs ----------
  function fixedPointOf(model) {
    if (!S.fpCache[model]) {
      const fp = model === "M2" ? WB.m2.fixedPoint({}, WB.m2.X_INIT_PLAN) : WB.m1.fixedPoint({});
      fp.nTransient = WB.m2.transientCycles(Math.max.apply(null, fp.eig.mod));
      S.fpCache[model] = fp;
    }
    return S.fpCache[model];
  }
  function hashSeed(seed, a) {
    return (Math.imul(seed >>> 0, 2654435761) ^ Math.imul(a + 1, 40503) ^ 0x5bd1e995) >>> 0;
  }
  function newLiveInput() {
    if (S.source === "file" && S.fileSeries) {
      S.live = WB.input.standardize(Float64Array.from(S.fileSeries), S.sigma);
    } else {
      const rng = new WB.input.RRng(hashSeed(S.seed, 100000 + S.liveBlocks));
      S.liveBlocks++;
      S.live = WB.input.inputArfima(8192, S.d, S.sigma, rng, 0);
    }
    S.livePtr = 0;
  }
  function nextEps() {
    if (!S.live || S.livePtr >= S.live.length) {
      if (S.source === "file" && S.fileSeries) S.livePtr = 0; else newLiveInput();
    }
    return S.live[S.livePtr++];
  }

  // ---------- live simulation ----------
  function startEngine() {
    S.engine = new (MD().mod.Engine)({});
    S.engine.start(S.fp.y0, nextEps);
  }
  // the windows of M2 are in strides; the streaming estimator works in steps
  function newStream() {
    const f = MD().strideUnits ? 2 : 1;
    S.stream = new WB.StreamDivergence({
      dt: MD().mod.DEFAULTS.dt, stepLen: S.fp.period,
      windowS: [S.windowsS[0] * f, S.windowsS[1] * f], windowL: [S.windowsL[0] * f, S.windowsL[1] * f],
      horizon: Math.max(MD().horizonMin, S.windowsL[1]) * f, theiler: MD().theilerSteps
    });
    el.lamS.textContent = "–";
    el.lamL.textContent = "–";
  }
  function resetLive() {
    S.steps = 0; S.falls = 0; S.durations = []; S.alpha = NaN; S.x0 = 0; S.trail = []; S.ticks = [];
    S.budget = 0;
    attr.n = 0;
    attr.box = { x0: -0.30, x1: 0.30, y0: -0.50, y1: 0.35 };
    replayAttractor();
    newLiveInput();
    startEngine();
    newStream();
    el.stepCount.textContent = "0";
    el.fallCount.textContent = "0";
    el.durVal.textContent = "–";
    el.epsVal.textContent = "–";
    el.alphaVal.textContent = "–";
  }
  // positions in leg lengths: stance arc centre, hip, swing arc centre (r = 0 for M1)
  function geometry(y, x0, r) {
    const th = y[0], psi = y[0] - y[2];
    const cx = x0 - r * th, cy = r;
    const hx = cx - (1 - r) * Math.sin(th), hy = cy + (1 - r) * Math.cos(th);
    return { th: th, psi: psi, cx: cx, cy: cy, hx: hx, hy: hy,
      wx: hx + (1 - r) * Math.sin(psi), wy: hy - (1 - r) * Math.cos(psi) };
  }
  function onContact(ev) {
    const r = MD().r;
    const g = geometry(ev.yPre, S.x0, r);
    // the swing foot becomes the stance foot: its arc centre keeps its position
    S.x0 = g.wx + r * S.engine.y[0];
    S.ticks.push({ x: g.wx, e: ev.u });
    if (S.ticks.length > 260) S.ticks.shift();
    S.durations.push(ev.duration);
    if (S.durations.length > 1200) S.durations.shift();
    S.steps++;
    if (S.steps % 5 === 0) S.alpha = WB.est.estimateDfa(S.durations.slice(-512));
    el.stepCount.textContent = String(S.steps);
    el.durVal.textContent = num(ev.duration, 3);
    el.epsVal.textContent = signed(S.engine.eps, MD().sigmaDigits);
    el.alphaVal.textContent = num(S.alpha, 2);
    const lam = S.stream.lambdas();
    if (lam) {
      const f = MD().strideUnits ? 2 * S.fp.period : 1;
      el.lamS.textContent = num(lam.ls * f, 3);
      el.lamL.textContent = signed(lam.ll * f, 3);
    }
  }
  function onFall() {
    S.falls++;
    el.fallCount.textContent = String(S.falls);
    S.fallUntil = performance.now() + 1300;
    el.fallMsg.style.display = "flex";
    S.stream.barrier();
    S.durations = [];
    S.alpha = NaN;
    el.alphaVal.textContent = "–";
    startEngine();
  }
  function advanceLive(dtModel) {
    S.budget += dtModel;
    const eng = S.engine, mod = MD().mod;
    for (let guard = 0; guard < 20000; guard++) {
      const need = eng.nextGridTime() - eng.tLoc;
      if (need > S.budget) break;
      const t0 = eng.tGlob + eng.tLoc;
      const r = eng.advance();
      if (r === mod.FALL) {
        S.budget = 0;
        onFall();
        break;
      }
      S.budget -= (eng.tGlob + eng.tLoc) - t0;
      if (r === mod.SAMPLE) {
        mod.toContinuousInto(eng.y, eng.stance, tmp4, 0);
        S.stream.add(tmp4[0], tmp4[1], tmp4[2], tmp4[3]);
        attractorAdd(tmp4[0], tmp4[1]);
      } else if (r === mod.CONTACT) {
        onContact(eng.lastEvent);
      }
    }
  }

  // ---------- colors and canvases ----------
  const css = {};
  function refreshCss() {
    const cs = getComputedStyle(document.documentElement);
    ["ink", "muted", "line", "line-soft", "teal", "purple", "coral", "blue", "card", "tick-up",
      "tick-dn", "shade"].forEach(function (k) { css[k] = cs.getPropertyValue("--" + k).trim(); });
  }
  function fontFamily() {
    return getComputedStyle(document.body).fontFamily;
  }
  function sizeCanvas(c, hCss) {
    const dpr = Math.min(window.devicePixelRatio || 1, 2);
    const w = c.clientWidth || 680;
    c.width = Math.round(w * dpr);
    c.height = Math.round(hCss * dpr);
    c.style.height = hCss + "px";
    const ctx = c.getContext("2d");
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    return ctx;
  }
  let ctx, ctxA, ctxB;
  const attr = { n: 0, cap: 65536, xs: new Float32Array(65536), ys: new Float32Array(65536),
    box: { x0: -0.30, x1: 0.30, y0: -0.50, y1: 0.35 }, off: null, octx: null };
  const AH = 240;
  function layout() {
    const hMain = Math.max(200, Math.min(270, el.cv.clientWidth * 0.34));
    ctx = sizeCanvas(el.cv, hMain);
    ctxA = sizeCanvas(el.cvA, AH);
    ctxB = sizeCanvas(el.cvB, AH);
    const dpr = Math.min(window.devicePixelRatio || 1, 2);
    attr.off = document.createElement("canvas");
    attr.off.width = el.cvA.width;
    attr.off.height = el.cvA.height;
    attr.octx = attr.off.getContext("2d");
    attr.octx.setTransform(dpr, 0, 0, dpr, 0, 0);
    replayAttractor();
    if (S.lastResults) drawDivergence(S.lastResults);
  }

  // ---------- walker (adapted from the demonstration; arced feet for M2) ----------
  function drawScene() {
    const W = el.cv.clientWidth, Hc = parseFloat(el.cv.style.height) || 300;
    ctx.clearRect(0, 0, W, Hc);
    const groundY = Hc - 44;
    // leg length in pixels: the hip, at most one leg length above the ground, stays 30 px
    // below the top edge; line widths follow the scale
    const lpx = Math.max(80, groundY - 30), k = lpx / 130;
    const r = MD().r;
    const g = geometry(S.engine.y, S.x0, r);
    const cam = g.hx * lpx - W * 0.64;
    const sx = function (x) { return x * lpx - cam; };
    const sy = function (y) { return groundY - y * lpx; };
    ctx.strokeStyle = css.line;
    ctx.lineWidth = 1.5;
    ctx.beginPath(); ctx.moveTo(0, groundY); ctx.lineTo(W, groundY); ctx.stroke();
    ctx.strokeStyle = css["line-soft"];
    const m0 = Math.floor(cam / 60) * 60;
    for (let x = m0; x < cam + W + 60; x += 60) {
      ctx.beginPath(); ctx.moveTo(x - cam, groundY); ctx.lineTo(x - cam, groundY + 7); ctx.stroke();
    }
    // input at each foot strike (epsilon for M1, floor height h for M2): bars from a line 22 px
    // below the ground, 8 px long at sigma_ref and 16 px at most, so that they stay in the canvas
    const scale = 8 / MD().sigmaRef;
    for (const t of S.ticks) {
      const x = sx(t.x);
      if (x < -10 || x > W + 10) continue;
      ctx.strokeStyle = t.e < 0 ? css["tick-up"] : css["tick-dn"];
      ctx.lineWidth = 2.5;
      const len = Math.min(16, Math.max(2, Math.abs(t.e) * scale));
      ctx.beginPath(); ctx.moveTo(x, groundY + 22); ctx.lineTo(x, groundY + 22 + (t.e > 0 ? len : -len)); ctx.stroke();
    }
    // hip trail
    S.trail.push([g.hx, g.hy]);
    if (S.trail.length > 900) S.trail.shift();
    ctx.strokeStyle = css.blue; ctx.globalAlpha = 0.35; ctx.lineWidth = 1.2;
    ctx.beginPath();
    let started = false;
    for (const p of S.trail) {
      const x = sx(p[0]);
      if (x < -5) { started = false; continue; }
      if (!started) { ctx.moveTo(x, sy(p[1])); started = true; } else ctx.lineTo(x, sy(p[1]));
    }
    ctx.stroke(); ctx.globalAlpha = 1;
    ctx.lineCap = "round";
    // swing leg, then stance leg
    const legs = [[g.wx, g.wy, g.psi, css.muted, 3 * k], [g.cx, g.cy, g.th, css.ink, 4.5 * k]];
    for (const L of legs) {
      ctx.strokeStyle = L[3]; ctx.lineWidth = L[4];
      // the leg runs from the hip through the centre of the arc to the sole (length 1)
      ctx.beginPath(); ctx.moveTo(sx(g.hx), sy(g.hy));
      ctx.lineTo(sx(L[0] + r * Math.sin(L[2])), sy(L[1] - r * Math.cos(L[2]))); ctx.stroke();
      if (r > 0) {
        // arc of the foot around its centre, about the direction of the leg
        const a0 = Math.atan2(Math.cos(L[2]), Math.sin(L[2]));
        ctx.beginPath(); ctx.arc(sx(L[0]), sy(L[1]), r * lpx, a0 - 0.75, a0 + 0.75); ctx.stroke();
      } else {
        ctx.fillStyle = L[3];
        ctx.beginPath(); ctx.arc(sx(L[0]), sy(L[1]), L[4] * 0.75, 0, 7); ctx.fill();
      }
    }
    ctx.fillStyle = css.coral;
    ctx.beginPath(); ctx.arc(sx(g.hx), sy(g.hy), 9 * k, 0, 7); ctx.fill();
  }

  // ---------- phase portrait, long exposure (adapted from the demonstration) ----------
  function aSX(v) {
    const W = el.cvA.clientWidth, pad = 30, b = attr.box;
    return pad + (v - b.x0) / (b.x1 - b.x0) * (W - 2 * pad);
  }
  function aSY(v) {
    const b = attr.box;
    return AH - 24 - (v - b.y0) / (b.y1 - b.y0) * (AH - 46);
  }
  // the box of the phase portrait grows with the cloud of points up to twice its first size, so
  // that the paths of the falls leave it without shrinking the limit cycle; points outside the
  // box are kept and not drawn
  const BOX_MAX = { x0: -0.60, x1: 0.60, y0: -1.00, y1: 0.70 };
  function inBox(x, y) {
    const b = attr.box;
    return x >= b.x0 && x <= b.x1 && y >= b.y0 && y <= b.y1;
  }
  function attractorAdd(x, y) {
    const i = attr.n % attr.cap;
    attr.xs[i] = x; attr.ys[i] = y; attr.n++;
    const b = attr.box;
    if (!inBox(x, y)) {
      const nb = { x0: Math.max(BOX_MAX.x0, Math.min(b.x0, x - 0.05)), x1: Math.min(BOX_MAX.x1, Math.max(b.x1, x + 0.05)),
        y0: Math.max(BOX_MAX.y0, Math.min(b.y0, y - 0.05)), y1: Math.min(BOX_MAX.y1, Math.max(b.y1, y + 0.05)) };
      if (nb.x0 !== b.x0 || nb.x1 !== b.x1 || nb.y0 !== b.y0 || nb.y1 !== b.y1) {
        attr.box = nb;
        replayAttractor();
        return;
      }
    }
    if (!attr.octx || !inBox(x, y)) return;
    attr.octx.globalAlpha = 0.28; attr.octx.fillStyle = css.teal;
    attr.octx.fillRect(aSX(x) - 0.7, aSY(y) - 0.7, 1.4, 1.4);
    attr.octx.globalAlpha = 1;
  }
  function replayAttractor() {
    if (!attr.octx) return;
    attr.octx.clearRect(0, 0, el.cvA.clientWidth, AH);
    attr.octx.globalAlpha = 0.28; attr.octx.fillStyle = css.teal;
    const n0 = Math.max(0, attr.n - attr.cap);
    for (let k = n0; k < attr.n; k++) {
      const i = k % attr.cap;
      if (inBox(attr.xs[i], attr.ys[i])) attr.octx.fillRect(aSX(attr.xs[i]) - 0.7, aSY(attr.ys[i]) - 0.7, 1.4, 1.4);
    }
    attr.octx.globalAlpha = 1;
  }
  function drawAttractor() {
    const W = el.cvA.clientWidth;
    ctxA.clearRect(0, 0, W, AH);
    ctxA.strokeStyle = css["line-soft"]; ctxA.lineWidth = 1;
    ctxA.beginPath(); ctxA.moveTo(aSX(0), 8); ctxA.lineTo(aSX(0), AH - 22); ctxA.stroke();
    ctxA.beginPath(); ctxA.moveTo(24, aSY(0)); ctxA.lineTo(W - 24, aSY(0)); ctxA.stroke();
    if (attr.off && attr.off.width > 0) ctxA.drawImage(attr.off, 0, 0, W, AH);
    MD().mod.toContinuousInto(S.engine.y, S.engine.stance, tmp4, 0);
    ctxA.fillStyle = css.coral;
    ctxA.beginPath(); ctxA.arc(aSX(tmp4[0]), aSY(tmp4[1]), 3.6, 0, 7); ctxA.fill();
    ctxA.fillStyle = css.muted;
    ctxA.font = "11px " + fontFamily();
    ctxA.fillText("θG (rad)", W - 76, aSY(0) - 6);
    ctxA.fillText("θ̇G", aSX(0) + 6, 16);
  }

  // ---------- step durations (adapted from the demonstration) ----------
  function drawDurations() {
    const W = el.cvB.clientWidth, Hc = AH;
    ctxB.clearRect(0, 0, W, Hc);
    const data = S.durations.slice(-160);
    const pad = 40;
    const font = fontFamily();
    if (data.length < 2) {
      ctxB.fillStyle = css.muted; ctxB.font = "12px " + font;
      ctxB.fillText(WB.t("waiting"), pad, Hc / 2);
      return;
    }
    const lo = Math.min.apply(null, data), hi = Math.max.apply(null, data);
    const span = Math.max(hi - lo, 0.002), y0 = lo - span * 0.15, y1 = hi + span * 0.15;
    const sx = function (i) { return pad + i / Math.max(data.length - 1, 1) * (W - pad - 14); };
    const sy = function (v) { return Hc - 26 - (v - y0) / (y1 - y0) * (Hc - 48); };
    const mu = data.reduce(function (a, b) { return a + b; }, 0) / data.length;
    ctxB.strokeStyle = css["line-soft"]; ctxB.setLineDash([4, 4]);
    ctxB.beginPath(); ctxB.moveTo(pad, sy(mu)); ctxB.lineTo(W - 14, sy(mu)); ctxB.stroke();
    ctxB.setLineDash([]);
    ctxB.strokeStyle = css.purple; ctxB.lineWidth = 1.4;
    ctxB.beginPath();
    data.forEach(function (v, i) { if (i === 0) ctxB.moveTo(sx(i), sy(v)); else ctxB.lineTo(sx(i), sy(v)); });
    ctxB.stroke();
    ctxB.fillStyle = css.purple;
    data.forEach(function (v, i) { ctxB.fillRect(sx(i) - 1, sy(v) - 1, 2, 2); });
    ctxB.fillStyle = css.muted; ctxB.font = "11px " + font;
    ctxB.fillText(num(hi, 3), 4, sy(hi) + 4);
    ctxB.fillText(num(lo, 3), 4, sy(lo) + 4);
    ctxB.fillText(WB.t("dur_legend"), pad, Hc - 8);
  }

  // ---------- reference run ----------
  function setStatus(s) {
    el.refStatus.textContent = s;
  }
  function tick() {
    return new Promise(function (resolve) { setTimeout(resolve, 30); });
  }
  function readWindows() {
    const v = [el.ws0, el.ws1, el.wl0, el.wl1].map(function (n) { return parseFloat(n.value); });
    const ok = v.every(Number.isFinite) && v[0] >= 0 && v[0] < v[1] && v[1] <= 10 &&
      v[2] >= 0 && v[2] < v[3] && v[3] <= 10;
    el.windowsError.textContent = ok ? "" : WB.t("windows_error");
    return ok ? { s: [v[0], v[1]], l: [v[2], v[3]] } : null;
  }
  function hideResults() {
    S.lastResults = null;
    el.refTable.hidden = true;
    el.divPanel.hidden = true;
    el.rPanel.hidden = true;
  }
  async function referenceRun() {
    if (S.running) return;
    const w = readWindows();
    if (!w) return;
    S.running = true;
    el.refBtn.disabled = true;
    hideResults();
    const tStart = performance.now();
    const model = S.model, mod = MD().mod, nT = S.nT;
    const settings = Object.assign(mod.settings(), { windowS: w.s, windowL: w.l, horizon: Math.max(MD().horizonMin, w.l[1]) });
    let input = null, run = null, redraws = 0, nKeep = N_KEEP, info = null;
    try {
      if (S.source === "file" && S.fileSeries) {
        const series = Float64Array.from(S.fileSeries.slice(0, N_KEEP));
        nKeep = series.length;
        input = WB.input.inputFixedSeries(series, nT, S.sigma);
        setStatus(WB.t("ref_sim", { a: 1 }));
        await tick();
        run = mod.simulateRun({}, input, nT + nKeep, S.fp.y0);
        if (run.fell) {
          setStatus(WB.t("ref_fall_file", { k: run.fallCycle }));
          return;
        }
        info = { kind: "file", sigma: S.sigma, fileName: S.fileName };
      } else {
        let seedUsed = S.seed;
        for (let a = 0; a <= 20; a++) {
          // attempt a + 1 uses the seed of the field plus a; the exported R code gives the seed
          // of the attempt that did not fall
          seedUsed = S.seed + a;
          setStatus((a > 0 ? WB.t("ref_fall_redraw", { k: run.fallCycle, a: a }) + " " : "") +
            WB.t("ref_sim_seed", { a: a + 1, s: seedUsed }));
          await tick();
          input = WB.input.inputArfima(nT + nKeep, S.d, S.sigma, new WB.input.RRng(seedUsed), nT);
          run = mod.simulateRun({}, input, nT + nKeep, S.fp.y0);
          if (!run.fell) break;
          redraws = a + 1;
        }
        if (run.fell) {
          setStatus(WB.t("ref_all_fell"));
          return;
        }
        redraws = Math.min(redraws, 20);
        info = { kind: "arfima", d: S.d, sigma: S.sigma, seed: seedUsed };
      }
      const tSim = performance.now() - tStart;
      setStatus(WB.t("ref_est"));
      await tick();
      const t1 = performance.now();
      const tr = mod.trimRun(run, nT);
      const res = WB.est.estimateAll(tr, input, settings);
      const tEst = performance.now() - t1;
      S.lastResults = { model: model, res: res, info: info, nT: nT, nKeep: nKeep, redraws: redraws,
        tSim: tSim, tEst: tEst, settings: settings };
      renderResults(S.lastResults);
      setStatus(WB.t("ref_done", { s: num((performance.now() - tStart) / 1000, 1) }) +
        (info.kind === "file" && nKeep < N_KEEP ? " " + WB.t("ref_short", { n: nKeep }) : ""));
    } catch (e) {
      setStatus("Error: " + (e && e.message ? e.message : String(e)));
    } finally {
      S.running = false;
      el.refBtn.disabled = false;
    }
  }
  function renderResults(R) {
    const r = R.res, info = R.info, mk = R.model === "M2" ? "_m2" : "_m1";
    const digits = MODELS[R.model].sigmaDigits;
    const inputText = info.kind === "file"
      ? WB.t("r_input_file", { s: num(info.sigma, digits) })
      : WB.t("r_input_arfima", { d: num(info.d, 2), s: num(info.sigma, digits), g: info.seed });
    const rows = [
      ["r_model", WB.t("model" + mk)],
      ["r_input", inputText],
      ["r_steps", R.nT + " + " + R.nKeep],
      ["r_redraws", String(R.redraws)],
      ["r_mean", num(r.meanDuration, 4)],
      ["r_sd", num(r.sdDuration, 4) + sep() + num(r.cvDuration, 4)],
      ["r_ls" + mk, num(r.lambdaS, 4) + sep() + num(r.lambdaSPerStep, 4)],
      ["r_ll" + mk, signed(r.lambdaL, 4) + sep() + signed(r.lambdaLPerStep, 4)],
      ["r_pairs", String(r.nPairs)],
      ["r_alpha", num(r.alphaInput, 3) + sep() + num(r.alphaDuration, 3) + sep() + num(r.alphaStrideDuration, 3)],
      ["r_time", num(R.tSim / 1000, 1) + " s" + sep() + num(R.tEst / 1000, 1) + " s"]
    ];
    el.refTable.textContent = "";
    for (const row of rows) {
      const tr = document.createElement("tr");
      const a = document.createElement("td");
      a.textContent = WB.t(row[0]);
      const b = document.createElement("td");
      b.className = "num";
      b.textContent = row[1];
      tr.appendChild(a); tr.appendChild(b);
      el.refTable.appendChild(tr);
    }
    el.refTable.hidden = false;
    el.divPanel.hidden = false;
    drawDivergence(R);
    el.rCode.textContent = rCode(R);
    el.rCopyStatus.textContent = "";
    el.rPanel.hidden = false;
  }
  // R code that reruns the reference run in walkbench (numbers written with a decimal point)
  function rCode(R) {
    const n = R.nT + R.nKeep, w = R.settings, info = R.info;
    const L = ["# Rerun in walkbench (version 0.7.0 or later) of the reference run of the web simulator",
      "# install.packages(\"remotes\"); remotes::install_github(\"Ph-Terrier/walkbench\")",
      "library(walkbench)"];
    if (R.model === "M1") {
      L.push("m <- model_m1()");
    } else {
      L.push("m <- model_m2(gamma = 0.004, r_f = 0.45, k = 5,");
      L.push("              x_init = c(" + WB.m2.X_INIT_PLAN.map(String).join(", ") + "))");
    }
    if (info.kind === "arfima") {
      L.push("inp <- input_arfima(" + n + ", d = " + String(info.d) + ", sd = " + String(info.sigma) +
        ", seed = " + info.seed + ", n_transient = " + R.nT + ")");
    } else {
      L.push("# one number per line, with a decimal point; the text after # is a comment");
      L.push("values <- scan(\"" + String(info.fileName).replace(/["\\]/g, "") + "\", comment.char = \"#\")");
      L.push("inp <- input_fixed_series(values[seq_len(" + R.nKeep + ")], n_transient = " + R.nT +
        ", sd = " + String(info.sigma) + ")");
    }
    L.push("run <- trim_run(simulate_run(m, inp, n_cycles = " + n + "), n_transient = " + R.nT + ")");
    L.push("s <- m$estimation_settings");
    L.push("s$window_s <- c(" + w.windowS[0] + ", " + w.windowS[1] + ")");
    L.push("s$window_l <- c(" + w.windowL[0] + ", " + w.windowL[1] + ")");
    L.push("s$horizon <- " + w.horizon);
    L.push("row <- estimate_all(run, m, settings = s, input = inp)");
    L.push("row[, c(\"lambda_s\", \"lambda_l\", \"lambda_s_per_step\", \"lambda_l_per_step\",");
    L.push("        \"alpha_input\", \"alpha_duration\", \"alpha_stride_duration\")]");
    return L.join("\n");
  }
  function copyCode() {
    const text = el.rCode.textContent;
    const selectIt = function () {
      const range = document.createRange();
      range.selectNodeContents(el.rCode);
      const sel = window.getSelection();
      sel.removeAllRanges();
      sel.addRange(range);
      el.rCopyStatus.textContent = WB.t("r_code_select");
    };
    if (navigator.clipboard && navigator.clipboard.writeText) {
      navigator.clipboard.writeText(text).then(function () {
        el.rCopyStatus.textContent = WB.t("r_code_copied");
      }, selectIt);
    } else {
      selectIt();
    }
  }
  function drawDivergence(R) {
    if (el.divPanel.hidden) return;
    const c = sizeCanvas(el.cvD, 260);
    const W = el.cvD.clientWidth, Hc = 260;
    const dv = R.res.divergence, u = dv.unitLen, s = R.settings;
    const font = fontFamily();
    c.clearRect(0, 0, W, Hc);
    const n = dv.mld.length;
    const xs = [], ys = [];
    for (let i = 0; i < n; i++) {
      if (Number.isFinite(dv.mld[i])) { xs.push(dv.time[i] / u); ys.push(dv.mld[i]); }
    }
    if (!ys.length) return;
    const xMax = dv.time[n - 1] / u;
    let yLo = Math.min.apply(null, ys), yHi = Math.max.apply(null, ys);
    const pad = (yHi - yLo) * 0.08 || 0.1;
    yLo -= pad; yHi += pad;
    const L = 46, Rr = 14, T = 10, B = 30;
    const sx = function (x) { return L + x / xMax * (W - L - Rr); };
    const sy = function (y) { return T + (yHi - y) / (yHi - yLo) * (Hc - T - B); };
    c.fillStyle = css.shade;
    [s.windowS, s.windowL].forEach(function (wn) {
      c.fillRect(sx(wn[0]), T, Math.max(1, sx(wn[1]) - sx(wn[0])), Hc - T - B);
    });
    c.strokeStyle = css.line; c.lineWidth = 1;
    c.beginPath(); c.moveTo(L, T); c.lineTo(L, Hc - B); c.lineTo(W - Rr, Hc - B); c.stroke();
    c.fillStyle = css.muted; c.font = "11px " + font;
    for (let k = 0; k <= Math.floor(xMax); k++) {
      c.beginPath(); c.moveTo(sx(k), Hc - B); c.lineTo(sx(k), Hc - B + 4); c.stroke();
      c.fillText(String(k), sx(k) - 3, Hc - B + 15);
    }
    c.fillText(WB.t(R.model === "M2" ? "div_x_m2" : "div_x_m1"), W - Rr - 90, Hc - 4);
    c.fillText(num(yHi, 1), 4, sy(yHi) + 10);
    c.fillText(num(yLo, 1), 4, sy(yLo));
    c.strokeStyle = css.muted; c.globalAlpha = 0.6; c.lineWidth = 1;
    c.beginPath();
    xs.forEach(function (x, i) { if (i === 0) c.moveTo(sx(x), sy(ys[i])); else c.lineTo(sx(x), sy(ys[i])); });
    c.stroke(); c.globalAlpha = 1;
    if (dv.mldSmooth !== dv.mld) {
      c.strokeStyle = css.ink; c.lineWidth = 2;
      c.beginPath();
      let st = false;
      for (let i = 0; i < n; i++) {
        const y = dv.mldSmooth[i];
        if (!Number.isFinite(y)) { st = false; continue; }
        const px = sx(dv.time[i] / u), py = sy(y);
        if (!st) { c.moveTo(px, py); st = true; } else c.lineTo(px, py);
      }
      c.stroke();
    }
    // slopes, drawn through the mean of the curve in each window
    [[s.windowS, R.res.lambdaS, css.teal], [s.windowL, R.res.lambdaL, css.blue]].forEach(function (it) {
      const wn = it[0], lam = it[1];
      if (!Number.isFinite(lam)) return;
      let sumY = 0, cnt = 0;
      for (let i = 0; i < n; i++) {
        const xi = dv.time[i] / u;
        if (xi >= wn[0] && xi <= wn[1] && Number.isFinite(dv.mldSmooth[i])) { sumY += dv.mldSmooth[i]; cnt++; }
      }
      if (!cnt) return;
      const xm = (wn[0] + wn[1]) / 2, ym = sumY / cnt, half = Math.max((wn[1] - wn[0]) / 2, 0.15);
      c.strokeStyle = it[2]; c.lineWidth = 2.5;
      c.beginPath();
      c.moveTo(sx(xm - half), sy(ym - lam * u * half));
      c.lineTo(sx(xm + half), sy(ym + lam * u * half));
      c.stroke();
    });
  }

  // ---------- controls ----------
  function fmtControls() {
    el.sigmaVal.textContent = num(parseFloat(el.sigma.value), MD().sigmaDigits);
    el.dVal.textContent = num(parseFloat(el.dmem.value), 2);
    el.speedVal.textContent = num(parseFloat(el.speed.value), 2);
  }
  function updateFileInfo() {
    if (!S.fileMsg) { el.fileInfo.textContent = ""; return; }
    if (S.fileMsg.bad) {
      // lines with a decimal comma first; at most five line numbers, and the text of the first
      const comma = S.fileMsg.bad.filter(function (b) { return b.kind === "comma"; });
      const list = comma.length ? comma : S.fileMsg.bad;
      const lines = list.slice(0, 5).map(function (b) { return b.line; }).join(", ") + (list.length > 5 ? ", …" : "");
      const text = list[0].text.length > 40 ? list[0].text.slice(0, 40) + "…" : list[0].text;
      el.fileInfo.textContent = WB.t(comma.length ? "file_bad_comma" : "file_bad_format",
        { n: list.length, lines: lines, text: text });
      return;
    }
    el.fileInfo.textContent = S.fileMsg.constant ? WB.t("file_constant") : S.fileMsg.error ? WB.t("file_error")
      : WB.t("file_read", { n: S.fileMsg.n, m: num(S.fileMsg.m, 4), s: num(S.fileMsg.s, 4) });
  }
  function modelLabels() {
    el.sigmaLabel.textContent = WB.t(mkey("sigma"));
    el.sigmaHint.textContent = WB.t(mkey("sigma_hint"));
    el.epsLabel.textContent = WB.t(mkey("eps_now"));
    el.sceneLegend.textContent = WB.t(mkey("scene_legend"));
    el.windowsLabel.textContent = WB.t(mkey("windows"));
    el.windowsNote.textContent = WB.t(mkey("windows_note"));
    el.refText.textContent = WB.t("ref_text", { nt: S.nT });
  }
  function applyNoise() {
    S.sigma = parseFloat(el.sigma.value);
    S.d = parseFloat(el.dmem.value);
    S.source = el.srcFile.checked && S.fileSeries ? "file" : "arfima";
    if (el.srcFile.checked && !S.fileSeries) el.srcArfima.checked = true;
    S.seed = Math.max(1, Math.floor(parseFloat(el.seed.value) || 1));
    newLiveInput();
    S.durations = [];
    S.alpha = NaN;
    el.alphaVal.textContent = "–";
    S.stream.barrier();
    S.stream.resetCurve();
    el.lamS.textContent = "–";
    el.lamL.textContent = "–";
  }
  let noiseTimer = null;
  function queueNoise() {
    fmtControls();
    clearTimeout(noiseTimer);
    noiseTimer = setTimeout(applyNoise, 150);
  }
  function setWindowInputs(w) {
    el.ws0.value = String(w.s[0]); el.ws1.value = String(w.s[1]);
    el.wl0.value = String(w.l[0]); el.wl1.value = String(w.l[1]);
    el.ws0.step = el.ws1.step = String(MD().winStepS);
  }
  function applyWindows() {
    const w = readWindows();
    if (!w) return;
    S.windowsS = w.s;
    S.windowsL = w.l;
    newStream();
  }
  function setModel(model) {
    if (S.running || model === S.model && S.engine) return;
    S.model = model;
    const md = MD();
    S.fp = fixedPointOf(model);
    S.nT = S.fp.nTransient;
    el.sigma.min = "0";
    el.sigma.max = String(md.sigmaMax);
    el.sigma.step = String(md.sigmaStep);
    el.sigma.value = String(md.sigmaRef);
    el.dmem.value = "0";
    S.windowsS = md.win.s.slice();
    S.windowsL = md.win.l.slice();
    setWindowInputs(md.win);
    readWindows();
    S.sigma = md.sigmaRef;
    S.d = 0;
    hideResults();
    setStatus("");
    modelLabels();
    fmtControls();
    resetLive();
  }
  function setPaused(p) {
    S.paused = p;
    el.playBtn.textContent = S.paused ? "▶ " + WB.t("play") : "⏸ " + WB.t("pause");
  }
  function applyLang() {
    document.documentElement.lang = WB.lang;
    document.title = WB.t("page_title");
    document.querySelectorAll("[data-i18n]").forEach(function (n) { n.textContent = WB.t(n.getAttribute("data-i18n")); });
    document.querySelectorAll("[data-i18n-html]").forEach(function (n) { n.innerHTML = WB.t(n.getAttribute("data-i18n-html")); });
    el.langBtn.textContent = WB.t("lang_button");
    setPaused(S.paused);
    modelLabels();
    fmtControls();
    updateFileInfo();
    readWindows();
    if (S.lastResults) renderResults(S.lastResults);
  }

  el.modelM1.addEventListener("change", function () { if (el.modelM1.checked) setModel("M1"); });
  el.modelM2.addEventListener("change", function () { if (el.modelM2.checked) setModel("M2"); });
  el.sigma.addEventListener("input", queueNoise);
  el.dmem.addEventListener("input", queueNoise);
  el.seed.addEventListener("change", applyNoise);
  el.speed.addEventListener("input", fmtControls);
  el.srcArfima.addEventListener("change", applyNoise);
  el.srcFile.addEventListener("change", applyNoise);
  el.playBtn.addEventListener("click", function () { setPaused(!S.paused); });
  el.resetBtn.addEventListener("click", resetLive);
  [el.ws0, el.ws1, el.wl0, el.wl1].forEach(function (n) { n.addEventListener("change", applyWindows); });
  el.windowsPlan.addEventListener("click", function () {
    setWindowInputs(MD().win);
    applyWindows();
  });
  el.refBtn.addEventListener("click", referenceRun);
  el.rCopy.addEventListener("click", copyCode);
  el.langBtn.addEventListener("click", function () {
    WB.lang = WB.lang === "fr" ? "en" : "fr";
    storageSet("walkbench_sim_lang", WB.lang);
    applyLang();
  });
  document.querySelectorAll(".presets button").forEach(function (b) {
    b.addEventListener("click", function () {
      const p = b.getAttribute("data-p").split(",");
      const md = MD();
      const sig = p[0] === "none" ? 0 : p[0] === "ref" ? md.sigmaRef : md.sigmaBeyond;
      el.sigma.value = String(sig);
      el.dmem.value = p[1];
      fmtControls();
      applyNoise();
    });
  });
  el.fileInput.addEventListener("change", function () {
    const f = el.fileInput.files && el.fileInput.files[0];
    if (!f) return;
    const reader = new FileReader();
    reader.onload = function () {
      const read = WB.input.readSeriesText(reader.result);
      const vals = read.values;
      if (read.bad.length) {
        S.fileSeries = null;
        S.fileMsg = { bad: read.bad };
      } else if (WB.input.seriesProblem(vals, 120)) {
        S.fileSeries = null;
        S.fileMsg = WB.input.seriesProblem(vals, 120) === "constant" ? { constant: true } : { error: true };
      } else {
        S.fileSeries = vals;
        S.fileName = f.name;
        S.fileMsg = { n: vals.length, m: WB.est.meanR(vals), s: WB.est.sdR(vals) };
        el.srcFile.checked = true;
      }
      updateFileInfo();
      applyNoise();
    };
    reader.readAsText(f);
  });
  window.addEventListener("keydown", function (e) {
    if (e.code === "Space" && !/INPUT|BUTTON|SELECT|TEXTAREA/.test(document.activeElement.tagName)) {
      e.preventDefault();
      setPaused(!S.paused);
    }
  });
  window.addEventListener("resize", layout);
  function onTheme() {
    refreshCss();
    replayAttractor();
    if (S.lastResults) drawDivergence(S.lastResults);
  }
  window.matchMedia("(prefers-color-scheme: dark)").addEventListener("change", onTheme);
  new MutationObserver(onTheme).observe(document.documentElement, { attributes: true, attributeFilter: ["data-theme"] });

  // ---------- main loop ----------
  function frame(t) {
    requestAnimationFrame(frame);
    if (S.lastT === null) { S.lastT = t; return; }
    let dt = (t - S.lastT) / 1000;
    S.lastT = t;
    if (dt > 0.06) dt = 0.06;
    const falling = performance.now() < S.fallUntil;
    if (!falling) el.fallMsg.style.display = "none";
    if (!S.paused && !falling && !S.running) advanceLive(dt * MODEL_PER_SECOND * parseFloat(el.speed.value));
    drawScene();
    drawAttractor();
    drawDurations();
  }

  // hook for checks of the interface from the console of a browser (advance the animation by a
  // given model time and draw, without requestAnimationFrame)
  WB.app = {
    state: S,
    step: function (dtModel) { advanceLive(dtModel); drawScene(); drawAttractor(); drawDurations(); }
  };

  // ---------- start ----------
  WB.lang = initialLang();
  refreshCss();
  layout();
  S.model = "";
  el.modelM1.checked = true;
  setModel("M1");
  applyLang();
  if (window.matchMedia("(prefers-reduced-motion: reduce)").matches) setPaused(true);
  requestAnimationFrame(frame);
})();
