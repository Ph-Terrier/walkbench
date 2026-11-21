/*
 * Title: Indicative divergence exponents computed during the animation (streaming estimator)
 * Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
 * Produced by: adapted by the orchestrating model of the web-simulator session of SIM-ACI
 *   (Claude Opus 5.5), on 2026-11-19, from the streaming estimator of
 *   feasibility/walker_demo.html, written on 2026-08-20 by Claude Fable 5, orchestrating
 *   model of the feasibility session; no code-generation model
 * Date: 2026-11-19
 * Description: Rosenstein-type estimate updated during the animation. A ring buffer holds the
 *   samples of the global grid (step 0.03) in continuous coordinates. At regular intervals, the
 *   sample that lies one horizon back serves as reference: its nearest neighbor is searched
 *   among the earlier samples, outside a Theiler window of one stride, and the log distance of
 *   the pair is followed over the horizon. The curve is a running mean of the pairs, then an
 *   exponential mean of memory about 100 steps. Smoothing, local slopes and windows follow
 *   divergence_core() of walkbench. The values are indicative: the search is restricted to
 *   the past and the average is not that of a run. The reference values of the page come from
 *   WB.est.estimateAll() on a whole run.
 *   Changes from the demonstration: samples on the grid of walkbench (0.03) in place of a
 *   decimation of the integration steps (0.024); windows, horizon and memory set by the
 *   caller; running mean before the exponential mean; edges of the smoothing left empty as in
 *   walkbench.
 * Dependencies: wb_estimators.js (WB.est.meanR)
 * Input: none
 * Output: the constructor WB.StreamDivergence on the global object
 */
(function (root) {
  "use strict";
  const WB = (root.WB = root.WB || {});

  function StreamDivergence(opts) {
    this.dt = opts.dt;
    this.stepLen = opts.stepLen;
    this.windowS = opts.windowS.slice();
    this.windowL = opts.windowL.slice();
    this.HOR = Math.round(opts.horizon * opts.stepLen / opts.dt);
    this.THEILER = Math.round((opts.theiler || 2) * opts.stepLen / opts.dt);
    this.RING = 1 << 16;
    this.SEARCH = opts.search || 24000;
    this.REF_EVERY = opts.refEvery || 32;
    this.BETA = opts.beta || 1 / 400;
    this.buf = new Float64Array(this.RING * 4);
    this.mld = new Float64Array(this.HOR + 1);
    this.n = 0;
    this.pairs = 0;
    this.barrierN = 0;
  }

  // Samples after a fall or a change of the input start a new segment: no pair spans two segments
  StreamDivergence.prototype.barrier = function () {
    this.barrierN = this.n;
  };

  // Forget the curve (after a change of the input)
  StreamDivergence.prototype.resetCurve = function () {
    this.pairs = 0;
    this.mld.fill(0);
  };

  StreamDivergence.prototype.add = function (x0, x1, x2, x3) {
    const b = (this.n % this.RING) * 4;
    this.buf[b] = x0; this.buf[b + 1] = x1; this.buf[b + 2] = x2; this.buf[b + 3] = x3;
    this.n++;
    if (this.n % this.REF_EVERY === 0) this.reference();
  };

  StreamDivergence.prototype.reference = function () {
    const j = this.n - 1 - this.HOR;
    if (j < this.barrierN) return;
    const kmin = Math.max(0, j - this.SEARCH, this.n - this.RING, this.barrierN);
    const kmax = j - this.THEILER - 1;
    if (kmax <= kmin) return;
    const buf = this.buf, R = this.RING, jb = (j % R) * 4;
    let best = -1, bd = Infinity;
    for (let k = kmin; k <= kmax; k++) {
      const kb = (k % R) * 4;
      const d0 = buf[jb] - buf[kb], d1 = buf[jb + 1] - buf[kb + 1];
      const d2 = buf[jb + 2] - buf[kb + 2], d3 = buf[jb + 3] - buf[kb + 3];
      const s = d0 * d0 + d1 * d1 + d2 * d2 + d3 * d3;
      if (s < bd) { bd = s; best = k; }
    }
    if (best < 0 || bd === 0) return;
    // running mean for the first pairs, then exponential mean
    const wgt = Math.max(this.BETA, 1 / (this.pairs + 1));
    for (let l = 0; l <= this.HOR; l++) {
      const ib = ((j + l) % R) * 4, kb = ((best + l) % R) * 4;
      const e0 = buf[ib] - buf[kb], e1 = buf[ib + 1] - buf[kb + 1];
      const e2 = buf[ib + 2] - buf[kb + 2], e3 = buf[ib + 3] - buf[kb + 3];
      const s = e0 * e0 + e1 * e1 + e2 * e2 + e3 * e3;
      if (s <= 0) continue;
      const ln = 0.5 * Math.log(s);
      this.mld[l] = this.pairs === 0 ? ln : this.mld[l] + (ln - this.mld[l]) * wgt;
    }
    this.pairs++;
  };

  // lambda_S and lambda_L of the current curve (per unit of model time), or null
  StreamDivergence.prototype.lambdas = function () {
    if (this.pairs < 30) return null;
    const H = this.HOR, dt = this.dt, mld = this.mld;
    const w = [];
    for (let j = 0; j < 7; j++) w.push(0.54 - 0.46 * Math.cos(2 * Math.PI * j / 6));
    const ws = w.reduce(function (a, b) { return a + b; }, 0);
    const sm = new Float64Array(H + 1).fill(NaN);
    for (let i = 3; i <= H - 3; i++) {
      let z = 0;
      for (let j = 0; j < 7; j++) z += (w[j] / ws) * mld[i + 3 - j];
      sm[i] = z;
    }
    const s0 = this.windowS[0] * this.stepLen, s1 = this.windowS[1] * this.stepLen;
    const l0 = this.windowL[0] * this.stepLen, l1 = this.windowL[1] * this.stepLen;
    const sh = [], lo = [];
    for (let i = 1; i <= H - 1; i++) {
      const sl = (sm[i + 1] - sm[i - 1]) / (2 * dt), t = i * dt;
      if (!Number.isFinite(sl)) continue;
      if (t >= s0 && t <= s1) sh.push(sl);
      if (t >= l0 && t <= l1) lo.push(sl);
    }
    if (!sh.length || !lo.length) return null;
    return { ls: WB.est.meanR(sh), ll: WB.est.meanR(lo), pairs: this.pairs };
  };

  WB.StreamDivergence = StreamDivergence;
})(typeof globalThis !== "undefined" ? globalThis : this);
