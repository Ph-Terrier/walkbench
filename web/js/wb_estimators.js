/*
 * Title: Reference estimators of walkbench in JavaScript: divergence exponents and DFA
 * Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
 * Produced by: written by the orchestrating model of the web-simulator session of SIM-ACI
 *   (Claude Opus 5.5), without a code-generation model
 * Date: 2026-11-19
 * Description: port of the estimators of walkbench 0.6.2 (R/estimators.R) used by the page:
 *   - divergence curve of the Rosenstein engine of RACING v1.0.2 (find_nearest_neighbors()
 *     and compute_divergence() of rosenstein_divergence.R): nearest neighbor of every sample
 *     outside a Theiler window, mean log distance over the pairs that still exist after k
 *     samples, zero distances excluded. The exact nearest neighbor is found with a k-d tree;
 *     RACING finds it with the k-d tree of the R package RANN;
 *   - initial separation, Hamming smoothing of 7 points, slopes and windows of
 *     divergence_core() and estimate_divergence() (sampled coordinates, no resampling);
 *   - DFA of Terrier (2026), dfa_terrier2026() and estimate_dfa() with dfa_settings();
 *   - the outcomes of estimate_all() that the page shows.
 *   Means use a compensated sum, close to the long-double sums of R.
 *   Change of 2026-11-20 by the same model: the number of points of the resampling grid follows
 *   grid_count() of walkbench 0.7.0 (count from the number of cycles); the override of that
 *   number, used by the tests to align the page with the grid of walkbench 0.6.2, is removed.
 * Dependencies: none
 * Input: none
 * Output: the object WB.est on the global object
 */
(function (root) {
  "use strict";
  const WB = (root.WB = root.WB || {});

  // --- Numerical helpers close to R --------------------------------------------------------

  // Neumaier compensated sum
  function sumC(a, from, to) {
    from = from || 0;
    to = to === undefined ? a.length : to;
    let s = 0, c = 0;
    for (let i = from; i < to; i++) {
      const v = a[i], t = s + v;
      if (Math.abs(s) >= Math.abs(v)) c += (s - t) + v; else c += (v - t) + s;
      s = t;
    }
    return s + c;
  }

  // mean() of R: sum, division, then one correction pass
  function meanR(a) {
    const n = a.length;
    if (n === 0) return NaN;
    let m = sumC(a) / n;
    if (Number.isFinite(m)) {
      let s = 0, c = 0;
      for (let i = 0; i < n; i++) {
        const v = a[i] - m, t = s + v;
        if (Math.abs(s) >= Math.abs(v)) c += (s - t) + v; else c += (v - t) + s;
        s = t;
      }
      m += (s + c) / n;
    }
    return m;
  }

  // sd() of R (denominator n - 1)
  function sdR(a) {
    const n = a.length;
    if (n < 2) return NaN;
    const m = meanR(a);
    const d2 = new Float64Array(n);
    for (let i = 0; i < n; i++) d2[i] = (a[i] - m) * (a[i] - m);
    return Math.sqrt(sumC(d2) / (n - 1));
  }

  // round() of R for 0 digits: halves to the even integer
  function roundR(x) {
    const r = Math.round(x);
    if (Math.abs(x - Math.trunc(x)) === 0.5) return 2 * Math.round(x / 2);
    return r;
  }

  // cumsum() of R (long-double accumulation, approached by a compensated running sum)
  function cumsumC(a) {
    const out = new Float64Array(a.length);
    let s = 0, c = 0;
    for (let i = 0; i < a.length; i++) {
      const v = a[i], t = s + v;
      if (Math.abs(s) >= Math.abs(v)) c += (s - t) + v; else c += (v - t) + s;
      s = t;
      out[i] = s + c;
    }
    return out;
  }

  // Least-squares slope of y on x (lm.fit with an intercept)
  function olsSlope(x, y) {
    const n = x.length, mx = meanR(x), my = meanR(y);
    const sxy = new Float64Array(n), sxx = new Float64Array(n);
    for (let i = 0; i < n; i++) {
      sxy[i] = (x[i] - mx) * (y[i] - my);
      sxx[i] = (x[i] - mx) * (x[i] - mx);
    }
    return sumC(sxy) / sumC(sxx);
  }

  // --- Exact nearest neighbor outside a Theiler window, by a k-d tree -----------------------

  const LEAF = 12;

  function KdTree(X, N, dim) {
    this.X = X;
    this.N = N;
    this.dim = dim;
    const perm = new Int32Array(N);
    for (let i = 0; i < N; i++) perm[i] = i;
    this.perm = perm;
    const maxNodes = 4 * Math.ceil(N / LEAF) + 16;
    this.lo = new Int32Array(maxNodes);
    this.hi = new Int32Array(maxNodes);
    this.sd = new Int8Array(maxNodes);
    this.split = new Float64Array(maxNodes);
    this.left = new Int32Array(maxNodes).fill(-1);
    this.right = new Int32Array(maxNodes).fill(-1);
    this.nNodes = 0;
    this.build();
  }

  // quickselect: places the element of rank k (by coordinate d) of perm[lo..hi) at index k
  KdTree.prototype.select = function (lo, hi, k, d) {
    const X = this.X, perm = this.perm, dim = this.dim;
    let l = lo, r = hi - 1;
    while (r > l) {
      const pivot = X[perm[(l + r) >> 1] * dim + d];
      let i = l, j = r;
      while (i <= j) {
        while (X[perm[i] * dim + d] < pivot) i++;
        while (X[perm[j] * dim + d] > pivot) j--;
        if (i <= j) {
          const tmp = perm[i]; perm[i] = perm[j]; perm[j] = tmp;
          i++; j--;
        }
      }
      if (k <= j) r = j;
      else if (k >= i) l = i;
      else break;
    }
  };

  KdTree.prototype.build = function () {
    const X = this.X, perm = this.perm, dim = this.dim;
    const stackNode = [], stackLo = [], stackHi = [];
    const rootNode = this.nNodes++;
    stackNode.push(rootNode); stackLo.push(0); stackHi.push(this.N);
    while (stackNode.length) {
      const node = stackNode.pop(), lo = stackLo.pop(), hi = stackHi.pop();
      this.lo[node] = lo;
      this.hi[node] = hi;
      if (hi - lo <= LEAF) continue;
      // dimension of largest spread
      let bestD = 0, bestSpread = -1;
      for (let d = 0; d < dim; d++) {
        let mn = Infinity, mx = -Infinity;
        for (let t = lo; t < hi; t++) {
          const v = X[perm[t] * dim + d];
          if (v < mn) mn = v;
          if (v > mx) mx = v;
        }
        if (mx - mn > bestSpread) { bestSpread = mx - mn; bestD = d; }
      }
      const mid = (lo + hi) >> 1;
      this.select(lo, hi, mid, bestD);
      this.sd[node] = bestD;
      this.split[node] = X[perm[mid] * dim + bestD];
      const l = this.nNodes++, r = this.nNodes++;
      this.left[node] = l;
      this.right[node] = r;
      stackNode.push(l); stackLo.push(lo); stackHi.push(mid);
      stackNode.push(r); stackLo.push(mid); stackHi.push(hi);
    }
  };

  // Nearest point j with |j - qi| > w to the point qi; -1 when there is none
  KdTree.prototype.nearestExcluding = function (qi, w, stackNode, stackBound) {
    const X = this.X, perm = this.perm, dim = this.dim, qo = qi * dim;
    let best = Infinity, bestIdx = -1, sp = 0;
    stackNode[sp] = 0; stackBound[sp] = 0; sp++;
    while (sp > 0) {
      sp--;
      const node = stackNode[sp], bound = stackBound[sp];
      if (bound >= best) continue;
      const l = this.left[node];
      if (l < 0) {
        for (let t = this.lo[node]; t < this.hi[node]; t++) {
          const j = perm[t];
          if (j - qi <= w && qi - j <= w) continue;
          const jo = j * dim;
          let d2 = 0;
          for (let d = 0; d < dim; d++) {
            const diff = X[qo + d] - X[jo + d];
            d2 += diff * diff;
          }
          if (d2 < best) { best = d2; bestIdx = j; }
        }
        continue;
      }
      const diff = X[qo + this.sd[node]] - this.split[node];
      const near = diff < 0 ? l : this.right[node];
      const far = diff < 0 ? this.right[node] : l;
      // lower bounds of the squared distance: the plane for the far child, the parent bound
      // for the near child
      const fb = diff * diff;
      stackNode[sp] = far; stackBound[sp] = fb > bound ? fb : bound; sp++;
      stackNode[sp] = near; stackBound[sp] = bound; sp++;
    }
    return bestIdx;
  };

  // find_nearest_neighbors() of RACING: for every sample, the nearest sample outside the
  // Theiler window |j - i| <= w; indices from 0
  function nearestNeighbors(X, N, dim, w) {
    const tree = new KdTree(X, N, dim);
    const nbr = new Int32Array(N);
    const depth = 4 * Math.ceil(Math.log2(N + 2)) + 64;
    const stackNode = new Int32Array(depth * 2), stackBound = new Float64Array(depth * 2);
    for (let i = 0; i < N; i++) nbr[i] = tree.nearestExcluding(i, w, stackNode, stackBound);
    return nbr;
  }

  function dist(X, dim, a, b) {
    let d2 = 0;
    for (let d = 0; d < dim; d++) {
      const diff = X[a * dim + d] - X[b * dim + d];
      d2 += diff * diff;
    }
    return Math.sqrt(d2);
  }

  // Mean of log distances with zero distances excluded; NaN when no pair is left
  function meanLog(X, dim, idxA, idxB, n, shift) {
    let s = 0, c = 0, m = 0;
    for (let q = 0; q < n; q++) {
      const dd = dist(X, dim, idxA[q] + shift, idxB[q] + shift);
      if (dd > 0) {
        const v = Math.log(dd), t = s + v;
        if (Math.abs(s) >= Math.abs(v)) c += (s - t) + v; else c += (v - t) + s;
        s = t;
        m++;
      }
    }
    return m > 0 ? (s + c) / m : NaN;
  }

  // divergence_core() of walkbench for the settings of the page; nbrGiven, when given, replaces
  // the neighbor search (indices from 0)
  function divergenceCore(X, N, dim, dt, unitLen, s, nbrGiven) {
    const nHor = roundR(s.horizon * unitLen / dt);
    const theiler = roundR(s.theiler * unitLen / dt);
    const nbr = nbrGiven || nearestNeighbors(X, N, dim, theiler);
    let nPairs = 0;
    for (let i = 0; i < N; i++) if (i + 1 <= N - nHor && nbr[i] >= 0 && nbr[i] + 1 <= N - nHor) nPairs++;
    const fixed = s.pairs === "fixed";
    // pairs entering each horizon k: i and nbr(i) both at most N - k (indices from 1)
    const idxA = new Int32Array(N), idxB = new Int32Array(N);
    let nAll = 0;
    for (let i = 0; i < N; i++) {
      if (nbr[i] < 0) continue;
      if (fixed && !(i + 1 <= N - nHor && nbr[i] + 1 <= N - nHor)) continue;
      idxA[nAll] = i; idxB[nAll] = nbr[i]; nAll++;
    }
    const mld = new Float64Array(nHor + 1);
    mld[0] = meanLog(X, dim, idxA, idxB, nAll, 0);
    // compute_divergence(): after k samples, the pairs whose two members are at most N - k
    // (indices from 1), in the order of the reference samples
    const A = new Int32Array(nAll), B = new Int32Array(nAll);
    for (let k = 1; k <= nHor; k++) {
      let nValid = 0;
      for (let q = 0; q < nAll; q++) {
        if (idxA[q] <= N - 1 - k && idxB[q] <= N - 1 - k) { A[nValid] = idxA[q]; B[nValid] = idxB[q]; nValid++; }
      }
      mld[k] = meanLog(X, dim, A, B, nValid, k);
    }
    let mldF = mld;
    if (s.smoothing === "hamming7") {
      const w = new Float64Array(7);
      for (let j = 0; j < 7; j++) w[j] = 0.54 - 0.46 * Math.cos(2 * Math.PI * j / 6);
      const ws = sumC(w);
      for (let j = 0; j < 7; j++) w[j] = w[j] / ws;
      mldF = new Float64Array(nHor + 1).fill(NaN);
      for (let i = 3; i <= nHor - 3; i++) {
        let z = 0;
        for (let j = 0; j < 7; j++) z += w[j] * mld[i + 3 - j];
        mldF[i] = z;
      }
    }
    const tt = new Float64Array(nHor + 1);
    for (let i = 0; i <= nHor; i++) tt[i] = i * dt;
    const slopes = new Float64Array(nHor + 1).fill(NaN);
    let lambdaS, lambdaL;
    const s0 = s.windowS[0] * unitLen, s1 = s.windowS[1] * unitLen;
    const l0 = s.windowL[0] * unitLen, l1 = s.windowL[1] * unitLen;
    if (s.slope === "mean_local") {
      for (let i = 1; i <= nHor - 1; i++) slopes[i] = (mldF[i + 1] - mldF[i - 1]) / (2 * dt);
      const sh = [], lo = [];
      for (let i = 0; i <= nHor; i++) {
        if (!Number.isFinite(slopes[i])) continue;
        if (tt[i] <= s1 && tt[i] >= s0) sh.push(slopes[i]);
        if (tt[i] >= l0 && tt[i] <= l1) lo.push(slopes[i]);
      }
      lambdaS = meanR(sh);
      lambdaL = meanR(lo);
    } else if (s.slope === "lsq") {
      const fit = function (a, b) {
        const xs = [], ys = [];
        for (let i = 0; i <= nHor; i++) {
          if (tt[i] >= a && tt[i] <= b && Number.isFinite(mldF[i])) { xs.push(tt[i]); ys.push(mldF[i]); }
        }
        return olsSlope(xs, ys);
      };
      lambdaS = fit(s0, s1);
      lambdaL = fit(l0, l1);
    } else {
      throw new Error("unknown slope method: " + s.slope);
    }
    return {
      mld: mld, mldSmooth: mldF, slopes: slopes, time: tt, lambdaS: lambdaS, lambdaL: lambdaL,
      nPairs: nPairs, neighbors: nbr, nHorizon: nHor, theiler: theiler
    };
  }

  // approx() of R, linear interpolation at v of the points (x, y), x increasing; NaN outside
  function approxR(x, y, n, v) {
    let i = 0, j = n - 1;
    if (v < x[i] || v > x[j]) return NaN;
    while (i < j - 1) {
      const ij = (i + j) >> 1;
      if (v < x[ij]) j = ij; else i = ij;
    }
    if (v === x[j]) return y[j];
    if (v === x[i]) return y[i];
    return y[i] + (y[j] - y[i]) * ((v - x[i]) / (x[j] - x[i]));
  }

  // grid_count() of walkbench 0.7.0: number of points of the uniform grid, nPer points per unit
  // of perCycles cycles, the start and the end of the run included, counted from the number of
  // cycles. Walkbench 0.6.2 and the first version of this file computed it from the times,
  // floor((t1 - t0) / mean_unit * n_per) + 1, whose argument equals an integer in exact
  // arithmetic for a run of whole strides; a rounding error of 1e-12 could then drop the last
  // grid point (validation/validation_m2.md, section 6)
  function gridCount(nCycles, nPer, perCycles) {
    return Math.floor(nCycles * nPer / perCycles) + 1;
  }

  // resample_states() with method "global_by_cycle": uniform grid of nPer samples per unit of
  // perCycles cycles on average over the run, each grid point interpolated inside its own cycle
  // between the bounds of the cycle; toModel, when given, maps the continuous coordinates of
  // each cycle to the state variables of the model for the stance leg of that cycle
  function resampleGlobalByCycle(run, nPer, perCycles, toModel) {
    const ev = run.events, nc = ev.duration.length;
    const t0 = ev.tStart[0], t1 = ev.tEnd[nc - 1];
    const meanUnit = meanR(ev.duration) * perCycles;
    const nOut = gridCount(nc, nPer, perCycles);
    const grid = new Float64Array(nOut);
    for (let i = 0; i < nOut; i++) grid[i] = Math.min(t0 + i * meanUnit / nPer, t1);
    // findInterval(grid, t_start): cycle k (from 0) with t_start[k] <= t, the last cycle after
    const kOf = new Int32Array(nOut);
    let k = 0;
    for (let i = 0; i < nOut; i++) {
      while (k + 1 < nc && ev.tStart[k + 1] <= grid[i]) k++;
      kOf[i] = k;
    }
    // samples of each cycle (cycles numbered from 1 in run.cycle)
    const first = new Int32Array(nc).fill(-1), count = new Int32Array(nc);
    for (let s = 0; s < run.nSamples; s++) {
      const c = run.cycle[s] - 1;
      if (c < 0 || c >= nc) continue;
      if (first[c] < 0) first[c] = s;
      count[c]++;
    }
    const X = new Float64Array(nOut * 4);
    const cont = new Float64Array(4), mod = new Float64Array(4);
    let i = 0;
    while (i < nOut) {
      const kc = kOf[i];
      let iEnd = i;
      while (iEnd < nOut && kOf[iEnd] === kc) iEnd++;
      const m = count[kc] + 2;
      const tt = new Float64Array(m), yy = new Float64Array(m * 4);
      const leg = ev.leg[kc];
      const put = function (q, c4) {
        if (toModel) { toModel(c4, 0, leg, mod, 0); for (let v = 0; v < 4; v++) yy[q * 4 + v] = mod[v]; }
        else for (let v = 0; v < 4; v++) yy[q * 4 + v] = c4[v];
      };
      tt[0] = ev.tStart[kc];
      put(0, run.boundsStart[kc]);
      for (let q = 0; q < count[kc]; q++) {
        const s = first[kc] + q;
        tt[q + 1] = run.t[s];
        for (let v = 0; v < 4; v++) cont[v] = run.X[4 * s + v];
        put(q + 1, cont);
      }
      tt[m - 1] = ev.tEnd[kc];
      put(m - 1, run.boundsEnd[kc]);
      const yv = new Float64Array(m);
      for (let v = 0; v < 4; v++) {
        for (let q = 0; q < m; q++) yv[q] = yy[q * 4 + v];
        for (let g = i; g < iEnd; g++) X[g * 4 + v] = approxR(tt, yv, m, grid[g]);
      }
      i = iEnd;
    }
    for (let q = 0; q < X.length; q++) {
      if (!Number.isFinite(X[q])) throw new Error("non-finite state after the resampling of each cycle");
    }
    return { X: X, n: nOut, dt: 1 / nPer, grid: grid };
  }

  // estimate_divergence(): sampled coordinates without resampling (settings of M1), or the
  // resampling "global_by_cycle" with the state variables of the model (settings of M2)
  function estimateDivergence(run, s, cyclesPerStride) {
    cyclesPerStride = cyclesPerStride || 2;
    const method = (s.resample && s.resample.method) || "none";
    const cycleLen = meanR(run.events.duration);
    const cycPerUnit = s.windowUnit === "stride" ? cyclesPerStride : 1;
    let X, N, dt, unitLen, stepLen, unit, rs = null;
    if (method === "none") {
      X = run.X; N = run.nSamples; dt = run.dt;
      unitLen = cycPerUnit === 1 ? cycleLen : cycPerUnit * cycleLen;
      stepLen = cycleLen * (cyclesPerStride === 2 ? 1 : 0.5);
      unit = "per unit of model time";
    } else if (method === "global_by_cycle") {
      const per = s.resample.per || s.windowUnit;
      const perCycles = per === "stride" ? cyclesPerStride : 1;
      rs = resampleGlobalByCycle(run, s.resample.nPer, perCycles, s.coordinates === "model" ? s.modelCoords : null);
      X = rs.X; N = rs.n; dt = rs.dt;
      unitLen = cycPerUnit / perCycles;
      stepLen = (cyclesPerStride === 2 ? 1 : 0.5) / perCycles;
      unit = "per " + per;
    } else {
      throw new Error("resampling not ported: " + method);
    }
    const res = divergenceCore(X, N, 4, dt, unitLen, s);
    res.lambdaSPerStep = res.lambdaS * stepLen;
    res.lambdaLPerStep = res.lambdaL * stepLen;
    res.unitLen = unitLen;
    res.unit = unit;
    res.resampled = rs;
    return res;
  }

  // --- DFA of Terrier (2026), configuration of the analysis plan -----------------------------

  function dfaTerrier2026(x, nMin, nMaxRatio, nSizes) {
    nMin = nMin || 16;
    nMaxRatio = nMaxRatio || 1 / 3;
    nSizes = nSizes || 30;
    const nx = x.length;
    const nMax = Math.floor(nx * nMaxRatio);
    if (nMax < nMin + 2) return NaN;
    // seq(0, 1, length.out = nSizes)
    const sq = new Float64Array(nSizes);
    const by = 1 / (nSizes - 1);
    for (let i = 1; i < nSizes - 1; i++) sq[i] = i * by;
    sq[nSizes - 1] = 1;
    const raw = [];
    for (let i = 0; i < nSizes; i++) raw.push(roundR(nMin * Math.pow(nMax / nMin, sq[i])));
    const sizes = raw.filter(function (v, i) { return raw.indexOf(v) === i && v >= nMin && v <= nMax; });
    if (sizes.length < 5) return NaN;
    const m = meanR(x);
    const xc = new Float64Array(nx);
    for (let i = 0; i < nx; i++) xc[i] = x[i] - m;
    const y = cumsumC(xc);
    const fn = sizes.map(function (n) {
      const nBox = Math.floor(nx / n);
      const tBar = (n + 1) / 2, stt = n * (n * n - 1) / 12;
      const res2 = new Float64Array(nBox * n);
      for (let b = 0; b < nBox; b++) {
        const o = b * n;
        let sy = 0, sty = 0;
        for (let t = 0; t < n; t++) sy += y[o + t];
        const yBar = sy / n;
        for (let t = 0; t < n; t++) sty += (t + 1 - tBar) * (y[o + t] - yBar);
        const slope = sty / stt;
        for (let t = 0; t < n; t++) {
          const r = y[o + t] - yBar - slope * (t + 1 - tBar);
          res2[o + t] = r * r;
        }
      }
      return Math.sqrt(meanR(res2));
    });
    const lx = [], ly = [];
    for (let i = 0; i < sizes.length; i++) {
      if (fn[i] > 0) { lx.push(Math.log10(sizes[i])); ly.push(Math.log10(fn[i])); }
    }
    if (lx.length < 5) return NaN;
    return olsSlope(lx, ly);
  }

  // estimate_dfa() with dfa_settings(): no alpha below 96 values or for a constant series
  function estimateDfa(x) {
    if (x.length < 96) return NaN;
    for (let i = 0; i < x.length; i++) if (!Number.isFinite(x[i])) return NaN;
    if (sdR(x) === 0) return NaN;
    return dfaTerrier2026(x);
  }

  // Outcomes of estimate_all() shown by the page, for a trimmed run of M1
  function estimateAll(run, inputValues, s) {
    const d = run.events.duration;
    const meanD = meanR(d), sdD = sdR(d);
    const dv = estimateDivergence(run, s, 2);
    const nT = run.nTransient || 0;
    const kept = inputValues ? Array.prototype.slice.call(inputValues, nT, nT + run.nCyclesDone) : null;
    const nStr = Math.floor(d.length / 2);
    const strides = [];
    for (let j = 0; j < nStr; j++) strides.push(d[2 * j] + d[2 * j + 1]);
    return {
      meanDuration: meanD, sdDuration: sdD, cvDuration: sdD / meanD,
      lambdaS: dv.lambdaS, lambdaL: dv.lambdaL, lambdaSPerStep: dv.lambdaSPerStep,
      lambdaLPerStep: dv.lambdaLPerStep, nPairs: dv.nPairs,
      alphaInput: kept ? estimateDfa(kept) : NaN, alphaCommand: estimateDfa(run.events.u),
      alphaDuration: estimateDfa(d), alphaStrideDuration: nStr < 1 ? NaN : estimateDfa(strides),
      divergence: dv
    };
  }

  WB.est = {
    sumC: sumC, meanR: meanR, sdR: sdR, roundR: roundR, cumsumC: cumsumC, olsSlope: olsSlope,
    KdTree: KdTree, nearestNeighbors: nearestNeighbors, divergenceCore: divergenceCore,
    approxR: approxR, gridCount: gridCount, resampleGlobalByCycle: resampleGlobalByCycle,
    estimateDivergence: estimateDivergence, dfaTerrier2026: dfaTerrier2026,
    estimateDfa: estimateDfa, estimateAll: estimateAll
  };
})(typeof globalThis !== "undefined" ? globalThis : this);
