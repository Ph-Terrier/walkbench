/*
 * Title: Model M2 of walkbench in JavaScript: walker with arced feet and hip spring
 * Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
 * Produced by: written by the orchestrating model of the web-simulator session of SIM-ACI
 *   (Claude Opus 5.5), without a code-generation model
 * Date: 2026-11-19
 * Description: port of model_m2() of walkbench 0.6.2 (R/model_m2.R): extended simplest walker
 *   of Hobbelen and Wisse (2007), used by Bruijn et al. (2012), with the equations and the
 *   derived impact map of walkbench. Point hip mass, legs of length 1, arced feet of radius r_f
 *   centered on the leg at 1 - r_f from the hip, hip spring of stiffness k, slope gamma. The
 *   input is the height h of the floor at the next contact, relative to the stance foot, one
 *   value per step. Foot strike when (1 - r_f)(cos theta - cos(theta - phi)) = h, with the foot
 *   descending, the swing leg in front (theta < -0.05) and t > t_min = 0.25; fall when |theta|
 *   reaches 1.2 rad or when no foot strike occurs within 15 time units. Integration as in
 *   wb_m1.js (fourth-order Runge-Kutta, substeps of at most 1e-3 between the points of the
 *   sampling grid of step 0.03, bisection on the length of the last substep). The fixed point
 *   follows fixed_point() of model_m2() (relaxation, then a damped Newton iteration). The
 *   eigenvalues of 4 x 4 matrices come from the characteristic polynomial (Faddeev-LeVerrier)
 *   and the Durand-Kerner iteration. The printed impact map of walkbench (option
 *   impact = "printed") is not ported.
 * Dependencies: none
 * Input: none
 * Output: the object WB.m2 on the global object
 */
(function (root) {
  "use strict";
  const WB = (root.WB = root.WB || {});

  // Configuration of the injection experiments of the analysis plan (section 4.10)
  const DEFAULTS = Object.freeze({
    gamma: 0.004, k: 5, rF: 0.45, dt: 0.03, tMin: 0.25, tMax: 15, fallTheta: 1.2,
    thGuard: -0.05, hMax: 1e-3
  });

  // Fixed point of this configuration computed by walkbench 0.6.2 on 2026-11-19 (continuation
  // of the README of walkbench, tools/02_make_vectors_m2.R), used as the initial guess of the
  // search of the page; the default guess of model_m2() does not reach it
  const X_INIT_PLAN = [0.17184140584493454, -0.27465602777778253, 0.3436828116898686,
    -0.008800993038983606];

  const SAMPLE = 0, CONTACT = 1, FALL = 2;

  function rhs(y, p, out) {
    const th = y[0], thd = y[1], ph = y[2];
    const r = p.rF, gam = p.gamma;
    const dd = (1 - r) * (1 - r) + r * r + 2 * r * (1 - r) * Math.cos(th);
    const thdd = ((1 - r) * Math.sin(th - gam) + r * (1 - r) * Math.sin(th) * (thd * thd) - r * Math.sin(gam)) / dd;
    const psi = th - ph;
    const phdd = thdd + Math.sin(psi - gam) - thdd * (r * Math.cos(psi) + (1 - r) * Math.cos(ph)) +
      (1 - r) * (thd * thd) * Math.sin(ph) - p.k * ph;
    out[0] = thd;
    out[1] = thdd;
    out[2] = y[3];
    out[3] = phdd;
  }

  function makeRk4(p) {
    const k1 = new Float64Array(4), k2 = new Float64Array(4), k3 = new Float64Array(4);
    const k4 = new Float64Array(4), yt = new Float64Array(4);
    return function (y, h, out) {
      rhs(y, p, k1);
      for (let i = 0; i < 4; i++) yt[i] = y[i] + 0.5 * h * k1[i];
      rhs(yt, p, k2);
      for (let i = 0; i < 4; i++) yt[i] = y[i] + 0.5 * h * k2[i];
      rhs(yt, p, k3);
      for (let i = 0; i < 4; i++) yt[i] = y[i] + h * k3[i];
      rhs(yt, p, k4);
      for (let i = 0; i < 4; i++) out[i] = y[i] + (h / 6) * (k1[i] + 2 * k2[i] + 2 * k3[i] + k4[i]);
    };
  }

  // m2_impact(): impact at foot strike with exchange of the legs (derived map of walkbench)
  function impact(y, r) {
    const thm = y[0], thdm = y[1], phm = y[2];
    const thp = thm - phm;
    const dp = (1 - r) * (1 - r) + r * r + 2 * r * (1 - r) * Math.cos(thp);
    const lm = thdm * ((1 - r) * (1 - r) * Math.cos(phm) + r * (1 - r) * (Math.cos(thm - phm) + Math.cos(thm)) + r * r);
    const thdp = lm / dp;
    const phdp = thdp * (1 - (1 - r) * Math.cos(phm) - r * Math.cos(thm)) - r * (1 - Math.cos(thm)) * thdm;
    return [thp, thdp, -phm, phdp];
  }

  // Continuous leg coordinates (m2_leg_coords(), as to_continuous() of M1)
  function toContinuousInto(y, stance, out, o) {
    if (stance === 1) {
      out[o] = y[0]; out[o + 1] = y[1]; out[o + 2] = -y[2]; out[o + 3] = -y[3];
    } else {
      out[o] = y[0] - y[2]; out[o + 1] = y[1] - y[3]; out[o + 2] = y[2]; out[o + 3] = y[3];
    }
    return out;
  }

  // State variables of the model referred to the stance leg (m2_model_coords())
  function toModelCoordsInto(c, o, stance, out, q) {
    if (stance === 1) {
      out[q] = c[o]; out[q + 1] = c[o + 1]; out[q + 2] = -c[o + 2]; out[q + 3] = -c[o + 3];
    } else {
      out[q] = c[o] + c[o + 2]; out[q + 1] = c[o + 1] + c[o + 3]; out[q + 2] = c[o + 2]; out[q + 3] = c[o + 3];
    }
    return out;
  }

  function Engine(params) {
    const p = Object.assign({}, DEFAULTS, params || {});
    this.p = p;
    this.rk4 = makeRk4(p);
    this.y = new Float64Array(4);
    this.yPrev = new Float64Array(4);
    this.yTmp = new Float64Array(4);
    this.yStart = new Float64Array(4);
    this.lastT = NaN;
    this.lastEvent = null;
    this.fallReason = null;
  }

  Engine.prototype.start = function (y0, epsOf) {
    this.y.set(y0);
    this.yStart.set(y0);
    this.tLoc = 0;
    this.tGlob = 0;
    this.stance = 1;
    this.k = 1;
    this.epsOf = epsOf;
    this.eps = epsOf(1);
    this.fallReason = null;
    this.resetGrid();
  };

  Engine.prototype.resetGrid = function () {
    const dt = this.p.dt;
    this.gridFirst = Math.ceil((this.tGlob + this.tLoc - 1e-12) / dt) * dt - this.tGlob;
    this.gridJ = 0;
    while (this.gridFirst + this.gridJ * dt <= this.tLoc + 1e-12) this.gridJ++;
  };

  Engine.prototype.nextGridTime = function () {
    return this.gridFirst + this.gridJ * this.p.dt;
  };

  // foot height above the floor of the next contact: (1 - r)(cos theta - cos(theta - phi)) - h
  Engine.prototype.g = function (y) {
    return (1 - this.p.rF) * (Math.cos(y[0]) - Math.cos(y[0] - y[2])) - this.eps;
  };

  Engine.prototype.locateStrike = function (h) {
    let lo = 0, hi = h;
    for (let it = 0; it < 200; it++) {
      const mid = 0.5 * (lo + hi);
      if (mid <= lo || mid >= hi) break;
      this.rk4(this.yPrev, mid, this.yTmp);
      if (this.g(this.yTmp) <= 0) hi = mid; else lo = mid;
    }
    return hi;
  };

  Engine.prototype.advance = function () {
    const p = this.p;
    const tg = this.nextGridTime();
    if (tg > p.tMax) {
      this.fallReason = "no foot strike within " + p.tMax + " time units";
      return FALL;
    }
    const t0 = this.tLoc;
    const span = tg - t0;
    const n = Math.max(1, Math.ceil(span / p.hMax - 1e-9));
    const hs = span / n;
    const r = p.rF;
    for (let i = 0; i < n; i++) {
      const ta = t0 + i * hs;
      const tb = i === n - 1 ? tg : t0 + (i + 1) * hs;
      const h = tb - ta;
      this.yPrev.set(this.y);
      this.rk4(this.yPrev, h, this.y);
      const ga = this.g(this.yPrev), gb = this.g(this.y);
      if (ga > 0 && gb <= 0) {
        // downward crossing; walkbench keeps it when t > t_min, the foot descends and the swing
        // leg is in front
        const tau = this.locateStrike(h);
        const tRoot = ta + tau;
        if (tRoot > p.tMin) {
          this.rk4(this.yPrev, tau, this.yTmp);
          const y = this.yTmp;
          const rate = (1 - r) * (-Math.sin(y[0]) * y[1] + Math.sin(y[0] - y[2]) * (y[1] - y[3]));
          if (rate < 0 && y[0] < p.thGuard) return this.contact(tRoot, y);
        }
      }
      if (Math.abs(this.y[0]) >= p.fallTheta) {
        this.fallReason = "stance angle reached " + p.fallTheta + " rad in absolute value";
        return FALL;
      }
      this.tLoc = tb;
    }
    this.gridJ++;
    this.lastT = tg + this.tGlob;
    return SAMPLE;
  };

  Engine.prototype.contact = function (tRoot, yRoot) {
    const ev = {
      k: this.k, tStart: this.tGlob, tEnd: this.tGlob + tRoot, duration: tRoot, u: this.eps,
      leg: this.stance, post: Array.from(this.yStart), yPre: Array.from(yRoot)
    };
    const yNew = impact(yRoot, this.p.rF);
    this.y.set(yNew);
    this.yStart.set(yNew);
    this.tGlob = this.tGlob + tRoot;
    this.tLoc = 0;
    this.stance = 3 - this.stance;
    this.k += 1;
    this.eps = this.epsOf(this.k);
    this.resetGrid();
    this.lastEvent = ev;
    return CONTACT;
  };

  function integrateOneStep(params, y0, u, dtGrid) {
    const eng = new Engine(Object.assign({}, params, { dt: dtGrid }));
    eng.start(y0, function () { return u; });
    for (;;) {
      const r = eng.advance();
      if (r === CONTACT) return { fell: false, tEnd: eng.lastEvent.duration, yEnd: eng.lastEvent.yPre };
      if (r === FALL) return { fell: true, tEnd: NaN, yEnd: null };
    }
  }

  // cycle_map() of model_m2(): one step from the post-strike state x, then the impact
  function cycleMap(params, x, u) {
    const st = integrateOneStep(params, x, u, 1);
    if (st.fell) throw new Error("fall during cycleMap()");
    return { xNext: impact(st.yEnd, params.rF), duration: st.tEnd };
  }

  // --- Linear algebra for 4 x 4 matrices ------------------------------------------------------

  // Solution of a x = b by Gaussian elimination with partial pivoting; null when singular
  function solve(a, b) {
    const n = b.length;
    const m = a.map(function (row, i) { return row.concat([b[i]]); });
    for (let c = 0; c < n; c++) {
      let piv = c;
      for (let r = c + 1; r < n; r++) if (Math.abs(m[r][c]) > Math.abs(m[piv][c])) piv = r;
      if (m[piv][c] === 0) return null;
      const tmp = m[c]; m[c] = m[piv]; m[piv] = tmp;
      for (let r = c + 1; r < n; r++) {
        const f = m[r][c] / m[c][c];
        for (let j = c; j <= n; j++) m[r][j] -= f * m[c][j];
      }
    }
    const x = new Array(n).fill(0);
    for (let r = n - 1; r >= 0; r--) {
      let s = m[r][n];
      for (let j = r + 1; j < n; j++) s -= m[r][j] * x[j];
      x[r] = s / m[r][r];
    }
    return x;
  }

  // Eigenvalues of a real square matrix: characteristic polynomial by Faddeev-LeVerrier, roots
  // by Durand-Kerner; sorted by decreasing modulus, positive imaginary part first
  function eigen(a) {
    const n = a.length;
    const mul = function (x, y) {
      return x.map(function (row) {
        return y[0].map(function (_, j) {
          let s = 0;
          for (let k = 0; k < n; k++) s += row[k] * y[k][j];
          return s;
        });
      });
    };
    let m = a.map(function (row, i) { return row.map(function (_, j) { return i === j ? 1 : 0; }); });
    const c = [1];
    for (let k = 1; k <= n; k++) {
      const am = mul(a, m);
      let tr = 0;
      for (let i = 0; i < n; i++) tr += am[i][i];
      const ck = -tr / k;
      c.push(ck);
      m = am.map(function (row, i) { return row.map(function (v, j) { return i === j ? v + ck : v; }); });
    }
    // Durand-Kerner on the monic polynomial z^n + c1 z^(n-1) + ... + cn
    const re = [], im = [];
    for (let i = 0; i < n; i++) {
      // initial points (0.4 + 0.9i)^i
      let zr = 1, zi = 0;
      for (let t = 0; t < i; t++) {
        const nr = zr * 0.4 - zi * 0.9, ni = zr * 0.9 + zi * 0.4;
        zr = nr; zi = ni;
      }
      re.push(zr); im.push(zi);
    }
    for (let it = 0; it < 2000; it++) {
      let delta = 0;
      for (let i = 0; i < n; i++) {
        // p(z) by Horner
        let pr = 1, pi = 0;
        for (let k = 1; k <= n; k++) {
          const nr = pr * re[i] - pi * im[i] + c[k], ni = pr * im[i] + pi * re[i];
          pr = nr; pi = ni;
        }
        let qr = 1, qi = 0;
        for (let j = 0; j < n; j++) {
          if (j === i) continue;
          const dr = re[i] - re[j], di = im[i] - im[j];
          const nr = qr * dr - qi * di, ni = qr * di + qi * dr;
          qr = nr; qi = ni;
        }
        const den = qr * qr + qi * qi;
        const wr = (pr * qr + pi * qi) / den, wi = (pi * qr - pr * qi) / den;
        re[i] -= wr; im[i] -= wi;
        delta = Math.max(delta, Math.hypot(wr, wi));
      }
      if (delta < 1e-15) break;
    }
    const roots = re.map(function (r, i) {
      const v = Math.abs(im[i]) < 1e-13 * Math.max(1, Math.abs(r)) ? 0 : im[i];
      return { re: r, im: v, mod: Math.hypot(r, v) };
    });
    roots.sort(function (x, y) { return y.mod - x.mod || y.im - x.im; });
    return {
      re: roots.map(function (z) { return z.re; }), im: roots.map(function (z) { return z.im; }),
      mod: roots.map(function (z) { return z.mod; })
    };
  }

  function norm(v) {
    let s = 0;
    for (let i = 0; i < v.length; i++) s += v[i] * v[i];
    return Math.sqrt(s);
  }

  // Initial guess of model_m2(): expansion of Garcia et al. (1998) in the slope
  function garciaGuess(gamma) {
    const g3 = Math.pow(gamma, 1 / 3);
    const th0 = 0.970956 * g3 - 0.270837 * gamma;
    const thd0 = -1.045203 * 0.970956 * g3 + (1.045203 * 0.270837 + 1.062895) * gamma;
    return [th0, thd0, 2 * th0, thd0 * (1 - Math.cos(2 * th0))];
  }

  // fixed_point() of model_m2(): relaxation and damped Newton iteration
  function fixedPoint(params, xInit, relaxSteps) {
    const p = Object.assign({}, DEFAULTS, params || {});
    relaxSteps = relaxSteps === undefined ? 200 : relaxSteps;
    let x = xInit ? xInit.slice() : garciaGuess(p.gamma);
    const safe = function (z) {
      try { return cycleMap(p, z, 0).xNext; } catch (e) { return null; }
    };
    const diff = function (a, b) { return a.map(function (v, i) { return v - b[i]; }); };
    const newton = function (x0) {
      let xc = x0.slice();
      for (let i = 0; i < 60; i++) {
        const s0 = safe(xc);
        if (s0 === null) return null;
        const f0 = diff(s0, xc);
        if (norm(f0) < 1e-12) return xc;
        const jac = [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]];
        for (let j = 0; j < 4; j++) {
          const xp = xc.slice();
          xp[j] += 1e-7;
          const sp = safe(xp);
          if (sp === null) return null;
          for (let r = 0; r < 4; r++) jac[r][j] = (sp[r] - xp[r] - f0[r]) / 1e-7;
        }
        const step = solve(jac, f0);
        if (step === null) return null;
        let lambda = 1, xt;
        for (;;) {
          xt = xc.map(function (v, r) { return v - lambda * step[r]; });
          const st = safe(xt);
          if (st !== null && norm(diff(st, xt)) < norm(f0)) break;
          lambda /= 2;
          if (lambda < 1e-4) return null;
        }
        xc = xt;
      }
      const s0 = safe(xc);
      return s0 !== null && norm(diff(s0, xc)) < 1e-9 ? xc : null;
    };
    const relax = function (x0) {
      let xc = x0.slice();
      for (let i = 0; i < relaxSteps; i++) {
        const xn = safe(xc);
        if (xn === null) return null;
        if (norm(diff(xn, xc)) < 1e-9) return xn;
        xc = xn;
      }
      return xc;
    };
    const sGuess = safe(x);
    const close = sGuess !== null && norm(diff(sGuess, x)) < 1e-4;
    let xs = close ? newton(x) : null;
    if (xs === null) {
      const xr = relax(x);
      xs = xr === null ? null : newton(xr);
    }
    if (xs === null && !close) xs = newton(x);
    if (xs === null) throw new Error("fixed point of M2 not found");
    const cm = cycleMap(p, xs, 0);
    if (norm(diff(cm.xNext, xs)) > 1e-9) throw new Error("fixed point of M2 not found");
    const s = [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]];
    for (let j = 0; j < 4; j++) {
      const xp = xs.slice();
      xp[j] += 1e-7;
      const sp = cycleMap(p, xp, 0).xNext;
      for (let r = 0; r < 4; r++) s[r][j] = (sp[r] - cm.xNext[r]) / 1e-7;
    }
    return { xStar: xs, period: cm.duration, y0: xs.slice(), eig: eigen(s) };
  }

  // Continuation of the README of walkbench: from (r_f = 0, k = 0), r_f by steps of 0.05 up to
  // rF, then k by steps of 0.5 up to k
  function continuation(params) {
    const p = Object.assign({}, DEFAULTS, params || {});
    let x = fixedPoint(Object.assign({}, p, { rF: 0, k: 0 })).xStar;
    for (let i = 1; i * 0.05 <= p.rF + 1e-12; i++) {
      x = fixedPoint(Object.assign({}, p, { rF: Math.round(i * 5) / 100, k: 0 }), x).xStar;
    }
    for (let i = 1; i * 0.5 <= p.k + 1e-12; i++) {
      x = fixedPoint(Object.assign({}, p, { k: i * 0.5 }), x).xStar;
    }
    return fixedPoint(p, x);
  }

  // linearize() of walkbench: central differences of the cycle map, step h
  function linearize(params, h, fp) {
    const p = Object.assign({}, DEFAULTS, params || {});
    h = h || 1e-6;
    fp = fp || fixedPoint(p, X_INIT_PLAN);
    const xs = fp.xStar;
    const one = function (x, u) {
      const cm = cycleMap(p, x, u);
      return cm.xNext.concat([cm.duration]);
    };
    const cols = [];
    for (let j = 0; j < 4; j++) {
      const xp = xs.slice(), xm = xs.slice();
      xp[j] += h;
      xm[j] -= h;
      const a = one(xp, 0), b = one(xm, 0);
      cols.push(a.map(function (v, i) { return (v - b[i]) / (2 * h); }));
    }
    const up = one(xs, h), um = one(xs, -h);
    cols.push(up.map(function (v, i) { return (v - um[i]) / (2 * h); }));
    const jMap = [0, 1, 2, 3].map(function (r) { return [0, 1, 2, 3].map(function (c) { return cols[c][r]; }); });
    return {
      xStar: xs, period: fp.period, jMap: jMap, bVec: [0, 1, 2, 3].map(function (r) { return cols[4][r]; }),
      aVec: [0, 1, 2, 3].map(function (c) { return cols[c][4]; }), eSc: cols[4][4], eig: eigen(jMap)
    };
  }

  // transient_cycles(): smallest n with mu^(2n) <= tol, at least `minimum` cycles
  function transientCycles(mu, tol, minimum) {
    tol = tol || 0.005;
    minimum = minimum === undefined ? 20 : minimum;
    let n = mu === 0 ? 0 : Math.max(0, Math.ceil(Math.log(tol) / (2 * Math.log(mu))));
    while (n > 0 && Math.pow(mu, 2 * (n - 1)) <= tol) n--;
    while (Math.pow(mu, 2 * n) > tol) n++;
    return Math.max(minimum, n);
  }

  // A run of nCycles steps (simulate_run()), with the bounds of each completed step
  function simulateRun(params, input, nCycles, y0) {
    const p = Object.assign({}, DEFAULTS, params || {});
    const eng = new Engine(p);
    eng.start(y0, function (k) { return k <= input.length ? input[k - 1] : 0; });
    let cap = Math.ceil(nCycles * 2 / p.dt) + 64;
    let T = new Float64Array(cap), X = new Float64Array(cap * 4), C = new Int32Array(cap);
    let ns = 0;
    const ev = { tStart: [], tEnd: [], duration: [], u: [], leg: [], post: [] };
    const bS = [], bE = [];
    const tmp = new Float64Array(4);
    let fell = false, fallCycle = null;
    for (;;) {
      const r = eng.advance();
      if (r === SAMPLE) {
        if (ns === cap) {
          cap *= 2;
          const T2 = new Float64Array(cap), X2 = new Float64Array(cap * 4), C2 = new Int32Array(cap);
          T2.set(T); X2.set(X); C2.set(C);
          T = T2; X = X2; C = C2;
        }
        T[ns] = eng.lastT;
        toContinuousInto(eng.y, eng.stance, X, 4 * ns);
        C[ns] = eng.k;
        ns++;
      } else if (r === CONTACT) {
        const e = eng.lastEvent;
        ev.tStart.push(e.tStart); ev.tEnd.push(e.tEnd); ev.duration.push(e.duration);
        ev.u.push(e.u); ev.leg.push(e.leg); ev.post.push(e.post);
        bS.push(Array.from(toContinuousInto(e.post, e.leg, tmp, 0)));
        bE.push(Array.from(toContinuousInto(e.yPre, e.leg, tmp, 0)));
        if (eng.k > nCycles) break;
      } else {
        fell = true;
        fallCycle = eng.k;
        break;
      }
    }
    return {
      model: "M2", dt: p.dt, t: T.subarray(0, ns), X: X.subarray(0, 4 * ns), cycle: C.subarray(0, ns),
      nSamples: ns, events: ev, boundsStart: bS, boundsEnd: bE, fell: fell, fallCycle: fallCycle,
      fallReason: fell ? eng.fallReason : null, nCyclesDone: ev.duration.length, nTransient: 0
    };
  }

  // trim_run(), with the bounds of the kept steps
  function trimRun(run, nTransient) {
    if (run.fell) throw new Error("a run that fell is not trimmed");
    const t0 = run.events.tStart[nTransient];
    let first = 0;
    while (first < run.nSamples && run.cycle[first] <= nTransient) first++;
    const ns = run.nSamples - first;
    const T = new Float64Array(ns), C = new Int32Array(ns);
    for (let i = 0; i < ns; i++) {
      T[i] = run.t[first + i] - t0;
      C[i] = run.cycle[first + i] - nTransient;
    }
    const ev = {};
    for (const key of Object.keys(run.events)) ev[key] = run.events[key].slice(nTransient);
    ev.tStart = ev.tStart.map(function (v) { return v - t0; });
    ev.tEnd = ev.tEnd.map(function (v) { return v - t0; });
    return Object.assign({}, run, {
      t: T, X: run.X.slice(4 * first, 4 * run.nSamples), cycle: C, nSamples: ns, events: ev,
      boundsStart: run.boundsStart.slice(nTransient), boundsEnd: run.boundsEnd.slice(nTransient),
      nCyclesDone: run.nCyclesDone - nTransient, nTransient: nTransient
    });
  }

  WB.m2 = {
    DEFAULTS: DEFAULTS, X_INIT_PLAN: X_INIT_PLAN, SAMPLE: SAMPLE, CONTACT: CONTACT, FALL: FALL,
    rhs: rhs, impact: impact, toContinuousInto: toContinuousInto, toModelCoordsInto: toModelCoordsInto,
    Engine: Engine, cycleMap: cycleMap, solve: solve, eigen: eigen, garciaGuess: garciaGuess,
    fixedPoint: fixedPoint, continuation: continuation, linearize: linearize,
    transientCycles: transientCycles, simulateRun: simulateRun, trimRun: trimRun,
    // estimation settings of settings_m2() (analysis plan, table 5.1)
    settings: function () {
      return {
        coordinates: "model", modelCoords: toModelCoordsInto,
        resample: { method: "global_by_cycle", nPer: 100, per: "stride" }, windowUnit: "stride",
        theiler: 0.5, horizon: 10, windowS: [0, 0.5], windowL: [4, 10], slope: "lsq",
        smoothing: "none", pairs: "shrinking"
      };
    },
    cyclesPerStride: 2
  };
})(typeof globalThis !== "undefined" ? globalThis : this);
