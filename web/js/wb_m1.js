/*
 * Title: Model M1 of walkbench in JavaScript: dynamics, foot strikes, fixed point, runs
 * Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
 * Produced by: written by the orchestrating model of the web-simulator session of SIM-ACI
 *   (Claude Opus 5.5), without a code-generation model
 * Date: 2026-11-19
 * Description: port of model_m1() of walkbench 0.6.2 (R/model_m1.R and the pinned walker file
 *   inst/pinned/simaci/feasibility/scripts/00_functions_walker.R), injection point
 *   foot_strike_timing. Equations of the simplest walker (Garcia et al. 1998); foot strike when
 *   phi - 2 theta = epsilon, upward crossing after t_min; heel-strike map of the literal Eq. 3 of
 *   Su and Dingwell (2007); fall when |theta| reaches 1.2 rad or when no foot strike occurs
 *   within 15 time units. The state is integrated by a fixed-step fourth-order Runge-Kutta
 *   scheme (substeps of at most 1e-3) between the points of the global sampling grid (step
 *   0.03), and the foot strike is located by bisection on the length of the last substep.
 *   walkbench integrates with lsodar (deSolve, rtol 1e-10, atol 1e-12); tests/run_tests_m1.js
 *   measures the agreement. The same engine drives the animation and the reference runs.
 * Dependencies: none (plain JavaScript; classic script in the page, module import in Deno)
 * Input: none
 * Output: the object WB.m1 on the global object
 */
(function (root) {
  "use strict";
  const WB = (root.WB = root.WB || {});

  // Parameters of model_m1() and of the pinned integrate_step()
  const DEFAULTS = Object.freeze({
    gamma: 0.009, dt: 0.03, tMin: 1.0, tMax: 15, fallTheta: 1.2, hMax: 1e-3
  });

  const SAMPLE = 0, CONTACT = 1, FALL = 2;

  // Equations of motion (walker_rhs() of the pinned walker file)
  function rhs(y, gamma, out) {
    const s = Math.sin(y[0] - gamma), c = Math.cos(y[0] - gamma), sp = Math.sin(y[2]);
    out[0] = y[1];
    out[1] = s;
    out[2] = y[3];
    out[3] = s + y[1] * y[1] * sp - c * sp;
  }

  // One classic Runge-Kutta step of length h from y into out (out must differ from y)
  function makeRk4(gamma) {
    const k1 = new Float64Array(4), k2 = new Float64Array(4), k3 = new Float64Array(4);
    const k4 = new Float64Array(4), yt = new Float64Array(4);
    return function (y, h, out) {
      rhs(y, gamma, k1);
      for (let i = 0; i < 4; i++) yt[i] = y[i] + 0.5 * h * k1[i];
      rhs(yt, gamma, k2);
      for (let i = 0; i < 4; i++) yt[i] = y[i] + 0.5 * h * k2[i];
      rhs(yt, gamma, k3);
      for (let i = 0; i < 4; i++) yt[i] = y[i] + h * k3[i];
      rhs(yt, gamma, k4);
      for (let i = 0; i < 4; i++) out[i] = y[i] + (h / 6) * (k1[i] + 2 * k2[i] + 2 * k3[i] + k4[i]);
    };
  }

  // Heel-strike maps of heelstrike_map(): "timing" (literal Eq. 3) and "full" (used with
  // eps = 0 by the frozen step_map() of the fixed-point search)
  function heelstrikeTiming(y) {
    const c = Math.cos(2 * y[0]);
    return [-y[0], y[1] * c, -2 * y[0], y[1] * c * (1 - c)];
  }
  function heelstrikeFull(y, eps) {
    const cp = Math.cos(y[2]);
    return [y[0] - y[2] + eps / 2, y[1] * cp, -y[2], y[1] * cp * (1 - cp)];
  }

  // Post-contact state of the reduced state (theta, theta_dot)
  function postContactState(x) {
    return [x[0], x[1], 2 * x[0], x[1] * (1 - Math.cos(2 * x[0]))];
  }

  // Continuous leg coordinates (thG, thGd, phG, phGd) of to_continuous(), written into out at
  // position o
  function toContinuousInto(y, stance, out, o) {
    if (stance === 1) {
      out[o] = y[0]; out[o + 1] = y[1]; out[o + 2] = -y[2]; out[o + 3] = -y[3];
    } else {
      out[o] = y[0] - y[2]; out[o + 1] = y[1] - y[3]; out[o + 2] = y[2]; out[o + 3] = y[3];
    }
    return out;
  }

  // Resumable integration engine: each call of advance() integrates to the next point of the
  // global sampling grid, or stops earlier at a foot strike or a fall
  function Engine(params) {
    const p = Object.assign({}, DEFAULTS, params || {});
    this.p = p;
    this.rk4 = makeRk4(p.gamma);
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

  // Grid of integrate_step(): first + j dt, local times strictly after t_loc + 1e-12
  Engine.prototype.resetGrid = function () {
    const dt = this.p.dt;
    this.gridFirst = Math.ceil((this.tGlob + this.tLoc - 1e-12) / dt) * dt - this.tGlob;
    this.gridJ = 0;
    while (this.gridFirst + this.gridJ * dt <= this.tLoc + 1e-12) this.gridJ++;
  };

  // Local time of the next grid point
  Engine.prototype.nextGridTime = function () {
    return this.gridFirst + this.gridJ * this.p.dt;
  };

  // Bisection on the length tau of the last substep: g(rk4(yPrev, tau)) changes sign in (0, h]
  Engine.prototype.locateStrike = function (h) {
    let lo = 0, hi = h;
    for (let it = 0; it < 200; it++) {
      const mid = 0.5 * (lo + hi);
      if (mid <= lo || mid >= hi) break;
      this.rk4(this.yPrev, mid, this.yTmp);
      if (this.yTmp[2] - 2 * this.yTmp[0] - this.eps >= 0) hi = mid; else lo = mid;
    }
    return hi;
  };

  Engine.prototype.advance = function () {
    const p = this.p;
    const tg = this.nextGridTime();
    if (tg > p.tMax) {
      // the pinned integrate_step() samples up to t_max only: no foot strike before it
      this.fallReason = "no foot strike within " + p.tMax + " time units";
      return FALL;
    }
    const t0 = this.tLoc;
    const span = tg - t0;
    const n = Math.max(1, Math.ceil(span / p.hMax - 1e-9));
    const hs = span / n;
    for (let i = 0; i < n; i++) {
      const ta = t0 + i * hs;
      const tb = i === n - 1 ? tg : t0 + (i + 1) * hs;
      const h = tb - ta;
      this.yPrev.set(this.y);
      this.rk4(this.yPrev, h, this.y);
      const ga = this.yPrev[2] - 2 * this.yPrev[0] - this.eps;
      const gb = this.y[2] - 2 * this.y[0] - this.eps;
      if (ga < 0 && gb >= 0) {
        // upward crossing of the foot-strike condition; a root before t_min is the step onset
        const tau = this.locateStrike(h);
        const tRoot = ta + tau;
        if (tRoot > p.tMin) {
          this.rk4(this.yPrev, tau, this.yTmp);
          if (this.yTmp[3] - 2 * this.yTmp[1] > 0) return this.contact(tRoot, this.yTmp);
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
      leg: this.stance, thetaPost: this.yStart[0], thetaDotPost: this.yStart[1],
      yPre: Array.from(yRoot)
    };
    const yNew = heelstrikeTiming(yRoot);
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

  // One step from the post-contact state y0 with input eps (sampling grid dtGrid)
  function integrateOneStep(params, y0, eps, dtGrid) {
    const eng = new Engine(Object.assign({}, params, { dt: dtGrid }));
    eng.start(y0, function () { return eps; });
    for (;;) {
      const r = eng.advance();
      if (r === CONTACT) return { fell: false, tEnd: eng.lastEvent.duration, yEnd: eng.lastEvent.yPre };
      if (r === FALL) return { fell: true, tEnd: NaN, yEnd: null };
    }
  }

  // Frozen step_map(): unperturbed step, then the "full" map with eps = 0
  function stepMap(params, x) {
    const st = integrateOneStep(params, postContactState(x), 0, 1);
    if (st.fell) return null;
    const yNew = heelstrikeFull(st.yEnd, 0);
    return { xNext: [yNew[0], yNew[1]], period: st.tEnd };
  }

  // cycle_map() of model_m1(), injection point foot_strike_timing
  function cycleMap(params, x, u) {
    const st = integrateOneStep(params, postContactState(x), u, 1);
    if (st.fell) throw new Error("fall during cycleMap()");
    const yNew = heelstrikeTiming(st.yEnd);
    return { xNext: [yNew[0], yNew[1]], duration: st.tEnd };
  }

  function solve2(a, b) {
    const det = a[0][0] * a[1][1] - a[0][1] * a[1][0];
    return [(b[0] * a[1][1] - a[0][1] * b[1]) / det, (a[0][0] * b[1] - b[0] * a[1][0]) / det];
  }

  // Eigenvalues of a 2 x 2 matrix, sorted by decreasing modulus, positive imaginary part first
  function eigen2(m) {
    const tr = m[0][0] + m[1][1], det = m[0][0] * m[1][1] - m[0][1] * m[1][0];
    const disc = tr * tr / 4 - det;
    if (disc >= 0) {
      const r1 = tr / 2 + Math.sqrt(disc), r2 = tr / 2 - Math.sqrt(disc);
      const v = Math.abs(r1) >= Math.abs(r2) ? [r1, r2] : [r2, r1];
      return { re: v, im: [0, 0], mod: [Math.abs(v[0]), Math.abs(v[1])] };
    }
    const re = tr / 2, im = Math.sqrt(-disc), mod = Math.hypot(re, im);
    return { re: [re, re], im: [im, -im], mod: [mod, mod] };
  }

  // Frozen find_fixed_point(): Newton iterations with a forward-difference Jacobian
  function fixedPoint(params) {
    const p = Object.assign({}, DEFAULTS, params || {});
    let x = [0.2, -0.2];
    const h = 1e-7;
    for (let it = 0; it < 50; it++) {
      const s0 = stepMap(p, x);
      if (s0 === null) throw new Error("step map failed during the Newton iteration");
      const f0 = [s0.xNext[0] - x[0], s0.xNext[1] - x[1]];
      if (Math.hypot(f0[0], f0[1]) < 1e-12) break;
      const jac = [[0, 0], [0, 0]];
      for (let j = 0; j < 2; j++) {
        const xp = x.slice();
        xp[j] += h;
        const sj = stepMap(p, xp);
        jac[0][j] = (sj.xNext[0] - xp[0] - f0[0]) / h;
        jac[1][j] = (sj.xNext[1] - xp[1] - f0[1]) / h;
      }
      const dx = solve2(jac, f0);
      x = [x[0] - dx[0], x[1] - dx[1]];
    }
    const sm = stepMap(p, x);
    const s = [[0, 0], [0, 0]];
    for (let j = 0; j < 2; j++) {
      const xp = x.slice();
      xp[j] += h;
      const sj = stepMap(p, xp);
      s[0][j] = (sj.xNext[0] - sm.xNext[0]) / h;
      s[1][j] = (sj.xNext[1] - sm.xNext[1]) / h;
    }
    return { xStar: x, period: sm.period, y0: postContactState(x), eig: eigen2(s) };
  }

  // linearize() of walkbench for M1: central differences of the cycle map, step h
  function linearize(params, h) {
    const p = Object.assign({}, DEFAULTS, params || {});
    h = h || 1e-6;
    const fp = fixedPoint(p);
    const xs = fp.xStar;
    const one = function (x, u) {
      const cm = cycleMap(p, x, u);
      return [cm.xNext[0], cm.xNext[1], cm.duration];
    };
    const col = [];
    for (let j = 0; j < 2; j++) {
      const xp = xs.slice(), xm = xs.slice();
      xp[j] += h;
      xm[j] -= h;
      const a = one(xp, 0), b = one(xm, 0);
      col.push([(a[0] - b[0]) / (2 * h), (a[1] - b[1]) / (2 * h), (a[2] - b[2]) / (2 * h)]);
    }
    const up = one(xs, h), um = one(xs, -h);
    col.push([(up[0] - um[0]) / (2 * h), (up[1] - um[1]) / (2 * h), (up[2] - um[2]) / (2 * h)]);
    const jMap = [[col[0][0], col[1][0]], [col[0][1], col[1][1]]];
    const bVec = [col[2][0], col[2][1]];
    const aVec = [col[0][2], col[1][2]];
    const eSc = col[2][2];
    const ij = [[1 - jMap[0][0], -jMap[0][1]], [-jMap[1][0], 1 - jMap[1][1]]];
    const sol = solve2(ij, bVec);
    return {
      xStar: xs, period: fp.period, jMap: jMap, bVec: bVec, aVec: aVec, eSc: eSc,
      staticGain: eSc + aVec[0] * sol[0] + aVec[1] * sol[1], eig: eigen2(jMap)
    };
  }

  // A run of nCycles steps from y0 with the input values input[0], input[1], ...
  // (simulate_run() of walkbench). Samples on the global grid in continuous coordinates.
  function simulateRun(params, input, nCycles, y0) {
    const p = Object.assign({}, DEFAULTS, params || {});
    const eng = new Engine(p);
    eng.start(y0, function (k) { return k <= input.length ? input[k - 1] : 0; });
    let cap = Math.ceil(nCycles * 4.5 / p.dt) + 64;
    let T = new Float64Array(cap), X = new Float64Array(cap * 4), C = new Int32Array(cap);
    let ns = 0;
    const ev = { tStart: [], tEnd: [], duration: [], u: [], leg: [], thetaPost: [], thetaDotPost: [] };
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
        ev.u.push(e.u); ev.leg.push(e.leg); ev.thetaPost.push(e.thetaPost);
        ev.thetaDotPost.push(e.thetaDotPost);
        if (eng.k > nCycles) break;
      } else {
        fell = true;
        fallCycle = eng.k;
        break;
      }
    }
    return {
      model: "M1", dt: p.dt, t: T.subarray(0, ns), X: X.subarray(0, 4 * ns),
      cycle: C.subarray(0, ns), nSamples: ns, events: ev, fell: fell, fallCycle: fallCycle,
      fallReason: fell ? eng.fallReason : null, nCyclesDone: ev.duration.length, nTransient: 0
    };
  }

  // trim_run(): removes the transient cycles, renumbers the kept cycles and rebases the times
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
    const X = run.X.slice(4 * first, 4 * run.nSamples);
    const ev = {};
    for (const key of Object.keys(run.events)) ev[key] = run.events[key].slice(nTransient);
    ev.tStart = ev.tStart.map(function (v) { return v - t0; });
    ev.tEnd = ev.tEnd.map(function (v) { return v - t0; });
    return Object.assign({}, run, {
      t: T, X: X, cycle: C, nSamples: ns, events: ev,
      nCyclesDone: run.nCyclesDone - nTransient, nTransient: nTransient
    });
  }

  WB.m1 = {
    DEFAULTS: DEFAULTS, SAMPLE: SAMPLE, CONTACT: CONTACT, FALL: FALL,
    rhs: rhs, makeRk4: makeRk4, heelstrikeTiming: heelstrikeTiming, heelstrikeFull: heelstrikeFull,
    postContactState: postContactState, toContinuousInto: toContinuousInto, Engine: Engine,
    stepMap: stepMap, cycleMap: cycleMap, fixedPoint: fixedPoint, linearize: linearize,
    eigen2: eigen2, simulateRun: simulateRun, trimRun: trimRun,
    // estimation settings of settings_m1() (analysis plan, table 5.1)
    settings: function () {
      return {
        windowUnit: "step", theiler: 2, horizon: 4, windowS: [0, 0.1], windowL: [3, 4],
        slope: "mean_local", smoothing: "hamming7", pairs: "shrinking"
      };
    },
    cyclesPerStride: 2
  };
})(typeof globalThis !== "undefined" ? globalThis : this);
