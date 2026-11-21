/*
 * Title: Inputs of the perturbation port in JavaScript: ARFIMA series, fixed series, generator
 * Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
 * Produced by: written by the orchestrating model of the web-simulator session of SIM-ACI
 *   (Claude Opus 5.5), without a code-generation model
 * Date: 2026-11-19
 * Description: inputs of walkbench 0.6.2 (R/perturbation_port.R and the frozen arfima_series()
 *   of the pinned walker file):
 *   - exact Gaussian ARFIMA(0,d,0) series: autocorrelation of Hosking (1981) and the
 *     Durbin-Levinson recursion, which gives the same series as the Cholesky factor of
 *     walkbench for the same normal draws;
 *   - input_arfima(): rescaling of the whole series, or of its part after the transient;
 *   - input_fixed_series(): centering, rescaling and circular start of a fixed series;
 *   - the default generator of R: Mersenne-Twister seeded as by set.seed(), uniform draws as
 *     by runif(), normal draws by inversion as by rnorm() (algorithm AS 241 of Wichura, as in
 *     qnorm() of R). A seed of the page therefore gives the series of input_arfima(seed = )
 *     of walkbench. Written by the model after the algorithms of the C sources of R
 *     (src/main/RNG.c, src/nmath/snorm.c, src/nmath/qnorm.c), which were not opened in this
 *     session, and checked against draws of R by tests/run_tests_m1.js (check T13). Change of
 *     2026-11-19 (same session), after the decision of the
 *     author on the export of the page: this generator replaces the xoshiro128** generator of
 *     the first prototype;
 *   - reading of a series from a text file chosen by the visitor (the file stays in the
 *     browser). Change of 2026-11-21 by the same model, after the decision of the author
 *     relayed by the coordination session of SIM-ACI (Claude Fable 5.2): the former reading
 *     kept the last number of each line and turned "0,123" into 123; the new reading accepts
 *     one number per line with a decimal point and reports every other line (readSeriesText).
 *     Later that day, after a second decision of the author: seriesProblem() tells a series
 *     with too few values from a series whose values are all equal.
 * Dependencies: wb_estimators.js (WB.est.meanR, WB.est.sdR), loaded before this file
 * Input: none
 * Output: the object WB.input on the global object
 */
(function (root) {
  "use strict";
  const WB = (root.WB = root.WB || {});

  // --- Default generator of R ----------------------------------------------------------------

  // set.seed(seed): initial scrambling by the congruential generator 69069 s + 1, then the 625
  // words of the state (mti and the 624 words of Mersenne-Twister); mti is set to 624
  function RRng(seed) {
    let s = seed >>> 0;
    for (let j = 0; j < 50; j++) s = (Math.imul(69069, s) + 1) >>> 0;
    this.d = new Uint32Array(625);
    for (let j = 0; j < 625; j++) {
      s = (Math.imul(69069, s) + 1) >>> 0;
      this.d[j] = s;
    }
    this.d[0] = 624;
  }

  // MT_genrand() of R: d[0] holds mti and d[1 + i] the word mt[i]
  RRng.prototype.genrand = function () {
    const d = this.d, N = 624, M = 397, UPPER = 0x80000000, LOWER = 0x7fffffff, A = 0x9908b0df;
    let mti = d[0];
    if (mti >= N) {
      let kk, y;
      for (kk = 0; kk < N - M; kk++) {
        y = (d[kk + 1] & UPPER) | (d[kk + 2] & LOWER);
        d[kk + 1] = d[kk + M + 1] ^ (y >>> 1) ^ (y & 1 ? A : 0);
      }
      for (; kk < N - 1; kk++) {
        y = (d[kk + 1] & UPPER) | (d[kk + 2] & LOWER);
        d[kk + 1] = d[kk + (M - N) + 1] ^ (y >>> 1) ^ (y & 1 ? A : 0);
      }
      y = (d[N] & UPPER) | (d[1] & LOWER);
      d[N] = d[M] ^ (y >>> 1) ^ (y & 1 ? A : 0);
      mti = 0;
    }
    let y = d[mti + 1];
    mti++;
    y ^= y >>> 11;
    y ^= (y << 7) & 0x9d2c5680;
    y ^= (y << 15) & 0xefc60000;
    y ^= y >>> 18;
    d[0] = mti;
    return (y >>> 0) * 2.3283064365386963e-10;
  };

  // unif_rand() of R: draw in [0, 1) moved inside (0, 1) by fixup()
  RRng.prototype.uniform = function () {
    const x = this.genrand(), i2_32m1 = 2.328306437080797e-10;
    if (x <= 0) return 0.5 * i2_32m1;
    if (1 - x <= 0) return 1 - 0.5 * i2_32m1;
    return x;
  };

  // norm_rand() of R with normal.kind "Inversion"
  RRng.prototype.normal = function () {
    const BIG = 134217728;
    let u = this.uniform();
    u = Math.floor(BIG * u) + this.uniform();
    return qnorm(u / BIG);
  };

  // qnorm(p) of R for 0 < p < 1, lower tail (algorithm AS 241, Wichura 1988)
  function qnorm(p) {
    const q = p - 0.5;
    let r, val;
    if (Math.abs(q) <= 0.425) {
      r = 0.180625 - q * q;
      return q * (((((((r * 2509.0809287301226727 +
        33430.575583588128105) * r + 67265.770927008700853) * r +
        45921.953931549871457) * r + 13731.693765509461125) * r +
        1971.5909503065514427) * r + 133.14166789178437745) * r +
        3.387132872796366608) /
        (((((((r * 5226.495278852545925 +
          28729.085735721942674) * r + 39307.89580009271061) * r +
          21213.794301586595867) * r + 5394.1960214247511077) * r +
          687.1870074920579083) * r + 42.313330701600911252) * r + 1.0);
    }
    r = Math.sqrt(-Math.log(q > 0 ? 1 - p : p));
    if (r <= 5) {
      r += -1.6;
      val = (((((((r * 7.7454501427834140764e-4 +
        0.0227238449892691845833) * r + 0.24178072517745061177) *
        r + 1.27045825245236838258) * r +
        3.64784832476320460504) * r + 5.7694972214606914055) *
        r + 4.6303378461565452959) * r +
        1.42343711074968357734) /
        (((((((r *
          1.05075007164441684324e-9 + 5.475938084995344946e-4) *
          r + 0.0151986665636164571966) * r +
          0.14810397642748007459) * r + 0.68976733498510000455) *
          r + 1.6763848301838038494) * r +
          2.05319162663775882187) * r + 1.0);
    } else {
      r += -5;
      val = (((((((r * 2.01033439929228813265e-7 +
        2.71155556874348757815e-5) * r +
        0.0012426609473880784386) * r + 0.026532189526576123093) *
        r + 0.29656057182850489123) * r +
        1.7848265399172913358) * r + 5.4637849111641143699) *
        r + 6.6579046435011037772) /
        (((((((r *
          2.04426310338993978564e-15 + 1.4215117583164458887e-7) *
          r + 1.8463183175100546818e-5) * r +
          7.868691311456132591e-4) * r + 0.0148753612908506148525) *
          r + 0.13692988092273580531) * r +
          0.59983220655588793769) * r + 1.0);
    }
    return q < 0 ? -val : val;
  }

  // --- ARFIMA(0,d,0) ---------------------------------------------------------------------------

  // Autocorrelation of fractionally integrated noise: rho(k) = rho(k-1) (k - 1 + d) / (k - d)
  function fiAutocorrelation(n, d) {
    const rho = new Float64Array(n);
    rho[0] = 1;
    for (let k = 1; k < n; k++) rho[k] = rho[k - 1] * (k - 1 + d) / (k - d);
    return rho;
  }

  // Series with the autocorrelation of ARFIMA(0,d,0) from the normal draws z, by the
  // Durbin-Levinson recursion; for d = 0 the draws are returned, as in arfima_series()
  function arfimaFromZ(z, d) {
    const n = z.length;
    const x = new Float64Array(n);
    if (d === 0) {
      x.set(z);
      return x;
    }
    const rho = fiAutocorrelation(n, d);
    let a = new Float64Array(n), ap = new Float64Array(n);
    let v = 1;
    x[0] = z[0];
    for (let k = 1; k < n; k++) {
      let num = rho[k];
      for (let j = 1; j < k; j++) num -= ap[j] * rho[k - j];
      const akk = num / v;
      a[k] = akk;
      for (let j = 1; j < k; j++) a[j] = ap[j] - akk * ap[k - j];
      v *= 1 - akk * akk;
      let m = 0;
      for (let j = 1; j <= k; j++) m += a[j] * x[k - j];
      x[k] = m + Math.sqrt(v) * z[k];
      const tmp = ap;
      ap = a;
      a = tmp;
    }
    return x;
  }

  // (x - mean(x)) / sd(x) * sdTarget
  function standardize(x, sdTarget) {
    const m = WB.est.meanR(x), s = WB.est.sdR(x);
    const out = new Float64Array(x.length);
    for (let i = 0; i < x.length; i++) out[i] = (x[i] - m) / s * sdTarget;
    return out;
  }

  // input_arfima() from given normal draws: whole series rescaled (nTransient = 0), or rescaled
  // so that the values after the transient have the mean 0 and the standard deviation sd
  function inputArfimaFromZ(z, d, sd, nTransient) {
    const x = arfimaFromZ(z, d);
    if (!nTransient) return standardize(x, sd);
    const x1 = standardize(x, 1);
    const kept = x1.subarray(nTransient);
    const m = WB.est.meanR(kept), s = WB.est.sdR(kept);
    const out = new Float64Array(x1.length);
    for (let i = 0; i < x1.length; i++) out[i] = (x1[i] - m) / s * sd;
    return out;
  }

  function normals(rng, n) {
    const z = new Float64Array(n);
    for (let i = 0; i < n; i++) z[i] = rng.normal();
    return z;
  }

  function inputArfima(n, d, sd, rng, nTransient) {
    return inputArfimaFromZ(normals(rng, n), d, sd, nTransient || 0);
  }

  // input_fixed_series(): centered window rescaled to sd, preceded by its last nTransient values
  function inputFixedSeries(values, nTransient, sd) {
    const n = values.length;
    if (n < 3) throw new Error("values must be at least 3 finite numbers");
    if (nTransient < 0 || nTransient > n) throw new Error("nTransient must lie in 0 to length(values)");
    const z = standardize(values, sd);
    const out = new Float64Array(n + nTransient);
    out.set(z.subarray(n - nTransient), 0);
    out.set(z, nTransient);
    return out;
  }

  // Numbers of a text file, rule of 2026-11-21: one number per line, written with a decimal
  // point (optional sign and exponent); the text from # to the end of a line is a comment; blank
  // lines and a leading byte-order mark are skipped; the line ends are LF, CRLF or CR. Any other
  // line is reported, and the page then refuses the file. The accepted files are a subset of the
  // files that scan(file, comment.char = "#") of R reads, and both read the same numbers in the
  // same order. kind "comma" marks a number written with a decimal comma.
  const NUMBER_RE = /^[+-]?(\d+\.?\d*|\.\d+)([eE][+-]?\d+)?$/;
  const COMMA_RE = /^[+-]?\d*,\d+([eE][+-]?\d+)?$/;
  function readSeriesText(text) {
    const values = [], bad = [];
    const lines = String(text).replace(/^﻿/, "").split(/\r\n|\r|\n/);
    for (let i = 0; i < lines.length; i++) {
      const hash = lines[i].indexOf("#");
      const s = (hash >= 0 ? lines[i].slice(0, hash) : lines[i]).replace(/^[ \t]+|[ \t]+$/g, "");
      if (!s) continue;
      const v = NUMBER_RE.test(s) ? Number(s) : NaN;
      if (Number.isFinite(v)) values.push(v);
      else bad.push({ line: i + 1, text: s, kind: COMMA_RE.test(s) ? "comma" : "format" });
    }
    return { values: values, bad: bad };
  }
  // Problem of a series read from a file, checked in this order: "too_few" below nMin values,
  // "constant" when all values are equal (standard deviation 0, as sd() of R gives), null
  // otherwise
  function seriesProblem(values, nMin) {
    if (values.length < nMin) return "too_few";
    if (!(WB.est.sdR(values) > 0)) return "constant";
    return null;
  }
  // The numbers of a file that follows the rule; an error names the first line that does not
  function parseSeriesText(text) {
    const r = readSeriesText(text);
    if (r.bad.length) throw new Error("line " + r.bad[0].line + " does not hold one number: " + r.bad[0].text);
    return r.values;
  }

  WB.input = {
    RRng: RRng, qnorm: qnorm, fiAutocorrelation: fiAutocorrelation, arfimaFromZ: arfimaFromZ,
    standardize: standardize, inputArfimaFromZ: inputArfimaFromZ, normals: normals,
    inputArfima: inputArfima, inputFixedSeries: inputFixedSeries, readSeriesText: readSeriesText,
    parseSeriesText: parseSeriesText, seriesProblem: seriesProblem
  };
})(typeof globalThis !== "undefined" ? globalThis : this);
