/*
 * Title: Validation of the JavaScript port of M1 against the test vectors of walkbench
 * Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
 * Produced by: written by the orchestrating model of the web-simulator session of SIM-ACI
 *   (Claude Opus 5.5), without a code-generation model
 * Date: 2026-11-19
 * Description: compares the JavaScript port (js/wb_m1.js, js/wb_estimators.js, js/wb_input.js)
 *   with the values that walkbench computed (tests/vectors/m1_vectors.json, written by
 *   tools/01_make_vectors_m1.R). The tolerances are those approved by the author on 2026-11-19
 *   (two levels): fixed point and period 1e-9; moduli of the eigenvalues 1e-4; run with a fixed
 *   input, durations and states 1e-7, same falls; computations on the same values as R 1e-9;
 *   end to end (simulation and estimation in JavaScript) lambda_S and lambda_L 1e-4, alpha
 *   1e-6. Every difference is an absolute difference.
 * Dependencies: Deno 1.46 or later
 * Input: tests/vectors/m1_vectors.json
 * Output: validation/results_m1.json and validation/results_m1.md; exit code 1 when a test
 *   fails
 * Usage, from the folder web/: deno run --allow-read --allow-write tests/run_tests_m1.js
 */
import "../js/wb_estimators.js";
import "../js/wb_input.js";
import "../js/wb_m1.js";

const WB = globalThis.WB;
const here = new URL(".", import.meta.url);
const vec = JSON.parse(await Deno.readTextFile(new URL("vectors/m1_vectors.json", here)));

const TOL = {
  fixedPoint: 1e-9,
  eigMod: 1e-4,
  run: 1e-7,
  sameInput: 1e-9,
  endToEndLambda: 1e-4,
  endToEndAlpha: 1e-6
};

const results = [];
function record(id, label, diff, tol, extra) {
  const pass = tol === null ? null : Number.isFinite(diff) && diff <= tol;
  results.push(Object.assign({ id: id, label: label, max_abs_diff: diff, tolerance: tol, pass: pass }, extra || {}));
}
function maxAbsDiff(a, b) {
  if (a.length !== b.length) return Infinity;
  let m = 0;
  for (let i = 0; i < a.length; i++) {
    const x = a[i], y = b[i];
    const fx = x !== null && Number.isFinite(x), fy = y !== null && Number.isFinite(y);
    if (fx !== fy) return Infinity;
    if (fx) m = Math.max(m, Math.abs(x - y));
  }
  return m;
}
function nanEq(x, y) {
  const fx = x !== null && Number.isFinite(x), fy = y !== null && Number.isFinite(y);
  if (!fx && !fy) return 0;
  if (fx !== fy) return Infinity;
  return Math.abs(x - y);
}
function timed(f) {
  const t0 = performance.now();
  const v = f();
  return { value: v, ms: performance.now() - t0 };
}

const params = { gamma: vec.fixed_point.params.gamma, dt: vec.fixed_point.params.dt, tMin: vec.fixed_point.params.t_min };

// 1. Fixed point and period, against walkbench at tight lsodar tolerances (decision of the author
// of 2026-11-19, option 1: tolerance 1e-9 against the frozen functions run at rtol 1e-13);
// against walkbench at its default tolerances for information
const tref = vec.tight_reference;
const fp = timed(() => WB.m1.fixedPoint(params));
record("T1", `fixed point (theta, theta_dot) and period, against walkbench at rtol ${tref.rtol}`, Math.max(
  maxAbsDiff(fp.value.xStar, tref.x_star), Math.abs(fp.value.period - tref.period)),
  TOL.fixedPoint, { ms: fp.ms });
record("T1b", "fixed point and period, against walkbench at its default tolerances (information)", Math.max(
  maxAbsDiff(fp.value.xStar, vec.fixed_point.x_star), Math.abs(fp.value.period - vec.fixed_point.period)), null);

// 2. Linearization: moduli of the eigenvalues; J, b, a, e and the static gain for information
const lin = timed(() => WB.m1.linearize(params, vec.linearization.h));
const L = lin.value;
record("T2", "eigenvalue moduli of the linearized step map (linearize())",
  maxAbsDiff(L.eig.mod, vec.linearization.eig.mod), TOL.eigMod, { ms: lin.ms });
record("T2b", "eigenvalue moduli, against the forward differences of fixed_point() (information)",
  maxAbsDiff(fp.value.eig.mod, vec.fixed_point.eig.mod), null);
const jRow = [L.jMap[0][0], L.jMap[0][1], L.jMap[1][0], L.jMap[1][1]];
record("T2c", "J, b, a, e and static gain of linearize() (information)", Math.max(
  maxAbsDiff(jRow, vec.linearization.j_map_by_row), maxAbsDiff(L.bVec, vec.linearization.b_vec),
  maxAbsDiff(L.aVec, vec.linearization.a_vec), Math.abs(L.eSc - vec.linearization.e_sc),
  Math.abs(L.staticGain - vec.linearization.static_gain)), null);

// 3. ARFIMA inputs from the normal draws of R
let dArf = 0;
for (const c of vec.arfima) {
  const x = WB.input.inputArfimaFromZ(Float64Array.from(c.z), c.d, c.sd, c.n_transient);
  dArf = Math.max(dArf, maxAbsDiff(Array.from(x), c.values));
}
record("T3", "ARFIMA inputs from the normal draws of R (5 series, d from -0.30 to 0.45)", dArf, TOL.sameInput);

// 4. Fixed series with circular start
const fsx = WB.input.inputFixedSeries(Float64Array.from(vec.fixed_series.window), vec.fixed_series.n_transient, vec.fixed_series.sd);
record("T4", "fixed series: rescaling and circular start", maxAbsDiff(Array.from(fsx), vec.fixed_series.values), TOL.sameInput);

// 5. Main run with the input of R
const mr = vec.main_run;
const runT = timed(() => WB.m1.simulateRun(params, mr.input, mr.n_cycles, mr.y0));
const run = runT.value;
const ev = run.events, rev = mr.events;
const legsEqual = ev.leg.length === rev.leg.length && ev.leg.every((v, i) => v === rev.leg[i]);
// Durations and cumulative start and end times, against the whole run computed by walkbench at
// tight lsodar tolerances (decision of the author of 2026-11-19); the first run of this script
// compared them with walkbench at its default tolerances and failed at 2.1e-7 on the cumulative
// times (validation/validation_m1.md, section 5); that comparison stays for information
const trun = tref.main_run;
record("T5", `run of 620 steps: step durations, against walkbench at rtol ${tref.rtol}`,
  maxAbsDiff(ev.duration, trun.duration), TOL.run, { ms: runT.ms });
record("T5f", `run of 620 steps: start and end times of the steps, cumulated over the run, against walkbench at rtol ${tref.rtol}`,
  Math.max(maxAbsDiff(ev.tStart, trun.t_start), maxAbsDiff(ev.tEnd, trun.t_end)), TOL.run);
record("T5g", "run of 620 steps: durations and cumulated times, against walkbench at its default tolerances (information)",
  Math.max(maxAbsDiff(ev.duration, rev.duration), maxAbsDiff(ev.tStart, rev.t_start), maxAbsDiff(ev.tEnd, rev.t_end)), null);
record("T5b", "run of 620 steps: post-strike states (theta, theta_dot)", Math.max(
  maxAbsDiff(ev.thetaPost, rev.theta_post), maxAbsDiff(ev.thetaDotPost, rev.theta_dot_post)), TOL.run);
const sameCounts = run.fell === mr.fell && run.nCyclesDone === mr.n_cycles_done && run.nSamples === mr.samples_n && legsEqual;
record("T5c", "run of 620 steps: no fall, same number of steps, of samples and same legs", sameCounts ? 0 : Infinity, 0,
  { detail: `steps ${run.nCyclesDone} / ${mr.n_cycles_done}, samples ${run.nSamples} / ${mr.samples_n}` });
// samples of the cycles of the subset, in order
const sub = mr.samples_subset;
const want = new Set(sub.cycles);
const js = { t: [], thG: [], thGd: [], phG: [], phGd: [] };
for (let i = 0; i < run.nSamples; i++) {
  if (!want.has(run.cycle[i])) continue;
  js.t.push(run.t[i]);
  js.thG.push(run.X[4 * i]); js.thGd.push(run.X[4 * i + 1]);
  js.phG.push(run.X[4 * i + 2]); js.phGd.push(run.X[4 * i + 3]);
}
record("T5d", "run of 620 steps: sample times of steps 1, 2, 310, 311, 619, 620", maxAbsDiff(js.t, sub.t), TOL.sameInput);
// sampled states against the tight reference (decision of the author of 2026-11-19, taken for
// M2 and applied to both models); against walkbench at its default tolerances for information
const tsub = trun.samples_subset;
record("T5e", `run of 620 steps: sampled states of the same steps, against walkbench at rtol ${tref.rtol}`, Math.max(
  maxAbsDiff(js.t, tsub.t), maxAbsDiff(js.thG, tsub.thG), maxAbsDiff(js.thGd, tsub.thGd), maxAbsDiff(js.phG, tsub.phG),
  maxAbsDiff(js.phGd, tsub.phGd)), TOL.run);
record("T5h", "run of 620 steps: sampled states of the same steps, against walkbench at its default tolerances (information)", Math.max(
  maxAbsDiff(js.thG, sub.thG), maxAbsDiff(js.thGd, sub.thGd), maxAbsDiff(js.phG, sub.phG),
  maxAbsDiff(js.phGd, sub.phGd)), null);

// 6. Run that falls
const fr = vec.fall_run;
const frun = WB.m1.simulateRun(params, fr.input, fr.n_cycles, vec.fixed_point.y0);
record("T6", `run that falls (seed ${fr.seed}, sd ${fr.sd}): same step of the fall`,
  frun.fell === fr.fell && frun.fallCycle === fr.fall_cycle && frun.nCyclesDone === fr.n_cycles_done ? 0 : Infinity, 0,
  { detail: `fall at step ${frun.fallCycle} / ${fr.fall_cycle} (${frun.fallReason})` });
record("T6b", "run that falls: durations of the steps before the fall", maxAbsDiff(frun.events.duration, fr.durations), TOL.run);

// 7 and 8. Estimators on the samples of R (short run)
const sr = vec.short_run;
const shortRun = {
  X: Float64Array.from(sr.samples_by_row), nSamples: sr.n_samples, dt: sr.dt,
  events: { duration: sr.durations }
};
function checkDivergence(id, label, settings, ref, withNeighbors) {
  const t = timed(() => WB.est.estimateDivergence(shortRun, settings, 2));
  const d = t.value;
  const curve = Math.max(maxAbsDiff(Array.from(d.mld), ref.mld), maxAbsDiff(Array.from(d.mldSmooth), ref.mld_smooth),
    maxAbsDiff(Array.from(d.slopes), ref.slopes));
  const lam = Math.max(nanEq(d.lambdaS, ref.lambda_s), nanEq(d.lambdaL, ref.lambda_l),
    nanEq(d.lambdaSPerStep, ref.lambda_s_per_step), nanEq(d.lambdaLPerStep, ref.lambda_l_per_step));
  record(id, label + ": divergence curve, smoothed curve and local slopes", curve, TOL.sameInput, { ms: t.ms });
  record(id + "b", label + ": lambda_S and lambda_L (per time unit and per step)", lam, TOL.sameInput,
    { detail: `lambda_S ${d.lambdaS}, lambda_L ${d.lambdaL}` });
  let mism = 0;
  if (withNeighbors) {
    for (let i = 0; i < ref.neighbors.length; i++) if (d.neighbors[i] + 1 !== ref.neighbors[i]) mism++;
  }
  const same = d.nPairs === ref.n_pairs && d.nHorizon === ref.n_horizon && d.theiler === ref.theiler && mism === 0;
  record(id + "c", label + ": same neighbors, number of pairs, horizon and Theiler window", same ? 0 : Infinity, 0,
    { detail: `neighbors differing ${mism}; pairs ${d.nPairs} / ${ref.n_pairs}; horizon ${d.nHorizon} / ${ref.n_horizon}; Theiler ${d.theiler} / ${ref.theiler}` });
}
const sPrim = WB.m1.settings();
checkDivergence("T7", "short run, settings of the plan", sPrim, sr.divergence_primary, true);
const sAlt = Object.assign(WB.m1.settings(), {
  windowS: sr.settings_alternative.window_s, windowL: sr.settings_alternative.window_l,
  horizon: sr.settings_alternative.horizon
});
checkDivergence("T8", "short run, windows 0-0.2 and 4-6 steps", sAlt, sr.divergence_alternative, false);

// 9. DFA on the values of R
let dDfa = 0;
for (const c of vec.dfa) dDfa = Math.max(dDfa, nanEq(WB.est.estimateDfa(c.x), c.alpha));
record("T9", "DFA alpha on the values of R (5 series, one too short)", dDfa, TOL.sameInput);

// 10. End to end: the JavaScript run of test 5, trimmed, with the JavaScript estimators
const tr = WB.m1.trimRun(run, mr.n_transient);
const estT = timed(() => WB.est.estimateAll(tr, mr.input, WB.m1.settings()));
const o = estT.value, ro = mr.outcomes;
record("T10", "end to end: lambda_S and lambda_L (per time unit and per step)", Math.max(
  nanEq(o.lambdaS, ro.lambda_s), nanEq(o.lambdaL, ro.lambda_l), nanEq(o.lambdaSPerStep, ro.lambda_s_per_step),
  nanEq(o.lambdaLPerStep, ro.lambda_l_per_step)), TOL.endToEndLambda, { ms: estT.ms,
  detail: `JavaScript lambda_S ${o.lambdaS}, lambda_L ${o.lambdaL}; walkbench ${ro.lambda_s}, ${ro.lambda_l}` });
record("T10b", "end to end: alpha of the input, of the command, of the step and stride durations", Math.max(
  nanEq(o.alphaInput, ro.alpha_input), nanEq(o.alphaCommand, ro.alpha_command), nanEq(o.alphaDuration, ro.alpha_duration),
  nanEq(o.alphaStrideDuration, ro.alpha_stride_duration)), TOL.endToEndAlpha);
record("T10c", "end to end: mean, standard deviation and coefficient of variation of the step durations", Math.max(
  nanEq(o.meanDuration, ro.mean_duration), nanEq(o.sdDuration, ro.sd_duration), nanEq(o.cvDuration, ro.cv_duration)), TOL.run);
record("T10d", "end to end: number of complete pairs (information)", Math.abs(o.nPairs - ro.n_pairs), null,
  { detail: `${o.nPairs} / ${ro.n_pairs}` });

// 11. Short run end to end: durations of the JavaScript run with the input of R
const srun = WB.m1.simulateRun(params, sr.input, sr.n_cycles, vec.fixed_point.y0);
const strim = WB.m1.trimRun(srun, sr.n_transient);
record("T11", "short run: durations of the kept steps", maxAbsDiff(strim.events.duration, sr.durations), TOL.run,
  { detail: `samples ${strim.nSamples} / ${sr.n_samples}` });

// 12. Integration error of walkbench at its default tolerances (information)
record("T12", `walkbench at its default tolerances against walkbench at rtol ${tref.rtol}: period (information)`,
  Math.abs(vec.fixed_point.period - tref.period), null);
record("T12b", `walkbench at its default tolerances against walkbench at rtol ${tref.rtol}: end time of the run (information)`,
  Math.abs(rev.t_end[rev.t_end.length - 1] - trun.t_end[trun.t_end.length - 1]), null);

// 13. Generator of the page against the generator of R (runif, rnorm after set.seed)
let dU = 0, dZ = 0;
for (const c of vec.rng) {
  const g1 = new WB.input.RRng(c.seed);
  dU = Math.max(dU, maxAbsDiff(c.runif.map(() => g1.uniform()), c.runif));
  const g2 = new WB.input.RRng(c.seed);
  dZ = Math.max(dZ, maxAbsDiff(c.rnorm.map(() => g2.normal()), c.rnorm));
}
record("T13", "generator of the page: uniform draws of runif() after set.seed() (5 seeds)", dU, TOL.sameInput);
record("T13b", "generator of the page: normal draws of rnorm() after set.seed() (5 seeds, 1000 draws each)", dZ, TOL.sameInput);
let dSeed = 0;
for (const c of vec.arfima) {
  const x = WB.input.inputArfima(c.n, c.d, c.sd, new WB.input.RRng(c.seed), c.n_transient);
  dSeed = Math.max(dSeed, maxAbsDiff(Array.from(x), c.values));
}
record("T13c", "input_arfima() from the seed alone, generator of the page (5 series)", dSeed, TOL.sameInput);

// --- Report ------------------------------------------------------------------------------------
const nFail = results.filter((r) => r.pass === false).length;
const out = {
  produced_by: "tests/run_tests_m1.js, written by the orchestrating model of the web-simulator session of SIM-ACI (Claude Opus 5.5)",
  date: new Date().toISOString(),
  runtime: `Deno ${Deno.version.deno}, V8 ${Deno.version.v8}`,
  vectors: { walkbench_version: vec.meta.walkbench_version, git_commit: vec.meta.git_commit, date: vec.meta.date,
    r_version: vec.meta.r_version },
  tolerances: TOL, n_tests: results.length, n_failed: nFail, results: results
};
const fmt = (x) => (x === null ? "" : !Number.isFinite(x) ? String(x) : x === 0 ? "0" : x.toExponential(2));
let md = "| Test | Check | Largest difference | Tolerance | Result |\n|---|---|---|---|---|\n";
const cell = (s) => String(s).replace(/\|/g, "\\|");
for (const r of results) {
  md += `| ${r.id} | ${cell(r.label + (r.detail ? " (" + r.detail + ")" : ""))} | ${fmt(r.max_abs_diff)} | ${r.tolerance === null ? "information" : fmt(r.tolerance)} | ${r.pass === null ? "" : r.pass ? "pass" : "FAIL"} |\n`;
}
const valDir = new URL("../validation/", here);
await Deno.mkdir(valDir, { recursive: true });
await Deno.writeTextFile(new URL("results_m1.json", valDir), JSON.stringify(out, null, 2));
await Deno.writeTextFile(new URL("results_m1.md", valDir),
  `<!-- Written by tests/run_tests_m1.js (${out.date}); ${out.runtime}; vectors of walkbench ${vec.meta.walkbench_version}, commit ${vec.meta.git_commit} -->\n\n` + md);
console.log(md);
console.log(`${results.length} checks, ${nFail} failed; JavaScript run ${runT.ms.toFixed(0)} ms, estimators ${estT.ms.toFixed(0)} ms`);
if (nFail > 0) Deno.exit(1);
