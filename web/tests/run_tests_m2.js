/*
 * Title: Validation of the JavaScript port of M2 against the test vectors of walkbench
 * Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
 * Produced by: written by the orchestrating model of the web-simulator session of SIM-ACI
 *   (Claude Opus 5.5), without a code-generation model
 * Date: 2026-11-19
 * Description: compares the JavaScript port of M2 (js/wb_m2.js, js/wb_estimators.js,
 *   js/wb_input.js) with the values that walkbench computed (tests/vectors/m2_vectors.json,
 *   written by tools/02_make_vectors_m2.R), in the configuration of section 4.10 of the
 *   analysis plan. The tolerances repeat those that the author approved for M1 on 2026-11-19:
 *   fixed point and period 1e-9 and durations and cumulative times 1e-7, against walkbench run
 *   at tight lsodar tolerances; moduli of the eigenvalues 1e-4; post-strike states 1e-7 and
 *   same falls, against walkbench at its default tolerances; computations on the same values as
 *   R 1e-9; end to end, lambda_S and lambda_L 1e-4 and alpha 1e-6. Absolute differences. After
 *   the first run, the author chose (2026-11-19) the tight reference for the sampled states and
 *   the number of grid points of walkbench for the end-to-end exponents.
 *   Change of 2026-11-20 by the same model, for walkbench 0.7.0: the page counts its grid points
 *   with grid_count(), as walkbench does; T10 uses the grid of the page, and T10e checks that
 *   the two counts are equal (the vectors record the count of walkbench as n_grid).
 * Dependencies: Deno 1.46 or later
 * Input: tests/vectors/m2_vectors.json
 * Output: validation/results_m2.json and validation/results_m2.md; exit code 1 when a test
 *   fails
 * Usage, from the folder web/: deno run --allow-read=. --allow-write=./validation tests/run_tests_m2.js
 */
import "../js/wb_estimators.js";
import "../js/wb_input.js";
import "../js/wb_m2.js";

const WB = globalThis.WB;
const M2 = WB.m2;
const here = new URL(".", import.meta.url);
const vec = JSON.parse(await Deno.readTextFile(new URL("vectors/m2_vectors.json", here)));

const TOL = { fixedPoint: 1e-9, eigMod: 1e-4, run: 1e-7, sameInput: 1e-9, endToEndLambda: 1e-4, endToEndAlpha: 1e-6 };

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
function chunk4(flat) {
  const out = [];
  for (let i = 0; i < flat.length; i += 4) out.push(flat.slice(i, i + 4));
  return out;
}

const pv = vec.fixed_point.params;
const params = { gamma: pv.gamma, k: pv.k, rF: pv.r_f, dt: pv.dt, tMin: pv.t_min };
const tref = vec.tight_reference;

// 1. Fixed point: from the fixed point of walkbench as initial guess, and by the continuation
const fp = timed(() => M2.fixedPoint(params, M2.X_INIT_PLAN));
const fpErr = (f, xs, per) => Math.max(maxAbsDiff(f.xStar, xs), Math.abs(f.period - per));
record("T1", `fixed point (4 state variables) and period, against walkbench at rtol ${tref.rtol}`,
  fpErr(fp.value, tref.x_star, tref.period), TOL.fixedPoint, { ms: fp.ms });
record("T1b", "fixed point and period, against walkbench at its default tolerances (information)",
  fpErr(fp.value, vec.fixed_point.x_star, vec.fixed_point.period), null);
const cont = timed(() => M2.continuation(params));
record("T1c", `fixed point reached by the continuation of the README of walkbench, against walkbench at rtol ${tref.rtol}`,
  fpErr(cont.value, tref.x_star, tref.period), TOL.fixedPoint, { ms: cont.ms });
const sc = vec.start_config;
const fp0 = M2.fixedPoint({ gamma: sc.params.gamma, k: 0, rF: 0, dt: sc.params.dt, tMin: sc.params.t_min });
record("T1d", "fixed point of the starting configuration (r_f = 0, k = 0) from the default guess, against walkbench at its default tolerances (information)",
  fpErr(fp0, sc.x_star, sc.period), null);

// 2. Linearization and transient
const lin = timed(() => M2.linearize(params, vec.linearization.h, fp.value));
record("T2", "eigenvalue moduli of the linearized step map (linearize())", maxAbsDiff(lin.value.eig.mod, vec.linearization.eig.mod),
  TOL.eigMod, { ms: lin.ms });
record("T2b", "eigenvalue moduli, against the forward differences of fixed_point() (information)",
  maxAbsDiff(fp.value.eig.mod, vec.fixed_point.eig.mod), null);
const nTr = M2.transientCycles(Math.max.apply(null, fp.value.eig.mod));
record("T2c", "transient of the analysis plan (smallest n with mu^(2n) <= 0.005)", nTr === vec.fixed_point.transient_cycles ? 0 : Infinity, 0,
  { detail: `${nTr} / ${vec.fixed_point.transient_cycles} steps` });

// 5. Main run with the input of R
const mr = vec.main_run, rev = mr.events;
const runT = timed(() => M2.simulateRun(params, mr.input, mr.n_cycles, mr.y0));
const run = runT.value, ev = run.events;
const trun = tref.main_run;
record("T5", `run of ${mr.n_cycles} steps: step durations, against walkbench at rtol ${tref.rtol}`, maxAbsDiff(ev.duration, trun.duration), TOL.run, { ms: runT.ms });
record("T5f", `run of ${mr.n_cycles} steps: start and end times of the steps, cumulated over the run, against walkbench at rtol ${tref.rtol}`,
  Math.max(maxAbsDiff(ev.tStart, trun.t_start), maxAbsDiff(ev.tEnd, trun.t_end)), TOL.run);
record("T5g", `run of ${mr.n_cycles} steps: durations and cumulated times, against walkbench at its default tolerances (information)`,
  Math.max(maxAbsDiff(ev.duration, rev.duration), maxAbsDiff(ev.tStart, rev.t_start), maxAbsDiff(ev.tEnd, rev.t_end)), null);
const postJs = ev.post;
const postR = rev.theta_post.map((_, i) => [rev.theta_post[i], rev.theta_dot_post[i], rev.phi_post[i], rev.phi_dot_post[i]]);
record("T5b", `run of ${mr.n_cycles} steps: post-strike states (4 state variables)`, maxAbsDiff(postJs.flat(), postR.flat()), TOL.run);
const legsEqual = ev.leg.length === rev.leg.length && ev.leg.every((v, i) => v === rev.leg[i]);
const same = run.fell === mr.fell && run.nCyclesDone === mr.n_cycles_done && run.nSamples === mr.samples_n && legsEqual;
record("T5c", `run of ${mr.n_cycles} steps: no fall, same number of steps, of samples and same legs`, same ? 0 : Infinity, 0,
  { detail: `steps ${run.nCyclesDone} / ${mr.n_cycles_done}, samples ${run.nSamples} / ${mr.samples_n}` });
const sub = mr.samples_subset, want = new Set(sub.cycles);
const js = { t: [], thG: [], thGd: [], phG: [], phGd: [] };
for (let i = 0; i < run.nSamples; i++) {
  if (!want.has(run.cycle[i])) continue;
  js.t.push(run.t[i]);
  js.thG.push(run.X[4 * i]); js.thGd.push(run.X[4 * i + 1]); js.phG.push(run.X[4 * i + 2]); js.phGd.push(run.X[4 * i + 3]);
}
record("T5d", "run: sample times of steps 1, 2, 317, 318 and of the last two steps", maxAbsDiff(js.t, sub.t), TOL.sameInput);
// sampled states against the tight reference (decision of the author of 2026-11-19); the first
// run of this script compared them with walkbench at its default tolerances and failed at 8.2e-7
// (validation/validation_m2.md, section 5); that comparison stays for information
const tsub = trun.samples_subset;
record("T5e", `run: sampled states of the same steps, against walkbench at rtol ${tref.rtol}`, Math.max(
  maxAbsDiff(js.t, tsub.t), maxAbsDiff(js.thG, tsub.thG), maxAbsDiff(js.thGd, tsub.thGd), maxAbsDiff(js.phG, tsub.phG),
  maxAbsDiff(js.phGd, tsub.phGd)), TOL.run);
record("T5h", "run: sampled states of the same steps, against walkbench at its default tolerances (information)", Math.max(
  maxAbsDiff(js.thG, sub.thG), maxAbsDiff(js.thGd, sub.thGd), maxAbsDiff(js.phG, sub.phG), maxAbsDiff(js.phGd, sub.phGd)), null);

// 6. Run that falls
const fr = vec.fall_run;
const frun = M2.simulateRun(params, fr.input, fr.n_cycles, vec.fixed_point.x_star);
record("T6", `run that falls (seed ${fr.seed}, d ${fr.d}, sd ${fr.sd}): same step of the fall`,
  frun.fell === fr.fell && frun.fallCycle === fr.fall_cycle && frun.nCyclesDone === fr.n_cycles_done ? 0 : Infinity, 0,
  { detail: `fall at step ${frun.fallCycle} / ${fr.fall_cycle} (${frun.fallReason})` });
record("T6b", "run that falls: durations of the steps before the fall", maxAbsDiff(frun.events.duration, fr.durations), TOL.run);

// 7 and 8. Estimators on the samples and bounds of R (short run)
const sr = vec.short_run;
const shortRun = {
  t: Float64Array.from(sr.samples_t), X: Float64Array.from(sr.samples_by_row), cycle: Int32Array.from(sr.samples_cycle),
  nSamples: sr.n_samples, dt: sr.dt,
  events: { tStart: sr.events.t_start, tEnd: sr.events.t_end, duration: sr.events.duration, leg: sr.events.leg },
  boundsStart: chunk4(sr.bounds_start_by_row), boundsEnd: chunk4(sr.bounds_end_by_row)
};
function checkDivergence(id, label, settings, ref, withNeighbors) {
  const t = timed(() => WB.est.estimateDivergence(shortRun, settings, 2));
  const d = t.value;
  if (withNeighbors) {
    record(id + "r", label + ": resampled state variables of the model (global grid, interpolation inside each step)",
      d.resampled.n === sr.resampled.n ? maxAbsDiff(Array.from(d.resampled.X), sr.resampled.X_by_row) : Infinity, TOL.sameInput,
      { detail: `${d.resampled.n} / ${sr.resampled.n} rows` });
  }
  record(id, label + ": divergence curve", maxAbsDiff(Array.from(d.mld), ref.mld), TOL.sameInput, { ms: t.ms });
  record(id + "b", label + ": lambda_S and lambda_L (per stride and per step)", Math.max(nanEq(d.lambdaS, ref.lambda_s),
    nanEq(d.lambdaL, ref.lambda_l), nanEq(d.lambdaSPerStep, ref.lambda_s_per_step), nanEq(d.lambdaLPerStep, ref.lambda_l_per_step)),
    TOL.sameInput, { detail: `lambda_S ${d.lambdaS}, lambda_L ${d.lambdaL}` });
  let mism = 0;
  if (withNeighbors) for (let i = 0; i < ref.neighbors.length; i++) if (d.neighbors[i] + 1 !== ref.neighbors[i]) mism++;
  const ok = d.nPairs === ref.n_pairs && d.nHorizon === ref.n_horizon && d.theiler === ref.theiler && mism === 0;
  record(id + "c", label + ": same neighbors, number of pairs, horizon and Theiler window", ok ? 0 : Infinity, 0,
    { detail: `neighbors differing ${mism}; pairs ${d.nPairs} / ${ref.n_pairs}; horizon ${d.nHorizon} / ${ref.n_horizon}; Theiler ${d.theiler} / ${ref.theiler}` });
}
checkDivergence("T7", "short run, settings of the plan", M2.settings(), sr.divergence_primary, true);
const sAlt = Object.assign(M2.settings(), { windowS: sr.settings_alternative.window_s, windowL: sr.settings_alternative.window_l });
checkDivergence("T8", "short run, windows 0-1 and 3-8 strides", sAlt, sr.divergence_alternative, false);

// 10. End to end: the JavaScript run of test 5, trimmed, with the JavaScript estimators.
// From walkbench 0.7.0, the number of points of the resampling grid comes from the number of
// steps (grid_count()), and the page follows it. With walkbench 0.6.2 it came from the times,
// and the tests of 2026-11-19 used the number of grid points of walkbench (rows T10e and T10f of
// that version; validation/validation_m2.md, section 6).
const tr = M2.trimRun(run, mr.n_transient);
const estT = timed(() => WB.est.estimateAll(tr, mr.input, M2.settings()));
const o = estT.value, ro = mr.outcomes;
const nGridJs = WB.est.gridCount(tr.events.duration.length, M2.settings().resample.nPer, 2);
record("T10e", "end to end: number of points of the resampling grid, page and grid_count() of walkbench", Math.abs(nGridJs - mr.n_grid), 0,
  { detail: `JavaScript ${nGridJs}; walkbench ${mr.n_grid}` });
record("T10", "end to end: lambda_S and lambda_L (per stride and per step)", Math.max(
  nanEq(o.lambdaS, ro.lambda_s), nanEq(o.lambdaL, ro.lambda_l), nanEq(o.lambdaSPerStep, ro.lambda_s_per_step),
  nanEq(o.lambdaLPerStep, ro.lambda_l_per_step)), TOL.endToEndLambda,
  { ms: estT.ms, detail: `JavaScript lambda_S ${o.lambdaS}, lambda_L ${o.lambdaL}; walkbench ${ro.lambda_s}, ${ro.lambda_l}` });
record("T10b", "end to end: alpha of the input, of the command, of the step and stride durations", Math.max(
  nanEq(o.alphaInput, ro.alpha_input), nanEq(o.alphaCommand, ro.alpha_command), nanEq(o.alphaDuration, ro.alpha_duration),
  nanEq(o.alphaStrideDuration, ro.alpha_stride_duration)), TOL.endToEndAlpha);
record("T10c", "end to end: mean, standard deviation and coefficient of variation of the step durations", Math.max(
  nanEq(o.meanDuration, ro.mean_duration), nanEq(o.sdDuration, ro.sd_duration), nanEq(o.cvDuration, ro.cv_duration)), TOL.run);
record("T10d", "end to end: number of complete pairs (information)", Math.abs(o.nPairs - ro.n_pairs), null, { detail: `${o.nPairs} / ${ro.n_pairs}` });

// 11. Short run: durations of the JavaScript run with the input of R
const srun = M2.simulateRun(params, sr.input, sr.n_cycles, vec.fixed_point.x_star);
const strim = M2.trimRun(srun, sr.n_transient);
record("T11", "short run: durations of the kept steps", maxAbsDiff(strim.events.duration, sr.events.duration), TOL.run,
  { detail: `samples ${strim.nSamples} / ${sr.n_samples}` });

// 12. Integration error of walkbench at its default tolerances (information)
record("T12", `walkbench at its default tolerances against walkbench at rtol ${tref.rtol}: period (information)`,
  Math.abs(vec.fixed_point.period - tref.period), null);
record("T12b", `walkbench at its default tolerances against walkbench at rtol ${tref.rtol}: end time of the run (information)`,
  Math.abs(rev.t_end[rev.t_end.length - 1] - trun.t_end[trun.t_end.length - 1]), null);

// --- Report ------------------------------------------------------------------------------------
const nFail = results.filter((r) => r.pass === false).length;
const out = {
  produced_by: "tests/run_tests_m2.js, written by the orchestrating model of the web-simulator session of SIM-ACI (Claude Opus 5.5)",
  date: new Date().toISOString(), runtime: `Deno ${Deno.version.deno}, V8 ${Deno.version.v8}`,
  vectors: { walkbench_version: vec.meta.walkbench_version, git_commit: vec.meta.git_commit, date: vec.meta.date, r_version: vec.meta.r_version },
  tolerances: TOL, n_tests: results.length, n_failed: nFail, results: results
};
const fmt = (x) => (x === null ? "" : !Number.isFinite(x) ? String(x) : x === 0 ? "0" : x.toExponential(2));
const cell = (s) => String(s).replace(/\|/g, "\\|");
let md = "| Test | Check | Largest difference | Tolerance | Result |\n|---|---|---|---|---|\n";
for (const r of results) {
  md += `| ${r.id} | ${cell(r.label + (r.detail ? " (" + r.detail + ")" : ""))} | ${fmt(r.max_abs_diff)} | ${r.tolerance === null ? "information" : fmt(r.tolerance)} | ${r.pass === null ? "" : r.pass ? "pass" : "FAIL"} |\n`;
}
const valDir = new URL("../validation/", here);
await Deno.mkdir(valDir, { recursive: true });
await Deno.writeTextFile(new URL("results_m2.json", valDir), JSON.stringify(out, null, 2));
await Deno.writeTextFile(new URL("results_m2.md", valDir),
  `<!-- Written by tests/run_tests_m2.js (${out.date}); ${out.runtime}; vectors of walkbench ${vec.meta.walkbench_version}, commit ${vec.meta.git_commit} -->\n\n` + md);
console.log(md);
console.log(`${results.length} checks, ${nFail} failed; JavaScript run ${runT.ms.toFixed(0)} ms, estimators ${estT.ms.toFixed(0)} ms, continuation ${cont.ms.toFixed(0)} ms`);
if (nFail > 0) Deno.exit(1);
