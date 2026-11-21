/*
 * Title: Outcomes of four reference runs of the page, for the check of the export of R code
 * Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
 * Produced by: written by the orchestrating model of the web-simulator session of SIM-ACI
 *   (Claude Opus 5.5), on 2026-11-20, without a code-generation model
 * Date: 2026-11-20
 * Description: runs the four reference runs of the check with the files of the page, through
 *   the same calls as referenceRun() of js/wb_app.js (fixed point, transient, input, run,
 *   trim, estimate_all with the windows of the plan). Two runs use ARFIMA inputs drawn from a
 *   seed (M1: seed 7, d = 0.3; M2: seed 1, d = 0); two use the integer series of
 *   serie_lcg.txt, rebuilt here by the recurrence of check_exports.R. On 2026-11-20 the page
 *   gave the same values in the browser pane of the Claude desktop app.
 * Dependencies: Deno 1.46 or later; js/wb_estimators.js, js/wb_input.js, js/wb_m1.js,
 *   js/wb_m2.js
 * Input: none
 * Output: page_values.json (this folder)
 * Usage, from the folder web/:
 *   deno run --allow-read=. --allow-write=./validation validation/export_check/page_values.js
 */
import "../../js/wb_estimators.js";
import "../../js/wb_input.js";
import "../../js/wb_m1.js";
import "../../js/wb_m2.js";

const WB = globalThis.WB;
const N_KEEP = 600;
// windows of the analysis plan and smallest horizon (js/wb_app.js, MODELS)
const PLAN = { M1: { s: [0, 0.1], l: [3, 4], horizonMin: 4 }, M2: { s: [0, 0.5], l: [4, 10], horizonMin: 10 } };

function fixedPointOf(model) {
  const fp = model === "M2" ? WB.m2.fixedPoint({}, WB.m2.X_INIT_PLAN) : WB.m1.fixedPoint({});
  fp.nTransient = WB.m2.transientCycles(Math.max.apply(null, fp.eig.mod));
  return fp;
}
// integer series: Park-Miller recurrence from 2026, then y_k = trunc(3 y_(k-1) / 5) + v_k
function seriesText() {
  let x = 2026, y = 0;
  const vals = [];
  for (let k = 0; k < 600; k++) {
    x = (16807 * x) % 2147483647;
    const v = (x % 2001) - 1000;
    y = Math.trunc(3 * y / 5) + v;
    vals.push(y);
  }
  return vals.join("\n") + "\n";
}
function referenceRun(model, source) {
  const mod = model === "M2" ? WB.m2 : WB.m1, fp = fixedPointOf(model), nT = fp.nTransient;
  const w = PLAN[model];
  const settings = Object.assign(mod.settings(), { windowS: w.s, windowL: w.l, horizon: Math.max(w.horizonMin, w.l[1]) });
  let input = null, run = null, seedUsed = null;
  if (source.kind === "file") {
    const series = Float64Array.from(WB.input.parseSeriesText(source.text).slice(0, N_KEEP));
    input = WB.input.inputFixedSeries(series, nT, source.sigma);
    run = mod.simulateRun({}, input, nT + series.length, fp.y0);
  } else {
    for (let a = 0; a <= 20; a++) {
      seedUsed = source.seed + a;
      input = WB.input.inputArfima(nT + N_KEEP, source.d, source.sigma, new WB.input.RRng(seedUsed), nT);
      run = mod.simulateRun({}, input, nT + N_KEEP, fp.y0);
      if (!run.fell) break;
    }
  }
  if (run.fell) throw new Error("the run fell: " + model);
  const res = WB.est.estimateAll(mod.trimRun(run, nT), input, settings);
  return { model: model, source: source.kind, seed: seedUsed, n_transient: nT,
    lambda_s: res.lambdaS, lambda_l: res.lambdaL, n_pairs: res.nPairs,
    alpha_input: res.alphaInput, alpha_duration: res.alphaDuration };
}

const text = seriesText();
const cases = {
  m1_arfima: referenceRun("M1", { kind: "arfima", d: 0.3, sigma: 0.00234, seed: 7 }),
  m2_arfima: referenceRun("M2", { kind: "arfima", d: 0, sigma: 0.00018, seed: 1 }),
  m1_file: referenceRun("M1", { kind: "file", text: text, sigma: 0.00234 }),
  m2_file: referenceRun("M2", { kind: "file", text: text, sigma: 0.00018 })
};
const meta = {
  produced_by: "validation/export_check/page_values.js, written by Claude Opus 5.5 (orchestrating model of the web-simulator session of SIM-ACI)",
  date: new Date().toISOString().slice(0, 10),
  runtime: "Deno " + Deno.version.deno + " (V8 " + Deno.version.v8 + ")"
};
await Deno.writeTextFile(new URL("page_values.json", import.meta.url),
  JSON.stringify({ meta: meta, cases: cases }, null, 2) + "\n");
console.log(JSON.stringify(cases, null, 1));
