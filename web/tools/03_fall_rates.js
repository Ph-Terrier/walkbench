/*
 * Title: Steps before a fall in the engines of the page, at multiples of sigma_max
 * Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
 * Produced by: written by the orchestrating model of the web-simulator session of SIM-ACI
 *   (Claude Opus 5.5), on 2026-11-20, without a code-generation model
 * Date: 2026-11-20
 * Description: simulates runs of M1 and M2 from the fixed point with the engines of the page.
 *   The inputs are ARFIMA series with d = 0 (white noise), drawn by the generator of the page
 *   with fixed seeds, at multiples of sigma_max of the analysis plan (M1: 0.0039 rad; M2: 0.0003
 *   leg lengths). The script counts the steps before the first fall. Its results set the
 *   amplitude of the preset "Beyond sigma_max" and the end of the amplitude slider in
 *   js/wb_app.js. They describe the animation of the page; the calibration of the analysis plan
 *   (runs of 300 strides, nine levels of d) remains the one of walkbench.
 * Dependencies: Deno 1.46 or later; js/wb_estimators.js, js/wb_input.js, js/wb_m1.js,
 *   js/wb_m2.js
 * Input: none
 * Output: validation/fall_rates.json, validation/fall_rates.md
 * Usage, from the folder web/:
 *   deno run --allow-read=. --allow-write=./validation tools/03_fall_rates.js
 */
import "../js/wb_estimators.js";
import "../js/wb_input.js";
import "../js/wb_m1.js";
import "../js/wb_m2.js";

const WB = globalThis.WB;
const N_RUNS = 60;          // runs per amplitude, seeds 1001 to 1060
const MAX_STEPS = 2000;     // a run without a fall stops here
const MULTIPLES = [1, 1.5, 2, 3, 4, 5];
const MODEL_PER_SECOND = 2.9; // model time per second of animation at speed 1 (js/wb_app.js)
const SIGMA_MAX = { M1: 0.0039, M2: 0.0003 };

function median(sorted) {
  const n = sorted.length;
  const a = sorted[Math.floor((n - 1) / 2)], b = sorted[Math.ceil((n - 1) / 2)];
  return Number.isFinite(a) && Number.isFinite(b) ? (a + b) / 2 : Infinity;
}

const rows = [];
for (const model of ["M1", "M2"]) {
  const mod = model === "M2" ? WB.m2 : WB.m1;
  const fp = model === "M2" ? WB.m2.fixedPoint({}, WB.m2.X_INIT_PLAN) : WB.m1.fixedPoint({});
  for (const m of MULTIPLES) {
    const sigma = Number((m * SIGMA_MAX[model]).toPrecision(6));
    const steps = [];
    const t0 = performance.now();
    for (let k = 0; k < N_RUNS; k++) {
      const input = WB.input.inputArfima(MAX_STEPS, 0, sigma, new WB.input.RRng(1001 + k), 0);
      const run = mod.simulateRun({}, input, MAX_STEPS, fp.y0);
      steps.push(run.fell ? run.fallCycle : Infinity);
    }
    const sorted = steps.slice().sort(function (a, b) { return a - b; });
    const med = median(sorted);
    rows.push({
      model: model, multiple: m, sigma: sigma, runs: N_RUNS,
      falls: steps.filter(Number.isFinite).length,
      median_steps: Number.isFinite(med) ? med : null,
      share_within_40_steps: steps.filter(function (x) { return x <= 40; }).length / N_RUNS,
      median_seconds_speed_1: Number.isFinite(med) ? med * fp.period / MODEL_PER_SECOND : null,
      step_period: fp.period,
      seconds_of_computation: (performance.now() - t0) / 1000
    });
    console.log(model, m, sigma, rows[rows.length - 1].falls + "/" + N_RUNS, med);
  }
}

const meta = {
  produced_by: "tools/03_fall_rates.js, written by Claude Opus 5.5 (orchestrating model of the web-simulator session of SIM-ACI)",
  date: new Date().toISOString().slice(0, 10),
  runtime: "Deno " + Deno.version.deno + " (V8 " + Deno.version.v8 + ")",
  runs_per_amplitude: N_RUNS, max_steps: MAX_STEPS, seeds: "1001 to " + (1000 + N_RUNS),
  d: 0, sigma_max: SIGMA_MAX, model_time_per_second_at_speed_1: MODEL_PER_SECOND
};
await Deno.writeTextFile(new URL("../validation/fall_rates.json", import.meta.url),
  JSON.stringify({ meta: meta, rows: rows }, null, 2) + "\n");

const fmt = function (x, d, censored) { return x === null ? censored : x.toFixed(d); };
const md = [
  "# Steps before a fall in the animation of the page",
  "",
  "Written by `tools/03_fall_rates.js` (" + meta.runtime + ") on " + meta.date + ". " +
    "Each amplitude: " + N_RUNS + " runs from the fixed point, white noise (d = 0) drawn by the generator of the page " +
    "with the seeds " + meta.seeds + ", at most " + MAX_STEPS + " steps. " +
    "The median time assumes the speed 1 of the animation (" + MODEL_PER_SECOND + " units of model time per second).",
  "",
  "| Model | Multiple of σmax | σ | Runs with a fall | Median number of steps before a fall | Share of runs with a fall within 40 steps | Median time before a fall at speed 1 (s) |",
  "|---|---|---|---|---|---|---|"
];
for (const r of rows) {
  md.push("| " + [r.model, r.multiple, r.sigma, r.falls + " of " + r.runs,
    fmt(r.median_steps, 0, "more than " + MAX_STEPS), r.share_within_40_steps.toFixed(2),
    fmt(r.median_seconds_speed_1, 0, "–")].join(" | ") + " |");
}
md.push("");
md.push("A median of more than " + MAX_STEPS + " steps means that fewer than half of the runs fell within " +
  MAX_STEPS + " steps; its time is then not given.");
md.push("");
await Deno.writeTextFile(new URL("../validation/fall_rates.md", import.meta.url), md.join("\n"));
console.log("written: validation/fall_rates.json, validation/fall_rates.md");
