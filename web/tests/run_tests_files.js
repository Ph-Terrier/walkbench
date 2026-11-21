/*
 * Title: Validation of the reading of a series from a file by the web simulator
 * Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
 * Produced by: written on 2026-11-21 by the orchestrating model of the web-simulator session of
 *   SIM-ACI (Claude Opus 5.5), without a code-generation model, after the decision of the author
 *   of 2026-11-21 relayed by the coordination session of SIM-ACI (Claude Fable 5.2)
 * Date: 2026-11-21
 * Description: reads each test file of tests/vectors/file_vectors.json (written by
 *   tools/04_make_vectors_files.R) with readSeriesText() and parseSeriesText() of js/wb_input.js.
 *   The bytes are decoded as UTF-8 with TextDecoder, which drops a byte-order mark as the
 *   FileReader of the browser does. Checks: the page accepts or refuses each file as the rule
 *   requires and reports the expected lines (exact); every file that the page accepts is read
 *   by scan() of R, with the same number of values (exact); the series that the page passes to
 *   input_fixed_series() agrees with walkbench at the level "computations on the same values as
 *   R" of the tolerances approved by the author on 2026-11-19 (1e-9, absolute); for
 *   information, the number of values whose IEEE bits differ from those of scan(). Change of
 *   2026-11-21 by the same model, after a second decision of the author: check F7 of
 *   seriesProblem(), which tells too few values from values that are all equal.
 * Dependencies: Deno 1.46 or later
 * Input: tests/vectors/file_vectors.json
 * Output: validation/results_files.json and validation/results_files.md; exit code 1 when a test
 *   fails
 * Usage, from the folder web/: deno run --allow-read=. --allow-write=./validation tests/run_tests_files.js
 */
import "../js/wb_estimators.js";
import "../js/wb_input.js";

const WB = globalThis.WB;
const here = new URL(".", import.meta.url);
const vec = JSON.parse(await Deno.readTextFile(new URL("vectors/file_vectors.json", here)));

const TOL = { sameInput: 1e-9 };

const results = [];
function record(id, label, diff, tol, extra) {
  const pass = tol === null ? null : Number.isFinite(diff) && diff <= tol;
  results.push(Object.assign({ id: id, label: label, max_abs_diff: diff, tolerance: tol, pass: pass }, extra || {}));
}
function decode(b64) {
  const bin = atob(b64);
  const bytes = new Uint8Array(bin.length);
  for (let i = 0; i < bin.length; i++) bytes[i] = bin.charCodeAt(i);
  return new TextDecoder("utf-8").decode(bytes);
}
const dv = new DataView(new ArrayBuffer(8));
function bits(x) {
  dv.setFloat64(0, x);
  let h = "";
  for (let i = 0; i < 8; i++) h += dv.getUint8(i).toString(16).padStart(2, "0");
  return h;
}
function bitsToNumber(h) {
  for (let i = 0; i < 8; i++) dv.setUint8(i, parseInt(h.slice(2 * i, 2 * i + 2), 16));
  return dv.getFloat64(0);
}

let nOutcome = 0, nCount = 0, nParse = 0, dFixed = 0, nFixed = 0, nBitsDiff = 0, nBits = 0, relBits = 0;
const outcomeDetail = [];
for (const c of vec.cases) {
  const text = decode(c.bytes_base64);
  const r = WB.input.readSeriesText(text);
  const accepts = r.bad.length === 0;
  const badGot = r.bad.map((b) => b.line + ":" + b.kind).join(",");
  const badExp = c.page_bad.map((b) => b.line + ":" + b.kind).join(",");
  if (accepts !== c.page_accepts || badGot !== badExp) {
    nOutcome++;
    outcomeDetail.push(`${c.name}: expected ${c.page_accepts ? "accepted" : "refused [" + badExp + "]"}, got ${accepts ? "accepted" : "refused [" + badGot + "]"}`);
  }
  // parseSeriesText(): the same values, or an error for a refused file
  let parsed = null;
  try { parsed = WB.input.parseSeriesText(text); } catch (e) { parsed = null; }
  if ((parsed === null) !== !accepts || (parsed && parsed.length !== r.values.length)) nParse++;
  if (!accepts) continue;
  // a file accepted by the page is read by scan() with the same number of values
  if (!c.r_accepts || c.r_n !== r.values.length) { nCount++; continue; }
  for (let i = 0; i < r.values.length; i++) {
    nBits++;
    if (bits(r.values[i]) !== c.r_bits[i]) {
      nBitsDiff++;
      const ref = bitsToNumber(c.r_bits[i]);
      relBits = Math.max(relBits, Math.abs(r.values[i] - ref) / Math.max(Math.abs(ref), Number.MIN_VALUE));
    }
  }
  if (c.fixed_series) {
    const fx = WB.input.inputFixedSeries(Float64Array.from(r.values), 0, vec.meta.sd_of_fixed_series);
    let m = fx.length === c.fixed_series.length ? 0 : Infinity;
    for (let i = 0; m < Infinity && i < fx.length; i++) m = Math.max(m, Math.abs(fx[i] - c.fixed_series[i]));
    dFixed = Math.max(dFixed, m);
    nFixed++;
  }
}
const nAcc = vec.cases.filter((c) => c.page_accepts).length;
const nRef = vec.cases.length - nAcc;
record("F1", `outcome of the page for ${vec.cases.length} files (${nAcc} accepted, ${nRef} refused) and the lines that it reports: number of files that differ from the rule`, nOutcome, 0,
  outcomeDetail.length ? { detail: outcomeDetail.join("; ") } : undefined);
record("F2", "parseSeriesText(): same values as readSeriesText() for an accepted file, error for a refused file: number of files that differ", nParse, 0);
record("F3", "files accepted by the page and refused by scan() of R, or read with another number of values", nCount, 0);
record("F4", `input_fixed_series() on the values read by the page and by scan() (${nFixed} files, sd ${vec.meta.sd_of_fixed_series})`, dFixed, TOL.sameInput);
record("F5", `values read by the page whose IEEE bits differ from those of scan() (${nBitsDiff} of ${nBits}; the column gives the largest relative difference)`, relBits, null);
const decComma = vec.cases.filter((c) => c.page_bad.some((b) => b.kind === "comma"));
record("F6", `files with a decimal comma accepted by the page (${decComma.length} file)`, decComma.filter((c) => WB.input.readSeriesText(decode(c.bytes_base64)).bad.length === 0).length, 0);

// seriesProblem(): too few values first, then a series whose values are all equal (decision of
// the author of 2026-11-21: own message for equal values)
const valid600 = WB.input.readSeriesText(decode(vec.cases[0].bytes_base64)).values;
const fill = (n, v) => Array(n).fill(v);
const spCases = [
  ["no value", [], "too_few"],
  ["119 different values", valid600.slice(0, 119), "too_few"],
  ["119 equal values", fill(119, 0.5), "too_few"],
  ["120 values equal to 0.1", fill(120, 0.1), "constant"],
  ["600 values equal to 1/3", fill(600, 1 / 3), "constant"],
  ["600 values equal to 123.456", fill(600, 123.456), "constant"],
  ["600 values equal to 0", fill(600, 0), "constant"],
  ["119 values equal to 0.1 and one value 0.2", fill(119, 0.1).concat([0.2]), null],
  ["the 600 values of the first test file", valid600, null]
];
const spBad = spCases.filter((c) => WB.input.seriesProblem(c[1], 120) !== c[2]);
record("F7", `seriesProblem() with 120 values at least: number of the ${spCases.length} series whose class (too few values, all values equal, valid) differs from the expected one`,
  spBad.length, 0, spBad.length ? { detail: spBad.map((c) => c[0]).join("; ") } : undefined);

// --- Report ------------------------------------------------------------------------------------
const nFail = results.filter((r) => r.pass === false).length;
const out = {
  produced_by: "tests/run_tests_files.js, written by the orchestrating model of the web-simulator session of SIM-ACI (Claude Opus 5.5)",
  date: new Date().toISOString(),
  runtime: `Deno ${Deno.version.deno}, V8 ${Deno.version.v8}`,
  vectors: { walkbench_version: vec.meta.walkbench_version, git_commit: vec.meta.git_commit, date: vec.meta.date,
    r_version: vec.meta.r_version, scan_call: vec.meta.scan_call },
  tolerances: TOL, n_tests: results.length, n_failed: nFail, results: results,
  files: vec.cases.map((c) => ({ name: c.name, page_accepts: c.page_accepts, r_accepts: c.r_accepts,
    r_n: c.r_accepts ? c.r_n : null, r_error: c.r_error }))
};
const fmt = (x) => (x === null ? "" : !Number.isFinite(x) ? String(x) : x === 0 ? "0" : Number.isInteger(x) ? String(x) : x.toExponential(2));
let md = "| Test | Check | Largest difference | Tolerance | Result |\n|---|---|---|---|---|\n";
const cell = (s) => String(s).replace(/\|/g, "\\|");
for (const r of results) {
  md += `| ${r.id} | ${cell(r.label + (r.detail ? " (" + r.detail + ")" : ""))} | ${fmt(r.max_abs_diff)} | ${r.tolerance === null ? "information" : fmt(r.tolerance)} | ${r.pass === null ? "" : r.pass ? "pass" : "FAIL"} |\n`;
}
md += "\n| File | Page | scan() of R |\n|---|---|---|\n";
for (const c of vec.cases) {
  const page = c.page_accepts ? "accepted" : "refused; reported lines: " + c.page_bad.map((b) => b.line + (b.kind === "comma" ? " (decimal comma)" : "")).join(", ");
  const r = c.r_accepts ? `${c.r_n} values${c.r_all_finite === false ? ", not all finite" : ""}` : "error";
  md += `| ${cell(c.name)} | ${page} | ${r} |\n`;
}
const valDir = new URL("../validation/", here);
await Deno.mkdir(valDir, { recursive: true });
await Deno.writeTextFile(new URL("results_files.json", valDir), JSON.stringify(out, null, 2));
await Deno.writeTextFile(new URL("results_files.md", valDir),
  `<!-- Written by tests/run_tests_files.js (${out.date}); ${out.runtime}; vectors of walkbench ${vec.meta.walkbench_version}, commit ${vec.meta.git_commit} -->\n\n` + md);
console.log(md);
console.log(`${results.length} checks, ${nFail} failed`);
if (nFail > 0) Deno.exit(1);
