/*
 * Title: Texts of the web simulator in French and English
 * Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
 * Produced by: written by the orchestrating model of the web-simulator session of SIM-ACI
 *   (Claude Opus 5.5), without a code-generation model; some texts adapted from
 *   feasibility/walker_demo.html (Claude Fable 5, 2026-08-20)
 * Date: 2026-11-19
 * Description: one dictionary per language; WB.t(key, values) returns the text of the current
 *   language with {name} replaced by values.name. The language follows the browser at the
 *   first visit (French when the browser language starts with "fr", English otherwise), and the
 *   choice of the visitor is kept in the local storage of the browser when it is available.
 *   Second version of the same day: texts of model M2 and of the export of R code.
 *   Change of 2026-11-20 by the same model, after the remarks of the author on the first trial:
 *   note under the presets (definition of sigma_max, amplitude of the preset "Beyond sigma_max").
 *   Later the same day: the banner and the footer name walkbench 0.7.0, a local version not yet
 *   released, whose grid count of resample_states() the page now follows. Then, for the
 *   publication on GitHub Pages chosen by the author: the banner and the footer name walkbench
 *   0.7.0 with the concept DOI (checked on DataCite on 2026-11-20), without the mentions of a
 *   local prototype, and the footer states what the host receives. Last, after the decision of
 *   the author: the footer cites the preprint of article P1 (arXiv:2611.26182, DOI checked on
 *   DataCite on 2026-11-20). Change of 2026-11-21 by the same model, after the decision of the
 *   author relayed by the coordination session of SIM-ACI (Claude Fable 5.2): the hint on the
 *   local file states the format of the file, and two messages report the lines of a refused
 *   file (decimal comma; other lines). Later that day, after a second decision of the author:
 *   a message for a file whose values are all equal; the status line of an attempt shows its
 *   seed; the footer names the source file of the drawing and its producer, as web/README.md
 *   does.
 * Dependencies: none
 * Input: none
 * Output: WB.i18n, WB.t and WB.lang on the global object
 */
(function (root) {
  "use strict";
  const WB = (root.WB = root.WB || {});

  const fr = {
    page_title: "walkbench : simulateur des modèles M1 et M2",
    title: "walkbench : simulateur des modèles M1 et M2",
    subtitle: "Deux modèles de marche passive du paquet walkbench, perturbés à chaque pas par une série ARFIMA : M1, le marcheur le plus simple (Garcia et al. 1998 ; Su et Dingwell 2007), et M2, le marcheur à pieds arrondis et ressort de hanche (Hobbelen et Wisse 2007 ; Bruijn et al. 2012). La marche, le portrait de phase et les durées de pas sont calculés en direct dans le navigateur.",
    lang_button: "English",
    banner: "Démonstration pédagogique. La page reproduit les modèles M1 et M2 du paquet R walkbench 0.7.0 (doi:10.5281/zenodo.24354488, DOI de toutes les versions) ; les rapports de validation (dossier web/validation) mesurent l'accord. Les résultats publiés du projet SIM-ACI proviennent de walkbench.",
    fall_overlay: "chute : retour au point fixe",
    model_label: "Modèle",
    model_m1: "M1 : marcheur le plus simple",
    model_m2: "M2 : pieds arrondis et ressort de hanche",
    play: "Lecture",
    pause: "Pause",
    reset: "Réinitialiser",
    speed: "Vitesse",
    sigma_m1: "Amplitude σ de ε (rad)",
    sigma_m2: "Amplitude σ de h (longueur de jambe)",
    sigma_hint_m1: "σref du plan : 0,00234 rad ; σmax : 0,0039 rad",
    sigma_hint_m2: "σref du plan : 0,00018 ; σmax : 0,00030 (longueur de jambe)",
    dmem: "Mémoire d",
    presets: "Préréglages",
    preset_none: "Sans bruit",
    preset_white: "Blanc",
    preset_pers: "Persistant",
    preset_anti: "Anti-persistant",
    preset_storm: "Au-delà de σmax",
    presets_note: "σmax est l'amplitude de la calibration du plan juste en dessous de la première où plus de 5 % des runs de 300 foulées chutaient, pour au moins une valeur de d. Juste au-delà de σmax, les chutes restent rares dans l'animation ; le préréglage « Au-delà de σmax » prend 3 σmax, où elles sont fréquentes.",
    steps: "Pas",
    falls: "Chutes",
    step_duration: "Durée du pas",
    eps_now_m1: "ε courant (rad)",
    eps_now_m2: "h courant (longueur de jambe)",
    alpha_live: "α des durées (indicatif)",
    ls_live: "λS indicatif",
    ll_live: "λL indicatif (ACI)",
    live_note: "Valeurs indicatives, calculées pendant l'animation sur environ les 100 derniers pas, par unité de temps du modèle (M1) ou par foulée (M2). Après un changement de réglage, elles mettent quelques dizaines de pas à se stabiliser. Les valeurs de référence viennent du run de référence, plus bas.",
    phase_title: "Portrait de phase (θG, θ̇G)",
    phase_note: "le cycle limite est l'attracteur ; le bruit l'épaissit",
    dur_title: "Durées de pas (160 dernières)",
    dur_note: "α ≈ 0,5 : bruit blanc ; α < 0,5 : anti-persistant ; α > 0,5 : persistant",
    waiting: "en attente de pas…",
    dur_legend: "durée (unités de temps du modèle) ; pointillé : moyenne",
    scene_legend_m1: "repère de la pente (γ = 0,009 rad) ; marques colorées : ε à chaque contact",
    scene_legend_m2: "pente γ = 0,004 rad ; pieds de rayon 0,45 ; marques colorées : h à chaque contact",
    settings_title: "Entrée et fenêtres d'estimation",
    source: "Entrée",
    source_arfima: "Bruit ARFIMA généré dans la page",
    source_file: "Série d'un fichier local",
    file_hint: "Fichier texte, un nombre par ligne, écrit avec un point décimal (0.123) ; le texte qui suit # est un commentaire, et les lignes vides sont ignorées. Un fichier qui contient une autre ligne est refusé. Le fichier reste dans le navigateur ; rien n'est envoyé. La série est centrée et ramenée à l'écart type σ.",
    file_read: "{n} valeurs lues ; moyenne {m} ; écart type {s}",
    file_error: "Moins de 120 valeurs lisibles dans le fichier.",
    file_constant: "Fichier refusé : toutes les valeurs sont égales, et une série constante ne peut pas être ramenée à l'écart type σ.",
    file_bad_comma: "Fichier refusé : virgule décimale dans {n} ligne(s) (numéros de ligne : {lines}), par exemple « {text} ». La page et la fonction scan() de R lisent les nombres écrits avec un point décimal (0.123).",
    file_bad_format: "Fichier refusé : {n} ligne(s) ne contiennent pas un seul nombre (numéros de ligne : {lines}), par exemple « {text} ». Format attendu : un nombre par ligne, écrit avec un point décimal ; le texte qui suit # est un commentaire.",
    seed: "Graine",
    seed_hint: "Graine de set.seed() dans R : avec la même graine, input_arfima() de walkbench tire la même série que le run de référence. Après une chute, le run essaie la graine suivante.",
    windows_m1: "Fenêtres d'estimation, en pas",
    windows_m2: "Fenêtres d'estimation, en foulées",
    ws_from: "λS de",
    wl_from: "λL de",
    to: "à",
    windows_plan: "Valeurs du plan",
    windows_note_m1: "Valeurs du plan d'analyse pour M1 : λS de 0 à 0,1 pas, λL de 3 à 4 pas, moyenne des pentes locales sur l'état continu. Fenêtre de Theiler : une foulée. Horizon : 4 pas, ou la fin de la fenêtre de λL si elle est plus loin (10 pas au plus).",
    windows_note_m2: "Valeurs du plan d'analyse pour M2 : λS de 0 à 0,5 foulée, λL de 4 à 10 foulées, pentes par moindres carrés sur les variables d'état du modèle rééchantillonnées à 100 points par foulée. Fenêtre de Theiler : une demi-foulée. Horizon : 10 foulées.",
    windows_error: "Fenêtres invalides : il faut 0 ≤ début < fin ≤ 10.",
    ref_title: "Run de référence, avec les réglages de walkbench",
    ref_text: "Un run de {nt} pas de transitoire et 600 pas gardés, avec l'amplitude, la mémoire, l'entrée et les fenêtres choisies. Les grandeurs sont calculées comme par estimate_all() de walkbench : réglages du plan pour le modèle (tableau 5.1) et DFA de Terrier (2026). Après une chute, la série est tirée à nouveau, jusqu'à 20 fois ; une série de fichier n'est pas tirée à nouveau.",
    ref_button: "Lancer le run de référence",
    ref_sim: "Simulation du run, tentative {a}…",
    ref_sim_seed: "Simulation du run, tentative {a} (graine {s})…",
    ref_fall_redraw: "Chute au pas {k} : nouveau tirage ({a} sur 20).",
    ref_fall_file: "Chute au pas {k}. Une série de fichier n'est pas tirée à nouveau ; une amplitude plus faible évite la chute.",
    ref_all_fell: "Les 21 tentatives ont chuté ; une amplitude plus faible évite les chutes.",
    ref_est: "Recherche des voisins et courbe de divergence…",
    ref_done: "Terminé en {s} s.",
    ref_short: "La série du fichier compte {n} valeurs ; le run garde {n} pas.",
    r_model: "Modèle",
    r_steps: "Pas simulés (transitoire + gardés)",
    r_redraws: "Nouveaux tirages après une chute",
    r_mean: "Durée moyenne du pas (unités de temps)",
    r_sd: "Écart type ; coefficient de variation des durées",
    r_ls_m1: "λS : par unité de temps ; par pas",
    r_ll_m1: "λL (ACI) : par unité de temps ; par pas",
    r_ls_m2: "λS : par foulée ; par pas",
    r_ll_m2: "λL (ACI) : par foulée ; par pas",
    r_pairs: "Paires complètes",
    r_alpha: "α de l'entrée ; des durées de pas ; des durées de foulée",
    r_time: "Temps de calcul (simulation ; estimation)",
    r_input: "Entrée",
    r_input_arfima: "ARFIMA, d = {d}, σ = {s}, graine {g}",
    r_input_file: "série du fichier, σ = {s}",
    div_title: "Courbe de divergence du run de référence",
    div_note: "logarithme moyen de la distance des paires (trait fin : brut ; trait épais : lissé, pour M1) ; zones grisées : fenêtres de λS et de λL ; droites : pentes",
    div_x_m1: "temps (pas)",
    div_x_m2: "temps (foulées)",
    r_code_title: "Code R qui rejoue ce run dans walkbench",
    r_code_note: "Avec la même graine, input_arfima() de walkbench tire la même série que la page. Les valeurs de walkbench diffèrent de celles de la page dans les limites des rapports de validation.",
    r_code_copy: "Copier le code",
    r_code_copied: "Code copié.",
    r_code_select: "Code sélectionné : Ctrl+C pour le copier.",
    footer_html: "Modèles : Garcia M, Chatterjee A, Ruina A, Coleman M (1998), <i>J Biomech Eng</i> 120:281-288, <a href=\"https://doi.org/10.1115/1.2798313\">doi:10.1115/1.2798313</a> ; Su JL-S, Dingwell JB (2007), <i>J Biomech Eng</i> 129:802-810, <a href=\"https://doi.org/10.1115/1.2800760\">doi:10.1115/1.2800760</a> ; Hobbelen DGE, Wisse M (2007), <i>IEEE Trans Robot</i> 23:1213-1224, <a href=\"https://doi.org/10.1109/TRO.2007.904908\">doi:10.1109/TRO.2007.904908</a> ; Bruijn SM, Bregman DJJ, Meijer OG, Beek PJ, van Dieën JH (2012), <i>Med Eng Phys</i> 34:428-436, <a href=\"https://doi.org/10.1016/j.medengphy.2011.07.024\">doi:10.1016/j.medengphy.2011.07.024</a>. Code de référence : walkbench 0.7.0, <a href=\"https://doi.org/10.5281/zenodo.24354488\">doi:10.5281/zenodo.24354488</a> (DOI de toutes les versions) ; moteur de Rosenstein de RACING v1.0.2, <a href=\"https://doi.org/10.5281/zenodo.20542521\">doi:10.5281/zenodo.20542521</a> ; plan d'analyse SIM-ACI, <a href=\"https://doi.org/10.17605/OSF.IO/M74TK\">doi:10.17605/OSF.IO/M74TK</a> ; archive de reproduction de l'article P1, <a href=\"https://doi.org/10.5281/zenodo.24365284\">doi:10.5281/zenodo.24365284</a> ; préprint de l'article P1 : Terrier P (2026), arXiv:2611.26182, <a href=\"https://doi.org/10.48550/arXiv.2611.26182\">doi:10.48550/arXiv.2611.26182</a>.<br>Auteur responsable : Philippe Terrier (HE-Arc Santé, Neuchâtel). Code de la page écrit par Claude Opus 5.5, modèle orchestrateur de la session du simulateur web (19 novembre 2026) ; dessin, mise en page et estimateur indicatif adaptés du fichier feasibility/walker_demo.html du projet SIM-ACI, écrit le 20 août 2026 par Claude Fable 5, modèle orchestrateur de la session de faisabilité. Licence MIT. La page ne fait aucune requête réseau et ne contient aucune donnée de tiers. La version en ligne est hébergée par GitHub Pages : GitHub reçoit les adresses IP des visiteurs dans ses journaux, et le code de la page n'envoie rien."
  };

  const en = {
    page_title: "walkbench: simulator of models M1 and M2",
    title: "walkbench: simulator of models M1 and M2",
    subtitle: "Two passive walking models of the walkbench package, perturbed at each step by an ARFIMA series: M1, the simplest walking model (Garcia et al. 1998; Su and Dingwell 2007), and M2, the walker with arced feet and a hip spring (Hobbelen and Wisse 2007; Bruijn et al. 2012). The walk, the phase portrait and the step durations are computed live in the browser.",
    lang_button: "Français",
    banner: "Teaching demonstration. The page reproduces models M1 and M2 of the R package walkbench 0.7.0 (doi:10.5281/zenodo.24354488, the DOI of all versions); the validation reports (folder web/validation) measure the agreement. The published results of the SIM-ACI project come from walkbench.",
    fall_overlay: "fall: back to the fixed point",
    model_label: "Model",
    model_m1: "M1: simplest walker",
    model_m2: "M2: arced feet and hip spring",
    play: "Play",
    pause: "Pause",
    reset: "Reset",
    speed: "Speed",
    sigma_m1: "Amplitude σ of ε (rad)",
    sigma_m2: "Amplitude σ of h (leg lengths)",
    sigma_hint_m1: "σref of the plan: 0.00234 rad; σmax: 0.0039 rad",
    sigma_hint_m2: "σref of the plan: 0.00018; σmax: 0.00030 (leg lengths)",
    dmem: "Memory d",
    presets: "Presets",
    preset_none: "No noise",
    preset_white: "White",
    preset_pers: "Persistent",
    preset_anti: "Anti-persistent",
    preset_storm: "Beyond σmax",
    presets_note: "σmax is the amplitude of the calibration of the plan just below the first one at which more than 5% of the runs of 300 strides fell, at one or more levels of d. Just above σmax, falls remain rare in the animation; the preset “Beyond σmax” uses 3 σmax, where they are frequent.",
    steps: "Steps",
    falls: "Falls",
    step_duration: "Step duration",
    eps_now_m1: "Current ε (rad)",
    eps_now_m2: "Current h (leg lengths)",
    alpha_live: "α of step durations (indicative)",
    ls_live: "Indicative λS",
    ll_live: "Indicative λL (ACI)",
    live_note: "Indicative values, computed during the animation over about the last 100 steps, per unit of model time (M1) or per stride (M2). After a change of setting, they need a few tens of steps to settle. The reference values come from the reference run, below.",
    phase_title: "Phase portrait (θG, θ̇G)",
    phase_note: "the limit cycle is the attractor; the noise widens it",
    dur_title: "Step durations (last 160)",
    dur_note: "α ≈ 0.5: white noise; α < 0.5: anti-persistent; α > 0.5: persistent",
    waiting: "waiting for steps…",
    dur_legend: "duration (model time units); dashed: mean",
    scene_legend_m1: "frame of the slope (γ = 0.009 rad); colored marks: ε at each foot strike",
    scene_legend_m2: "slope γ = 0.004 rad; feet of radius 0.45; colored marks: h at each foot strike",
    settings_title: "Input and fitting windows",
    source: "Input",
    source_arfima: "ARFIMA noise generated in the page",
    source_file: "Series from a local file",
    file_hint: "Text file, one number per line, written with a decimal point (0.123); the text after # is a comment, and blank lines are skipped. A file with any other line is refused. The file stays in the browser; nothing is sent. The series is centered and rescaled to the standard deviation σ.",
    file_read: "{n} values read; mean {m}; standard deviation {s}",
    file_error: "Fewer than 120 readable values in the file.",
    file_constant: "File refused: all values are equal, and a constant series cannot be rescaled to the standard deviation σ.",
    file_bad_comma: "File refused: decimal comma in {n} line(s) (line numbers: {lines}), for example “{text}”. The page and the function scan() of R read numbers written with a decimal point (0.123).",
    file_bad_format: "File refused: {n} line(s) do not hold a single number (line numbers: {lines}), for example “{text}”. Expected format: one number per line, written with a decimal point; the text after # is a comment.",
    seed: "Seed",
    seed_hint: "Seed of set.seed() in R: with the same seed, input_arfima() of walkbench draws the same series as the reference run. After a fall, the run tries the next seed.",
    windows_m1: "Fitting windows, in steps",
    windows_m2: "Fitting windows, in strides",
    ws_from: "λS from",
    wl_from: "λL from",
    to: "to",
    windows_plan: "Values of the plan",
    windows_note_m1: "Values of the analysis plan for M1: λS from 0 to 0.1 step, λL from 3 to 4 steps, mean of the local slopes on the continuous state. Theiler window: one stride. Horizon: 4 steps, or the end of the λL window when it lies further (10 steps at most).",
    windows_note_m2: "Values of the analysis plan for M2: λS from 0 to 0.5 stride, λL from 4 to 10 strides, least-squares slopes on the state variables of the model resampled to 100 points per stride. Theiler window: half a stride. Horizon: 10 strides.",
    windows_error: "Invalid windows: 0 ≤ start < end ≤ 10 is required.",
    ref_title: "Reference run, with the settings of walkbench",
    ref_text: "A run of {nt} transient steps and 600 kept steps, with the chosen amplitude, memory, input and windows. The quantities are computed as by estimate_all() of walkbench: settings of the plan for the model (table 5.1) and DFA of Terrier (2026). After a fall, the series is drawn again, up to 20 times; a series from a file is not drawn again.",
    ref_button: "Start the reference run",
    ref_sim: "Simulating the run, attempt {a}…",
    ref_sim_seed: "Simulating the run, attempt {a} (seed {s})…",
    ref_fall_redraw: "Fall at step {k}: new draw ({a} of 20).",
    ref_fall_file: "Fall at step {k}. A series from a file is not drawn again; a smaller amplitude avoids the fall.",
    ref_all_fell: "All 21 attempts fell; a smaller amplitude avoids the falls.",
    ref_est: "Nearest neighbors and divergence curve…",
    ref_done: "Done in {s} s.",
    ref_short: "The series of the file has {n} values; the run keeps {n} steps.",
    r_model: "Model",
    r_steps: "Simulated steps (transient + kept)",
    r_redraws: "Redraws after a fall",
    r_mean: "Mean step duration (time units)",
    r_sd: "Standard deviation; coefficient of variation of the durations",
    r_ls_m1: "λS: per time unit; per step",
    r_ll_m1: "λL (ACI): per time unit; per step",
    r_ls_m2: "λS: per stride; per step",
    r_ll_m2: "λL (ACI): per stride; per step",
    r_pairs: "Complete pairs",
    r_alpha: "α of the input; of the step durations; of the stride durations",
    r_time: "Computing time (simulation; estimation)",
    r_input: "Input",
    r_input_arfima: "ARFIMA, d = {d}, σ = {s}, seed {g}",
    r_input_file: "series from the file, σ = {s}",
    div_title: "Divergence curve of the reference run",
    div_note: "mean logarithm of the distance of the pairs (thin line: raw; thick line: smoothed, for M1); shaded areas: windows of λS and λL; straight lines: slopes",
    div_x_m1: "time (steps)",
    div_x_m2: "time (strides)",
    r_code_title: "R code that reruns this run in walkbench",
    r_code_note: "With the same seed, input_arfima() of walkbench draws the same series as the page. The values of walkbench differ from those of the page within the limits of the validation reports.",
    r_code_copy: "Copy the code",
    r_code_copied: "Code copied.",
    r_code_select: "Code selected: press Ctrl+C to copy it.",
    footer_html: "Models: Garcia M, Chatterjee A, Ruina A, Coleman M (1998), <i>J Biomech Eng</i> 120:281-288, <a href=\"https://doi.org/10.1115/1.2798313\">doi:10.1115/1.2798313</a>; Su JL-S, Dingwell JB (2007), <i>J Biomech Eng</i> 129:802-810, <a href=\"https://doi.org/10.1115/1.2800760\">doi:10.1115/1.2800760</a>; Hobbelen DGE, Wisse M (2007), <i>IEEE Trans Robot</i> 23:1213-1224, <a href=\"https://doi.org/10.1109/TRO.2007.904908\">doi:10.1109/TRO.2007.904908</a>; Bruijn SM, Bregman DJJ, Meijer OG, Beek PJ, van Dieën JH (2012), <i>Med Eng Phys</i> 34:428-436, <a href=\"https://doi.org/10.1016/j.medengphy.2011.07.024\">doi:10.1016/j.medengphy.2011.07.024</a>. Reference code: walkbench 0.7.0, <a href=\"https://doi.org/10.5281/zenodo.24354488\">doi:10.5281/zenodo.24354488</a> (the DOI of all versions); Rosenstein engine of RACING v1.0.2, <a href=\"https://doi.org/10.5281/zenodo.20542521\">doi:10.5281/zenodo.20542521</a>; SIM-ACI analysis plan, <a href=\"https://doi.org/10.17605/OSF.IO/M74TK\">doi:10.17605/OSF.IO/M74TK</a>; reproduction archive of article P1, <a href=\"https://doi.org/10.5281/zenodo.24365284\">doi:10.5281/zenodo.24365284</a>; preprint of article P1: Terrier P (2026), arXiv:2611.26182, <a href=\"https://doi.org/10.48550/arXiv.2611.26182\">doi:10.48550/arXiv.2611.26182</a>.<br>Responsible author: Philippe Terrier (HE-Arc Santé, Neuchâtel). Code of the page written by Claude Opus 5.5, orchestrating model of the web-simulator session (19 November 2026); drawing, layout and indicative estimator adapted from feasibility/walker_demo.html of the SIM-ACI project, written on 20 August 2026 by Claude Fable 5, orchestrating model of the feasibility session. MIT license. The page makes no network request and holds no third-party data. The online version is hosted by GitHub Pages: GitHub receives the IP addresses of the visitors in its logs, and the code of the page sends nothing."
  };

  WB.i18n = { fr: fr, en: en };
  WB.lang = "fr";
  WB.t = function (key, values) {
    const dict = WB.i18n[WB.lang] || WB.i18n.fr;
    let s = dict[key];
    if (s === undefined) s = WB.i18n.fr[key] !== undefined ? WB.i18n.fr[key] : key;
    if (values) {
      s = s.replace(/\{(\w+)\}/g, function (m, name) {
        return Object.prototype.hasOwnProperty.call(values, name) ? String(values[name]) : m;
      });
    }
    return s;
  };
})(typeof globalThis !== "undefined" ? globalThis : this);
