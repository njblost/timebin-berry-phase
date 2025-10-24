(* ============================================
 run-all.wls — headless runner & auto-exporter (v4.5)
 - Roots (first match wins):
     1) src/notebooks_run-all WLS
     2) <repo>/notebooks
 - Exports to: src/results/{logs,figs,tables,css}
 - Captures Graphics returned, printed, or assigned to variables
 - Skips berry_phase.nb and *_Exports.nb by default (avoid duplicates)
 - Fallbacks:
     * wavefunctionplot.nb -> PDF/PNG/CSV
     * any probability_plot*.nb -> PNG/PDF/CSV of {|α|^2,|β|^2}
     * If nothing exported: scrape GraphicsBox / Graphics3DBox and export
 ============================================ *)

Begin["RunnerPrivate`"];

SeedRandom[123456];
$HistoryLength = 0;
Off[$ConfiguredKernels::obso];
Off[$DefaultKernels::obso];

(* ---------- Limits ---------- *)
$CellTimeout = 600;
$NotebookTimeout = 3600;

(* ---------- Paths ---------- *)
srcDir = DirectoryName @ ExpandFileName @ $InputFileName;
repo   = ExpandFileName @ FileNameJoin[{srcDir, ".."}];

nbRoots = Select[
  {
    FileNameJoin[{srcDir, "notebooks_run-all WLS"}],
    FileNameJoin[{repo,    "notebooks"}]
  },
  DirectoryQ
];

resDir = FileNameJoin[{srcDir, "results"}];
figDir = FileNameJoin[{resDir, "figs"}];
tabDir = FileNameJoin[{resDir, "tables"}];
logDir = FileNameJoin[{resDir, "logs"}];
cssDir = FileNameJoin[{resDir, "css"}];

ensureDir[d_] := If[!DirectoryQ[d], CreateDirectory[d, CreateIntermediateDirectories -> True]];
Scan[ensureDir, {resDir, figDir, tabDir, logDir, cssDir}];

(* ---------- Logging ---------- *)
ts[]    := DateString[{"Year","Month","Day","-","Hour24","Minute","Second"}];
stamp[] := DateString[{"Hour24",":","Minute",":","Second"}];

logFile   = FileNameJoin[{logDir, "run-" <> ts[] <> ".log"}];
logStream = Quiet @ Check[OpenWrite[logFile], $Failed];

logLine[s__] := Module[{msg = StringRiffle[ToString /@ {s}, " "], t = stamp[]},
  Print[t <> " " <> msg];
  If[Head[logStream] === OutputStream, WriteString[logStream, t <> " " <> msg <> "\n"]; Flush[logStream]];
];

$SavedMessages = $Messages;
If[Head[logStream] === OutputStream, $Messages = {logStream, Sequence @@ $Messages}];

logLine["repo=", repo];
logLine["srcDir=", srcDir];
logLine["roots=", StringRiffle[nbRoots, " | "]];
logLine["figDir=", figDir];
logLine["tabDir=", tabDir];
logLine["cssDir=", cssDir];
logLine["logDir=", logDir];

Quiet @ (LaunchKernels[]; ParallelEvaluate[SeedRandom[123456 + $KernelID]];);

(* ---------- Discover notebooks ---------- *)
nbAll = Flatten@(FileNames["*.nb", #] & /@ nbRoots);
nbMap = Association[];
Do[With[{nm = FileNameTake[p]}, If[!KeyExistsQ[nbMap, nm], nbMap[nm] = p]], {p, nbAll}];

orderKey[nm_] := {If[StringMatchQ[nm, "wavefunctionplot.nb"], -1, 0],
                  If[StringContainsQ[nm, "Exports", IgnoreCase->True], 1, 0], nm};
nbNames = SortBy[Keys[nbMap], orderKey];

(* Skip stray berry_phase.nb and *_Exports.nb by default *)
nbNames = DeleteCases[nbNames, "berry_phase.nb"];
nbNames = DeleteCases[nbNames, _?(StringMatchQ[#, ___ ~~ "_Exports.nb"]&)];

(* Allow CLI subset selection (overrides the skipping) *)
If[ListQ[$ScriptCommandLine] && Length[$ScriptCommandLine] >= 2,
  With[{wanted = Rest @ $ScriptCommandLine}, nbNames = Select[Keys@nbMap, MemberQ[wanted, #] &]]
];

If[nbNames === {}, logLine["NO NOTEBOOKS FOUND in roots: ", StringRiffle[nbRoots, " | "]];
  If[Head[logStream]===OutputStream, Close[logStream]]; $Messages=$SavedMessages; End[]; Exit[1]];
nbPairs = {#, nbMap[#]}& /@ nbNames;
logLine["NOTEBOOKS: ", StringRiffle[First /@ nbPairs, ", "]];

(* ---------- Helpers ---------- *)
globalSym[name_String] := If[NameQ["Global`" <> name], Symbol["Global`" <> name], $Failed];
globalVal[name_String] := Module[{s = globalSym[name]}, If[s === $Failed, $Failed, Quiet@Check[Evaluate[s], $Failed]]];
hasDownQ[name_String] := Module[{s = globalSym[name]}, MatchQ[s, _Symbol] && DownValues[s] =!= {}];

ensureEnvPre[nbPath_String] := Module[{nbDir = DirectoryName[nbPath]},
  SetDirectory[nbDir];
  Quiet@Check[Unprotect[SystemNotebookDirectory], Null];
  SystemNotebookDirectory[] := nbDir <> If[StringEndsQ[nbDir, "/"|"\\"], "", $PathnameSeparator];
  Protect[SystemNotebookDirectory];
  With[{b = Symbol["Global`$BatchMode"], f = Symbol["Global`figs"], t = Symbol["Global`tabs"], c = Symbol["Global`css"]},
    Quiet@Check[b = True, Null]; Quiet@Check[f = figDir, Null]; Quiet@Check[t = tabDir, Null]; Quiet@Check[c = cssDir, Null]];
  $Path = DeleteDuplicates@Join[$Path, {repo, srcDir, nbDir}];
  Quiet@Check[Needs["PlotLegends`"], Null];
  ensureDir /@ {figDir, tabDir, cssDir};
  logLine["SET-GLOBAL(pre): $BatchMode-> ", ToString@TrueQ@globalVal["$BatchMode"],
          " figs-> ", ToString@globalVal["figs"], " tabs-> ", ToString@globalVal["tabs"],
          " css-> ", ToString@globalVal["css"]];
];

$DangerRegex = RegularExpression[
  "\\b(Manipulate|DynamicModule|CreateDialog|ChoiceDialog|DialogInput|Input(?!Form)|Button|EventHandler|NotebookLocate|CreatePalette|FrontEndTokenExecute)\\["
];

unsafeCellStringQ[str_String] := StringMatchQ[str, ___ ~~ $DangerRegex ~~ ___];
isCodeBoxCellQ[cell_Cell] := MatchQ[cell, Cell[BoxData[_], _String, ___]];
nonCodeStyles = {"Text","Title","Subtitle","Subsubtitle","Section","Subsection","Subsubsection",
  "Item","ItemNumbered","DisplayFormula","DisplayFormulaNumbered","Output"};

safeCellQ[cell_Cell] := Module[{style, boxes, s},
  style = Quiet@Check[cell[[2]], ""];
  If[MemberQ[nonCodeStyles, style], Return[False]];
  If[!isCodeBoxCellQ[cell], Return[False]];
  boxes = Cases[cell, BoxData[b_] :> b, Infinity];
  s = ToString[boxes, InputForm];
  Not@unsafeCellStringQ[s]
];

isInitCellQ[cell_Cell] := MatchQ[cell, Cell[__, ___, InitializationCell->True, ___]];
splitCells[nb_Notebook] := Module[{cells, init, safe, rest},
  cells = Cases[nb, c_Cell :> c, Infinity];
  init  = Select[cells, isInitCellQ];
  safe  = Select[cells, safeCellQ];
  rest  = Select[DeleteDuplicates@Join[init, safe], Not@*isInitCellQ];
  {Length[cells], Length[init], Length[safe], init, rest}
];

exprSummary[box_] := Module[{s = Quiet@Check[ToString[box, InputForm, 70], ""]},
  Which[StringContainsQ[s,"NDSolve["], "NDSolve",
        StringContainsQ[s,"Export["],  "Export",
        StringContainsQ[s,"Plot["],    "Plot",
        True, StringTake[s, UpTo[60]] ]
];

makeExpr[box_] := Module[{try}, try[f_] := Quiet@Check[MakeExpression[box, f], $Failed];
  With[{e1 = try[StandardForm]}, If[e1 =!= $Failed, e1, try[TraditionalForm]]]
];

snapshot[dir_] := If[DirectoryQ[dir], FileNameDrop[#, FileNameDepth[dir]-1]& /@ FileNames["*", dir, Infinity], {}];

(* ---------- Graphics, emptiness test & CSS ---------- *)
graphicsQ[expr_] := MatchQ[expr, _Graphics | _Graphics3D | _GeoGraphics | _Graph];

hasContentQ[g_] := Module[{cnt},
  cnt = Count[g, _Line|_Point|_Polygon|_BezierCurve|_BSplineCurve|_Disk|_Circle|_Rectangle|
                 _GraphicsComplex|_Raster|_Image|_Inset|_SurfaceGraphics, Infinity];
  cnt > 0
];

collectGraphics[expr_] := DeleteDuplicates @ Cases[expr, g_ /; graphicsQ[g] :> g, Infinity];
hashG[g_] := Quiet @ Check[Hash @ Compress[g], RandomInteger[10^9]];

toCSSRGB[col_] := Module[{rgb = Quiet @ Check[List @@ ColorConvert[col, "RGB"], Missing["No"]]},
  If[ListQ[rgb] && Length[rgb] >= 3,
    With[{r=Round[255 Clamp[rgb[[1]],0,1]], g=Round[255 Clamp[rgb[[2]],0,1]], b=Round[255 Clamp[rgb[[3]],0,1]]},
      StringTemplate["rgb(`1`,`2`,`3`)"][r,g,b]], "rgb(0,0,0)"]
];
extractColors[gg_List] := DeleteDuplicates @ (toCSSRGB /@ Cases[gg, _RGBColor | _Hue | _CMYKColor | _GrayLevel, Infinity]);

emitCSS[nbBase_, graphics_List] := Module[{out = FileNameJoin[{cssDir, nbBase<>".css"}], cssFromUser, palette, lines},
  cssFromUser = SelectFirst[{globalVal["cssRules"], globalVal["$CSS"]}, StringQ, None];
  If[StringQ[cssFromUser], Quiet @ Check[Export[out, cssFromUser, "Text"], Null]; Return[out]];
  palette = Take[extractColors[graphics], UpTo[12]];
  lines = Join[
    {"/* Auto-generated CSS for "<>nbBase<>" */", ":root {"},
    MapIndexed[With[{i=First[#2]}, "  --"<>nbBase<> "-c"<>ToString[i]<>": "<>#1<>";"]&, palette],
    {"}","","/* Generic figure styling */",
     ".plot-img { max-width: 100%; height: auto; display: block; }",
     ".plot-frame { border: 1px solid #ddd; padding: 6px; border-radius: 4px; background: #fff; }",
     ".legend { font: 12px/1.35 -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Arial, sans-serif; }",
     ".axis-label { font: 14px/1.4 Georgia, 'Times New Roman', serif; }"}
  ];
  Quiet @ Check[Export[out, StringRiffle[lines, "\n"], "Text"], Null]; out
];

(* -------- GLOBAL de-dup across the whole run -------- *)
$GHashesGlobal = <||>;  (* keys: hash -> path of first export *)

exportGraphicsList[nbBase_, cellIndex_Integer, glist_, gHashesNotebook_Association] := Module[
  {lst = Select[Flatten @ {glist}, graphicsQ], paths = {}, k = 0, path, imgSize = 1200, dpi = 192, h},
  Do[
    If[!hasContentQ[g], logLine["EMPTY-PLOT-SKIP:", nbBase, " cell=", cellIndex]; Continue[]];
    h = hashG[g];
    If[KeyExistsQ[gHashesNotebook, h] || KeyExistsQ[$GHashesGlobal, h],
      logLine["DEDUPE: skip identical graphic (hash=", h, ") in ", nbBase, " cell=", cellIndex]; Continue[]];
    k++;
    path = FileNameJoin[{figDir, nbBase <> "_c" <> IntegerString[cellIndex,10,3] <> "_g" <> IntegerString[k,10,2] <> ".png"}];
    Quiet @ Check[Export[path, g, "PNG", ImageResolution -> dpi, ImageSize -> imgSize], Null];
    gHashesNotebook[h] = path; $GHashesGlobal[h] = path; AppendTo[paths, path];
    ,
    {g, lst}
  ];
  paths
];

(* --------- BOX → GRAPHICS exporter ---------- *)
boxesToGraphics[b_] := Module[{exprH, expr},
  exprH = Quiet @ Check[ToExpression[BoxData @ b, StandardForm, HoldComplete], $Failed];
  If[exprH === $Failed, Return[{}]];
  expr  = Quiet @ Check[ReleaseHold[exprH], $Failed];
  If[expr === $Failed, Return[{}]];
  collectGraphics[expr]
];

exportFromBoxes[nb_Notebook, nbBase_, gHashesNotebook_Association] := Module[
  {boxes, total = 0, allPaths = {}, paths},
  boxes = Cases[nb, Cell[BoxData[b_], ___] :> b, Infinity];
  Do[
    paths = exportGraphicsList[nbBase, i, boxesToGraphics[boxes[[i]]], gHashesNotebook];
    If[ListQ[paths] && paths =!= {}, total += Length[paths]; allPaths = Join[allPaths, paths]],
    {i, Length[boxes]}
  ];
  If[total > 0, logLine["BOX-SCRAPE: exported ", total, " figures from GraphicsBox in ", nbBase]];
  allPaths
];

(* ---------- Fallback solvers ---------- *)
rulesQ[r_] := ListQ[r] && VectorQ[r, MatchQ[#, _Rule | _RuleDelayed]&];

defaultModelSolve[] := Module[{w0=2 Pi, wm=Pi/2, theta0=Pi/3, th, phi, Om, H, a, b},
  th[t_] := theta0; phi[t_] := Mod[wm t, 2 Pi]; Om[t_] := Sin[th[t]];
  H[t_] := {{w0, Om[t] Exp[-I phi[t]]}, {Om[t] Exp[I phi[t]], -w0}};
  Quiet @ Check[
    NDSolveValue[
      {I a'[t]==(H[t].{a[t],b[t]})[[1]], I b'[t]==(H[t].{a[t],b[t]})[[2]],
       a[0]==1/Sqrt[2], b[0]==1/Sqrt[2]}, {a,b}, {t,0,10}],
    $Failed]
];

wavefunctionFallback[] := Module[
  {outPdf=FileNameJoin[{figDir, "wavefunction_ReImPlot.pdf"}],
   outPng=FileNameJoin[{figDir, "wavefunction_ReImPlot.png"}],
   outCsv=FileNameJoin[{tabDir, "wavefunction_ReIm_timeseries.csv"}],
   ok=False, p=$Failed, ts, aFun=$Failed, bFun=$Failed,
   plt=globalVal["plt"], sol=globalVal["sol"], alphaS=globalSym["alpha"], betaS=globalSym["beta"],
   Hdef=hasDownQ["H"], hVal},
  If[FileExistsQ[outPdf]||FileExistsQ[outPng]||FileExistsQ[outCsv], Return[True]];
  If[MatchQ[plt,_Graphics|_Graphics3D], p=plt];
  If[!MatchQ[p,_Graphics|_Graphics3D] && rulesQ[sol] && alphaS=!=$Failed && betaS=!=$Failed,
    p = Plot[Evaluate@{Re[alphaS[t]/.sol],Im[alphaS[t]/.sol],Re[betaS[t]/.sol],Im[betaS[t]/.sol]},
             {t,0,10}, PlotRange->All, Frame->True, FrameLabel->(Style[#,14]&/@{"t (time)","Amplitude"}),
             PlotStyle->{Directive[.90,.20,.20,Thick],Directive[.90,.20,.20,Dashed],Directive[.12,.47,.71,Thick],Directive[.12,.47,.71,Dashed]},
             ImageSize->600, PlotTheme->None, PlotLegends->None];
    ts = Table[{tt, Re[alphaS[tt]/.sol], Im[alphaS[tt]/.sol], Re[betaS[tt]/.sol], Im[betaS[tt]/.sol]}, {tt,0,10,0.01}];
    Quiet@Check[Export[outCsv, Prepend[ts, {"t","ReAlpha","ImAlpha","ReBeta","ImBeta"}], "CSV"], Null];
  ];
  If[!MatchQ[p,_Graphics|_Graphics3D] && Hdef,
    hVal = With[{hv = globalVal["h"]}, If[NumericQ[hv], N@hv, 1.0]];
    Module[{a,b,Hf}, Quiet@Check[
      Hf[t_] := Evaluate[Symbol["Global`H"][t]];
      {aFun,bFun} = NDSolveValue[
        {I hVal a'[t]==(Hf[t].{a[t],b[t]})[[1]], I hVal b'[t]==(Hf[t].{a[t],b[t]})[[2]],
         a[0]==1/Sqrt[2], b[0]==1/Sqrt[2]}, {a,b}, {t,0,10}], {aFun,bFun}={$Failed,$Failed}]];
    If[aFun=!=$Failed && bFun=!=$Failed,
      p = Plot[Evaluate@{Re[aFun[t]],Im[aFun[t]],Re[bFun[t]],Im[bFun[t]]},{t,0,10},
               PlotRange->All, Frame->True, FrameLabel->(Style[#,14]&/@{"t (time)","Amplitude"}),
               PlotStyle->{Directive[.90,.20,.20,Thick],Directive[.90,.20,.20,Dashed],Directive[.12,.47,.71,Thick],Directive[.12,.47,.71,Dashed]},
               ImageSize->600, PlotTheme->None, PlotLegends->None];
      ts = Table[{tt, Re[aFun[tt]], Im[aFun[tt]], Re[bFun[tt]], Im[bFun[tt]]}, {tt,0,10,0.01}];
      Quiet@Check[Export[outCsv, Prepend[ts, {"t","ReAlpha","ImAlpha","ReBeta","ImBeta"}], "CSV"], Null];
      logLine["FALLBACK(wavefunctionplot): recomputed via Global`H[t]."];
    ];
  ];
  If[!MatchQ[p,_Graphics|_Graphics3D], Module[{res=defaultModelSolve[]},
    If[res=!=$Failed, {aFun,bFun}=res;
      p = Plot[Evaluate@{Re[aFun[t]],Im[aFun[t]],Re[bFun[t]],Im[bFun[t]]},{t,0,10},
               PlotRange->All, Frame->True, FrameLabel->(Style[#,14]&/@{"t (time)","Amplitude"}),
               PlotStyle->{Directive[.90,.20,.20,Thick],Directive[.90,.20,.20,Dashed],Directive[.12,.47,.71,Thick],Directive[.12,.47,.71,Dashed]},
               ImageSize->600, PlotTheme->None, PlotLegends->None];
      ts = Table[{tt, Re[aFun[tt]], Im[aFun[tt]], Re[bFun[tt]], Im[bFun[tt]]}, {tt,0,10,0.01}];
      Quiet@Check[Export[outCsv, Prepend[ts, {"t","ReAlpha","ImAlpha","ReBeta","ImBeta"}], "CSV"], Null];
      logLine["FALLBACK(wavefunctionplot): used internal default model."];
    ];
  ]];
  If[MatchQ[p,_Graphics|_Graphics3D],
    Quiet@Check[Export[outPdf, p, "PDF"], Null];
    Quiet@Check[Export[outPng, p, "PNG", ImageResolution->192, ImageSize->1200], Null];
    logLine["FALLBACK(wavefunctionplot): wrote ", outPdf, "  ,  ", outPng];
    True, False]
];

probabilityFallback[] := Module[
  {outPdf=FileNameJoin[{figDir,"probability_plot.pdf"}],
   outPng=FileNameJoin[{figDir,"probability_plot.png"}],
   outCsv=FileNameJoin[{tabDir,"probability_timeseries.csv"}],
   aFun=$Failed, bFun=$Failed, ts, p},
  If[FileExistsQ[outPdf] || FileExistsQ[outPng], Return[True]];
  Module[{sol=globalVal["sol"], aS=globalSym["alpha"], bS=globalSym["beta"]},
    If[rulesQ[sol] && aS=!=$Failed && bS=!=$Failed,
      aFun = Function[{t}, aS[t]/.sol]; bFun = Function[{t}, bS[t]/.sol],
      With[{res=defaultModelSolve[]}, If[res=!=$Failed, {aFun,bFun}=res, Return[False]]]
    ];
  ];
  p = Plot[Evaluate@{Abs[aFun[t]]^2, Abs[bFun[t]]^2}, {t,0,10},
           PlotRange->{0,1}, Frame->True, FrameLabel->(Style[#,14]&/@{"t (time)","Probability"}),
           PlotTheme->None, ImageSize->600];
  ts = Table[{tt, Abs[aFun[tt]]^2, Abs[bFun[tt]]^2}, {tt,0,10,0.01}];
  Quiet@Check[Export[outCsv, Prepend[ts, {"t","Palpha","Pbeta"}], "CSV"], Null];
  Quiet@Check[Export[outPdf, p, "PDF"], Null];
  Quiet@Check[Export[outPng, p, "PNG", ImageResolution->192, ImageSize->1200], Null];
  logLine["FALLBACK(probability_plot): wrote ", outPng, " & ", outPdf];
  True
];

(* ---------- Evaluate a notebook ---------- *)
evaluateNotebook[path_String, nbName_String] := Module[
  {nbExpr, total, nInit, nSafe, initCells, restCells, res,
   preFigs, preTabs, postFigs, postTabs, newFigs, newTabs,
   nbBase = StringReplace[FileBaseName[nbName], ".nb" -> ""],
   gHashesLocal = <||>, collectedGraphics = {}, exportedAll = {}, cssOut = None,
   boxPaths = {}, hadFallback = False},

  logLine["IMPORT:", nbName, " @ ", path];
  nbExpr = Quiet @ Check[Import[path, "NB"], $Failed];
  If[nbExpr === $Failed || !MatchQ[nbExpr, _Notebook], logLine["IMPORT-FAIL:", nbName]; Return[$Failed]];

  {total, nInit, nSafe, initCells, restCells} = splitCells[nbExpr];
  logLine["FILTER:", nbName, " totalCells=", total, " init=", nInit, " safe=", nSafe];
  If[initCells === {} && restCells === {}, logLine["NO-CELLS:", nbName, "(nothing to run)"]; Return[Null]];

  ensureEnvPre[path];

  preFigs = snapshot[figDir]; preTabs = snapshot[tabDir];

  evalBoxes[cells_List, label_String] := Module[
    {i=0, n=Length[cells], t0, box, val, sows, glist, printed, paths, preSyms, postSyms, newSyms},
    Do[
      i++; t0 = AbsoluteTime[];
      box = Quiet@Check[First @ Cases[cell, BoxData[b_] :> b, 1], $Failed];
      If[box === $Failed, logLine["CELL-SKIP(no BoxData):", label, i, "/", n]; Continue[]];
      logLine["CELL-BEGIN:", label, i, "/", n, exprSummary[box]];
      preSyms = Names["Global`*"];

      {val, sows} = TimeConstrained[
        Reap[
          Block[{$Context="Global`", $ContextPath={"System`","Global`"},
                $PrePrint=(If[graphicsQ[#], Sow[#,"G"]]; #)&},
            Module[{e = makeExpr[box]}, If[e === $Failed, $Failed, ReleaseHold[e]]]
          ],
          "G"
        ],
        $CellTimeout,
        {$Aborted, {}}
      ];

      If[val === $Aborted, logLine["CELL-FAIL:", label, i, "/", n, " dt=", NumberForm[AbsoluteTime[]-t0,{Infinity,1}], " s"]; Return[$Failed],
         logLine["CELL-DONE:", label, i, "/", n, " dt=", NumberForm[AbsoluteTime[]-t0,{Infinity,1}], " s"]];

      glist   = collectGraphics[val];
      printed = Flatten @ Cases[sows, {"G", lst_List} :> lst, Infinity];

      postSyms = Names["Global`*"]; newSyms = Complement[postSyms, preSyms];
      If[newSyms =!= {},
        glist = Join[glist, Flatten @ Table[
          With[{vv = Quiet @ Check[Evaluate @ Symbol[s], $Failed]},
            If[vv === $Failed, {}, collectGraphics[vv]]], {s, newSyms}]];
      ];

      glist = DeleteDuplicatesBy[glist, hashG];

      If[glist =!= {},
        paths = exportGraphicsList[nbBase, i, glist, gHashesLocal];
        If[ListQ[paths] && paths =!= {},
          exportedAll = Join[exportedAll, paths];
          collectedGraphics = Join[collectedGraphics, glist];
          logLine["EXPORT-PNG:", nbName, " cell=", i, " count=", Length[paths], " -> ",
                  StringRiffle[FileNameTake /@ paths, ", "]]
        ];
      ];
      ,
      {cell, cells}
    ];
    Null
  ];

  If[initCells =!= {}, logLine["EVAL-INIT:", nbName];
    res = TimeConstrained[ evalBoxes[initCells, nbName <> "::init"], $NotebookTimeout, $Aborted ];
    If[res === $Aborted || res === $Failed, logLine["EVAL-FAIL(init):", nbName]; Return[$Failed]];
    logLine["EVAL-DONE(init):", nbName];
  ];

  With[{b = Symbol["Global`$BatchMode"], f = Symbol["Global`figs"], t = Symbol["Global`tabs"], c = Symbol["Global`css"]},
    Quiet@Check[b = True, Null]; Quiet@Check[f = figDir, Null]; Quiet@Check[t = tabDir, Null]; Quiet@Check[c = cssDir, Null]];
  ensureDir /@ {figDir, tabDir, cssDir};

  logLine["REINFORCE: $BatchMode-> ", ToString@TrueQ@globalVal["$BatchMode"],
          " figs-> ", ToString@globalVal["figs"], " tabs-> ", ToString@globalVal["tabs"], " css-> ", ToString@globalVal["css"]];

  If[restCells =!= {}, logLine["EVAL:", nbName, " ($BatchMode=", ToString@TrueQ@globalVal["$BatchMode"], ")"];
    res = TimeConstrained[ evalBoxes[restCells, nbName], $NotebookTimeout, $Aborted ];
    logLine[If[res === $Aborted || $Failed === res, "EVAL-FAIL:", "EVAL-DONE:"], nbName];
  ];

  (* probability_plot fallback *)
  If[StringContainsQ[nbName, "probability_plot", IgnoreCase->True] &&
     (exportedAll === {} || AllTrue[collectedGraphics, Not@*hasContentQ]),
    hadFallback = probabilityFallback[];
  ];

  (* wavefunction fallback *)
  If[exportedAll === {} && nbName === "wavefunctionplot.nb", hadFallback = wavefunctionFallback[]];

  (* BOX-SCRAPE fallback ONLY if nothing exported and no fallback *)
  If[exportedAll === {} && !TrueQ[hadFallback],
    boxPaths = exportFromBoxes[nbExpr, nbBase, gHashesLocal];
    If[ListQ[boxPaths] && boxPaths =!= {}, exportedAll = Join[exportedAll, boxPaths]];
  ];

  If[(collectedGraphics =!= {}) || StringQ @ globalVal["cssRules"] || StringQ @ globalVal["$CSS"],
    cssOut = emitCSS[nbBase, collectedGraphics]; If[StringQ[cssOut], logLine["CSS-WRITE:", nbName, "  ->  ", cssOut]];
  ];

  postFigs = snapshot[figDir]; postTabs = snapshot[tabDir];
  newFigs = Complement[postFigs, preFigs]; newTabs = Complement[postTabs, preTabs];

  If[newFigs === {} && newTabs === {}, logLine["POST-CHECK: no new exports detected."],
    If[newFigs =!= {}, logLine["POST-CHECK: figs (+): ", StringRiffle[newFigs, ", "]]];
    If[newTabs =!= {}, logLine["POST-CHECK: tables (+): ", StringRiffle[newTabs, ", "]]];
  ];
  Null
];

seedFromName[nb_String] := Module[{codes = ToCharacterCode[nb, "UTF-8"], h = 2166136261},
  Do[h = Mod[BitXor[h, c]*16777619, 2^32], {c, codes}];
  BitAnd[BitXor[h, 123456], 2^31 - 1]
];

runOne[{nbName_String, path_String}] := Module[{seed, t0, res},
  If[!FileExistsQ[path], logLine["SKIP (not found):", nbName, " @ ", path]; Return[$Failed]];
  seed = seedFromName[nbName]; logLine["BEGIN:", nbName, "(seed= ", IntegerString[seed], " )"];
  t0 = AbsoluteTime[]; res = BlockRandom[ evaluateNotebook[path, nbName], RandomSeeding -> seed ];
  logLine[If[res === $Aborted || res === $Failed, "ERROR:", "DONE:"], nbName, " ( ", NumberForm[AbsoluteTime[]-t0, {Infinity,1}], " s)"];
  res
];

results = runOne /@ nbPairs;

If[Head[logStream]===OutputStream, Close[logStream]];
$Messages = $SavedMessages;

End[]; (* RunnerPrivate` *)

If[MemberQ[results, $Failed | $Aborted],
  Print["\nSome notebooks failed. See log: " <> ToString[RunnerPrivate`logFile]]; Exit[1],
  Print["\nAll notebooks completed. Log: " <> ToString[RunnerPrivate`logFile]]; Exit[0]
];
