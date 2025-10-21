(* ============================================
   run-all.wls — headless runner for batch notebooks
   - Looks in:  src/notebooks_run-all WLS/
   - Exports go to: src/results/{logs,figs,tables}
   - Evaluates only safe (non-interactive) code cells in kernel
   ============================================ *)

SeedRandom[123456]; $HistoryLength = 0;
Off[$ConfiguredKernels::obso]; Off[$DefaultKernels::obso];

(* ---------- Tunables ---------- *)
$CellTimeout      = 600;     (* per cell, seconds *)
$NotebookTimeout  = 3600;    (* per notebook, seconds *)

(* ---------- Paths (RESULTS under src/) ---------- *)
srcDir = DirectoryName @ ExpandFileName @ $InputFileName;   (* .../src *)
repo   = ExpandFileName @ FileNameJoin[{srcDir, ".."}];     (* repo root *)
nbDir  = FileNameJoin[{srcDir, "notebooks_run-all WLS"}];

resDir = FileNameJoin[{srcDir, "results"}];
figDir = FileNameJoin[{resDir, "figs"}];
tabDir = FileNameJoin[{resDir, "tables"}];
logDir = FileNameJoin[{resDir, "logs"}];

ensureDir[d_] := If[!DirectoryQ[d], CreateDirectory[d, CreateIntermediateDirectories -> True], d];
Scan[ensureDir, {resDir, figDir, tabDir, logDir}];

(* ---------- Logging ---------- *)
ts[]    := DateString[{"Year","Month","Day","-","Hour24","Minute","Second"}];
stamp[] := DateString[{"Hour24",":","Minute",":","Second"}];
logFile   = FileNameJoin[{logDir, "run-"<>ts[]<>".log"}];
logStream = Quiet @ Check[OpenWrite[logFile], $Failed];
logLine[s__] := Module[{msg = StringRiffle[ToString /@ {s}, " "], t = stamp[]},
  Print[t<>" "<>msg];
  If[Head[logStream]===OutputStream, WriteString[logStream, t<>" "<>msg<>"\n"]; Flush[logStream]];
];

logLine["repo=", repo];
logLine["srcDir=", srcDir];
logLine["nbDir=", nbDir];
logLine["figDir=", figDir];
logLine["tabDir=", tabDir];
logLine["logDir=", logDir];

(* ---------- Optional: seed parallel kernels ---------- *)
Quiet[(LaunchKernels[]; ParallelEvaluate[SeedRandom[123456 + $KernelID]];)];

(* ---------- Notebook order: wavefunctionplot first, Exports last ---------- *)
nbAll = FileNameTake /@ Sort @ FileNames["*.nb", nbDir];
orderKey[name_] := {
  If[StringMatchQ[name, "wavefunctionplot.nb"], -1, 0],         (* first *)
  If[StringContainsQ[name, "Exports", IgnoreCase->True], 1, 0], (* last *)
  name                                                          (* tie-break *)
};
nbOrder = SortBy[nbAll, orderKey];
If[nbOrder === {}, logLine["NO NOTEBOOKS FOUND in ", nbDir]; If[Head[logStream]===OutputStream, Close[logStream]]; Exit[1]];
logLine["NOTEBOOKS: ", StringRiffle[nbOrder, ", "]];

(* ---------- Deterministic seed from filename ---------- *)
seedFromName[nb_String] := Module[{codes = ToCharacterCode[nb, "UTF-8"], h = 2166136261},
  Do[h = Mod[BitXor[h, c]*16777619, 2^32], {c, codes}];  (* FNV-1a 32-bit *)
  BitAnd[BitXor[h, 123456], 2^31 - 1]
];

(* ---------- Env for each notebook ---------- *)
ensureEnv[nbPath_String] := Module[{nbDirLocal = DirectoryName[nbPath]},
  SetDirectory[srcDir];
  $Path = DeleteDuplicates@Join[$Path, {repo, srcDir, nbDirLocal}];
  figs = figDir; tabs = tabDir;
  ensureDir /@ {figs, tabs};
  $BatchMode = True;  (* make your export cells run *)
  Quiet@Check[Needs["PlotLegends`"], Null];
  If[!ValueQ[ReImPlot],
    Options[ReImPlot] = Options[Plot];
    ReImPlot[fs_List, {t_, a_, b_}, opts:OptionsPattern[]] := Module[{flat},
      flat = Replace[OptionValue[PlotStyle], s_List :> Flatten[s, 1], 1];
      Plot[Evaluate@Join[Re /@ fs, Im /@ fs], {t, a, b},
        PlotStyle -> flat, Evaluate @ FilterRules[{opts}, Options[Plot]]]
    ];
  ];
];

(* ---------- Safe-cell filtering ---------- *)
unsafeCellStringQ[str_String] := StringContainsQ[
  str, Alternatives[
    "Manipulate[","DynamicModule[","CreateDialog[","ChoiceDialog[",
    "DialogInput[","Input[","Button[","EventHandler[","NotebookLocate[",
    "CreatePalette[","FrontEndTokenExecute["], IgnoreCase->False
];
isCodeBoxCellQ[cell_Cell] := MatchQ[cell, Cell[BoxData[_], _String, ___]];
nonCodeStyles = {"Text","Title","Subtitle","Subsubtitle","Section","Subsection","Subsubsection",
                 "Item","ItemNumbered","DisplayFormula","DisplayFormulaNumbered","Output"};
safeCellQ[cell_Cell] := Module[{style, boxes, s},
  style = Quiet @ Check[cell[[2]], ""];
  If[MemberQ[nonCodeStyles, style], Return[False]];
  If[!isCodeBoxCellQ[cell], Return[False]];
  boxes = Cases[cell, BoxData[b_] :> b, Infinity];
  s = ToString[boxes, InputForm];
  Not @ unsafeCellStringQ[s]
];
isInitCellQ[cell_Cell] := MatchQ[cell, Cell[__, ___, InitializationCell -> True, ___]];
filterCellsForBatch[nb_Notebook] := Module[{cells, init, safe, total},
  cells = Cases[nb, c_Cell :> c, Infinity];
  total = Length[cells]; init = Select[cells, isInitCellQ]; safe = Select[cells, safeCellQ];
  {total, Length[init], Length[safe], Join[init, safe]}
];

(* ---------- Evaluate cells in kernel ---------- *)
exprSummary[box_] := Module[{s = Quiet@Check[ToString[box, InputForm, 70], ""]},
  Which[StringContainsQ[s,"NDSolve["],"NDSolve",
        StringContainsQ[s,"Export["],"Export",
        StringContainsQ[s,"Plot["],"Plot",
        True, StringTake[s,UpTo[60]]]
];
evalCellBoxes[cells_List, nbName_String] := Module[{res = Null, i = 0, n = Length[cells], t0, box, expr},
  Do[
    i++; t0 = AbsoluteTime[];
    box = Quiet @ Check[First @ Cases[c, BoxData[b_] :> b, 1], $Failed];
    If[box === $Failed, logLine["CELL-SKIP(no BoxData):", nbName, i, "/", n]; Continue[]];
    logLine["CELL-BEGIN:", nbName, i, "/", n, exprSummary[box]];
    res = Quiet @ Check[
      TimeConstrained[(expr = MakeExpression[box, StandardForm]; ReleaseHold[expr]), $CellTimeout, $Failed],
      $Failed
    ];
    If[res === $Failed,
      logLine["CELL-FAIL:", nbName, i, "/", n, " dt=", NumberForm[AbsoluteTime[]-t0, {Infinity,1}], "s"]; 
      Return[$Failed],
      logLine["CELL-DONE:", nbName, i, "/", n, " dt=", NumberForm[AbsoluteTime[]-t0, {Infinity,1}], "s"]
    ];
  , {c, cells}];
  res
];

evaluateNotebookSafely[path_String, nbName_String] := Module[
  {nbExpr, counts, total, nInit, nSafe, runCells, res},
  logLine["IMPORT:", nbName];
  nbExpr = Quiet @ Check[Import[path, "NB"], $Failed];
  If[nbExpr === $Failed || !MatchQ[nbExpr, _Notebook], logLine["IMPORT-FAIL:", nbName]; Return[$Failed]];
  counts = filterCellsForBatch[nbExpr];
  {total, nInit, nSafe, runCells} = counts;
  logLine["FILTER:", nbName, " totalCells=", total, " init=", nInit, " safe=", nSafe];
  If[runCells === {}, logLine["NO-CELLS:", nbName, "(nothing to run)"]; Return[Null]];
  ensureEnv[path];
  logLine["EVAL:", nbName];
  res = Quiet @ Check[ TimeConstrained[ evalCellBoxes[runCells, nbName], $NotebookTimeout, $Failed ], $Failed ];
  logLine[If[res===$Failed, "EVAL-FAIL:", "EVAL-DONE:"], nbName];
  res
];

runWithSeed[nb_String] := Module[{path, seed, t0, res},
  path = FileNameJoin[{nbDir, nb}];
  If[!FileExistsQ[path], logLine["SKIP (not found):", nb]; Return[$Failed]];
  seed = seedFromName[nb];
  logLine["BEGIN:", nb, "(seed=", IntegerString[seed], ")"];
  t0 = AbsoluteTime[];
  res = BlockRandom[ evaluateNotebookSafely[path, nb], RandomSeeding -> seed ];
  If[res === $Failed,
    logLine["ERROR:", nb, " (", NumberForm[AbsoluteTime[] - t0, {Infinity,1}], " s)"],
    logLine["DONE:",  nb, " (", NumberForm[AbsoluteTime[] - t0, {Infinity,1}], " s)"]
  ];
  res
];

results = runWithSeed /@ nbOrder;

If[Head[logStream]===OutputStream, Close[logStream]];
If[MemberQ[results, $Failed],
  Print["\nSome notebooks failed. See log: " <> ToString[logFile]]; Exit[1],
  Print["\nAll notebooks completed. Log: " <> ToString[logFile]]; Exit[0]
];
