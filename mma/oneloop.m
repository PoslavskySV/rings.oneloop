BeginPackage["oneloop`"];

$DatabasePath::usage = "Path to the database of calculated integrals";
$OneloopVerbose::usage = "Whether to print logging information";
I2::usage = "I2[n1, n2, di, s12] computes massless 2-point integral in (d + di) dimensions";
I3::usage = "I3[n1, n2, n3, di, s23, s13, s12] computes massless 3-point integral in (d + di) dimensions";
I4::usage = "I4[n1, n2, n3, n4, di, s12, s23, s34, s14, s24, s13] computes massless 4-point integral in (d + di) dimensions";
I5::usage = "I5[n1, n2, n3, n4, n5, di, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35] computes massless 5-point integral in (d + di) dimensions";

Begin["`Private`"];
Needs["JLink`"]

$OneloopVerbose = True;
PrintLog[any_] := If[$OneloopVerbose, Print@any];

$PackageDir = DirectoryName @ $InputFileName;
$DatabasePath = ToFileName[$PackageDir , "integrals.db"];

PrintLog["Default database file " <> $DatabasePath];

$JavaLink = ReinstallJava[ClassPath -> $PackageDir];
$MainClass = LoadJavaClass["cc.redberry.rings.oneloop.Main"];
$GenericOptions = {
  Factorize -> True,
  Characteristic -> 0,
  DatabaseFile -> $DatabasePath,
  Indices -> None,
  Global`s12 -> Global`s12,
  Global`s13 -> Global`s13,
  Global`s14 -> Global`s14,
  Global`s15 -> Global`s15,
  Global`s23 -> Global`s23,
  Global`s24 -> Global`s24,
  Global`s25 -> Global`s25,
  Global`s34 -> Global`s34,
  Global`s35 -> Global`s35,
  Global`s45 -> Global`s45
};


Options[$insertOps] = $GenericOptions;
$insertOps[list_, OptionsPattern[]] := Module[{jOps = list},
  If[OptionValue[Factorize],
    jOps = jOps ~ Join ~ {"--factorize"}];
  If[OptionValue[Characteristic] != 0,
    jOps = jOps ~ Join ~ {"--characteristic", ToString[OptionValue[Characteristic]]}];
  If[OptionValue[DatabaseFile] =!= None,
    jOps = jOps ~ Join ~ {"--database", ToString[OptionValue[DatabaseFile]]}];
  If[OptionValue[Indices] =!= None,
    jOps = jOps ~ Join ~ {"--indices", ToString[OptionValue[Indices]]}];
  Return[jOps];
];

Options[I2] = $GenericOptions;
I2[n1_Integer, n2_Integer, di_Integer, ops : OptionsPattern[]] := Module[{jOps, jObj, stdout, stderr},
  jOps = {"i2",
    "--n1", ToString@n1,
    "--n2", ToString@n2,
    "--di", ToString@di,
    "--s12", ToString@OptionValue[s12]};

  jOps = $insertOps[jOps, ops];
  PrintLog[("Running rings.oneloop with options " <> ToString[jOps])];

  jObj = Main`mainToString[jOps];
  stdout = jObj@U1;
  stderr = jObj@U2;

  PrintLog[stderr];
  Return[ToExpression[StringSplit[stdout, "="][[2]]]];
];

Options[I3] = $GenericOptions;
I3[n1_Integer, n2_Integer, n3_Integer, di_Integer, ops : OptionsPattern[]] := Module[{jOps, jObj, stdout, stderr},
  jOps = {"i3",
    "--n1", ToString@n1,
    "--n2", ToString@n2,
    "--n3", ToString@n3,
    "--di", ToString@di,
    "--s23", ToString@OptionValue[s23],
    "--s13", ToString@OptionValue[s13],
    "--s12", ToString@OptionValue[s12]};

  jOps = $insertOps[jOps, ops];
  PrintLog[("Running rings.oneloop with options " <> ToString[jOps])];

  jObj = Main`mainToString[jOps];
  stdout = jObj@U1;
  stderr = jObj@U2;

  PrintLog[stderr];
  Return[ToExpression[StringSplit[stdout, "="][[2]]]];
];

Options[I4] = $GenericOptions;
I4[n1_Integer, n2_Integer, n3_Integer, n4_Integer, di_Integer, ops : OptionsPattern[]] := Module[{jOps, jObj, stdout, stderr},
  jOps = {"i4",
    "--n1", ToString@n1,
    "--n2", ToString@n2,
    "--n3", ToString@n3,
    "--n4", ToString@n4,
    "--di", ToString@di,
    "--s12", ToString@OptionValue[s12],
    "--s23", ToString@OptionValue[s23],
    "--s34", ToString@OptionValue[s34],
    "--s14", ToString@OptionValue[s14],
    "--s24", ToString@OptionValue[s24],
    "--s13", ToString@OptionValue[s13]};

  jOps = $insertOps[jOps, ops];
  PrintLog[("Running rings.oneloop with options " <> ToString[jOps])];

  jObj = Main`mainToString[jOps];
  stdout = jObj@U1;
  stderr = jObj@U2;

  PrintLog[stderr];
  Return[ToExpression[StringSplit[stdout, "="][[2]]]];
];

Options[I5] = $GenericOptions;
I5[n1_Integer, n2_Integer, n3_Integer, n4_Integer, n5_Integer, di_Integer, ops : OptionsPattern[]] := Module[{jOps, jObj, stdout, stderr},
  jOps = {"i5",
    "--n1", ToString@n1,
    "--n2", ToString@n2,
    "--n3", ToString@n3,
    "--n4", ToString@n4,
    "--n5", ToString@n5,
    "--di", ToString@di,
    "--s12", ToString@OptionValue[s12],
    "--s23", ToString@OptionValue[s23],
    "--s34", ToString@OptionValue[s34],
    "--s45", ToString@OptionValue[s45],
    "--s15", ToString@OptionValue[s15],
    "--s13", ToString@OptionValue[s13],
    "--s14", ToString@OptionValue[s14],
    "--s24", ToString@OptionValue[s24],
    "--s25", ToString@OptionValue[s25],
    "--s35", ToString@OptionValue[s35]};

  jOps = $insertOps[jOps, ops];
  PrintLog[("Running rings.oneloop with options " <> ToString[jOps])];

  jObj = Main`mainToString[jOps];
  stdout = jObj@U1;
  stderr = jObj@U2;

  PrintLog[stderr];
  Return[ToExpression[StringSplit[stdout, "="][[2]]]];
];

End[];
EndPackage[];
