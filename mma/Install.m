If[$VersionNumber == 8,
(*To use FetchURL in MMA8 we need to load URLTools first *)
  Needs["Utilities`URLTools`"];
];

$GithubRelease = ""

Options[DoInstall] = {
  InstallTo -> FileNameJoin[{$UserBaseDirectory, "Applications", "oneloop"}]
};

DoInstall[OptionsPattern[]] := Module[{zip},
  toPath = OptionValue[InstallTo];

  If[$VersionNumber == 8,
    $GetUrl[x_] := Utilities`URLTools`FetchURL[x],
    $GetUrl[x_] := URLSave[x, CreateTemporary[]]
  ];

  zip = $GetUrl[$GithubRelease];
  ExtractArchive[zip, toPath];

  Quiet@DeleteFile[zip];
  Quiet@DeleteDirectory[unzipDir, DeleteContents -> True];
];