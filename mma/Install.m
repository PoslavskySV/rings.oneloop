If[$VersionNumber == 8,
(*To use FetchURL in MMA8 we need to load URLTools first *)
  Needs["Utilities`URLTools`"];
];

$OneloopVersion = "v1.0-beta";
$GithubRelease = "https://github.com/PoslavskySV/rings.oneloop/releases/download/" <> $OneloopVersion <> "/rings.oneloop-" <> $OneloopVersion <> ".zip";

Options[DoInstall] = {
  InstallTo -> FileNameJoin[{$UserBaseDirectory, "Applications", "oneloop"}]
};

DoInstall[OptionsPattern[]] := Module[{zip, unzipDir},
  If[$VersionNumber == 8,
    $GetUrl[x_] := Utilities`URLTools`FetchURL[x],
    $GetUrl[x_] := URLSave[x, CreateTemporary[]]
  ];

  zip = $GetUrl[$GithubRelease];
  unzipDir = zip <> ".dir";
  CreateDirectory[unzipDir];
  ExtractArchive[unzipDir, OptionValue[InstallTo]];
  CopyDirectory[FileNameJoin[{unzipDir, "rings.oneloop-" <> $OneloopVersion}], OptionValue[InstallTo]];

  Quiet@DeleteFile[zip];
  Quiet@DeleteDirectory[unzipDir, DeleteContents -> True];
];