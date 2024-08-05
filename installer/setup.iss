[Setup]
AppName=cripto-tool
AppVersion=1.0
DefaultDirName={pf}\cripto-tool
DefaultGroupName=cripto-tool
OutputDir=.
OutputBaseFilename=cripto-toolSetup
PrivilegesRequired=admin

[Files]
Source: "cripto-tool.exe"; DestDir: "{app}"; Flags: ignoreversion

[Run]
Filename: "{cmd}"; Parameters: "/C setx PATH ""%PATH%;{app}"""; Flags: runhidden