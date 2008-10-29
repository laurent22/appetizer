library DelphiTools;


uses
  SysUtils,
  Classes, Dialogs, VersionInformation;


procedure DllMessage; stdcall; export;
begin
  ShowMessage('Hello world from a Delphi DLL') ;
end;


function GetVersionInfo_FileDescription(const filePath: PWideChar): PWideChar; export;
begin
  versionInfo := TVersionInfo.CreateFile(filePath);
  result := StringToOleStr(versionInfo.FileDescription);
end;


exports DllMessage;
exports GetVersionInfo_FileDescription;


begin
end.
