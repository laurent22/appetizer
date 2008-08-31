unit SystemUtils;

interface

uses Windows;

function OpenSaveFileDialog(ParentHandle: THandle; const DefExt, Filter,
InitialDir, Title: string; var FileName: string; IsOpenDialog: Boolean):
Boolean;

implementation
uses ShlObj, SysUtils;

type
  POpenFilenameA = ^TOpenFilenameA;
  POpenFilename = POpenFilenameA;
  tagOFNA = packed record
    lStructSize: DWORD;
    hWndOwner: HWND;
    hInstance: HINST;
    lpstrFilter: PAnsiChar;
    lpstrCustomFilter: PAnsiChar;
    nMaxCustFilter: DWORD;
    nFilterIndex: DWORD;
    lpstrFile: PAnsiChar;
    nMaxFile: DWORD;
    lpstrFileTitle: PAnsiChar;
    nMaxFileTitle: DWORD;
    lpstrInitialDir: PAnsiChar;
    lpstrTitle: PAnsiChar;
    Flags: DWORD;
    nFileOffset: Word;
    nFileExtension: Word;
    lpstrDefExt: PAnsiChar;
    lCustData: LPARAM;
    lpfnHook: function(Wnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): UINT stdcall;
    lpTemplateName: PAnsiChar;
  end;
  TOpenFilenameA = tagOFNA;
  TOpenFilename = TOpenFilenameA;

function GetOpenFileName(var OpenFile: TOpenFilename): Bool; stdcall; external 'comdlg32.dll'  name 'GetOpenFileNameA';
function GetSaveFileName(var OpenFile: TOpenFilename): Bool; stdcall; external 'comdlg32.dll'  name 'GetSaveFileNameA';

const
  OFN_DONTADDTORECENT = $02000000;
  OFN_FILEMUSTEXIST = $00001000;
  OFN_HIDEREADONLY = $00000004;
  OFN_PATHMUSTEXIST = $00000800;

function CharReplace(const Source: string; oldChar, newChar: Char): string;
var
  i: Integer;
begin
  Result := Source;
  for i := 1 to Length(Result) do
    if Result[i] = oldChar then
      Result[i] := newChar
end;


// http://www.scalabium.com/faq/dct0156.htm
function OpenSaveFileDialog(ParentHandle: THandle; const DefExt, Filter, InitialDir, Title: string; var FileName: string; IsOpenDialog: Boolean): Boolean;
var
  ofn: TOpenFileName;
  szFile: array[0..MAX_PATH] of Char;
begin
  Result := False;
  FillChar(ofn, SizeOf(TOpenFileName), 0);
  with ofn do
  begin
    lStructSize := SizeOf(TOpenFileName);
    hwndOwner := ParentHandle;
    lpstrFile := szFile;
    nMaxFile := SizeOf(szFile);
    if (Title <> '') then
      lpstrTitle := PChar(Title);
    if (InitialDir <> '') then
      lpstrInitialDir := PChar(InitialDir);
    StrPCopy(lpstrFile, FileName);
    lpstrFilter := PChar(CharReplace(Filter, '|', #0)+#0#0);
    if DefExt <> '' then
      lpstrDefExt := PChar(DefExt);
  end;
  if IsOpenDialog then
  begin
    if GetOpenFileName(ofn) then
    begin
      Result := True;
      FileName := StrPas(szFile);
    end;
  end
  else
  begin
    if GetSaveFileName(ofn) then
    begin
      Result := True;
      FileName := StrPas(szFile);
    end;
  end
end;

end.
