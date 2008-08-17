unit FileSystemUtils;

interface

uses ShellAPI, Graphics, Windows, SysUtils, Dialogs, Classes;

type

	TDirectoryContents = Array of String[255];


function GetExecutableSmallIcon(const filePath: String):TIcon;
function GetExecutableLargeIcon(const filePath: String):TIcon;
function GetApplicationDirectory():String;
function IsDirectory(const filePath: String): Boolean;
function GetDirectoryContents(const filePath: String; const depth: Integer; const fileExtension: String): TStringList ;
function GetFmtFileVersion(const FileName: String = '';
  const Fmt: String = '%d.%d.%d.%d'): String;


implementation




function IsDirectory(const filePath: String): Boolean;
begin
	result := DirectoryExists(filePath);
end;


function GetDirectoryContents(const filePath: String; const depth: Integer; const fileExtension: String): TStringList ;
var
	rec : TSearchRec;
	foundFilePath: String;
  tempResult: TStringList;
  i: Word;
  itemIsDirectory: Boolean;
begin
	result := TStringList.Create();
  
  if not IsDirectory(filePath) then Exit;

  if findFirst(filePath + '\*.*', faAnyFile, rec) = 0 then begin
    try

      repeat
        if (rec.name = '.') or (rec.name = '..') then continue;

        foundFilePath := filePath + '\' + rec.name;

        itemIsDirectory := IsDirectory(foundFilePath);

        if ((fileExtension <> '*') and (not itemIsDirectory)) then begin
        	if (ExtractFileExt(rec.name) <> '.' + fileExtension) then continue;
        end;

        result.Add(foundFilePath);

        if (itemIsDirectory) and (depth <> 0) then begin
          tempResult := GetDirectoryContents(foundFilePath, depth - 1, fileExtension);
          if tempResult.Count > 0 then begin
            for i := 0 to tempResult.Count - 1 do begin
              result.Add(tempResult[i]);
            end;
          end;
        end;

      until findNext(rec) <> 0;

    finally
      findClose(rec) ;
    end;
  end;
end;






function GetExecutableSmallIcon(const filePath: String):TIcon;
var
  largeIcon : Hicon;
  smallIcon : Hicon;
begin
	ExtractIconEx(PChar(filePath), 0, largeIcon, smallIcon, 1);

  if smallIcon <= 1 then begin
  	result := nil;
  end else begin
  	result := TIcon.Create();
    result.Handle := smallIcon;
  end;
end;



function GetExecutableLargeIcon(const filePath: String):TIcon;
var
  largeIcon : Hicon;
  smallIcon : Hicon;
begin
	ExtractIconEx(PChar(filePath), 0, largeIcon, smallIcon, 1);

  if largeIcon <= 1 then begin
  	result := nil;
  end else begin
  	result := TIcon.Create();
    result.Handle := largeIcon;
  end;
end;



function GetApplicationDirectory():String;
begin
	GetDir(0, result);
end;



/// <summary>
///   This function reads the file resource of "FileName" and returns
///   the version number as formatted text. By Martin Stoeckli:
///   http://www.martinstoeckli.ch/delphi/delphi.html#AppVersion</summary>
/// <example>
///   Sto_GetFmtFileVersion() = '4.13.128.0'
///   Sto_GetFmtFileVersion('', '%.2d-%.2d-%.2d') = '04-13-128'
/// </example>
/// <remarks>If "Fmt" is invalid, the function may raise an
///   EConvertError exception.</remarks>
/// <param name="FileName">Full path to exe or dll. If an empty
///   string is passed, the function uses the filename of the
///   running exe or dll.</param>
/// <param name="Fmt">Format string, you can use at most four integer
///   values.</param>
/// <returns>Formatted version number of file, '' if no version
///   resource found.</returns>
function GetFmtFileVersion(const FileName: String = '';
  const Fmt: String = '%d.%d.%d.%d'): String;
var
  sFileName: String;
  iBufferSize: DWORD;
  iDummy: DWORD;
  pBuffer: Pointer;
  pFileInfo: Pointer;
  iVer: array[1..4] of Word;
begin
  // set default value
  Result := '';
  // get filename of exe/dll if no filename is specified
  sFileName := FileName;
  if (sFileName = '') then
  begin
    // prepare buffer for path and terminating #0
    SetLength(sFileName, MAX_PATH + 1);
    SetLength(sFileName,
      GetModuleFileName(hInstance, PChar(sFileName), MAX_PATH + 1));
  end;
  // get size of version info (0 if no version info exists)
  iBufferSize := GetFileVersionInfoSize(PChar(sFileName), iDummy);
  if (iBufferSize > 0) then
  begin
    GetMem(pBuffer, iBufferSize);
    try
    // get fixed file info (language independent)
    GetFileVersionInfo(PChar(sFileName), 0, iBufferSize, pBuffer);
    VerQueryValue(pBuffer, '\', pFileInfo, iDummy);
    // read version blocks
    iVer[1] := HiWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionMS);
    iVer[2] := LoWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionMS);
    iVer[3] := HiWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionLS);
    iVer[4] := LoWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionLS);
    finally
      FreeMem(pBuffer);
    end;
    // format result string
    Result := Format(Fmt, [iVer[1], iVer[2], iVer[3], iVer[4]]);
  end;
end;




end.
