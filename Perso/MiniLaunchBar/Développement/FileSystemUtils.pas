unit FileSystemUtils;

interface

uses ShellAPI, Graphics, Windows, SysUtils, Dialogs, Classes, IniFiles;

type

	TDirectoryContents = Array of String[255];

//  TFileInfo = record
//    Icon : hIcon;
//    Image : Integer;
//    DisplayName : String;
//    TypeName : String;
//    Size : Integer;
//    SizeDescription : String;
//    DateTime : TDateTime;
//    AttrArchive : Boolean;
//    AttrReadOnly : Boolean;
//    AttrSystem : Boolean;
//    AttrHidden : Boolean;
//    AttrVolume : Boolean;
//    AttrDirectory : Boolean;
//  end;
//
//
//function scGetSizeDescription(const IntSize : Int64) : String;
//procedure scGetFileInfo(StrPath : String; var Info : TFileInfo);

function GetExecutableSmallIcon(const filePath: String):TIcon;
function GetExecutableLargeIcon(const filePath: String):TIcon;
function GetApplicationDirectory():String;
function IsDirectory(const filePath: String): Boolean;
function GetDirectoryContents(const filePath: String; const depth: Integer; const fileExtension: String): TStringList ;
function GetFmtFileVersion(const FileName: String = '';
const Fmt: String = '%d.%d.%d.%d'): String;

function GetFolderIcon (const aFilename : String; b32BitIcon : Boolean) : TIcon;


implementation


function GetFolderIcon (const aFileName : String; b32BitIcon : Boolean) : TIcon;
var
  FileInfo: TSHFileInfo;
  filename: String;
  iniFile: TIniFile;
  iconFilePath: String;
  savedDir: String;
  gotIt: Boolean;
begin
  Result := TIcon.Create;

  filename := ExcludeTrailingBackslash(aFilename);

  if FileExists(filename + '\Desktop.ini') then begin
  	iniFile := TIniFile.Create(filename + '\Desktop.ini');
    iconFilePath := iniFile.ReadString('.ShellClassInfo', 'IconFile', 'NONPAS!');
    FreeAndNil(iniFile);

    if iconFilePath <> 'NONPAS!' then begin
      savedDir := GetCurrentDir();
      SetCurrentDir(filename);
      try
      	Result.LoadFromFile(iconFilePath);
        gotIt := true;
      except
      	gotIt := false;
      end;
      SetCurrentDir(savedDir);

      if gotIt then Exit;
    end;
  end;
  

  if b32BitIcon then
    SHGetFileInfo( PChar(aFilename), FILE_ATTRIBUTE_DIRECTORY, FileInfo,
              SizeOf(TSHFileInfo),
              SHGFI_ICON OR SHGFI_LARGEICON OR   SHGFI_USEFILEATTRIBUTES)
  else
    SHGetFileInfo( PChar(aFilename), FILE_ATTRIBUTE_DIRECTORY, FileInfo,
              SizeOf(TSHFileInfo),
              SHGFI_ICON OR SHGFI_SMALLICON OR SHGFI_USEFILEATTRIBUTES);
 
  Result.Handle := FileInfo.hIcon;
end;


//function fnGetFileIcon (const sFileName : String; b32BitIcon : Boolean) : TIcon;
//  var
//    FileInfo: TSHFileInfo;
//  begin
//    Result := TIcon.Create;
// 
//    if b32BitIcon then
//      SHGetFileInfo( PChar(sFileName), $080, FileInfo,
//                SizeOf(TSHFileInfo),
//                SHGFI_ICON OR SHGFI_LARGEICON OR   SHGFI_USEFILEATTRIBUTES)
//    else
//      SHGetFileInfo( PChar(sFileName), $080, FileInfo,
//                SizeOf(TSHFileInfo),
//                SHGFI_ICON OR SHGFI_SMALLICON OR SHGFI_USEFILEATTRIBUTES);
// 
//    Result.Handle := FileInfo.hIcon;
//end;

//// ----------------------------------------------------------------
//// Return string with formatted file size (bytes, Kb, Mb or Gb)
//// ----------------------------------------------------------------
//function scGetSizeDescription(const IntSize : Int64) : String;
//begin
//  if IntSize < 1024 then
//    Result := IntToStr(IntSize)+' bytes'
//  else
//  begin
//    if IntSize < (1024 * 1024) then
//      Result := FormatFloat('####0.##',IntSize / 1024)+' Kb'
//    else
//      if IntSize < (1024 * 1024 * 1024) then
//        Result := FormatFloat('####0.##',IntSize / 1024 / 1024)+' Mb'
//      else
//        Result := FormatFloat('####0.##',IntSize / 1024 / 1024 / 1024)+' Gb';
//  end;
//end;
//
//// ----------------------------------------------------------------
//// Return record with all information about given file
//// How to use icon : ImageFile.Picture.Icon.Handle:=Info.Icon;
//// ----------------------------------------------------------------
//procedure scGetFileInfo(StrPath : String; var Info : TFileInfo);
//var
//  SHFileInfo : TSHFileInfo;
//  SearchRec : TSearchRec;
//begin
//  if Trim(StrPath) = '' then
//    Exit;
//
//  ShGetFileInfo(PChar(StrPath), 0, SHFileInfo, SizeOf (TSHFileInfo),
//    SHGFI_TYPENAME or SHGFI_DISPLAYNAME or SHGFI_SYSICONINDEX or SHGFI_ICON);
//
//  with Info do
//  begin
//    Icon  := SHFileInfo.hIcon;
//    Image := SHFileInfo.iIcon;
//    DisplayName := SHFileInfo.szDisplayName;
//    TypeName := SHFileInfo.szTypeName;
//  end;
//
//  FindFirst(StrPath, 0, SearchRec);
//  with Info do
//  begin
////    try
////      DateTime := FileDateToDateTime(SearchRec.Time);
////    except
////      DateTime := Now();
////    end;
//		DateTime := Now();
//
//    AttrReadOnly := ((SearchRec.Attr and faReadOnly) > 0);
//    AttrSystem := ((SearchRec.Attr and faSysFile) > 0);
//    AttrHidden := ((SearchRec.Attr and faHidden) > 0);
//    AttrArchive := ((SearchRec.Attr and faArchive) > 0);
//    AttrVolume := ((SearchRec.Attr and faVolumeID) > 0);
//    AttrDirectory := ((SearchRec.Attr and faDirectory) > 0);
//
//    Size := SearchRec.Size;
//
//    SizeDescription := scGetSizeDescription(Size);
//  end;
//end;



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
