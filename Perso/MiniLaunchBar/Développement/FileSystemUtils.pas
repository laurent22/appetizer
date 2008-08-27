unit FileSystemUtils;

interface

uses ShellAPI, Graphics, Windows, SysUtils, Dialogs, Classes, IniFiles, Registry;

type

	TDirectoryContents = Array of String[255];

function GetSystemDir(): TFileName;
function GetExecutableSmallIcon(const filePath: String):TIcon;
function GetExecutableLargeIcon(const filePath: String):TIcon;
function GetApplicationDirectory():String;
function GetApplicationDrive():String;
function IsDirectory(const filePath: String): Boolean;
function GetDirectoryContents(const filePath: String; const depth: Integer; const fileExtension: String): TStringList ;
function GetFmtFileVersion(const FileName: String = '';
const Fmt: String = '%d.%d.%d.%d'): String;

function GetDocumentIcon(const filePath: String; const returnSmallIcon: Boolean): TIcon;
function GetFolderItemIcon(const folderItemPath: String; const returnSmallIcon: Boolean): TIcon;

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
    iconFilePath := iniFile.ReadString('.ShellClassInfo', 'IconFile', 'NON!');
    FreeAndNil(iniFile);

    if iconFilePath <> 'NON!' then begin
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



function IsDirectory(const filePath: String): Boolean;
begin
	result := DirectoryExists(filePath);
end;


function GetDirectoryContents(const filePath: String; const depth: Integer; const fileExtension: String): TStringList ;
var
	rec : TSearchRec;
	foundFilePath: String;
  tempResult: TStringList;
  i: Integer;
  itemIsDirectory: Boolean;
  addIt: Boolean;
begin
	result := TStringList.Create();
  
  if not IsDirectory(filePath) then Exit;

  if findFirst(filePath + '\*.*', faAnyFile, rec) = 0 then begin
    try

      repeat
        if (rec.name = '.') or (rec.name = '..') then continue;

        foundFilePath := filePath + '\' + rec.name;

        itemIsDirectory := DirectoryExists(foundFilePath);

        addIt := true;

        if (fileExtension <> '*') then begin
          if (ExtractFileExt(rec.name) <> '.' + fileExtension) then begin
          	addIt := false;
          	if not DirectoryExists(foundFilePath) then continue;
          end;
        end;

        if addIt then result.Add(foundFilePath);

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
  FileInfo: TSHFileInfo;
begin
   ExtractIconEx(PChar(filePath), 0, LargeIcon, SmallIcon, 1);
   if SmallIcon <= 1 then begin
     Result := nil;
   end else begin
   		result := TIcon.Create();
     result.Handle := SmallIcon;
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



function GetDocumentIcon(const filePath: String; const returnSmallIcon: Boolean): TIcon;
var buffer: array[0..2048] of char;
	hIcon: Windows.HICON;
  shfi: TShFileInfo;
begin
  if returnSmallIcon then begin
    try
      FillChar(shfi, SizeOf(TShFileInfo), 0);
      ShGetFileInfo(PChar(filePath), 0, shfi, SizeOf(TShFileInfo),
                      SHGFI_ICON or SHGFI_SMALLICON);
      hIcon := shfi.hIcon;
    except
      hIcon := 0;
    end;
  end else begin
    try
      FillChar(shfi, SizeOf(TShFileInfo), 0);
      ShGetFileInfo(PChar(filePath), 0, shfi,
                              SizeOf(TShFileInfo), SHGFI_ICON);
      hIcon := shfi.hIcon;
    except
      hIcon := 0;
    end;
  end;

  if hIcon > 1 then begin
  	result := TIcon.Create();
    result.Handle := hIcon;
  end else begin
  	result := nil;
  end;

end;


{*------------------------------------------------------------------------------
  This function returns the icon for the path passed as a parameter. It uses
  the most appropriate method depending on the folder item type, whether
  it's a directory, executable or document.

  @param filePath Folder item path
  @param dmDebug Where Debug messages will be written
  @see   WriteMessage
-------------------------------------------------------------------------------}
function GetFolderItemIcon(const folderItemPath: String; const returnSmallIcon: Boolean): TIcon;
var fileExt: String;
  hLargeIcon : Hicon;
  hSmallIcon : Hicon;
  shell32Path: TFileName;
  iconIndex: uint;
begin

  result := nil;

  // ---------------------------------------------------------------------------
  // If the folder item is a directory
  // ---------------------------------------------------------------------------

  if IsDirectory(folderItemPath) then begin
  	result := GetFolderIcon(folderItemPath, not returnSmallIcon);
  	Exit;
  end;

  // ---------------------------------------------------------------------------
  // If it's an executable or icon
  // ---------------------------------------------------------------------------

  fileExt := UpperCase(ExtractFileExt(folderItemPath));

  if (fileExt = '.EXE') or (fileExt = '.ICO') then begin
  	if returnSmallIcon then begin
    	result := GetExecutableSmallIcon(folderItemPath);
    end else begin
    	result := GetExecutableLargeIcon(folderItemPath);
    end;
  end; 

  // ---------------------------------------------------------------------------
  // Otherwise assume it's a document and try to get the icon
  // ---------------------------------------------------------------------------

  if result = nil then begin
  	result := GetDocumentIcon(folderItemPath, returnSmallIcon);
  end;

  // ---------------------------------------------------------------------------
  // If everything has failed so far, try to get the icon from SHELL32.DLL
  // Ref: http://www.latiumsoftware.com/en/delphi/00014.php
  // If it fails here as well, just return nil
  // ---------------------------------------------------------------------------

  if (result = nil) then begin
    try
      shell32Path := IncludeTrailingBackslash(GetSystemDir) + 'SHELL32.DLL';
    except
      shell32Path := 'C:\WINDOWS\SYSTEM\SHELL32.DLL';
    end;

    if      (fileExt = '.DOC') then iconIndex := 1
    else if (fileExt = '.EXE')
         or (fileExt = '.COM') then iconIndex := 2
    else if (fileExt = '.HLP') then iconIndex := 23
    else if (fileExt = '.INI')
         or (fileExt = '.INF') then iconIndex := 63
    else if (fileExt = '.TXT') then iconIndex := 64
    else if (fileExt = '.BAT') then iconIndex := 65
    else if (fileExt = '.DLL')
         or (fileExt = '.SYS')
         or (fileExt = '.VBX')
         or (fileExt = '.OCX')
         or (fileExt = '.VXD') then iconIndex := 66
    else if (fileExt = '.FON') then iconIndex := 67
    else if (fileExt = '.TTF') then iconIndex := 68
    else if (fileExt = '.FOT') then iconIndex := 69
    else iconIndex := 0;

    if ExtractIconEx(pchar(shell32Path), iconIndex, hLargeIcon, hSmallIcon, 1) > 1 then
    begin
      result := TIcon.Create();
      if returnSmallIcon then begin
        result.Handle := hSmallIcon;
      end else begin
        result.Handle := hLargeIcon;
      end;
    end;
  end;
  
  
end;


function GetApplicationDirectory;
begin
	GetDir(0, result);
end;


function GetApplicationDrive;
begin
	GetDir(0, result);
  result := Copy(result, 0, 2);
end;


function GetSystemDir: TFileName;
var
  SysDir: array [0..MAX_PATH-1] of char;
begin
  SetString(Result, SysDir, GetSystemDirectory(SysDir, MAX_PATH));
  if Result = '' then
    raise Exception.Create(SysErrorMessage(GetLastError));
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
