unit VersionInformation;

{
	Found on Experts Exchange:
  http://www.experts-exchange.com/Programming/Languages/Pascal/Delphi/Q_23623918.html
}

interface

const
  VI_MAJOR_VERSION = 1;
  VI_MINOR_VERSION = 2;
  VI_RELEASE       = 3;
  VI_BUILD         = 4;

  VI_COMPANY_NAME      = 1;
  VI_FILE_DESCRIPTION  = 2;
  VI_FILE_VERSION      = 3;
  VI_INTERNAL_NAME     = 4;
  VI_LEGAL_COPYRIGHT   = 5;
  VI_ORIGINAL_FILENAME = 6;
  VI_PRODUCT_NAME      = 7;
  VI_PRODUCT_VERSION   = 8;
  VI_COMMENTS          = 9;
  VI_LEGAL_TRADEMARKS  = 10;

type
  TVersionInfo = class
  private
    iDataSize : Integer;
    pData     : Pointer;

    function iGetVersionInfo(Index : Integer) : Integer;
    function sGetVersionInfo(Index : Integer) : String;
    function GetVersionDateTime : TDateTime;

  public
    constructor Create;
    constructor CreateFile(FileName : String);
    destructor Destroy; override;
    function GetVersionString(Key : String): String;

    property MajorVersion     : Integer index VI_MAJOR_VERSION read iGetVersionInfo;
    property MinorVersion     : Integer index VI_MINOR_VERSION read iGetVersionInfo;
    property Release          : Integer index VI_RELEASE       read iGetVersionInfo;
    property Build            : Integer index VI_BUILD         read iGetVersionInfo;
    property DateTime         : TDateTime                      read GetVersionDateTime;
    property CompanyName      : String index VI_COMPANY_NAME      read sGetVersionInfo;
    property FileDescription  : String index VI_FILE_DESCRIPTION  read sGetVersionInfo;
    property FileVersion      : String index VI_FILE_VERSION      read sGetVersionInfo;
    property InternalName     : String index VI_INTERNAL_NAME     read sGetVersionInfo;
    property LegalCopyright   : String index VI_LEGAL_COPYRIGHT   read sGetVersionInfo;
    property OriginalFilename : String index VI_ORIGINAL_FILENAME read sGetVersionInfo;
    property ProductName      : String index VI_PRODUCT_NAME      read sGetVersionInfo;
    property ProductVersion   : String index VI_PRODUCT_VERSION   read sGetVersionInfo;
    property Comments         : String index VI_COMMENTS          read sGetVersionInfo;
    property LegalTrademarks  : String index VI_LEGAL_TRADEMARKS  read sGetVersionInfo;
 end;

var
  VersionInfo : TVersionInfo = NIL;

implementation

uses
  Windows, SysUtils;

function TVersionInfo.iGetVersionInfo(Index: Integer): Integer;
var
  FixedFileInfo : PVSFixedFileInfo;
  BufferLen     : Cardinal;
begin
  Result := -1;
  if iDataSize > 0 then begin
    VerQueryValue(pData, '\', Pointer(FixedFileInfo), BufferLen);
    with FixedFileInfo^ do
      case Index of
        VI_MAJOR_VERSION : Result := HiWord(dwFileVersionMS);
        VI_MINOR_VERSION : Result := LoWord(dwFileVersionMS);
        VI_RELEASE       : Result := HiWord(dwFileVersionLS);
        VI_BUILD         : Result := LoWord(dwFileVersionLS);
    end;
  end;
end;

function TVersionInfo.GetVersionString(Key: String): String;
var
 BufferLen  : Cardinal;
 Buffer     : PChar;
 P          : Pointer;
 S          : String;

begin
  Result := '';
  if iDataSize > 0 then begin
    VerQueryValue(pData, '\VarFileInfo\Translation', P, BufferLen);
    S := Format('\StringFileInfo\%.4x%.4x\%s',
      [LoWord(Integer(P^)), HiWord(Integer(P^)), Key]);
    if VerQueryValue(pData, PChar(S), Pointer(Buffer), BufferLen) then
      Result := StrPas(Buffer);
  end;
end;

function TVersionInfo.GetVersionDateTime: TDateTime;
var
  FixedFileInfo : PVSFixedFileInfo;
  BufferLen     : Cardinal;
  FileTime      : TFileTime;
  SystemTime    : TSystemTime;

begin
  Result := 0;
  if iDataSize > 0 then begin
    VerQueryValue(pData, '\', Pointer(FixedFileInfo), BufferLen);
    with FixedFileInfo^ do begin
      FileTime.dwLowDateTime  := dwFileDateLS;
      FileTime.dwHighDateTime := dwFileDateMS;
      FileTimeToSystemTime(FileTime, SystemTime);
    with SystemTime do
      Result := EncodeDate(wYear, wMonth, wDay) +
      EncodeTime(wHour, wMinute, wSecond, wMilliSeconds);
    end;
  end;
end;

function TVersionInfo.sGetVersionInfo(Index : Integer): String;
var
  KeyName : String;

begin
  Result := '';
  case Index of
    VI_COMPANY_NAME      : KeyName := 'CompanyName';
    VI_FILE_DESCRIPTION  : KeyName := 'FileDescription';
    VI_FILE_VERSION      : KeyName := 'FileVersion';
    VI_INTERNAL_NAME     : KeyName := 'InternalName';
    VI_LEGAL_COPYRIGHT   : KeyName := 'LegalCopyright';
    VI_ORIGINAL_FILENAME : KeyName := 'OriginalFilename';
    VI_PRODUCT_NAME      : KeyName := 'ProductName';
    VI_PRODUCT_VERSION   : KeyName := 'ProductVersion';
    VI_COMMENTS          : KeyName := 'Comments';
    VI_LEGAL_TRADEMARKS  : KeyName := 'LegalTrademarks';
  end;
  Result := GetVersionString(KeyName);
end;

constructor TVersionInfo.Create;
var
  BufferLen : Cardinal;

begin
  inherited;
  iDataSize := GetFileVersionInfoSize(PChar(ParamStr(0)), BufferLen);
  if iDataSize > 0 then begin
    GetMem(pData, iDataSize);
    Win32Check(GetFileVersionInfo(PChar(ParamStr(0)), 0, iDataSize, pData));
  end;
end;

constructor TVersionInfo.CreateFile(FileName: String);
var
  BufferLen : Cardinal;

begin
  inherited;
  iDataSize := GetFileVersionInfoSize(PChar(FileName), BufferLen);
  if iDataSize > 0 then begin
    GetMem(pData, iDataSize);
    Win32Check(GetFileVersionInfo(PChar(FileName), 0, iDataSize, pData));
  end;
end;

destructor TVersionInfo.Destroy;
begin
  FreeMem(pData,iDataSize);
  inherited;
end;

end.
