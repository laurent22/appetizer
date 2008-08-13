unit Main;

interface

uses Dialogs, MainForm, FileSystemUtils, SysUtils, Classes, StringUtils,
	DebugWindow, LocalizationUtils, IniFiles, Forms, Windows;


const

	VERSION = '0.5.1';
  DATA_FOLDER_NAME = 'Data';

	SETTING_FOLDER_PATH = DATA_FOLDER_NAME + '\Settings';
  SKIN_FOLDER_PATH = DATA_FOLDER_NAME + '\Skin';
  LOCALE_FOLDER_PATH = DATA_FOLDER_NAME + '\Locales';

  EXCLUSION_FILE_PATH = SETTING_FOLDER_PATH + '\Exclusions.txt';
  INI_FILE_PATH = SETTING_FOLDER_PATH + '\Config.ini';
  DEBUG = true;


type

  TBarMainPanelStyle = record
    paddingLeft: Integer;
    paddingRight: Integer;
    paddingTop: Integer;
    paddingBottom: Integer;
    paddingH: Integer;
    paddingV: Integer;
  end;

  TBarInnerPanelStyle = record
    paddingLeft: Integer;
    paddingRight: Integer;
    paddingTop: Integer;
    paddingBottom: Integer;
    paddingH: Integer;
    paddingV: Integer;
  end;

  TOptionPanelStyle = record
    paddingLeft: Integer;
    paddingRight: Integer;
    paddingTop: Integer;
    paddingBottom: Integer;
    paddingH: Integer;
    paddingV: Integer;
  end;

  TFolderItem = class

  	private
    	class var pUniqueID : Integer;
    public
      FilePath: String;
      ID: Integer;
    published
    	constructor Create();

  end;



  TFolderItems = Array[0..255] of TFolderItem;

  TStyle = record
    barMainPanel: TBarMainPanelStyle;
    barInnerPanel: TBarInnerPanelStyle;
    optionPanel: TOptionPanelStyle;
  end;

	TMain = class


  private  
    pIniFile: TIniFile;

    procedure OpenIniFile();

  public
  	loc: TLocalizationUtils;
  	style: TStyle;
    skinPath: String;
    mainForm : TMainForm;
    folderItems: TFolderItems;
    debugWindow: TDebugWindow;

  	constructor Create();
  	class function Instance: TMain;
    procedure RefreshFolderItems();

    function ErrorMessage(const text: String; const buttons: TMsgDlgButtons = [mbOk]): Integer;

    function GetUserSetting(const section: String; const key: String; const closeFileNow: Boolean = false): String;
    procedure SetUserSetting(const section: String; const key: String; const value: String; const closeFileNow: Boolean = false);
    procedure SaveUserSettings();

    function GetDefaultUserSetting(const section: String; const key: String): String;

    function GetFolderItemAt(const iIndex: Word): TFolderItem;
    function GetFolderItemByID(const iFolderItemID: Integer): TFolderItem;
    function FolderItemCount(): Word;

    procedure log(const s: String);
    procedure elog(const s: String);
    procedure wlog(const s: String);
    procedure ilog(const s: String);
  end;

implementation


var __singletonInstance: TMain;

class function TMain.Instance(): TMain;
begin
	if __singletonInstance = nil then begin
  	__singletonInstance := TMain.Create();
  end;
  result := __singletonInstance;
end;









constructor TFolderItem.Create();
begin
	pUniqueID := pUniqueID + 1;
	ID := pUniqueID; 
end;



procedure TMain.log(const s: String);
begin
	if debugWindow <> nil then debugWindow.log(s);
end;


procedure TMain.elog(const s: String);
begin
	log('[Error] ' + s);
end;


procedure TMain.wlog(const s: String);
begin
	log('[Warning] ' + s);
end;


procedure TMain.ilog(const s: String);
begin
	log('[Info] ' + s);
end;



function TMain.ErrorMessage(const text: String; const buttons: TMsgDlgButtons = [mbOk]): Integer;
begin
	result := MessageDlg(text, mtError, buttons, 0);
end;


procedure TMain.OpenIniFile();
begin
	if pIniFile <> nil then Exit;
	pIniFile := TIniFile.Create(INI_FILE_PATH);
end;


function TMain.GetDefaultUserSetting(const section: String; const key: String): String;
begin
  if section = 'Global' then begin
  	if key = 'Locale' then begin
    	result := 'en';
    end else begin

    end;
  end else begin

  end;
end;


function TMain.GetUserSetting(const section: String; const key: String; const closeFileNow: Boolean = false): String;
begin
  OpenIniFile();
  result := pIniFile.ReadString(section, key, GetDefaultUserSetting(section, key));
  if closeFileNow then SaveUserSettings();
end;


procedure TMain.SetUserSetting(const section: String; const key: String; const value: String; const closeFileNow: Boolean = false);
begin
	OpenIniFile();
  pIniFile.WriteString(section, key, value);
  if closeFileNow then SaveUserSettings();
end;



procedure TMain.SaveUserSettings();
begin
	if pIniFile = nil then Exit;
	pIniFile.Free();
  pIniFile := nil;
end;


constructor TMain.Create();
begin
	if DEBUG then begin
    debugWindow := TDebugWindow.Create(nil);
    debugWindow.Left := 20;
    debugWindow.Top := 20;
    debugWindow.Visible := true;
  end;

  skinPath := SKIN_FOLDER_PATH + '\Default';
  
  ilog('Version: ' + GetFmtFileVersion(ParamStr(0)));

	ilog('Settings folder: ' + SETTING_FOLDER_PATH);
  ilog('Skin folder: ' + skinPath);
  ilog('Locale folder: ' + LOCALE_FOLDER_PATH);

	loc := TLocalizationUtils.Create();
  loc.LoadLocale('en', LOCALE_FOLDER_PATH);
  loc.CurrentLocale := GetUserSetting('Global', 'Locale');
  loc.LoadLocale(loc.CurrentLocale, LOCALE_FOLDER_PATH);

  ilog('Current locale: ' + loc.CurrentLocale);

  // ---------------------------------------------------------------------------
  // Initialize style
  // ---------------------------------------------------------------------------

	style.barMainPanel.paddingTop := 8;
  style.barMainPanel.paddingBottom := 8;
  style.barMainPanel.paddingLeft := 8;
  style.barMainPanel.paddingRight := 8;
  style.barMainPanel.paddingH := style.barMainPanel.paddingLeft + style.barMainPanel.paddingRight;
  style.barMainPanel.paddingV := style.barMainPanel.paddingTop + style.barMainPanel.paddingBottom;

	style.barInnerPanel.paddingTop := 4;
  style.barInnerPanel.paddingBottom := 4;
  style.barInnerPanel.paddingLeft := 8;
  style.barInnerPanel.paddingRight := 8;
  style.barInnerPanel.paddingH := style.barInnerPanel.paddingLeft + style.barInnerPanel.paddingRight;
  style.barInnerPanel.paddingV := style.barInnerPanel.paddingTop + style.barInnerPanel.paddingBottom;

	style.optionPanel.paddingTop := 5;
  style.optionPanel.paddingBottom := 5;
  style.optionPanel.paddingLeft := 7;
  style.optionPanel.paddingRight := 7;
  style.optionPanel.paddingH := style.optionPanel.paddingLeft + style.optionPanel.paddingRight;
  style.optionPanel.paddingV := style.optionPanel.paddingTop + style.optionPanel.paddingBottom;
end;






procedure TMain.RefreshFolderItems();
var
	directoryContents: TStringList;
	i, j: Word;
  folderItemIndex: Word;
  filePath: String;
	fileExtension: String;
  folderItem: TFolderItem;
  exclusions: TStringList;
  exclusionFile: TextFile;
  s: String;
  skipIt: Boolean;
begin

  exclusions := TStringList.Create();

  if FileExists(EXCLUSION_FILE_PATH) then begin
 		AssignFile(exclusionFile, EXCLUSION_FILE_PATH);
    Reset(exclusionFile);

  	while not Eof(exclusionFile) do begin
      ReadLn(exclusionFile, s);
      s := Trim(s);
      if s <> '' then exclusions.Add(s);
    end;

    CloseFile(exclusionFile);
  end;



  for i := 1 to Length(folderItems) do begin
    if folderItems[i] = nil then break;
  	folderItems[i].Destroy();
    folderItems[i] := nil;
  end;

  folderItemIndex := 1;

	directoryContents := GetDirectoryContents('.', true);

  if directoryContents.Count > 0 then begin
  	for i := 0 to directoryContents.Count - 1 do begin
      filePath := directoryContents[i];
      fileExtension := ExtractFileExt(filePath);
      if fileExtension = '.exe' then begin

      	skipIt := false;
      	for j := 0 to exclusions.Count - 1 do begin
        	if AnsiPos(exclusions[j], filePath) >= 1 then begin
          	skipIt := true;
            break;
          end;
        end;

        if not skipIt then begin
          folderItem := TFolderItem.Create();
          folderItem.filePath := filePath;
          folderItems[folderItemIndex] := folderItem;
          folderItemIndex := folderItemIndex + 1;
     		end;

      end;
    end;
  end;
end;


function TMain.GetFolderItemAt(const iIndex: Word): TFolderItem;
begin
	result := folderItems[iIndex];
end;


function TMain.GetFolderItemByID(const iFolderItemID: Integer): TFolderItem;
var i: Word;
begin
	result := nil;
	for i := 1 to Length(folderItems) do begin
  	if folderItems[i] = nil then break;
    if folderItems[i].ID = iFolderItemID then begin
      result := folderItems[i];
      break;
    end;
  end;
end;


function TMain.FolderItemCount(): Word;
var i: Word;
begin
	result := 0;
	for i := 1 to Length(folderItems) do begin
  	if folderItems[i] = nil then break;
    result := result + 1;
  end;
end;



end.
