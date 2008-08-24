unit Main;

interface

uses Dialogs, MainForm, FileSystemUtils, SysUtils, Classes, StringUtils,
	LocalizationUtils, IniFiles, Forms, Windows, Logger, Graphics, ShellAPI,
  User;


const

  DATA_FOLDER_NAME = 'Data';

	SETTING_FOLDER_PATH = DATA_FOLDER_NAME + '\Settings';
  SKIN_FOLDER_PATH = DATA_FOLDER_NAME + '\Skin';
  LOCALE_FOLDER_PATH = DATA_FOLDER_NAME + '\Locales';
  EXCLUSION_FILE_PATH = SETTING_FOLDER_PATH + '\Exclusions.txt';
  INI_FILE_PATH = SETTING_FOLDER_PATH + '\Config.ini';
  USER_SETTINGS_FILE_PATH = SETTING_FOLDER_PATH + '\MiniLaunchBar.xml';

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

  TSpecialFolderNames = record
  	Music: String;
    Pictures: String;
    Videos: String;
    Documents: String;
  end;

  TFolderItem = class

  	private
    	pSmallIcon: TIcon;
    	class var pUniqueID : Integer;
      function GetSmallIcon(): TIcon;
    public
      FilePath: String;
      ID: Integer;
      Name: String;
      IsSeparator: Boolean;
      property SmallIcon:TIcon read GetSmallIcon;
    	constructor Create();

  end;



  TFolderItems = Array of TFolderItem;

  TStyle = record
    barMainPanel: TBarMainPanelStyle;
    barInnerPanel: TBarInnerPanelStyle;
    optionPanel: TOptionPanelStyle;
  end;

	TMain = class


  private

    pSpecialFolderNames: TSpecialFolderNames;
    pUser: TUser;

  public

  	loc: TLocalizationUtils;
  	style: TStyle;
    skinPath: String;
    mainForm : TMainForm;
    folderItems: TFolderItems;

  	constructor Create();
  	class function Instance: TMain;
    procedure RefreshFolderItems();

    function ErrorMessage(const text: String; const buttons: TMsgDlgButtons = [mbOk]): Integer;

    function GetFolderItemAt(const iIndex: Word): TFolderItem;
    function GetFolderItemByID(const iFolderItemID: Integer): TFolderItem;
    function FolderItemCount(): Word;

   	property User: TUser read pUser;

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
	IsSeparator := false;
	pUniqueID := pUniqueID + 1;
	ID := pUniqueID; 
end;


function TFolderItem.GetSmallIcon(): TIcon;
var largeIcon, smallIcon: Windows.HICON;
begin
	result := nil;

	if IsSeparator then Exit;

	if pSmallIcon <> nil then begin
  	result := pSmallIcon;
    Exit;
  end;

  result := GetFolderItemIcon(FilePath, true);
end;


function TMain.ErrorMessage(const text: String; const buttons: TMsgDlgButtons = [mbOk]): Integer;
begin
	result := MessageDlg(text, mtError, buttons, 0);
end;


constructor TMain.Create();
begin

	if DEBUG then begin
  	ShowLoggerWindow();
  end;

  ilog('Version: ' + GetFmtFileVersion(ParamStr(0)));

  // ---------------------------------------------------------------------------
  // Initialize INI default settings
  // ---------------------------------------------------------------------------

  pUser := TUser.Create(USER_SETTINGS_FILE_PATH);
  
  // ---------------------------------------------------------------------------
  // Initialize localization manager
  // ---------------------------------------------------------------------------

	loc := TLocalizationUtils.Create();
  loc.LoadLocale('en', LOCALE_FOLDER_PATH);
  loc.CurrentLocale := pUser.GetUserSetting('Locale');
  loc.LoadLocale(loc.CurrentLocale, LOCALE_FOLDER_PATH);

  ilog('Current locale: ' + loc.CurrentLocale);

  pSpecialFolderNames.Music := 'Music';
  pSpecialFolderNames.Videos := 'Videos';
  pSpecialFolderNames.Pictures := 'Pictures';
  pSpecialFolderNames.Documents := 'Documents';

  // ---------------------------------------------------------------------------
  // Initialize style
  // ---------------------------------------------------------------------------

  skinPath := SKIN_FOLDER_PATH + '\Default';

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

	ilog('Settings folder: ' + SETTING_FOLDER_PATH);
  ilog('Skin folder: ' + skinPath);
  ilog('Locale folder: ' + LOCALE_FOLDER_PATH);
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
  documentsPath: String;
begin
  exclusions := TStringList.Create();

  AssignFile(exclusionFile, EXCLUSION_FILE_PATH);
  {$I-} Reset(exclusionFile); {$I+}

  if IOResult = 0 then begin
    while not Eof(exclusionFile) do begin
      ReadLn(exclusionFile, s);
      s := Trim(s);
      if s <> '' then exclusions.Add(s);
    end;

    CloseFile(exclusionFile);
  end;

  if Length(folderItems) > 0 then begin
    for i := 0 to Length(folderItems) - 1 do begin
      folderItems[i].Destroy();
      folderItems[i] := nil;
    end;
  end;

  SetLength(folderItems, 0);

	directoryContents := GetDirectoryContents(pUser.GetUserSetting('PortableAppsPath'), 1, 'exe');

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
          folderItem.Name := ExtractFileName(filePath);
          SetLength(folderItems, Length(folderItems) + 1);
          folderItems[Length(folderItems) - 1] := folderItem;
     		end;

      end;
    end;
  end;


  documentsPath := pUser.GetUserSetting('DocumentsPath');

  if DirectoryExists(documentsPath) then begin

    folderItem := TFolderItem.Create();
    folderItem.IsSeparator := true;
    SetLength(folderItems, Length(folderItems) + 1);
    folderItems[Length(folderItems) - 1] := folderItem;

    folderItem := TFolderItem.Create();
    folderItem.filePath := documentsPath;
    folderItem.Name := pSpecialFolderNames.Documents;
    SetLength(folderItems, Length(folderItems) + 1);
    folderItems[Length(folderItems) - 1] := folderItem;

    if DirectoryExists(documentsPath + '\' + pSpecialFolderNames.Music) then begin
      folderItem := TFolderItem.Create();
      folderItem.filePath := documentsPath + '\' + pSpecialFolderNames.Music;
      folderItem.Name := pSpecialFolderNames.Music;
      SetLength(folderItems, Length(folderItems) + 1);
      folderItems[Length(folderItems) - 1] := folderItem;
    end;

  	if DirectoryExists(documentsPath + '\' + pSpecialFolderNames.Pictures) then begin
      folderItem := TFolderItem.Create();
      folderItem.filePath := documentsPath + '\' + pSpecialFolderNames.Pictures;
      folderItem.Name := pSpecialFolderNames.Pictures;
      SetLength(folderItems, Length(folderItems) + 1);
      folderItems[Length(folderItems) - 1] := folderItem;
    end;

    if DirectoryExists(documentsPath + '\' + pSpecialFolderNames.Videos) then begin
      folderItem := TFolderItem.Create();
      folderItem.filePath := documentsPath + '\' + pSpecialFolderNames.Videos;
      folderItem.Name := pSpecialFolderNames.Videos;
      SetLength(folderItems, Length(folderItems) + 1);
      folderItems[Length(folderItems) - 1] := folderItem;
    end;

  end;


  folderItem := TFolderItem.Create();
  folderItem.FilePath := 'd:\temp\Clef USB\fw8ben signed 2.pdf';
  folderItem.Name := 'PDF TEST';
  SetLength(folderItems, Length(folderItems) + 1);
  folderItems[Length(folderItems) - 1] := folderItem;

end;


function TMain.GetFolderItemAt(const iIndex: Word): TFolderItem;
begin
	result := folderItems[iIndex];
end;


function TMain.GetFolderItemByID(const iFolderItemID: Integer): TFolderItem;
var i: Word;
begin
	result := nil;
	for i := 0 to Length(folderItems) - 1 do begin
    if folderItems[i].ID = iFolderItemID then begin
      result := folderItems[i];
      break;
    end;
  end;
end;


function TMain.FolderItemCount(): Word;
var i: Word;
begin
	result := Length(folderItems);
end;



end.
