unit Main;

interface

uses Dialogs, MainForm, FileSystemUtils, SysUtils, Classes, StringUtils,
	LocalizationUtils, IniFiles, Forms, Windows, Logger, Graphics, ShellAPI,
  User, FileCtrl, CmdLineParam, ComObj, MSXML2_TLB, ConfigFormUnit, Contnrs,
  WComponent;


const

  DATA_FOLDER_NAME = 'Data';
  SETTING_FOLDER_NAME = 'Settings';
  SKIN_FOLDER_NAME = 'Skin';
  LOCALES_FOLDER_NAME = 'Locales';
  USER_SETTINGS_FILE_NAME = 'MiniLaunchBar.xml';
  ICONS_FOLDER_NAME = 'Icons';
  SKIN_FILE_NAME = 'Skin.xml';



type

	TFilePaths = record
    DataDirectory: String;
    SettingsDirectory: String;
    SkinDirectory: String;
    LocalesDirectory: String;
    UserSettingsFile: String;
    IconsDirectory: String;
  end;

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

  TIconTooltipStyle = record
    paddingLeft: Integer;
    paddingRight: Integer;
    paddingTop: Integer;
    paddingBottom: Integer;
    paddingH: Integer;
    paddingV: Integer;
    FontColor: TColor;
    FontStyles: TFontStyles;
  end;

  TIconStyle = record
    paddingLeft: Integer;
    paddingRight: Integer;
    paddingTop: Integer;
    paddingBottom: Integer;
    paddingH: Integer;
    paddingV: Integer;
  end;

  TStyle = record
    barMainPanel: TBarMainPanelStyle;
    barInnerPanel: TBarInnerPanelStyle;
    optionPanel: TOptionPanelStyle;
    IconTooltip: TIconTooltipStyle;
    Icon: TIconStyle;
  end;


  TNotifyEventWrapper = class
    public
      Event: TNotifyEvent;
  end;


	TMain = class(TObject)  

  private
    
    pUser: TUser;
    skinXML: IXMLDomDocument;
    pLocalizableHandlers: Array of TNotifyEventWrapper;

  public

  	Loc: TLocalizationUtils;
  	Style: TStyle;
    MainForm : TMainForm;
    FilePaths: TFilePaths;

    CommandLineArgs: TCmdLineParam;

  	constructor Create();
  	class function Instance: TMain;

    procedure Localize();

    procedure EjectDrive();
    function ErrorMessage(const text: String; const buttons: TMsgDlgButtons = [mbOk]): Integer;
    function ConfirmationMessage(const text: String; const buttons: TMsgDlgButtons = [mbYes, mbNo]): Integer;
    function GetSkinAttribute(const objectName: String; const attributeName: String): String;
    function GetSkinAttributeAsStringList(const objectName: String; const attributeName: String): TStringList;
   	property User: TUser read pUser;
    procedure RegisterLocalizableObject(const localizableHandler: TNotifyEvent);
    procedure UnregisterLocalizableObject(localizableHandler: TNotifyEvent);
    procedure ShowConfigForm();
    procedure SetCurrentLocale(const localeCode: String);

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



procedure TMain.Localize;
var i: Integer;
    n: TNotifyEventWrapper;
begin

//  @func := pLocalizableHandlers[0];
//  func(nil);
//  FreeMem(func);


  for i := 0 to Length(pLocalizableHandlers) - 1 do begin
    n := pLocalizableHandlers[i];
    n.Event(Self);
  end;
end;


procedure TMain.RegisterLocalizableObject(const localizableHandler: TNotifyEvent);
var i: Integer;
    o: TNotifyEventWrapper;
begin
  o := TNotifyEventWrapper.Create();
  o.Event := localizableHandler;

  SetLength(pLocalizableHandlers, Length(pLocalizableHandlers) + 1);
  pLocalizableHandlers[Length(pLocalizableHandlers) - 1] := o;

  //pLocalizableHandlers.Add(@localizableHandler);
end;


procedure TMain.UnregisterLocalizableObject(localizableHandler: TNotifyEvent);
var i: Integer;
begin
//  for i := 0 to pLocalizableHandlers.Count - 1 do begin
//    if pLocalizableHandlers[i] = @localizableHandler then begin
//      pLocalizableHandlers[i] := nil;
//      Exit;
//    end;
//  end;
end;


//procedure TMain.Localize;
//var i: Integer;
//    func: TNotifyEvent;
//begin
//
//  @func := pLocalizableHandlers[0];
//  func(nil);
//  FreeMem(func);
//
//
////  for i := 0 to pLocalizableHandlers.Count - 1 do begin
////    @p := pLocalizableHandlers[i];
////    p(nil);
////    //FreeAndNil(p);
////  end;
//end;
//
//
//procedure TMain.RegisterLocalizableObject(const localizableHandler: TNotifyEvent);
//var i: Integer;
//begin
//  pLocalizableHandlers.Add(@localizableHandler);
//end;
//
//
//procedure TMain.UnregisterLocalizableObject(localizableHandler: TNotifyEvent);
//var i: Integer;
//begin
//  for i := 0 to pLocalizableHandlers.Count - 1 do begin
//    if pLocalizableHandlers[i] = @localizableHandler then begin
//      pLocalizableHandlers[i] := nil;
//      Exit;
//    end;
//  end;
//end;


procedure TMain.SetCurrentLocale(const localeCode: String);
begin
  if Loc.CurrentLocale = localeCode then Exit;
  
  Loc.CurrentLocale := pUser.GetUserSetting('Locale');
  Loc.LoadLocale(Loc.CurrentLocale, FilePaths.LocalesDirectory);
end;


procedure TMain.ShowConfigForm;
var configForm: TConfigForm;
begin
  configForm := TConfigForm.Create(MainForm);
  try
    configForm.ShowModal();
  finally
    FreeAndNil(configForm);
  end;
end;




function TMain.ErrorMessage;
begin
	result := MessageDlg(text, mtError, buttons, 0);
end;


function TMain.GetSkinAttribute(const objectName, attributeName: String): String;
var i: Integer;
    element: IXMLDOMElement;
begin
  for i := 0 to skinXML.documentElement.childNodes.length - 1 do begin
    element := skinXML.documentElement.childNodes.item[i] as IXMLDOMElement;

    if element.nodeName <> objectName then continue;

    result := element.getAttribute(attributeName);
    
    Exit;
  end;

  result := '';
end;


function TMain.GetSkinAttributeAsStringList(const objectName, attributeName: String): TStringList;
begin
  result := SplitString(',', GetSkinAttribute(objectName, attributeName));
end;


function TMain.ConfirmationMessage;
begin
	result := MessageDlg(text, mtConfirmation, buttons, 0);
end;


procedure TMain.EjectDrive;
begin
	ShellExecute(0, 'open', PChar('RunDll32.exe'), 'shell32.dll,Control_RunDLL hotplug.dll', nil, SW_SHOWNORMAL) ;
end;




constructor TMain.Create();
var applicationDirectory: String;
    success: Boolean;
begin
	applicationDirectory := GetApplicationDirectory();

	FilePaths.DataDirectory := applicationDirectory + '\' + DATA_FOLDER_NAME;
  FilePaths.SettingsDirectory := FilePaths.DataDirectory + '\' + SETTING_FOLDER_NAME;
  FilePaths.SkinDirectory := FilePaths.DataDirectory + '\' + SKIN_FOLDER_NAME + '\Default';
  FilePaths.LocalesDirectory := FilePaths.DataDirectory + '\' + LOCALES_FOLDER_NAME;
  FilePaths.UserSettingsFile := FilePaths.SettingsDirectory + '\' + USER_SETTINGS_FILE_NAME;
  FilePaths.IconsDirectory := FilePaths.SkinDirectory + '\' + ICONS_FOLDER_NAME;

	CommandLineArgs := TCmdLineParam.Create();

	if CommandLineArgs.HasArgument('showDebugWindow') then ShowLoggerWindow();
  if CommandLineArgs.HasArgument('logToFile') then LogToFile(FilePaths.SettingsDirectory + '\Log.txt');

  ilog('Version: ' + GetFmtFileVersion(ParamStr(0)));
  ilog('Application directory: ' + applicationDirectory);

  skinXML := CreateOleObject('Microsoft.XMLDOM') as IXMLDomDocument;
  if FileExists(FilePaths.SkinDirectory + '\' + SKIN_FILE_NAME) then begin
    success := skinXML.Load(FilePaths.SkinDirectory + '\' + SKIN_FILE_NAME);
    if not success then elog('Could not load skin description at: ' + FilePaths.SkinDirectory + '\' + SKIN_FILE_NAME);
  end;

  // ---------------------------------------------------------------------------
  // Initialize INI default settings
  // ---------------------------------------------------------------------------

  ForceDirectories(FilePaths.SettingsDirectory);
  pUser := TUser.Create(FilePaths.UserSettingsFile);
  
  // ---------------------------------------------------------------------------
  // Initialize localization manager
  // ---------------------------------------------------------------------------

	Loc := TLocalizationUtils.Create();
  Loc.LoadLocale('en', FilePaths.LocalesDirectory);
  SetCurrentLocale(pUser.GetUserSetting('Locale'));
  
  ilog('Current locale: ' + Loc.CurrentLocale);

  // ---------------------------------------------------------------------------
  // Initialize style
  // ---------------------------------------------------------------------------

	Style.barMainPanel.paddingTop := 8;
  Style.barMainPanel.paddingBottom := 8;
  Style.barMainPanel.paddingLeft := 8;
  Style.barMainPanel.paddingRight := 8;
  Style.barMainPanel.paddingH := Style.barMainPanel.paddingLeft + Style.barMainPanel.paddingRight;
  Style.barMainPanel.paddingV := Style.barMainPanel.paddingTop + Style.barMainPanel.paddingBottom;

	Style.barInnerPanel.paddingTop := 4;
  Style.barInnerPanel.paddingBottom := 4;
  Style.barInnerPanel.paddingLeft := 8;
  Style.barInnerPanel.paddingRight := 8;
  Style.barInnerPanel.paddingH := Style.barInnerPanel.paddingLeft + Style.barInnerPanel.paddingRight;
  Style.barInnerPanel.paddingV := Style.barInnerPanel.paddingTop + Style.barInnerPanel.paddingBottom;

	Style.optionPanel.paddingTop := 5;
  Style.optionPanel.paddingBottom := 5;
  Style.optionPanel.paddingLeft := 7;
  Style.optionPanel.paddingRight := 7;
  Style.optionPanel.paddingH := Style.optionPanel.paddingLeft + Style.optionPanel.paddingRight;
  Style.optionPanel.paddingV := Style.optionPanel.paddingTop + Style.optionPanel.paddingBottom;

	Style.IconTooltip.paddingTop := 4;
  Style.IconTooltip.paddingBottom := 4;
  Style.IconTooltip.paddingLeft := 6;
  Style.IconTooltip.paddingRight := 6;
  Style.IconTooltip.paddingH := Style.IconTooltip.paddingLeft + Style.IconTooltip.paddingRight;
  Style.IconTooltip.paddingV := Style.IconTooltip.paddingTop + Style.IconTooltip.paddingBottom;
  Style.IconTooltip.FontColor := clBlack;
  Style.IconTooltip.FontStyles := [fsBold];

	Style.Icon.paddingTop := 4;
  Style.Icon.paddingBottom := 4;
  Style.Icon.paddingLeft := 4;
  Style.Icon.paddingRight := 4;
  Style.Icon.paddingH := Style.Icon.paddingLeft + Style.Icon.paddingRight;
  Style.Icon.paddingV := Style.Icon.paddingTop + Style.Icon.paddingBottom;

	ilog('Settings folder: ' + FilePaths.SettingsDirectory);
  ilog('Skin folder: ' + FilePaths.SkinDirectory);
  ilog('Locale folder: ' + FilePaths.LocalesDirectory);
end;


end.
