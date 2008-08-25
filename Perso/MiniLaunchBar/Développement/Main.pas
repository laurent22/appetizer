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

  TStyle = record
    barMainPanel: TBarMainPanelStyle;
    barInnerPanel: TBarInnerPanelStyle;
    optionPanel: TOptionPanelStyle;
  end;


	TMain = class   

  private

    
    pUser: TUser;

  public

  	loc: TLocalizationUtils;
  	style: TStyle;
    skinPath: String;
    mainForm : TMainForm;

  	constructor Create();
  	class function Instance: TMain;                                                           
    function ErrorMessage(const text: String; const buttons: TMsgDlgButtons = [mbOk]): Integer;
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

  pUser := TUser.Create(GetApplicationDirectory() + '\' + USER_SETTINGS_FILE_PATH);
  
  // ---------------------------------------------------------------------------
  // Initialize localization manager
  // ---------------------------------------------------------------------------

	loc := TLocalizationUtils.Create();
  loc.LoadLocale('en', LOCALE_FOLDER_PATH);
  loc.CurrentLocale := pUser.GetUserSetting('Locale');
  loc.LoadLocale(loc.CurrentLocale, LOCALE_FOLDER_PATH);

  ilog('Current locale: ' + loc.CurrentLocale);

  // ---------------------------------------------------------------------------
  // Initialize style
  // ---------------------------------------------------------------------------

  skinPath := GetApplicationDirectory() + '\' + SKIN_FOLDER_PATH + '\Default';

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





end.
