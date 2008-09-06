unit ConfigFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, FileSystemUtils, LocalizationUtils, StringUtils,
  GUIUtils;

type

  TConfigForm = class(TForm)
    PageControl1: TPageControl;
    generalTab: TTabSheet;
    languageLabel: TLabel;
    languageComboBox: TComboBox;
    iconSizeLabel: TLabel;
    iconSizeComboBox: TComboBox;
    cancelButton: TButton;
    okButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure okButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ConfigForm: TConfigForm;

implementation

{$R *.dfm}

uses Main;


procedure TConfigForm.FormCreate(Sender: TObject);
var languageFiles: TStringList;
    i: Integer;
    localeFile: TextFile;
    line: String;
    languageName: String;
    languageCode: String;
    selectedIndex: Integer;
    currentLocale: String;
    possibleIconSizes: Array[0..1] of String;
    iconSizeName: String;
    currentIconSize: String;
begin
  currentLocale := TMain.Instance.User.GetUserSetting('Locale');
  languageFiles := GetDirectoryContents(TMain.Instance.FilePaths.LocalesDirectory, 0, 'txt');

  selectedIndex := 0;
  languageComboBox.Clear();
  for i := 0 to languageFiles.Count - 1 do begin
    languageName := TLocalizationUtils.GetLanguageName(languageFiles[i]);
    languageCode := ExtractFilenameNoExt(languageFiles[i]);
    languageComboBox.Items.AddObject(languageName, TObject(TStringContainer.Create(languageCode)));
    if languageCode = currentLocale then selectedIndex := i;
  end;

  languageComboBox.ItemIndex := selectedIndex;



  currentIconSize := TMain.Instance.User.GetUserSetting('ShorcutIconSize');

  possibleIconSizes[0] := '16';
  possibleIconSizes[1] := '32';

  selectedIndex := 0;
  iconSizeComboBox.Clear();
  for i := 0 to Length(possibleIconSizes) - 1 do begin
    iconSizeName := TMain.Instance.Loc.GetString('Icon.Size' + possibleIconSizes[i]);
    iconSizeComboBox.Items.AddObject(iconSizeName, TObject(TStringContainer.Create(possibleIconSizes[i])));
    if currentIconSize = possibleIconSizes[i] then selectedIndex := i;
  end;

  iconSizeComboBox.ItemIndex := selectedIndex;


  Text := TMain.Instance.Loc.GetString('ConfigDialog.Title');
  generalTab.Caption := TMain.Instance.Loc.GetString('ConfigDialog.GeneralTab');
  languageLabel.Caption := TMain.Instance.Loc.GetString('ConfigDialog.LanguageLabel');
  iconSizeLabel.Caption := TMain.Instance.Loc.GetString('ConfigDialog.IconSizeLabel');
  okButton.Caption := TMain.Instance.Loc.GetString('Global.OK');
  cancelButton.Caption := TMain.Instance.Loc.GetString('Global.Cancel');
end;


procedure TConfigForm.okButtonClick(Sender: TObject);
var newIconSize: String;
    newLocale: String;
begin

  newIconSize := TStringContainer(iconSizeComboBox.Items.Objects[iconSizeComboBox.ItemIndex]).Text;
  newLocale := TStringContainer(languageComboBox.Items.Objects[languageComboBox.ItemIndex]).Text;

  if TMain.Instance.User.GetUserSetting('Locale') <> newLocale then begin
    TMain.Instance.User.SetUserSetting('Locale', newLocale);
    TMain.Instance.SetCurrentLocale(newLocale);
    TMain.Instance.Localize();
  end;

  if TMain.Instance.User.GetUserSetting('ShorcutIconSize') <> newIconSize then begin
    TMain.Instance.User.SetUserSetting('ShorcutIconSize', newIconSize);
    TMain.Instance.MainForm.barInnerPanel.ReloadIcons();
  end;
  
  TMain.Instance.User.Save();
  
  ModalResult := mrOK;
end;

end.
