unit EditFolderItemUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, User, FileCtrl, Logger, SelectFolderOrFileUnit;

type
  TEditFolderItemForm = class(TForm)
    nameLabel: TLabel;
    locationLabel: TLabel;
    nameEdit: TEdit;
    locationEdit: TEdit;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure locationEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure nameEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    pLoadedFolderItem: TFolderItem;
  public
    { Public declarations }
    SaveButtonClicked: Boolean;
    procedure ConfirmForm();
    procedure CancelForm();
    procedure LoadFolderItem(const folderItem: TFolderItem);
  end;

var
  EditFolderItemForm: TEditFolderItemForm;

implementation

{$R *.dfm}

uses Main;


procedure TEditFolderItemForm.Button1Click(Sender: TObject);
begin
  CancelForm();
end;


procedure TEditFolderItemForm.Button2Click(Sender: TObject);
begin
  ConfirmForm();
end;


procedure TEditFolderItemForm.Button3Click(Sender: TObject);
var openDialog: TSelectFolderOrFileForm;
begin
  openDialog := TSelectFolderOrFileForm.Create(Self);
  openDialog.FilePath := TFolderItem.ResolveFilePath(Trim(locationEdit.Text));

  try
    if openDialog.ShowModal() <> mrCancel then begin
      locationEdit.Text := TFolderItem.ConvertToRelativePath(openDialog.FilePath);
      nameEdit.Text := TFolderItem.GetDisplayNameFromFilePath(openDialog.FilePath);
    end;
  finally
    FreeAndNil(openDialog);
  end;
end;


procedure TEditFolderItemForm.CancelForm;
begin
	SaveButtonClicked := false;
	Close();
end;


procedure TEditFolderItemForm.ConfirmForm;
begin
	SaveButtonClicked := true;
	pLoadedFolderItem.Name := Trim(nameEdit.Text);
  pLoadedFolderItem.FilePath := Trim(locationEdit.Text);
  Close();
end;


procedure TEditFolderItemForm.FormCreate(Sender: TObject);
begin
	Text := TMain.Instance.Loc.GetString('EditFolderItemForm.Title');
  nameLabel.Caption := TMain.Instance.Loc.GetString('EditFolderItemForm.NameLabel');
  locationLabel.Caption := TMain.Instance.Loc.GetString('EditFolderItemForm.LocationLabel');
end;


procedure TEditFolderItemForm.LoadFolderItem(const folderItem: TFolderItem);
begin
	pLoadedFolderItem := folderItem;
  nameEdit.Text := folderItem.Name;
  locationEdit.Text := folderItem.FilePath;
end;


procedure TEditFolderItemForm.locationEditKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (key = 10) or (key = 13) then begin
    ConfirmForm();
    key := 0;
  end;
end;


procedure TEditFolderItemForm.nameEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (key = 10) or (key = 13) then begin
    ConfirmForm();
    key := 0;
  end;
end;

end.
