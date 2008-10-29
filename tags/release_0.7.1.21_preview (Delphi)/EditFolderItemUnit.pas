unit EditFolderItemUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, User, FileCtrl;

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
  private
    { Private declarations }
    pLoadedFolderItem: TFolderItem;
  public
    { Public declarations }
    SaveButtonClicked: Boolean;
    procedure LoadFolderItem(const folderItem: TFolderItem);
  end;

var
  EditFolderItemForm: TEditFolderItemForm;

implementation

{$R *.dfm}

uses Main;


procedure TEditFolderItemForm.Button1Click(Sender: TObject);
begin
	SaveButtonClicked := false;
	Close();
end;


procedure TEditFolderItemForm.Button2Click(Sender: TObject);
begin
	SaveButtonClicked := true;
	pLoadedFolderItem.Name := nameEdit.Text;
  pLoadedFolderItem.FilePath := locationEdit.Text;
  Close();
end;


procedure TEditFolderItemForm.Button3Click(Sender: TObject);
begin
	if OpenDialog1.Execute() then begin
    locationEdit.Text := OpenDialog1.FileName;
  end;
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




end.
