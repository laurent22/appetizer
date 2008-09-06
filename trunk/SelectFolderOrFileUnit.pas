unit SelectFolderOrFileUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, FileCtrl, GUIUtils;

type
  TSelectFolderOrFileForm = class(TForm)
    DirectoryListBox1: TDirectoryListBox;
    FileListBox1: TFileListBox;
    DriveComboBox1: TDriveComboBox;
    cancelButton: TButton;
    selectFileButton: TButton;
    selectFolderButton: TButton;
    procedure DriveComboBox1Change(Sender: TObject);
    procedure DirectoryListBox1Change(Sender: TObject);
    procedure cancelButtonClick(Sender: TObject);
    procedure selectFolderButtonClick(Sender: TObject);
    procedure selectFileButtonClick(Sender: TObject);
    procedure FileListBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FileListBox1DblClick(Sender: TObject);

  private

    pFilePath: String;
    procedure UpdateAll();
    procedure SetFilePath(const Value: String);

  public

    procedure ConfirmForm();
    procedure CancelForm();
    property FilePath: String read pFilePath write SetFilePath;

  end;

var
  SelectFolderOrFileForm: TSelectFolderOrFileForm;

implementation

{$R *.dfm}

uses Main;


procedure TSelectFolderOrFileForm.cancelButtonClick(Sender: TObject);
begin
  CancelForm();
end;

procedure TSelectFolderOrFileForm.selectFileButtonClick(Sender: TObject);
begin
  FilePath := FileListBox1.FileName;
  ConfirmForm();
end;

procedure TSelectFolderOrFileForm.selectFolderButtonClick(Sender: TObject);
begin
  FilePath := DirectoryListBox1.Directory;
  ConfirmForm();
end;

procedure TSelectFolderOrFileForm.SetFilePath(const Value: String);
begin
  pFilePath := value;
  try
    DriveComboBox1.Drive := ExtractFileDrive(value)[1];
    if DirectoryExists(value) then begin
      DirectoryListBox1.Directory := value;
      FileListBox1.FileName := '';
    end else begin
      DirectoryListBox1.Directory := ExtractFilePath(value);
      FileListBox1.FileName := value;
    end;
  except
    on e: Exception do ; // Don't display anything if an error occurs
  end;
end;

procedure TSelectFolderOrFileForm.CancelForm;
begin
  ModalResult := mrCancel;
end;

procedure TSelectFolderOrFileForm.ConfirmForm;
begin
  ModalResult := mrOK;
end;

procedure TSelectFolderOrFileForm.DirectoryListBox1Change(Sender: TObject);
begin
  UpdateAll();
end;

procedure TSelectFolderOrFileForm.DriveComboBox1Change(Sender: TObject);
begin
  UpdateAll();
end;

procedure TSelectFolderOrFileForm.FileListBox1Change(Sender: TObject);
begin
  UpdateAll();
end;

procedure TSelectFolderOrFileForm.FileListBox1DblClick(Sender: TObject);
begin
  FilePath := FileListBox1.FileName;
  ConfirmForm();
end;

procedure TSelectFolderOrFileForm.FormCreate(Sender: TObject);
begin
  Text := TMain.Instance.Loc.GetString('SelectFolderOrFileDialog.Title');
  cancelButton.Caption := TMain.Instance.Loc.GetString('Global.Cancel');
  selectFileButton.Caption := TMain.Instance.Loc.GetString('SelectFolderOrFileDialog.SelectFile');
  selectFolderButton.Caption := TMain.Instance.Loc.GetString('SelectFolderOrFileDialog.SelectFolder');

  FitButtonToText(cancelButton, Self);
  FitButtonToText(selectFileButton, Self);
  FitButtonToText(selectFolderButton, Self);

  cancelButton.Left := FileListBox1.Left + FileListBox1.Width - cancelButton.Width;
  selectFileButton.Left := cancelButton.Left - 8 - selectFileButton.Width;
  selectFolderButton.Left := selectFileButton.Left - 8 - selectFolderButton.Width;

  UpdateAll();
end;

procedure TSelectFolderOrFileForm.UpdateAll;
begin
  try
    DirectoryListBox1.Drive := DriveComboBox1.Drive;
    FileListBox1.Directory := DirectoryListBox1.Directory;
  except
    on e: Exception do ;
  end;

  selectFolderButton.Enabled := DirectoryListBox1.Directory <> '';
  selectFileButton.Enabled := FileListBox1.FileName <> '';
end;

end.
