{
Searching for Files
Stop. This is the one and only solution to file searching. Use Delphi to find any file in any directory and/or subdirectory that match a certain mask. Start searching.

Article:
. http://delphi.about.com/library/weekly/aa051600a.htm

********************************************
Zarko Gajic
About.com Guide to Delphi Programming
http://delphi.about.com
email: delphi.guide@about.com
********************************************
}
unit uMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TfrMain = class(TForm)
    CheckBox1: TCheckBox;
    ListBox1: TListBox;
    Edit1: TEdit;
    Button1: TButton;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    lblNumberFound: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
  private
    procedure FileSearch(const PathName, FileName : string; const InDir : boolean);
  public
    { Public declarations }
  end;

var
  frMain: TfrMain;

implementation
{$R *.DFM}
uses uFileInfo;


procedure TfrMain.FileSearch(const PathName, FileName : string; const InDir : boolean);
var Rec  : TSearchRec;
    Path : string;
begin
Path := IncludeTrailingBackslash(PathName);
if FindFirst(Path + FileName, faAnyFile - faDirectory, Rec) = 0 then
 try
   repeat
     ListBox1.Items.Add(Path + Rec.Name);
   until FindNext(Rec) <> 0;
 finally
   FindClose(Rec);
 end;

If not InDir then Exit;

if FindFirst(Path + '*.*', faDirectory, Rec) = 0 then
 try
   repeat
    if ((Rec.Attr and faDirectory) <> 0)  and (Rec.Name<>'.') and (Rec.Name<>'..') then
     FileSearch(Path + Rec.Name, FileName, True);
   until FindNext(Rec) <> 0;
 finally
   FindClose(Rec);
 end;
end; //procedure FileSearch

procedure TfrMain.Button1Click(Sender: TObject);
begin
  ListBox1.Clear;
  lblNumberFound.Caption:=Inttostr(ListBox1.Items.Count) + ' files found.';
  FileSearch(Edit1.Text, Edit2.Text, CheckBox1.State in [cbChecked]);
  lblNumberFound.Caption:=Inttostr(ListBox1.Items.Count) + ' files found.';
end;

procedure TfrMain.ListBox1DblClick(Sender: TObject);
var SelectedFile : string;
    Rec          : TSearchRec;
    frInfo       : TfrFileInfo;
begin
SelectedFile := ListBox1.Items.Strings[ListBox1.ItemIndex];
if FindFirst(SelectedFile, faAnyFile, Rec) = 0 then
 begin
  frInfo := TfrFileInfo.Create(Self);
  try
    frInfo.lblFile.Caption := SelectedFile;
    frInfo.lblname.Caption := Rec.name;
    frInfo.lblSize.Caption := Format('%d bytes',[Rec.Size]);
    frInfo.lblModified.Caption := DateToStr(FileDateToDateTime(Rec.Time));
    frInfo.lblShortName.Caption := Rec.FindData.cAlternateFileName;
    frInfo.ShowModal;
  finally
    frInfo.Free;
  end;
  FindClose(Rec)
 end;
end;

end.
