unit ControlTest;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, ShellAPI,
  Dialogs, StdCtrls, ExtCtrls, PNGExtra, PNGImage, WImageButton, Imaging, WNineSlicesPanel,
  FileSystemUtils, WFileIcon, Menus, WContainer;

type
  TForm2 = class(TForm)
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.FormCreate(Sender: TObject);
var
 freakinPanel: TWNineSlicesPanel;
 p2: TWNineSlicesPanel;
  freakinButton: TWImageButton;
begin

	DoubleBuffered := true;

  freakinPanel := TWNineSlicesPanel.Create(self);
  freakinPanel.ImagePathPrefix := 'Data\Skin\Default\BarInnerPanel';
  freakinPanel.Visible := true;
  freakinPanel.Parent := self;
  freakinPanel.Width := 200;
  freakinPanel.Height := 200;
  freakinPanel.Left := 50;
  freakinPanel.Top := 50;

  p2 := TWNineSlicesPanel.Create(self);
  p2.ImagePathPrefix := 'Data\Skin\Default\BarInnerPanel';
  p2.Visible := true;
  //p2.ParentContainer := freakinPanel;
  p2.Width := 50;
  p2.Height := 50;
  p2.Left := 10;
  p2.Top := 10;


  //freakinPanel.AddChild(p2);

  freakinPanel.Left := 150;
  freakinPanel.Top := 150;

//  p2 := TWNineSlicesPanel.Create(self);
//  p2.ImagePathPrefix := 'Data\Skin\Default\BarInnerPanel';
//  p2.Left := 50;
//  p2.Top := 50;
//  p2.Width := 200;
//  p2.Height := 200;
//  p2.Visible := true;
//  p2.Parent := self;

//	p2 := TPanel.Create(self);
//  p2.Left := 50;
//  p2.Top := 50;
//  p2.Width := 200;
//  p2.Height := 200;
//  p2.Parent := self;


  freakinButton := TWImageButton.Create(self);
  freakinButton.ImagePathPrefix := 'Data\Skin\Default\OptionButton';
  freakinButton.Left := 0;
  freakinButton.Visible := true;

  freakinPanel.AddChild(freakinButton);
end;

end.
