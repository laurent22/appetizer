unit WFileIcon;

interface

uses
  SysUtils, Classes, Controls, ExtCtrls, Windows, PNGImage,
  Graphics, StdCtrls, Types, WComponent, FileSystemUtils, Messages, Dialogs;

type
  TWFileIcon = class(TWComponent)
  private
    { Private declarations }
    pFilePath: String;
    pFileIcon: TIcon;
    pOverlayImage: TPNGObject;
    pOverlayImagePath: String;
    pOnFileIconClick: TNotifyEvent;

    procedure SetFilePath(const Value: String);
    procedure SetOverlayImagePath(const Value: String);

  protected
    { Protected declarations }
    procedure FileIconClick; dynamic;
    procedure Paint; override;
    procedure Click; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseEnter(); override;
    procedure MouseLeave(); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property FilePath: String read pFilePath write SetFilePath;
    property OverlayImagePath: String read pOverlayImagePath write SetOverlayImagePath;
    property OnFileIconClick: TNotifyEvent read pOnFileIconClick write pOnFileIconClick;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TWFileIcon]);
end;


constructor TWFileIcon.Create(AOwner: TComponent);
begin
  {Calls ancestor}
  inherited Create(AOwner);

  pOverlayImagePath := '';
  Width := 32;
  Height := 32;
end;


destructor TWFileIcon.Destroy();
begin
  inherited Destroy;

  if pFileIcon <> nil then pFileIcon.Free();
end;


procedure TWFileIcon.Paint();
var
	rect: TRect;
begin
  if pFileIcon <> nil then begin

  	if (IsMouseOver) and (pOverlayImagePath <> '') then begin
    	if pOverlayImage = nil then begin
      	pOverlayImage := TPNGObject.Create();
        pOverlayImage.LoadFromFile(pOverlayImagePath);
      end;

      Canvas.Draw(0, 0, pOverlayImage);
    end;

 	 Canvas.Draw(Round((Width - pFileIcon.Width) / 2), Round((Height - pFileIcon.Height) / 2), pFileIcon);



  end;
end;


procedure TWFileIcon.SetOverlayImagePath(const value: String);
begin
	pOverlayImagePath := value;
end;


procedure TWFileIcon.SetFilePath(const value: String);
begin
	pFilePath := value;

  if pFileIcon <> nil then pFileIcon.Free();

  pFileIcon := GetExecutableLargeIcon(pFilePath);

  if pFileIcon = nil then pFileIcon := GetExecutableLargeIcon(pFilePath);

  Repaint();
end;


procedure TWFileIcon.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
	//Repaint();
end;



procedure TWFileIcon.FileIconClick();
begin
	if Assigned(pOnFileIconClick) then pOnFileIconClick(Self);
end;


procedure TWFileIcon.Click();
begin
	inherited Click;
end;


procedure TWFileIcon.MouseEnter();
begin
	inherited MouseEnter();
	Repaint();
end;


procedure TWFileIcon.MouseLeave();
begin
	inherited MouseLeave();
	Repaint();
end;



end.
