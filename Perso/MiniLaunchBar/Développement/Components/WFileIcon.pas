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

    pOverlayImageUp: TPNGObject;
    pOverlayImageUpPath: String;
    pOverlayImageDown: TPNGObject;
    pOverlayImageDownPath: String;
    pOnFileIconClick: TNotifyEvent;

    procedure SetFilePath(const Value: String);
    procedure SetOverlayImageUpPath(const Value: String);
    procedure SetOverlayImageDownPath(const Value: String);

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
    property OverlayImageUpPath: String read pOverlayImageUpPath write SetOverlayImageUpPath;
    property OverlayImageDownPath: String read pOverlayImageDownPath write SetOverlayImageDownPath;
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

  pOverlayImageUpPath := '';
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
  overlayToDraw: TPNGObject;
begin

	overlayToDraw := nil;

  if ButtonState = pbsDown then begin
    if (pOverlayImageDownPath <> '') then begin
      if pOverlayImageDown = nil then begin
        pOverlayImageDown := TPNGObject.Create();
        pOverlayImageDown.LoadFromFile(pOverlayImageDownPath);
      end;

      overlayToDraw := pOverlayImageDown;
    end;
  end else begin
    if (IsMouseOver) and (pOverlayImageUpPath <> '') then begin
      if pOverlayImageUp = nil then begin
        pOverlayImageUp := TPNGObject.Create();
        pOverlayImageUp.LoadFromFile(pOverlayImageUpPath);
      end;

      overlayToDraw := pOverlayImageUp;
    end;
  end;

  if overlayToDraw <> nil then begin
  	Canvas.Draw(0, 0, overlayToDraw);
  end;

  

  if pFileIcon <> nil then begin

    Canvas.Draw(Round((Width - pFileIcon.Width) / 2), Round((Height - pFileIcon.Height) / 2), pFileIcon);

  end;
end;


procedure TWFileIcon.SetOverlayImageUpPath(const value: String);
begin
	pOverlayImageUpPath := value;
end;


procedure TWFileIcon.SetOverlayImageDownPath(const Value: String);
begin
	pOverlayImageDownPath := value;
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
	Repaint();
end;



procedure TWFileIcon.FileIconClick();
begin
	if Assigned(pOnFileIconClick) then pOnFileIconClick(Self);
end;


procedure TWFileIcon.Click();
begin
	inherited Click;
  Repaint();
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
