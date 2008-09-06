unit WFileIcon;

interface

uses
  SysUtils, Classes, Controls, ExtCtrls, Windows, PNGImage,
  Graphics, StdCtrls, Types, WComponent, FileSystemUtils, Messages, Dialogs,
  Imaging;

type
  TWFileIcon = class(TWComponent)
  private
    { Private declarations }
    pFilePath: String;
    pFileIcon: TIcon;

    pOverlayImageUpSlices: TPNGNineSlices;
    pOverlayImageUpPath: String;
    pOverlayImageDownSlices: TPNGNineSlices;
    pOverlayImageDownPath: String;
    pOnFileIconClick: TNotifyEvent;
    pIconSize: Integer;

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
    //procedure ParentChange();
  public
    { Public declarations }
    procedure SetIconSize(const Value: Integer);
    property Icon: TIcon read pFileIcon;
    procedure ReloadIcon();
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DrawToCanvas(const targetCanvas: tCanvas; const x, y: Integer);
    procedure UpdateSize();
    property FilePath: String read pFilePath write SetFilePath;
    property OverlayImageUpPath: String read pOverlayImageUpPath write SetOverlayImageUpPath;
    property OverlayImageDownPath: String read pOverlayImageDownPath write SetOverlayImageDownPath;
    property OnFileIconClick: TNotifyEvent read pOnFileIconClick write pOnFileIconClick;
    property IconSize: Integer read pIconSize write SetIconSize;

  end;

procedure Register;

implementation


uses Main;


procedure Register;
begin
  RegisterComponents('Samples', [TWFileIcon]);
end;


constructor TWFileIcon.Create(AOwner: TComponent);
begin
  {Calls ancestor}
  inherited Create(AOwner);

  pOverlayImageUpPath := '';
  pIconSize := 32;

  UpdateSize();
end;


procedure TWFileIcon.ReloadIcon();
begin
  if pFileIcon <> nil then FreeAndNil(pFileIcon);
  pFileIcon := GetFolderItemIcon(pFilePath, pIconSize = 16);
  Invalidate();
end;


procedure TWFileIcon.SetFilePath(const value: String);
begin
	pFilePath := value;
  ReloadIcon();
end;


destructor TWFileIcon.Destroy();
begin
  inherited Destroy;

  if pFileIcon <> nil then pFileIcon.Free();
end;


procedure TWFileIcon.DrawToCanvas(const targetCanvas: tCanvas; const x, y: Integer);
var overlayToDraw: TPNGNineSlices;
    drawOverlay: Boolean;
begin
  drawOverlay := false;

  if ButtonState = pbsDown then begin
    overlayToDraw := pOverlayImageDownSlices;
    drawOverlay := true;
  end else begin
    if (IsMouseOver) then begin
      overlayToDraw := pOverlayImageUpSlices;
      drawOverlay := true;
    end;
  end;

  if drawOverlay then begin
    PNG_DrawNineSlices(Canvas, overlayToDraw, 0, 0, Width, Height);
  end;

  if pFileIcon <> nil then begin
  	targetCanvas.Brush.Style := bsClear;
    targetCanvas.Draw(x + Round((Width - pIconSize) / 2), y + Round((Height - pIconSize) / 2), pFileIcon);
  end;
end;


procedure TWFileIcon.Paint();
begin
	inherited Paint();
  DrawToCanvas(Canvas, 0, 0);
end;


procedure TWFileIcon.SetOverlayImageUpPath(const value: String);
var img: TPNGObject;
begin
  if pOverlayImageUpPath = value then Exit;

	pOverlayImageUpPath := value;
  if pOverlayImageUpSlices[0] <> nil then FreeAndNil(pOverlayImageUpSlices);

  img := TPNGObject.Create();
  try
    img.LoadFromFile(value);
    pOverlayImageUpSlices := PNG_ExtractNineSlices(img);
  except
    FreeAndNil(img);
  end;

  Invalidate();
end;


procedure TWFileIcon.UpdateSize;
begin
  Width := TMain.Instance.Style.Icon.paddingH + pIconSize;
  Height := TMain.Instance.Style.Icon.paddingV + pIconSize;
end;


procedure TWFileIcon.SetOverlayImageDownPath(const Value: String);
var img: TPNGObject;
begin
  if pOverlayImageDownPath = value then Exit;

	pOverlayImageDownPath := value;
  if pOverlayImageDownSlices[0] <> nil then FreeAndNil(pOverlayImageDownSlices);

  img := TPNGObject.Create();
  try
    img.LoadFromFile(value);
    pOverlayImageDownSlices := PNG_ExtractNineSlices(img);
  except
    FreeAndNil(img);
  end;

  Invalidate();
end;


procedure TWFileIcon.SetIconSize(const Value: Integer);
begin
  if value = pIconSize then Exit;

  pIconSize := value;
  ReloadIcon();
  UpdateSize();
  Invalidate();
end;


procedure TWFileIcon.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
	Invalidate();
end;



procedure TWFileIcon.FileIconClick();
begin
	if Assigned(pOnFileIconClick) then pOnFileIconClick(Self);
end;


procedure TWFileIcon.Click();
begin
	inherited Click;
  Invalidate();
end;


procedure TWFileIcon.MouseEnter();
begin
	inherited MouseEnter();
	Invalidate();
end;


procedure TWFileIcon.MouseLeave();
begin
	inherited MouseLeave();
	Invalidate();
end;



end.
