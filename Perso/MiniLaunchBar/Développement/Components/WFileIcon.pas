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
    //procedure ParentChange();
  public
    { Public declarations }
    property Icon: TIcon read pFileIcon;
    procedure ReloadIcon();
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DrawToCanvas(const targetCanvas: tCanvas; const x, y: Integer);
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



//procedure TWFileIcon.ParentChange();
//begin
//	ButtonState := pbsNormal;
//  Invalidate();
//end;


procedure TWFileIcon.ReloadIcon();
begin
  if pFileIcon <> nil then FreeAndNil(pFileIcon);
  pFileIcon := GetFolderItemIcon(pFilePath, false);  
  Invalidate();
end;


destructor TWFileIcon.Destroy();
begin
  inherited Destroy;

  if pFileIcon <> nil then pFileIcon.Free();
end;


procedure TWFileIcon.DrawToCanvas(const targetCanvas: tCanvas; const x, y: Integer);
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
  	targetCanvas.Draw(x, y, overlayToDraw);
  end;

  if pFileIcon <> nil then begin
  	targetCanvas.Brush.Style := bsClear;
    targetCanvas.Draw(x + Round((Width - pFileIcon.Width) / 2), y + Round((Height - pFileIcon.Height) / 2), pFileIcon);
  end;
end;


procedure TWFileIcon.Paint();
begin
	inherited Paint();
  DrawToCanvas(Canvas, 0, 0);
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

  if pFileIcon <> nil then FreeAndNil(pFileIcon);

  pFileIcon := GetFolderItemIcon(pFilePath, false);

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
