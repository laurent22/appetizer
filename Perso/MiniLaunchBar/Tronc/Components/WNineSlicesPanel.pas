unit WNineSlicesPanel;

interface

uses
  SysUtils, Classes, Controls, ExtCtrls, PNGImage, Imaging, Graphics, StdCtrls, Types,
  WComponent, Windows, Forms, Messages, WContainer;
  
type
  TWNineSlicesPanel = class(TWContainer)
  private
    { Private declarations }
    pImagePathPrefix: String;
    pBackgroundBitmap: TBitmap;
    pUpdateBitmap: Boolean;
    procedure SetImagePathPrefix(const Value: String);

  protected
    { Protected declarations }
    procedure Paint; override;


  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  published
    { Published declarations }
    property ImagePathPrefix: String read pImagePathPrefix write SetImagePathPrefix;    
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TWNineSlicesPanel]);
end;


constructor TWNineSlicesPanel.Create(AOwner: TComponent);
begin
  {Calls ancestor}
  inherited Create(AOwner);
  pUpdateBitmap := false;
end;


procedure TWNineSlicesPanel.Paint();
var bitmapRect: TRect;
begin
	inherited Paint;

	if (pUpdateBitmap) then begin
    pUpdateBitmap := false;
  end;

  Canvas.Brush.Style := bsClear;
  DrawNineSlices(Canvas, pImagePathPrefix, 0, 0, Width, Height);
end;



procedure TWNineSlicesPanel.SetImagePathPrefix(const value: String);
begin
	pImagePathPrefix := value;
  pUpdateBitmap := true;
  Repaint();
end;


end.


