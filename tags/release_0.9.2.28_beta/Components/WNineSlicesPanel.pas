unit WNineSlicesPanel;

interface

uses
  SysUtils, Classes, Controls, ExtCtrls, PNGImage, Imaging, Graphics, StdCtrls, Types,
  WComponent, Windows, Forms, Messages, WContainer, Logger;
  
type
  TWNineSlicesPanel = class(TWContainer)

  private

    pUpdateSlices: Boolean;
    pNineSlices: TPNGNineSlices;
    pImagePath: String;

    procedure SetImagePath(const Value: String);

  protected

    procedure Paint; override;

  public

    constructor Create(AOwner: TComponent); override;
    property ImagePath: String read pImagePath write SetImagePath;

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
  pUpdateSlices := false;
end;


procedure TWNineSlicesPanel.Paint();
var sourceImage: TPNGObject;
    i: Integer;
begin
	inherited Paint;

  if pImagePath = '' then Exit;

	if (pUpdateSlices) then begin

    for i := 0 to Length(pNineSlices) - 1 do begin
      FreeAndNil(pNineSlices[i]);
    end;

    sourceImage := TPNGObject.Create();

    try                                                 
      sourceImage.LoadFromFile(pImagePath);
      pNineSlices := PNG_ExtractNineSlices(sourceImage);
    finally
      FreeAndNil(sourceImage);
    end;

    pUpdateSlices := false;
  end;

  if pNineSlices[0] = nil then Exit;

  Canvas.Brush.Style := bsClear;

  PNG_DrawNineSlices(Canvas, pNineSlices, 0, 0, Width, Height);

  //DrawNineSlices_OLD(Canvas, pImagePathPrefix, 0, 0, Width, Height);
end;


procedure TWNineSlicesPanel.SetImagePath(const Value: String);
begin
  if pImagePath = value then Exit;

  pImagePath := value;
  pUpdateSlices := true;
  Invalidate();
end;


end.


