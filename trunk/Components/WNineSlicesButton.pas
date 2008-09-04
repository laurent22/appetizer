unit WNineSlicesButton;

interface

uses
  SysUtils, Classes, Controls, StdCtrls, WButtonBase, PNGImage, Windows,
  WComponent, Imaging, Graphics;

type


  TStateNineSlices = record
    Up: TPNGNineSlices;
    Down: TPNGNineSlices;
    Over: TPNGNineSlices;
    Disabled: TPNGNineSlices;
  end;



  TWNineSlicesButton = class(TWButtonBase)

  private

    pImagePathPrefix: String;
    pIconImagePath: String;
    pIconImage: TPNGObject;
    pImagePath: String;
    pUpdateNineSlices: Boolean;
    pNineSlices: TStateNineSlices;
    pNineSlicesGrid: TRect;
    pNineSlicesGridExplicitelySet: Boolean;

    procedure ClearNineSlices();
    procedure SetImagePathPrefix(const Value: String);
    procedure SetIconImagePath(const Value: String);  
    procedure SetImagePath(const Value: String);
    procedure SetNineSlicesGrid(const Value: TRect);

  protected

    procedure Paint; override;

  public

    procedure FitToContent();
    property NineSlicesGrid: TRect read pNineSlicesGrid write SetNineSlicesGrid;
    property ImagePathPrefix: String read pImagePathPrefix write SetImagePathPrefix;
    property ImagePath: String read pImagePath write SetImagePath;
    property IconImagePath: String read pIconImagePath write SetIconImagePath;

    constructor Create(AOwner: TComponent); override;
    
  end;

procedure Register;

implementation   


procedure Register;
begin
  RegisterComponents('Samples', [TWNineSlicesButton]);
end;


procedure TWNineSlicesButton.ClearNineSlices;
var i: Integer;
begin
  for i := 0 to Length(pNineSlices.Up) - 1 do FreeAndNil(pNineSlices.Up[i]);
  for i := 0 to Length(pNineSlices.Over) - 1 do FreeAndNil(pNineSlices.Over[i]);
  for i := 0 to Length(pNineSlices.Down) - 1 do FreeAndNil(pNineSlices.Down[i]);
  for i := 0 to Length(pNineSlices.Disabled) - 1 do FreeAndNil(pNineSlices.Disabled[i]);
end;


constructor TWNineSlicesButton.Create(AOwner: TComponent);
begin
  inherited;
  pNineSlicesGridExplicitelySet := false;
end;


procedure TWNineSlicesButton.FitToContent();
begin
  // NOT IMPLEMENTED
	Exit;
end;


procedure TWNineSlicesButton.SetImagePath(const Value: String);
begin
  if value = pImagePath then Exit;

  ClearNineSlices();

  pImagePath := value;
  Invalidate();
end;


procedure TWNineSlicesButton.SetImagePathPrefix(const value: String);
var i: Integer;
begin
  if value = pImagePathPrefix then Exit;

  for i := 0 to Length(pNineSlices.Up) - 1 do FreeAndNil(pNineSlices.Up[i]);
  for i := 0 to Length(pNineSlices.Over) - 1 do FreeAndNil(pNineSlices.Over[i]);
  for i := 0 to Length(pNineSlices.Down) - 1 do FreeAndNil(pNineSlices.Down[i]);
  for i := 0 to Length(pNineSlices.Disabled) - 1 do FreeAndNil(pNineSlices.Disabled[i]);

  pImagePathPrefix := value;
  Invalidate();
end;


procedure TWNineSlicesButton.SetNineSlicesGrid(const Value: TRect);
begin
  pNineSlicesGridExplicitelySet := true;
  pNineSlicesGrid := value;
  ClearNineSlices();
  Invalidate();
end;


procedure TWNineSlicesButton.Paint();
var path: String;
    i: Integer;
    sourceImage: TPNGObject;
    nineSlices: TPNGNineSlices;
    sourceImagePath: String;
begin
	inherited Paint();

  if pImagePathPrefix <> '' then begin

    case ButtonState of
      pbsOver: nineSlices := pNineSlices.Over;
      pbsDown: nineSlices := pNineSlices.Down;
      else nineSlices := pNineSlices.Up;
    end;

    if nineSlices[0] = nil then begin

      case ButtonState of
        pbsOver: sourceImagePath := pImagePathPrefix + 'Over.png';
        pbsDown: sourceImagePath := pImagePathPrefix + 'Down.png';
        else sourceImagePath := pImagePathPrefix + 'Up.png';
      end;

      sourceImage := TPNGObject.Create();

      try                                                 
        sourceImage.LoadFromFile(sourceImagePath);

        if pNineSlicesGridExplicitelySet then
          nineSlices := PNG_ExtractNineSlices(sourceImage, pNineSlicesGrid)
        else
          nineSlices := PNG_ExtractNineSlices(sourceImage);
      finally
        FreeAndNil(sourceImage);
      end;

    end;


    if nineSlices[0] <> nil then begin
      Canvas.Brush.Style := bsClear;
      PNG_DrawNineSlices(Canvas, nineSlices, 0, 0, Width, Height);
    end;

  end;


  if pIconImage <> nil then begin
  	Canvas.Draw(Round((Width - pIconImage.Width) / 2), Round((Height - pIconImage.Height) / 2), pIconImage);
  end;
  
end;


procedure TWNineSlicesButton.SetIconImagePath(const Value: String);
begin
	pIconImagePath := value;
  if pIconImage <> nil then pIconImage.Free();

  pIconImage := TPNGObject.Create();
  pIconImage.LoadFromFile(pIconImagePath);
  
  Invalidate();
end;


end.
