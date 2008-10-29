unit WNineSlicesButton;

interface

uses
  SysUtils, Classes, Controls, StdCtrls, WButtonBase, PNGImage, Windows,
  WComponent, Imaging;

type
  TWNineSlicesButton = class(TWButtonBase)
  private
    { Private declarations }
    pImagePathPrefix: String;
    pIconImagePath: String;
    pIconImage: TPNGObject;
    procedure SetImagePathPrefix(const Value: String);
    procedure SetIconImagePath(const Value: String);
  protected
    { Protected declarations }
    procedure Paint; override;
  public
    { Public declarations }
    procedure FitToContent();
  published
    { Published declarations }
    property ImagePathPrefix: String read pImagePathPrefix write SetImagePathPrefix;
    property IconImagePath: String read pIconImagePath write SetIconImagePath;
  end;

procedure Register;

implementation



procedure Register;
begin
  RegisterComponents('Samples', [TWNineSlicesButton]);
end;


procedure TWNineSlicesButton.FitToContent();
begin
	Exit;
end;


procedure TWNineSlicesButton.SetImagePathPrefix(const value: String);
begin
  pImagePathPrefix := value;
  Invalidate();
end;


procedure TWNineSlicesButton.Paint();
var path: String;
begin
	inherited Paint();

  if pImagePathPrefix <> '' then begin
    path := pImagePathPrefix + 'Up';
    if ButtonState = pbsOver then path := pImagePathPrefix + 'Over';
		if ButtonState = pbsDown then path := pImagePathPrefix + 'Down';

  	DrawNineSlices(Canvas, path, 0, 0, Width, Height);
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
