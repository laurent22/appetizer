unit WImageButton;

interface

uses
  SysUtils, Classes, Controls, StdCtrls, WButtonBase, PNGImage, Windows,
  WComponent;

type
  TWImageButton = class(TWButtonBase)
  private
    { Private declarations }
    pImagePathPrefix: String;
    pStateImageUp: TPNGObject;
    pStateImageDown: TPNGObject;
    pStateImageOver: TPNGObject;
    pIconImagePath: String;
    pIconImage: TPNGObject;
    procedure SetImagePathPrefix(const Value: String);
    procedure SetIconImagePath(const Value: String);
  protected
    { Protected declarations }
    procedure Paint; override;
  public
    { Public declarations }
  published
    { Published declarations }
    property ImagePathPrefix: String read pImagePathPrefix write SetImagePathPrefix;
    property IconImagePath: String read pIconImagePath write SetIconImagePath;
  end;

procedure Register;

implementation



procedure Register;
begin
  RegisterComponents('Samples', [TWImageButton]);
end;


procedure TWImageButton.SetImagePathPrefix(const value: String);
begin
  if pStateImageUp <> nil then pStateImageUp.Free;
  if pStateImageDown <> nil then pStateImageDown.Free;
  if pStateImageOver <> nil then pStateImageOver.Free;

  pStateImageUp := TPNGObject.create();
  pStateImageUp.loadFromFile(value + 'Up.png');

  pStateImageOver := TPNGObject.create();
  pStateImageOver.loadFromFile(value + 'Over.png');

  pStateImageDown := TPNGObject.create();
  pStateImageDown.loadFromFile(value + 'Down.png');

//  ImageNormal := pStateImageUp;
//  ImageOver := pStateImageOver;
//  ImageDown := pStateImageDown;

  Width := pStateImageUp.Width;
  Height := pStateImageUp.Height;
end;


procedure TWImageButton.Paint();
var rect: TRect;
	imageToDraw: TPNGObject;
begin
	inherited Paint();

  if Enabled then begin

    case (ButtonState) of

      pbsNormal: begin

        imageToDraw := pStateImageUp;
        end;

      pbsOver: begin

        imageToDraw := pStateImageOver;
        end;

      pbsDown: begin

        imageToDraw := pStateImageDown;
        end;

    end;

  end else begin

  	imageToDraw := pStateImageUp;

  end;

  Canvas.Draw(0, 0, imageToDraw);
      
  if pIconImage <> nil then begin
  	Canvas.Draw(Round((Width - pIconImage.Width) / 2), Round((Height - pIconImage.Height) / 2), pIconImage);
  end;
end;


procedure TWImageButton.SetIconImagePath(const Value: String);
var i, j, i2: Integer;
  tempPNG : TPNGObject;
begin
	pIconImagePath := value;
  if pIconImage <> nil then pIconImage.Free();

  pIconImage := TPNGObject.Create();
  pIconImage.LoadFromFile(pIconImagePath);
  
  Repaint();
end;


end.
