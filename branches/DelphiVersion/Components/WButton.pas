unit WButton;

interface

uses
  SysUtils, Classes, Controls, StdCtrls, WButtonBase, PNGImage, Windows,
  WComponent;

type
  TWButton = class(TWButtonBase)
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
  RegisterComponents('Samples', [TWImageButton]);
end;


procedure TWImageButton.FitToContent();
begin
	if pStateImageUp = nil then begin
  	Width := 0;
    Height := 0;
  end else begin
  	Width := pStateImageUp.Width;
    Height := pStateImageUp.Height;
  end;
end;


procedure TWImageButton.SetImagePathPrefix(const value: String);
begin
  FreeAndNil(pStateImageUp);
  FreeAndNil(pStateImageDown);
  FreeAndNil(pStateImageOver);

  pStateImageUp := TPNGObject.create();
  try
  	pStateImageUp.loadFromFile(value + 'Up.png');
  except
  	on E: Exception do begin
  		FreeAndNil(pStateImageUp);
  	end;
  end;

  pStateImageOver := TPNGObject.create();
  try
  	pStateImageOver.loadFromFile(value + 'Over.png');
  except
  	on E: Exception do begin
  		pStateImageOver := pStateImageUp;
  	end;
  end;

  pStateImageDown := TPNGObject.create();
  try
  	pStateImageDown.loadFromFile(value + 'Down.png');
  except
  	on E: Exception do begin
  		pStateImageDown := pStateImageUp;
  	end;
  end;

  Width := pStateImageUp.Width;
  Height := pStateImageUp.Height;
end;


procedure TWImageButton.Paint();
var imageToDraw: TPNGObject;
begin
	inherited Paint();

  imageToDraw := nil;

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

  if imageToDraw <> nil then Canvas.Draw(0, 0, imageToDraw);
      
  if pIconImage <> nil then begin
  	Canvas.Draw(Round((Width - pIconImage.Width) / 2), Round((Height - pIconImage.Height) / 2), pIconImage);
  end;
end;


procedure TWImageButton.SetIconImagePath(const Value: String);
begin
	pIconImagePath := value;
  if pIconImage <> nil then pIconImage.Free();

  pIconImage := TPNGObject.Create();
  pIconImage.LoadFromFile(pIconImagePath);
  
  Repaint();
end;


end.
