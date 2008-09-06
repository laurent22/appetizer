unit WImage;

interface

uses WComponent, PNGImage, Graphics, Windows, Classes, Imaging, Dialogs;

type

	TWImage = class(TWComponent)

  	private

    	pStretchToFit: Boolean;
      pFilePath: String;
      pMaintainAspectRatio: Boolean;
      pPNGObject: TPNGObject;          

      procedure SetStretchToFit(value:Boolean);
      procedure SetMaintainAspectRatio(value:Boolean);
      procedure SetFilePath(value:String);

    protected

    	procedure Paint; override;
    
    public 

    	property ImageObject: TPNGObject read pPNGObject;
    	property StretchToFit:Boolean read pStretchToFit write SetStretchToFit;
      property MaintainAspectRatio:Boolean read pMaintainAspectRatio write SetMaintainAspectRatio;
      property FilePath:String read pFilePath write SetFilePath;

      procedure FitToContent();
      constructor Create(AOwner: TComponent); override;
      
	end;


implementation


constructor TWImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  pMaintainAspectRatio := true;
	pStretchToFit := false;
  pFilePath := '';
end;


procedure TWImage.FitToContent();
begin
	if pPNGObject = nil then begin
  	Width := 0;
    Height := 0;
  end else begin
  	Width := pPNGObject.Width;
    Height := pPNGObject.Height;
  end;
end;


procedure TWImage.SetMaintainAspectRatio(value:Boolean);
begin
	if value = pMaintainAspectRatio then Exit;

  pMaintainAspectRatio := value;
  Invalidate();
end;


procedure TWImage.SetStretchToFit(value:Boolean);
begin
	if value = pStretchToFit then Exit;

	pStretchToFit := value;
	Invalidate();
end;

  
procedure TWImage.SetFilePath(value:String);
begin
	if pFilePath = value then Exit;

  if pPNGObject <> nil then begin
  	pPNGObject.Free();
    pPNGObject := nil;
  end;
  
	pFilePath := value;

  pPNGObject := TPNGObject.Create();  
  pPNGObject.LoadFromFile(pFilePath);
  
	Invalidate();
end;



procedure TWImage.Paint();
var rect: TRect;
	size: TSize;
begin

	if (pFilePath = '') or (pPNGObject = nil) or (Width = 0) or (Height = 0) then begin
  	{ TODO : Clear image }	
  end else begin

  	Canvas.Brush.Style := bsClear;

    if StretchToFit then begin      

      if pMaintainAspectRatio then begin
    		size := ConstraintResize(pPNGObject.Width, pPNGObject.Height, Width, Height, true);
        rect.Left := Round((Width - size.cx) / 2);
        rect.Top := Round((Height - size.cy) / 2);
        rect.Right := rect.Left + size.cx;
        rect.Bottom := rect.Top + size.cy;
      	Canvas.StretchDraw(rect, pPNGObject);
      end else begin
        rect.Top := 0;
        rect.Left := 0;
        rect.Right := Width;
        rect.Bottom := Height;
      	Canvas.StretchDraw(rect, pPNGObject);
      end;

    end else begin

    	if pMaintainAspectRatio then begin
        size := ConstraintResize(pPNGObject.Width, pPNGObject.Height, Width, Height, false);
        rect.Left := Round((Width - size.cx) / 2);
        rect.Top := Round((Height - size.cy) / 2);
        rect.Right := rect.Left + size.cx;
        rect.Bottom := rect.Top + size.cy;
      	Canvas.Draw(rect.Left, rect.Top, pPNGObject);
      end else begin
        Canvas.Draw(
          Round((Width - pPNGObject.Width) / 2),
          Round((Height - pPNGObject.Height) / 2),
          pPNGObject
        );
      end;
    end;           
  	
  end;
end;


end.
