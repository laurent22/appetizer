unit Imaging;

interface

uses
	Windows, Graphics, PNGImage, SysUtils, Types;

const
 WS_EX_LAYERED = $80000;
 LWA_COLORKEY = 1;
 LWA_ALPHA    = 2;

type

 TSetLayeredWindowAttributes = function (
     hwnd : HWND;         // handle to the layered window
     crKey : TColor;      // specifies the color key
     bAlpha : byte;       // value for the blend function
     dwFlags : DWORD      // action
     ): BOOL; stdcall;

	TRGBArray = array[0..32767] of TRGBTriple;
  PRGBArray = ^TRGBArray;

  procedure DrawNineSlices(const canvas: TCanvas; const filePathPrefix: String; const x, y, targetWidth, targetHeight: Integer);
  function CreateRegion(Bmp: TBitmap): THandle;
  function ConstraintResize(sourceWidth, sourceHeight, targetWidth, targetHeight: Real; fitSmallerDimensions: Boolean):TSize;
  procedure SetTransparentForm(AHandle : THandle; AValue : byte = 0);

implementation



// By serge_perevoznyk@hotmail.com
// http://users.telenet.be/ws36637/transparent1.html

procedure SetTransparentForm(AHandle : THandle; AValue : byte = 0);
var
 Info: TOSVersionInfo;
 SetLayeredWindowAttributes: TSetLayeredWindowAttributes;
begin
 //Check Windows version
 Info.dwOSVersionInfoSize := SizeOf(Info);
 GetVersionEx(Info);
 if (Info.dwPlatformId = VER_PLATFORM_WIN32_NT) and
 (Info.dwMajorVersion >= 5) then
   begin
     SetLayeredWindowAttributes := GetProcAddress(GetModulehandle(user32), 'SetLayeredWindowAttributes');
      if Assigned(SetLayeredWindowAttributes) then
       begin
        SetWindowLong(AHandle, GWL_EXSTYLE, GetWindowLong(AHandle, GWL_EXSTYLE) or WS_EX_LAYERED);
        //Make form transparent
        SetLayeredWindowAttributes(AHandle, 0, AValue, LWA_ALPHA);
      end;
   end;
end;


// By Gerard Oei
// http://www.delphi-central.com/BitmapShapedForm.aspx

function CreateRegion(Bmp: TBitmap): THandle;
var
  X, Y, StartX:Integer;
  Excl: THandle;
  Row: PRGBArray;
  TransparentColor: TRGBTriple;
begin
  // Change the format so we know how to compare 
  // the colors 
  Bmp.PixelFormat := pf24Bit;
    
  // Create a region of the whole bitmap 
  // later we will take the transparent   
  // bits away
  Result := CreateRectRGN(0, 0, Bmp.Width, Bmp.Height);

  // Loop down the bitmap   
  for Y := 0 to Bmp.Height - 1 do
  begin
    // Get the current row of pixels
    Row := Bmp.Scanline[Y];

    // If its the first get the transparent
    // color, it must be the top left pixel
    if Y = 0 then
    begin
      TransparentColor := Row[0];
    end;

    // Reset StartX (-1) to indicate we have
    // not found a transparent area yet
    StartX := -1;

    // Loop across the row
    for X := 0 to Bmp.Width do
    begin

      // Check for transparency by comparing the color
      if(X <> Bmp.Width) and
        (Row[X].rgbtRed = TransparentColor.rgbtRed) and
        (Row[X].rgbtGreen = TransparentColor.rgbtGreen) and
        (Row[X].rgbtBlue = TransparentColor.rgbtBlue) then
      begin
        // We have (X <> Bmp.Width) in the clause so that
        // when we go past the end of the row we we can
        // exclude the remaining transparent area (if any)
        // If its transparent and the previous wasn't
        // remember were the transparency started
        if StartX = -1 then
        begin
          StartX := X;
        end;
      end
      else
      begin
        // Its not transparent
        if StartX > -1 then
        begin
          // If previous pixels were transparent we
          // can now exclude the from the region
          Excl := CreateRectRGN(StartX, Y, X, Y + 1);
          try
            // Remove the exclusion from our original region
            CombineRGN(Result, Result, Excl, RGN_DIFF);

            // Reset StartX so we can start searching
            // for the next transparent area
            StartX := -1;
          finally
            DeleteObject(Excl);
          end;
     end;
      end;
    end;
  end; 
end;





function ConstraintResize(sourceWidth, sourceHeight, targetWidth, targetHeight: Real; fitSmallerDimensions: Boolean):TSize;
var targetRatio, sourceRatio: Real;
begin
  result.cx := Round(sourceWidth);
  result.cy := Round(sourceHeight);

  if (targetWidth = 0) or (targetHeight = 0) then Exit;
  if (sourceWidth = 0) or (sourceHeight = 0) then Exit;

	if (sourceWidth <= targetWidth) and (sourceHeight <= targetHeight) then begin

  	if fitSmallerDimensions then begin
    	targetRatio := targetWidth / targetHeight;
      sourceRatio := sourceWidth / sourceHeight;

      if sourceRatio < targetRatio then begin
      	result.cy := Round(targetHeight);
        result.cx := Round(result.cy * sourceRatio);
      end else begin
      	result.cx := Round(targetWidth);
        result.cy := Round(result.cx / sourceRatio);
      end;
    end;

  end else begin

    targetRatio := targetWidth / targetHeight;
    sourceRatio := sourceWidth / sourceHeight;

    if sourceRatio < targetRatio then begin
      result.cy := Round(targetHeight);
      result.cx := Round(result.cy * sourceRatio);
    end else begin
      result.cx := Round(targetWidth);
      result.cy := Round(result.cx / sourceRatio);
    end;
    
  end;

end;



procedure DrawNineSlices(const canvas: TCanvas; const filePathPrefix: String; const x, y, targetWidth, targetHeight: Integer);
var
	cornerSize: Integer;
	i, j: Byte;
  filePath: String;
  pngImage: TPNGObject;
  targetRect: TRect;
begin

	cornerSize := 0;
	targetRect.Left := 0;
  targetRect.Top := 0;

  for j := 0 to 2 do begin

  	for i := 0 to 2 do begin   

      filePath := filePathPrefix + IntToStr(i) + IntToStr(j) + '.png';
      pngImage := TPNGObject.Create();

      pngImage.LoadFromFile(filePath);

      if (cornerSize = 0) then cornerSize := pngImage.Width;

      case i of

      	0: begin
        	targetRect.Left := 0;
        	targetRect.Right := cornerSize;
        end;

        1: begin
          targetRect.Left := cornerSize;
        	targetRect.Right := targetWidth - cornerSize;
        end;

        2: begin
          targetRect.Left := targetWidth - cornerSize;
        	targetRect.Right := targetWidth;
        end;
        
      end;

      case j of

      	0: begin
        	targetRect.Top := 0;
        	targetRect.Bottom := cornerSize;
        end;

        1: begin
          targetRect.Top := cornerSize;
        	targetRect.Bottom := targetHeight - cornerSize;
        end;

        2: begin
          targetRect.Top := targetHeight - cornerSize;
        	targetRect.Bottom := targetHeight;
        end;
        
      end;

      targetRect.Left := targetRect.Left + x;
      targetRect.Right := targetRect.Right + x;
      targetRect.Top := targetRect.Top + y;
      targetRect.Bottom := targetRect.Bottom + y;

      canvas.StretchDraw(targetRect, pngImage);
      pngImage.Free();
      
    end;
  end;



end;


end.
