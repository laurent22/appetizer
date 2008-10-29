#include "Imaging.h"

void Imaging::StretchBlit(wxDC* destDC, wxDC* sourceDC, wxCoord destX, wxCoord destY, wxCoord destWidth, wxCoord destHeight, wxCoord srcX, wxCoord srcY, wxCoord srcWidth, wxCoord srcHeight) {
  if (destWidth <= 0 || destHeight <= 0 || srcWidth <= 0 || srcHeight <= 0) return;
  
  double saveScaleX, saveScaleY;
  sourceDC->GetUserScale(&saveScaleX, &saveScaleY);

  double scaleX = (double)srcWidth / (double)destWidth;
  double scaleY = (double)srcHeight / (double)destHeight;

  sourceDC->SetUserScale(scaleX, scaleY);
  
  destDC->Blit(destX, destY, destWidth, destHeight, sourceDC, srcX / scaleX, srcY / scaleY);

  sourceDC->SetUserScale(saveScaleX, saveScaleY);
}
