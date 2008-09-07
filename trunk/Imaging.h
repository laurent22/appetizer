#include "wx/wx.h" 
#include "wx/dcbuffer.h"

class Imaging {

public:
  
  static void StretchBlit(wxDC* destDC, wxDC* sourceDC, wxCoord destX, wxCoord destY, wxCoord destWidth, wxCoord destHeight, wxCoord srcX, wxCoord srcY, wxCoord srcWidth, wxCoord srcHeight);

};