#ifndef __NineSlicesPainter_H
#define __NineSlicesPainter_H

#include "wx/wx.h" 
#include "Imaging.h"


class NineSlicesPainter {

  private:

    wxRect pGrid;
    wxString pFilePath;
    bool pGridIsExplicitelySet;
    wxBitmap pSourceBitmap;
    wxMemoryDC pSourceDC;
    
  public:

    NineSlicesPainter();
    void LoadImage(const wxString& filePath);
    void SetGrid(wxRect *grid);
    void Draw(wxDC *destDC, wxCoord x, wxCoord y, wxCoord width, wxCoord height);

};

#endif