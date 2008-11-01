/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#ifndef __NineSlicesPainter_H
#define __NineSlicesPainter_H

#include "wx/wx.h" 
#include "Imaging.h"


class NineSlicesPainter {

  private:

    wxRect grid_;
    wxString filePath_;
    bool gridIsExplicitelySet_;
    wxBitmap* sourceBitmap_;
    wxBitmap* rotatedSourceBitmap_;
    wxMemoryDC sourceDC_;
    int rotation_;
    
  public:

    NineSlicesPainter();
    ~NineSlicesPainter();
    void LoadImage(const wxString& filePath, bool forceAlpha = true);
    void SetGrid(int left, int top, int width, int height);
    void Draw(wxDC *destDC, wxCoord x, wxCoord y, wxCoord width, wxCoord height);
    int GetRotation();
    void SetRotation(int rotation);

};

#endif