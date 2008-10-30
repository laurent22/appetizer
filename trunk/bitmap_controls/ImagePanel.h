/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#ifndef __ImagePanel_H
#define __ImagePanel_H

#include "wx/wx.h"
#include "BitmapControl.h"

class ImagePanel: public BitmapControl {

  public:

    ImagePanel(wxWindow *owner, int id, wxPoint point, wxSize size);
    void LoadImage(const wxString& filePath);
    void UpdateControlBitmap();
    void FitToContent();

  private:

    wxString filePath_;
    wxBitmap bitmap_;

};


#endif