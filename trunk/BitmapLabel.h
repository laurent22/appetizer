/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#ifndef __BitmapLabel_H
#define __BitmapLabel_H


#include "BitmapControl.h"

class BitmapLabel: public BitmapControl {

  public:

    BitmapLabel(wxWindow *owner, int id = wxID_ANY, wxPoint point = wxDefaultPosition, wxSize size = wxDefaultSize);
    void UpdateControlBitmap();
    void FitToContent();

  private:

    wxString filePath_;
    wxBitmap bitmap_;

};


#endif