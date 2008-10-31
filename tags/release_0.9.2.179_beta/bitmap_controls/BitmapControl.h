/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#ifndef __BitmapControl_H
#define __BitmapControl_H

#include "wx/wx.h"

class BitmapControl: public wxPanel {

  public:

    BitmapControl(wxWindow *owner, int id, wxPoint point, wxSize size);
    ~BitmapControl();

    wxBitmap* GetControlBitmap();
    void InvalidateControlBitmap();
    virtual void UpdateControlBitmap();

    virtual void OnPaint(wxPaintEvent& evt);
    virtual void OnEraseBackground(wxEraseEvent &evt);
    virtual void OnSize(wxSizeEvent& evt);
    virtual void OnMove(wxMoveEvent& evt);

  protected:

    wxBitmap* controlBitmap_;
    bool controlBitmapInvalidated_;

  DECLARE_EVENT_TABLE()

};


#endif