/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#ifndef __BitmapControl_H
#define __BitmapControl_H


/**
 * This is the base control for all the skinable controls. In order to
 * simulate transparency, it always draw its parent before drawing itself.
 */
class BitmapControl: public wxPanel {

  public:

    BitmapControl(wxWindow *owner, int id, wxPoint point, wxSize size);
    ~BitmapControl();
    virtual wxBitmap* GetControlBitmap();

    int GetBitmapRotation();
    void SetBitmapRotation(int rotation);
    void SetHorizontalFlip(bool horizontalFlip);

    void InvalidateControlBitmap();
    virtual void UpdateControlBitmap();

    virtual void OnPaint(wxPaintEvent& evt);
    virtual void OnEraseBackground(wxEraseEvent &evt);
    virtual void OnSize(wxSizeEvent& evt);
    virtual void OnMove(wxMoveEvent& evt);

  protected:

    wxBitmap* controlBitmap_;
    bool controlBitmapInvalidated_;
    int bitmapRotation_;
    bool horizontalFlip_;
    bool hasBeenFlipped_;

  DECLARE_EVENT_TABLE()

};


#endif