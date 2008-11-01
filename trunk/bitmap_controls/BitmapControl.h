/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#ifndef __BitmapControl_H
#define __BitmapControl_H

#include "wx/wx.h"


/**
 * This is the base control for all the skinable controls. In order to
 * simulate transparency, it always draw its parent before drawing itself.
 */
class BitmapControl: public wxPanel {

  public:

    /**
     * Constructor
     */
    BitmapControl(wxWindow *owner, int id, wxPoint point, wxSize size);
    
    /**
     * Destructor
     */
    ~BitmapControl();

    /**
     * This control bitmap
     */
    wxBitmap* GetControlBitmap();

    int GetBitmapRotation();
    void SetBitmapRotation(int rotation);
    void SetHorizontalFlip(bool horizontalFlip);

    /**
     * By calling this function, the control bitmap is going
     * to be redrawn on the next call to UpdateControlBitmap()
     */
    void InvalidateControlBitmap();

    /**
     * This is where the control bitmap should be drawn. Classes
     * which inherit from this class, should override this method.
     */
    virtual void UpdateControlBitmap();

    /**
     * PAINT event.
     */
    virtual void OnPaint(wxPaintEvent& evt);

    /**
     * ERASE_BACKGROUND event.
     */
    virtual void OnEraseBackground(wxEraseEvent &evt);

    /**
     * SIZE event.
     */
    virtual void OnSize(wxSizeEvent& evt);

    /**
     * MOVE event.
     */
    virtual void OnMove(wxMoveEvent& evt);

  protected:

    /**
     * This control bitmap
     */
    wxBitmap* controlBitmap_;

    /**
     * Tells whether the bitmap must be redrawn or not
     */
    bool controlBitmapInvalidated_;
    int bitmapRotation_;
    bool horizontalFlip_;
    bool hasBeenFlipped_;

  DECLARE_EVENT_TABLE()

};


#endif