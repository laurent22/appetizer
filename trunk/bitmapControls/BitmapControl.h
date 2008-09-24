#ifndef __BitmapControl_H
#define __BitmapControl_H

#include "wx/wx.h"

class BitmapControl: public wxPanel {

  public:

    BitmapControl(wxWindow *owner, int id, wxPoint point, wxSize size);
    wxBitmap* GetControlBitmap();
    void InvalidateControlBitmap();
    virtual void UpdateControlBitmap();

    virtual void OnPaint(wxPaintEvent& evt);
    virtual void OnEraseBackground(wxEraseEvent &evt);
    virtual void OnSize(wxSizeEvent& evt);
    virtual void OnMove(wxMoveEvent& evt);

  protected:

    wxBitmap* pControlBitmap;
    bool pControlBitmapInvalidated;

  DECLARE_EVENT_TABLE()

};


#endif