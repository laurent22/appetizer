#ifndef __ImageButton_H
#define __ImageButton_H

#include "wx/wx.h"
#include "BitmapControl.h"
#include "../imaging/NineSlicesPainter.h"

DECLARE_EVENT_TYPE(wxeEVT_CLICK, -1)

class ImageButton: public BitmapControl {

  public:

    ImageButton(wxWindow *owner, int id, wxPoint point, wxSize size);
    ~ImageButton();
    void LoadImage(const wxString& filePathPrefix);
    void UpdateControlBitmap();
    void SetGrid(int left, int top, int width, int height);

    void OnMouseDown(wxMouseEvent& evt);
    void OnMouseUp(wxMouseEvent& evt);
    void OnMouseOver(wxMouseEvent& evt);
    void OnMouseLeave(wxMouseEvent& evt);

  private:

    bool pressed_;
    wxRect grid_;
    wxString state_;
    wxString filePathPrefix_;
    bool gridIsExplicitelySet_;
    NineSlicesPainter* nineSlicesPainterUp_;
    NineSlicesPainter* nineSlicesPainterOver_;
    NineSlicesPainter* nineSlicesPainterDown_;
    NineSlicesPainter* nineSlicesPainterDisabled_;
    void SetState(const wxString& state);
    wxString GetState();

    DECLARE_EVENT_TABLE()

};


#endif // __ImageButton_H