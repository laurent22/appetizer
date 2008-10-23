#ifndef __ImageButton_H
#define __ImageButton_H

#include "wx/wx.h"
#include "BitmapControl.h"
#include "../imaging/NineSlicesPainter.h"

class ImageButton: public BitmapControl {

  public:

    ImageButton(wxWindow *owner, int id, wxPoint point, wxSize size);
    ~ImageButton();
    void LoadImage(const wxString& filePathPrefix);
    void UpdateControlBitmap();
    void SetGrid(int left, int top, int width, int height);

  private:

    wxRect grid_;
    wxString state_;
    wxString filePathPrefix_;
    bool gridIsExplicitelySet_;
    NineSlicesPainter* nineSlicesPainterUp_;
    NineSlicesPainter* nineSlicesPainterOver_;
    NineSlicesPainter* nineSlicesPainterDown_;
    NineSlicesPainter* nineSlicesPainterDisabled_;

};


#endif // __ImageButton_H