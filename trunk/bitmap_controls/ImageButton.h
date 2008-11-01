/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#ifndef __ImageButton_H
#define __ImageButton_H

#include "wx/wx.h"
#include "BitmapControl.h"
#include "../imaging/NineSlicesPainter.h"
#include "ImagePanel.h"


DECLARE_EVENT_TYPE(wxeEVT_CLICK, -1)

class ImageButton: public BitmapControl {

  public:

    ImageButton(wxWindow *owner, int id = wxID_ANY, wxPoint point = wxDefaultPosition, wxSize size = wxDefaultSize);
    ~ImageButton();
    void LoadImage(const wxString& filePathPrefix);
    void UpdateControlBitmap();
    void SetGrid(int left, int top, int width, int height);
    void SetGrid(const wxRect& rect);
    void FitToImage();
    void SetIcon(wxBitmap* iconBitmap, bool ownIt = true);

    void OnMouseDown(wxMouseEvent& evt);
    void OnMouseUp(wxMouseEvent& evt);
    void OnMouseOver(wxMouseEvent& evt);
    void OnMouseLeave(wxMouseEvent& evt);

    bool Enable(bool enable = true);

  private:

    wxBitmap* iconBitmap_;
    wxBitmap* rotatedIconBitmap_;
    bool ownIcon_;
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