/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#ifndef __Launchapp_H
#define __Launchapp_H


#include "../imaging/NineSlicesPainter.h"
#include "../bitmap_controls/ImagePanel.h"
#include "../bitmap_controls/NineSlicesPanel.h"
#include "../IconPanel.h"


class Launchapp: public wxFrame {

  public:

    Launchapp(wxWindow* parent);
    ~Launchapp();
    void ApplySkin(wxBitmap* mainBackgroundBitmap);
    void InvalidateLayout();
    void InvalidateMask();
    void UpdateMask();
    void UpdateLayout();
    void UpdateLayout(int width, int height);

  private:

    NineSlicesPanel* backgroundPanel_;
    NineSlicesPanel* backgroundLeftImage_;
    bool needLayoutUpdate_;
    bool needMaskUpdate_;
    NineSlicesPainter maskNineSlices_;
    wxTextCtrl* textInput_;
    IconPanel* iconPanel_;
    void UpdateFolderItemsFromText();
    int iconPanelWidth_;

    void OnPaint(wxPaintEvent& evt);
    void OnTextChange(wxCommandEvent& evt);

  DECLARE_EVENT_TABLE()

};

#endif // __Launchapp_H