/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#ifndef __IconPanelFrame_H
#define __IconPanelFrame_H

#include <wx/wx.h>
#include "IconPanel.h"
#include "bitmap_controls/NineSlicesPanel.h"
#include "imaging/NineSlicesPainter.h"


class IconPanelFrame : public wxFrame {

public:

  IconPanelFrame(wxWindow* parent, wxWindowID id = wxID_ANY, const wxString& title = wxEmptyString, const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize);
  IconPanel* GetIconPanel();
  void InvalidateLayout();
  void InvalidateMask();
  void UpdateMask();
  void UpdateLayout();
  void FitToIconPanel();

  void OnEraseBackground(wxEraseEvent &evt);
  void OnPaint(wxPaintEvent& evt);

private:

  IconPanel* iconPanel_;
  NineSlicesPainter maskNineSlices_;
  NineSlicesPanel* backgroundPanel_;
  bool layoutInvalidated_;
  bool maskInvalidated_;

  DECLARE_EVENT_TABLE()

};


#endif // __IconPanelFrame_H