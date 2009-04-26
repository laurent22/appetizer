/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#ifndef __ToolTipFrame_H
#define __ToolTipFrame_H


#include "bitmap_controls/NineSlicesPanel.h"


class ToolTipFrame: public wxFrame {

public:

  ToolTipFrame(wxWindow* parent = NULL, wxWindowID id = wxID_ANY);
  ~ToolTipFrame();
  void ShowToolTip(const wxString& text, const wxRect& objectScreenRect);
  void HideToolTip();
  void ApplySkin(const wxString& skinName = wxEmptyString);

private:

  wxStaticText* label_;
  NineSlicesPanel* backgroundPanel_;
  int maxWidth_;

};


#endif // __ToolTipFrame_H