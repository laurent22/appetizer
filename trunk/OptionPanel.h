/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#ifndef __OptionPanel_H
#define __OptionPanel_H

#include "wx/wx.h" 
#include "bitmap_controls/NineSlicesPanel.h"
#include <vector>
#include <wx/hashmap.h>
#include "OptionButton.h"
#include "gui/ConfigDialog.h"


typedef std::vector<OptionButton*> OptionButtonVector;


WX_DECLARE_STRING_HASH_MAP(wxString, ButtonTooltipHashMap);


enum {
  ID_BUTTON_OptionButton
};


class OptionPanel: public NineSlicesPanel {

public:

  OptionPanel(wxWindow *owner, int id = wxID_ANY, wxPoint point = wxDefaultPosition, wxSize size = wxDefaultSize);
  ~OptionPanel();
  void InvalidateLayout();
  void UpdateLayout();
  int GetRequiredWidth();
  ConfigDialog* GetConfigDialog();
  void Localize();
  void SetRotated(bool rotated);
  void ApplySkin();

  void OnSize(wxSizeEvent& evt);
  void OnPaint(wxPaintEvent& evt);
  void OnImageButtonClick(wxCommandEvent& evt);
  void OnMenuGetSupport(wxCommandEvent& evt);
  void OnMenuHelp(wxCommandEvent& evt);
  void OnMenuAbout(wxCommandEvent& evt);

private:

  bool rotated_;
  int requiredWidth_;
  OptionButtonVector buttons_;
  bool layoutInvalidated_;
  ConfigDialog* configDialog_;

  DECLARE_EVENT_TABLE()

};

#endif // __OptionPanel_H