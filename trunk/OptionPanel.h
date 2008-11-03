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
#include "OptionButton.h"
#include "gui/ConfigDialog.h"


typedef std::vector<OptionButton*> OptionButtonVector;


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
  void ApplySkin(const wxString& skinName);

  void OnSize(wxSizeEvent& evt);
  void OnPaint(wxPaintEvent& evt);
  void OnImageButtonClick(wxCommandEvent& evt);

private:

  bool rotated_;
  int requiredWidth_;
  OptionButtonVector buttons_;
  bool layoutInvalidated_;
  ConfigDialog* configDialog_;

};

#endif // __OptionPanel_H