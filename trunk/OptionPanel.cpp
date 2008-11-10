/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "OptionPanel.h"
#include "Constants.h"
#include "FilePaths.h"
#include "Log.h"
#include "Styles.h"
#include "MiniLaunchBar.h"
#include <wx/cursor.h>
#include <wx/filename.h>


BEGIN_EVENT_TABLE(OptionPanel, NineSlicesPanel)
  EVT_MENU(ID_MENU_OptionPanel_Help, OptionPanel::OnMenuHelp)
  EVT_MENU(ID_MENU_OptionPanel_About, OptionPanel::OnMenuAbout)
  EVT_MENU(ID_MENU_OptionPanel_GetSupport, OptionPanel::OnMenuGetSupport)
END_EVENT_TABLE()


OptionPanel::OptionPanel(wxWindow *owner, int id, wxPoint point, wxSize size):
NineSlicesPanel(owner, id, point, size) {
  rotated_ = false;
  configDialog_ = NULL;

  wxStringList buttonNames;
  buttonNames.Add(_T("Help"));
  buttonNames.Add(_T("AddShortcut"));
  buttonNames.Add(_T("Config"));
  buttonNames.Add(_T("MultiLaunch"));

  for (int i = 0; i < buttonNames.size(); i++) { 
    wxString n = buttonNames[i];

    OptionButton* button = new OptionButton(this, wxID_ANY);

    button->SetCursor(wxCursor(wxCURSOR_HAND));
    button->SetName(n);

    buttons_.push_back(button);

    button->Connect(
      wxID_ANY, 
      wxeEVT_CLICK,
      wxCommandEventHandler(OptionPanel::OnImageButtonClick),
      NULL,
      this);
  }

  Localize();
}


void OptionPanel::OnMenuGetSupport(wxCommandEvent& evt) {
  ::wxLaunchDefaultBrowser(_T("https://sourceforge.net/forum/?group_id=244299"), wxBROWSER_NEW_WINDOW);
}


void OptionPanel::OnMenuHelp(wxCommandEvent& evt) {  
  wxGetApp().GetUtilities().ShowHelpFile();
}


void OptionPanel::OnMenuAbout(wxCommandEvent& evt) {
  wxGetApp().GetUtilities().ShowAboutDialog();
}


void OptionPanel::ApplySkin() {
  LoadImage(FilePaths::GetSkinDirectory() + _T("/OptionPanel.png"));
  SetGrid(Styles::OptionPanel.ScaleGrid);

  for (int i = 0; i < buttons_.size(); i++) {    
    OptionButton* button = buttons_[i];

    wxImage image(FilePaths::GetSkinFile(_T("ButtonIcon_") + button->GetName() + _T(".png")));
    Imaging::ColorizeImage(image, Styles::OptionPanel.ButtonIconColor);
    button->SetIcon(new wxBitmap(image));
    button->ApplySkin();
  }

  InvalidateLayout();
}


void OptionPanel::SetRotated(bool rotated) {
  if (rotated == rotated_) return;
  rotated_ = rotated;
  
  SetBitmapRotation(rotated_ ? -90 : 0);

  InvalidateLayout();
}


void OptionPanel::Localize() {
  if (configDialog_) configDialog_->Localize();

  ButtonTooltipHashMap tooltipHashMap;
  tooltipHashMap[_T("Help")] = _("Help");
  tooltipHashMap[_T("AddShortcut")] = _("New shortcut...");
  tooltipHashMap[_T("Config")] = _("Configuration");
  tooltipHashMap[_T("MultiLaunch")] = _("Multi-launch");

  for (int i = 0; i < buttons_.size(); i++) {
    OptionButton* button = buttons_.at(i);
    wxString buttonName = button->GetName();
    button->SetToolTip(tooltipHashMap[buttonName]);
  }
}


int OptionPanel::GetRequiredWidth() {
  return requiredWidth_;
}


void OptionPanel::InvalidateLayout() {
  layoutInvalidated_ = true;
  Refresh();
}


void OptionPanel::OnPaint(wxPaintEvent& evt) {
  BitmapControl::OnPaint(evt);
  
  if (layoutInvalidated_) UpdateLayout();
}


void OptionPanel::OnSize(wxSizeEvent& evt) {
  BitmapControl::OnSize(evt);

  InvalidateLayout();
}


void OptionPanel::UpdateLayout() {
  layoutInvalidated_ = false;

  int x;
  int y;
  
  if (rotated_) {
    x = Styles::OptionPanel.Padding.Bottom;
    y = Styles::OptionPanel.Padding.Left;
  } else {
    x = Styles::OptionPanel.Padding.Left;
    y = Styles::OptionPanel.Padding.Top;
  }

  int buttonsTop;
  int buttonsBottom = 0;
  requiredWidth_ = 0;

  for (int i = 0; i < buttons_.size(); i++) {
    OptionButton* b = buttons_[i];

    int newX = x;
    int newY = y;

    if (rotated_) {

      if (newX + b->GetSize().GetWidth() > GetSize().GetWidth() - Styles::OptionPanel.Padding.Top) {
        newX = Styles::OptionPanel.Padding.Bottom;
        newY = newY + b->GetSize().GetHeight() + Styles::OptionPanel.ButtonVGap;
      }

      requiredWidth_ = newY + b->GetSize().GetHeight() + Styles::OptionPanel.Padding.Right;

    } else {

      if (newY + b->GetSize().GetHeight() > GetSize().GetHeight() - Styles::OptionPanel.Padding.Bottom) {
        newY = Styles::OptionPanel.Padding.Top;
        newX = newX + b->GetSize().GetWidth() + Styles::OptionPanel.ButtonHGap;
      }

      requiredWidth_ = newX + b->GetSize().GetWidth() + Styles::OptionPanel.Padding.Right;
    }

    b->Move(newX, newY);      

    if (rotated_) {
      x = newX + b->GetSize().GetWidth() + Styles::OptionPanel.ButtonHGap;
      y = newY;

      if (i == 0) buttonsTop = newX;
      if (newX + b->GetSize().GetWidth() > buttonsBottom) buttonsBottom = newX + b->GetSize().GetWidth();
    } else {
      x = newX;
      y = newY + b->GetSize().GetHeight() + Styles::OptionPanel.ButtonVGap;

      if (i == 0) buttonsTop = newY;
      if (newY + b->GetSize().GetHeight() > buttonsBottom) buttonsBottom = newY + b->GetSize().GetHeight();
    }
  }

  if (rotated_) {
    int leftGap = GetSize().GetWidth() - buttonsBottom;
    int leftOffset = (int)floor((leftGap + buttonsTop) / 2.0f) - buttonsTop;

    for (int i = 0; i < buttons_.size(); i++) {
      OptionButton* b = buttons_[i];
      b->Move(b->GetRect().GetLeft() + leftOffset, b->GetRect().GetTop());
    }
  } else {
    int bottomGap = GetSize().GetHeight() - buttonsBottom;
    int topOffset = (int)floor((bottomGap + buttonsTop) / 2.0f) - buttonsTop;

    for (int i = 0; i < buttons_.size(); i++) {
      OptionButton* b = buttons_[i];
      b->Move(b->GetRect().GetLeft(), b->GetRect().GetTop() + topOffset);
    }
  }

  if (requiredWidth_ <= 0) requiredWidth_ = 20;
}


void OptionPanel::OnImageButtonClick(wxCommandEvent& evt) {
  wxWindow* w = static_cast<wxWindow*>(evt.GetEventObject());
  wxString buttonName = w->GetName();

  if (buttonName == _T("Close")) {
    //***************************************************************************
    // CLOSE
    //***************************************************************************
    wxGetApp().GetMainFrame()->Close();

  } else if (buttonName == _T("Help")) {
    //***************************************************************************
    // HELP
    //***************************************************************************
    wxMenu menu;

    menu.Append(ID_MENU_OptionPanel_Help, _("Help Topics"));
    menu.Append(ID_MENU_OptionPanel_GetSupport, _("Get Support"));
    menu.AppendSeparator();
    menu.Append(ID_MENU_OptionPanel_About, wxString::Format(_("About %s..."), APPLICATION_NAME));
    
    wxPoint pos(w->GetRect().GetLeft(), w->GetRect().GetBottom());
    PopupMenu(&menu, pos);

  } else if (buttonName == _T("MultiLaunch")) {
    //***************************************************************************
    // MULTI-LAUNCH
    //***************************************************************************
    wxGetApp().GetUtilities().DoMultiLaunch();

  } else if (buttonName == _T("Config")) {
    //***************************************************************************
    // CONFIG
    //***************************************************************************
    wxGetApp().GetUtilities().ShowConfigDialog();

  } else if (buttonName == _T("AddShortcut")) {
    //***************************************************************************
    // ADD SHORTCUT
    //***************************************************************************
    wxGetApp().GetUtilities().CreateNewShortcut();

  }
}


ConfigDialog* OptionPanel::GetConfigDialog() {
  return configDialog_;
}


OptionPanel::~OptionPanel() {
  if (configDialog_) {
    configDialog_->Destroy();
    configDialog_ = NULL;
  }

  for (int i = 0; i < buttons_.size(); i++) {
    OptionButton* button = buttons_.at(i);
    button->Destroy();
  }
}