/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "OptionPanel.h"
#include "Controller.h"
#include "Constants.h"
#include "FilePaths.h"
#include "Styles.h"
#include "MainFrame.h"
#include "Localization.h"
#include "gui/AboutDialog.h"
#include <wx/cursor.h>
#include <wx/filename.h>
#include <wx/menu.h>


extern Controller gController;
extern MainFrame* gMainFrame;


BEGIN_EVENT_TABLE(OptionPanel, NineSlicesPanel)
  EVT_MENU(ID_MENU_OptionPanel_Help, OptionPanel::OnMenuHelp)
  EVT_MENU(ID_MENU_OptionPanel_About, OptionPanel::OnMenuAbout)
END_EVENT_TABLE()


OptionPanel::OptionPanel(wxWindow *owner, int id, wxPoint point, wxSize size):
NineSlicesPanel(owner, id, point, size) {
  configDialog_ = NULL;

  LoadImage(FilePaths::SkinDirectory + _T("/OptionPanel.png"));

  wxStringList buttonNames;
  buttonNames.Add(_T("Close"));
  buttonNames.Add(_T("Minimize"));
  buttonNames.Add(_T("Help"));
  buttonNames.Add(_T("Eject"));
  buttonNames.Add(_T("AddShortcut"));
  buttonNames.Add(_T("Config"));
  //buttonNames.Add(_T("Key"));
  buttonNames.Add(_T("MultiLaunch"));

  for (int i = 0; i < buttonNames.size(); i++) {
    OptionButton* button = new OptionButton(this, wxID_ANY);
    wxString n = buttonNames[i];
    button->SetCursor(wxCursor(wxCURSOR_HAND));
    button->SetName(n);
    button->SetIcon(new wxBitmap(FilePaths::IconsDirectory + _T("/ButtonIcon_") + n + _T(".png"), wxBITMAP_TYPE_PNG));
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


void OptionPanel::OnMenuHelp(wxCommandEvent& evt) {  
  wxString helpFile = FilePaths::HelpDirectory + _T("/") + gController.GetUser()->GetSettings()->Locale + _T("/") + HELP_FILE_NAME;
  if (!wxFileName::FileExists(helpFile)) {
    // Default to english
    helpFile = FilePaths::HelpDirectory + _T("/en/") + HELP_FILE_NAME;
  }

  FolderItem::Launch(helpFile);
}


void OptionPanel::OnMenuAbout(wxCommandEvent& evt) {
  AboutDialog aboutDialog;
  aboutDialog.LoadContent();
  aboutDialog.ShowModal();
}


void OptionPanel::Localize() {
  if (configDialog_) configDialog_->Localize();

  for (int i = 0; i < buttons_.size(); i++) {
    OptionButton* button = buttons_.at(i);
    wxString buttonName = button->GetName();
    wxString stringId = _T("OptionPanel.");
    stringId += buttonName;
    stringId += _T("ToolTip");

    wxString toolTip = LOC(stringId);
    button->SetToolTip(toolTip);
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
  
  int x = Styles::OptionPanel.PaddingLeft;
  int y = Styles::OptionPanel.PaddingTop;

  for (int i = 0; i < buttons_.size(); i++) {
    OptionButton* b = buttons_[i];

    int newX = x;
    int newY = y;

    if (newY + b->GetSize().GetHeight() > GetSize().GetHeight() - Styles::OptionPanel.PaddingBottom) {
      newY = Styles::OptionPanel.PaddingTop;
      newX = newX + b->GetSize().GetWidth() + Styles::OptionPanel.ButtonHGap;
    }

    b->Move(newX, newY);

    requiredWidth_ = newX + b->GetSize().GetWidth() + Styles::OptionPanel.PaddingRight;

    x = newX;
    y = newY + b->GetSize().GetHeight() + Styles::OptionPanel.ButtonVGap;
  }
}


void OptionPanel::OnImageButtonClick(wxCommandEvent& evt) {
  wxWindow* w = static_cast<wxWindow*>(evt.GetEventObject());
  wxString buttonName = w->GetName();

  if (buttonName == _T("Close")) {
    //***************************************************************************
    // CLOSE
    //***************************************************************************
    gMainFrame->Close();

  } else if (buttonName == _T("Minimize")) {
    //***************************************************************************
    // MINIMIZE
    //***************************************************************************
    gMainFrame->Hide();

  } else if (buttonName == _T("Eject")) {
    //***************************************************************************
    // EJECT
    //***************************************************************************
    gMainFrame->Close();
    #ifdef __WIN32__
    wxExecute(_T("RunDll32.exe shell32.dll,Control_RunDLL hotplug.dll"));
    #else
    wxLogDebug(_T("TO BE IMPLEMENTED"));
    #endif

  } else if (buttonName == _T("MultiLaunch")) {
    //***************************************************************************
    // MULTI-LAUNCH
    //***************************************************************************
    gController.GetUser()->DoMultiLaunch();

  } else if (buttonName == _T("Config")) {
    //***************************************************************************
    // CONFIG
    //***************************************************************************
    if (!configDialog_) configDialog_ = new ConfigDialog();
    configDialog_->LoadSettings();
    configDialog_->ShowModal();
  } else if (buttonName == _T("Help")) {
    //***************************************************************************
    // HELP
    //***************************************************************************
    wxMenu menu;

    menu.Append(ID_MENU_OptionPanel_Help, LOC(_T("OptionPanel.Help")));
    menu.AppendSeparator();
    menu.Append(ID_MENU_OptionPanel_About, LOC1(_T("OptionPanel.About"), APPLICATION_NAME));
    
    wxPoint pos(w->GetRect().GetLeft(), w->GetRect().GetBottom());
    PopupMenu(&menu, pos);
  } else if (buttonName == _T("AddShortcut")) {
    //***************************************************************************
    // ADD SHORTCUT
    //***************************************************************************
    gController.GetUser()->EditNewFolderItem();
  }
}


ConfigDialog* OptionPanel::GetConfigDialog() {
  return configDialog_;
}


OptionPanel::~OptionPanel() {
  if (configDialog_) {
    wxDELETE(configDialog_);
    configDialog_ = NULL;
  }
}