/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#include "OptionButton.h"
#include "MiniLaunchBar.h"
#include "FilePaths.h"
#include "Styles.h"


OptionButton::OptionButton(wxWindow *owner, int id, wxPoint point, wxSize size):
ImageButton(owner, id, point, size) {  
  skinInvalidated_ = false;
}


void OptionButton::ApplySkin() {
  skinInvalidated_ = false;

  wxString skinFilePath = FilePaths::GetSkinFile(_T("ButtonIcon_") + GetName() + _T(".png"));
  if (!wxFileName::FileExists(skinFilePath)) skinFilePath = FilePaths::GetSkinFile(_T("ButtonIcon_Default.png"));
  
  wxImage image(skinFilePath);

  if (image.IsOk()) {
    Imaging::ColorizeImage(image, Styles::OptionPanel.ButtonIconColor);
    SetIcon(new wxBitmap(image));
  }

  LoadImage(FilePaths::GetSkinDirectory() + _T("/OptionButton"));
  FitToImage();
  SetDownIconOffset(Styles::OptionPanel.ButtonDownIconOffset);
}


void OptionButton::InvalidateSkin() {
  skinInvalidated_ = true;
}


void OptionButton::OnPaint(wxPaintEvent& evt) {
  ImageButton::OnPaint(evt);

  if (skinInvalidated_) ApplySkin();
}