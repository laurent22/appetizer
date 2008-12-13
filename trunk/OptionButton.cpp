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

  OptionButtonStyle style = Styles::OptionButton;

  wxString skinFilePath = FilePaths::GetSkinFile(_T("ButtonIcon_") + GetName() + _T(".png"));
  if (!wxFileName::FileExists(skinFilePath)) skinFilePath = FilePaths::GetSkinFile(_T("ButtonIcon_Default.png"));
  
  wxImage image(skinFilePath);

  if (image.IsOk()) {
    Imaging::ColorizeImage(image, style.IconColor);
    SetIcon(new wxBitmap(image));
  }

  wxBitmap* buttonBitmap = new wxBitmap(FilePaths::GetSkinDirectory() + _T("/OptionButton.png"), wxBITMAP_TYPE_PNG);
  LoadImages(buttonBitmap);
  SetStateColors(wxNullColour, style.ColorOver, style.ColorDown);

  FitToImage();
  SetDownIconOffset(style.DownIconOffset);
}


void OptionButton::InvalidateSkin() {
  skinInvalidated_ = true;
}


void OptionButton::OnPaint(wxPaintEvent& evt) {
  ImageButton::OnPaint(evt);

  if (skinInvalidated_) ApplySkin();
}