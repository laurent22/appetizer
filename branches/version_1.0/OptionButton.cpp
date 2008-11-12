/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "precompiled.h"

#include "OptionButton.h"
#include "MiniLaunchBar.h"
#include "FilePaths.h"
#include "Styles.h"


OptionButton::OptionButton(wxWindow *owner, int id, wxPoint point, wxSize size):
ImageButton(owner, id, point, size) {  
}


void OptionButton::ApplySkin() {
  LoadImage(FilePaths::GetSkinDirectory() + _T("/OptionButton"));
  FitToImage();
  SetDownIconOffset(Styles::OptionPanel.ButtonDownIconOffset);
}