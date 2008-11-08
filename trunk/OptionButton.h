/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#ifndef __OptionButton_H
#define __OptionButton_H

#include "wx/wx.h" 
#include "bitmap_controls/ImageButton.h"


class OptionButton: public ImageButton {

  public:

    OptionButton(wxWindow *owner, int id = wxID_ANY, wxPoint point = wxDefaultPosition, wxSize size = wxDefaultSize);
    void ApplySkin();

};

#endif // __OptionButton_H