/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#ifndef __Styles_H
#define __Styles_H

#include "wx/wx.h"


class MainPanelStyle { public:
  int PaddingLeft;
  int PaddingRight;
  int PaddingTop;
  int PaddingBottom;
  int PaddingWidth;
  int PaddingHeight;
  wxRect ScaleGrid;
};

class InnerPanelStyle { public:
  int PaddingLeft;
  int PaddingRight;
  int PaddingTop;
  int PaddingBottom;
  int PaddingWidth;
  int PaddingHeight;
  wxRect ScaleGrid;
};

class OptionPanelStyle { public:
  int PaddingLeft;
  int PaddingRight;
  int PaddingTop;
  int PaddingBottom;
  int PaddingWidth;
  int PaddingHeight;
  int ArrowButtonWidth;
  wxRect ArrowButtonScaleGrid;
  wxRect ScaleGrid;
  int ButtonHGap;
  int ButtonVGap;
};

class IconStyle { public:
  int PaddingLeft;
  int PaddingRight;
  int PaddingTop;
  int PaddingBottom;
  int PaddingWidth;
  int PaddingHeight;
};

class Styles {

public:

  static MainPanelStyle MainPanel;
  static InnerPanelStyle InnerPanel;
  static IconStyle Icon;
  static OptionPanelStyle OptionPanel;

  static void LoadSkinFile(const wxString& filePath);

};

#endif // __Styles_H