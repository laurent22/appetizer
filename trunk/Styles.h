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
};

class InnerPanelStyle { public:
  int PaddingLeft;
  int PaddingRight;
  int PaddingTop;
  int PaddingBottom;
  int PaddingWidth;
  int PaddingHeight;
};

class OptionPanelStyle { public:
  int PaddingLeft;
  int PaddingRight;
  int PaddingTop;
  int PaddingBottom;
  int PaddingWidth;
  int PaddingHeight;
  int ArrowButtonWidth;
  int ButtonHGap;
  int ButtonVGap;
};

class IconTooltipStyle { public:
  int PaddingLeft;
  int PaddingRight;
  int PaddingTop;
  int PaddingBottom;
  int PaddingWidth;
  int PaddingHeight;
  wxColor FontColor;
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

};

#endif // __Styles_H