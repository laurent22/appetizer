/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#ifndef __Styles_H
#define __Styles_H

#include "utilities/XmlUtil.h"


class PaddingStyle {
 
public:

  int Left;
  int Right;
  int Bottom;
  int Top;
  int Width;
  int Height;
  void FromRect(const wxRect& rect);

};

class ArrowButtonStyle { public:
  wxRect SourceRectangle;
  wxColour ColorOver;
  wxColour ColorDown;
};

class BrowseButtonStyle { public:
  wxColour ColorOver;
  wxColour ColorDown;
};

class MainPanelStyle { public:
  PaddingStyle Padding;
  wxRect SourceRectangle;
};

class InnerPanelStyle { public:
  wxRect SourceRectangle;
  PaddingStyle Padding;
};

class OptionPanelStyle { public:
  wxRect SourceRectangle;
  PaddingStyle Padding;
  int ButtonHGap;
  int ButtonVGap;
};

class OptionButtonStyle { public:
  wxPoint DownIconOffset;
  wxColour IconColor;
  wxColour ColorOver;
  wxColour ColorDown;
};

class IconStyle { public:
  PaddingStyle Padding;
};

struct SkinMetadata {
  wxString Name;
  wxString CompatibleVersion;
  wxString Author;
};


class Styles {

public:

  static MainPanelStyle MainPanel;
  static InnerPanelStyle InnerPanel;
  static IconStyle Icon;
  static OptionButtonStyle OptionButton;
  static OptionPanelStyle OptionPanel;
  static ArrowButtonStyle ArrowButton;
  static BrowseButtonStyle BrowseButton;

  static void GetSkinMetadata(TiXmlElement* skinDocumentRoot, SkinMetadata& skinMetadata);
  static void GetSkinMetadata(const wxString& filePath, SkinMetadata& skinMetadata);
  static void LoadSkinFile(const wxString& filePath);
  static bool IsSkinVersionCompatible(const wxString& skinVersion);

};

#endif // __Styles_H