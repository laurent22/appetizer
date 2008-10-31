/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "Styles.h"
#include "utilities/XmlUtil.h"


MainPanelStyle Styles::MainPanel;
InnerPanelStyle Styles::InnerPanel;
IconStyle Styles::Icon;
OptionPanelStyle Styles::OptionPanel;


void Styles::LoadSkinFile(const wxString& filePath) {
  TiXmlDocument doc(filePath.mb_str());
  doc.LoadFile();

  TiXmlElement* root = doc.FirstChildElement("Skin");
  if (!root) {
    wxLogDebug(_T("Styles::LoadSkinFile: Could not load XML. No Skin element found."));
    return;
  }
  
  for (TiXmlElement* element = root->FirstChildElement(); element; element = element->NextSiblingElement()) {
    wxString elementName = wxString(element->Value(), wxConvUTF8);
    TiXmlHandle handle(element);

    if (elementName == _T("ArrowButton")) {
      XmlUtil::ReadElementTextAsRect(handle, "ScaleGrid", Styles::OptionPanel.ArrowButtonScaleGrid);
    }

    if (elementName == _T("BarBackground")) {
      XmlUtil::ReadElementTextAsRect(handle, "ScaleGrid", Styles::MainPanel.ScaleGrid);
    }

    if (elementName == _T("OptionPanel")) {
      XmlUtil::ReadElementTextAsRect(handle, "ScaleGrid", Styles::OptionPanel.ScaleGrid);
    }

    if (elementName == _T("IconPanel")) {
      XmlUtil::ReadElementTextAsRect(handle, "ScaleGrid", Styles::InnerPanel.ScaleGrid);
    }
  }

  int i = 0;

}