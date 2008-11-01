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
    wxRect resultRect;

    if (elementName == _T("ArrowButton")) {
      XmlUtil::ReadElementTextAsRect(handle, "ScaleGrid", Styles::OptionPanel.ArrowButtonScaleGrid);
      Styles::OptionPanel.ArrowButtonWidth = XmlUtil::ReadElementTextAsInt(handle, "Width");
    }

    if (elementName == _T("BarBackground")) {
      XmlUtil::ReadElementTextAsRect(handle, "ScaleGrid", Styles::MainPanel.ScaleGrid);
      XmlUtil::ReadElementTextAsRect(handle, "Padding", resultRect);
      Styles::MainPanel.PaddingLeft = resultRect.GetLeft();
      Styles::MainPanel.PaddingTop = resultRect.GetTop();
      Styles::MainPanel.PaddingRight = resultRect.GetWidth();
      Styles::MainPanel.PaddingBottom = resultRect.GetHeight();
      Styles::MainPanel.PaddingWidth = Styles::MainPanel.PaddingLeft + Styles::MainPanel.PaddingRight;
      Styles::MainPanel.PaddingHeight = Styles::MainPanel.PaddingTop + Styles::MainPanel.PaddingBottom;
    }

    if (elementName == _T("OptionPanel")) {
      XmlUtil::ReadElementTextAsRect(handle, "ScaleGrid", Styles::OptionPanel.ScaleGrid);
      XmlUtil::ReadElementTextAsRect(handle, "Padding", resultRect);
      Styles::OptionPanel.PaddingLeft = resultRect.GetLeft();
      Styles::OptionPanel.PaddingTop = resultRect.GetTop();
      Styles::OptionPanel.PaddingRight = resultRect.GetWidth();
      Styles::OptionPanel.PaddingBottom = resultRect.GetHeight();
      Styles::OptionPanel.PaddingWidth = Styles::OptionPanel.PaddingLeft + Styles::OptionPanel.PaddingRight;
      Styles::OptionPanel.PaddingHeight = Styles::OptionPanel.PaddingTop + Styles::OptionPanel.PaddingBottom;
      Styles::OptionPanel.ButtonHGap = XmlUtil::ReadElementTextAsInt(handle, "HGap");
      Styles::OptionPanel.ButtonVGap = XmlUtil::ReadElementTextAsInt(handle, "VGap");
    }

    if (elementName == _T("IconPanel")) {
      XmlUtil::ReadElementTextAsRect(handle, "ScaleGrid", Styles::InnerPanel.ScaleGrid);
      XmlUtil::ReadElementTextAsRect(handle, "Padding", resultRect);
      Styles::InnerPanel.PaddingLeft = resultRect.GetLeft();
      Styles::InnerPanel.PaddingTop = resultRect.GetTop();
      Styles::InnerPanel.PaddingRight = resultRect.GetWidth();
      Styles::InnerPanel.PaddingBottom = resultRect.GetHeight();
      Styles::InnerPanel.PaddingWidth = Styles::InnerPanel.PaddingLeft + Styles::InnerPanel.PaddingRight;
      Styles::InnerPanel.PaddingHeight = Styles::InnerPanel.PaddingTop + Styles::InnerPanel.PaddingBottom;
    }

    if (elementName == _T("Icon")) {
      XmlUtil::ReadElementTextAsRect(handle, "Padding", resultRect);
      Styles::Icon.PaddingLeft = resultRect.GetLeft();
      Styles::Icon.PaddingTop = resultRect.GetTop();
      Styles::Icon.PaddingRight = resultRect.GetWidth();
      Styles::Icon.PaddingBottom = resultRect.GetHeight();
      Styles::Icon.PaddingWidth = Styles::Icon.PaddingLeft + Styles::Icon.PaddingRight;
      Styles::Icon.PaddingHeight = Styles::Icon.PaddingTop + Styles::Icon.PaddingBottom;
    }
  }

  int i = 0;

}