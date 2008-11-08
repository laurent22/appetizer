/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "Styles.h"
#include "Log.h"
#include "utilities/XmlUtil.h"


MainPanelStyle Styles::MainPanel;
InnerPanelStyle Styles::InnerPanel;
IconStyle Styles::Icon;
OptionPanelStyle Styles::OptionPanel;


void PaddingStyle::FromRect(const wxRect& rect) {
  Left = rect.GetLeft();
  Top = rect.GetTop();
  Bottom = rect.GetHeight();
  Right = rect.GetWidth();
  Width = Left + Right;
  Height = Top + Bottom;
}


void Styles::LoadSkinFile(const wxString& filePath) {
  TiXmlDocument doc(filePath.mb_str());
  doc.LoadFile();

  TiXmlElement* root = doc.FirstChildElement("Skin");
  if (!root) {
    wlog("Styles::LoadSkinFile: Could not load XML. No Skin element found.");
    return;
  }

  // *****************************************************************
  // Set default values
  // *****************************************************************
  Styles::OptionPanel.ButtonIconColor = wxColour(255,255,255);
  
  // *****************************************************************
  // Load the XML
  // *****************************************************************
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
      Styles::MainPanel.Padding.FromRect(resultRect);
    }

    if (elementName == _T("OptionPanel")) {
      XmlUtil::ReadElementTextAsRect(handle, "ScaleGrid", Styles::OptionPanel.ScaleGrid);
      XmlUtil::ReadElementTextAsRect(handle, "Padding", resultRect);
      Styles::OptionPanel.Padding.FromRect(resultRect);
      Styles::OptionPanel.ButtonHGap = XmlUtil::ReadElementTextAsInt(handle, "HGap");
      Styles::OptionPanel.ButtonVGap = XmlUtil::ReadElementTextAsInt(handle, "VGap");
    }

    if (elementName == _T("IconPanel")) {
      XmlUtil::ReadElementTextAsRect(handle, "ScaleGrid", Styles::InnerPanel.ScaleGrid);
      XmlUtil::ReadElementTextAsRect(handle, "Padding", resultRect);
      Styles::InnerPanel.Padding.FromRect(resultRect);
    }

    if (elementName == _T("Icon")) {
      XmlUtil::ReadElementTextAsRect(handle, "Padding", resultRect);
      Styles::Icon.Padding.FromRect(resultRect);
    }

    if (elementName == _T("OptionButton")) {
      XmlUtil::ReadElementTextAsPoint(handle, "DownIconOffset", Styles::OptionPanel.ButtonDownIconOffset);
      XmlUtil::ReadElementTextAsColor(handle, "IconColor", Styles::OptionPanel.ButtonIconColor);
    }
  }

}