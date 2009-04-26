/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#include "Styles.h"
#include "Constants.h"
#include "MessageBoxes.h"
#include "utilities/XmlUtil.h"
#include "utilities/StringUtil.h"
#include "utilities/VersionInfo.h"


MainPanelStyle Styles::MainPanel;
InnerPanelStyle Styles::InnerPanel;
IconStyle Styles::Icon;
OptionPanelStyle Styles::OptionPanel;
ArrowButtonStyle Styles::ArrowButton;
OptionButtonStyle Styles::OptionButton;
BrowseButtonStyle Styles::BrowseButton;


void PaddingStyle::FromRect(const wxRect& rect) {
  Left = rect.GetLeft();
  Top = rect.GetTop();
  Bottom = rect.GetHeight();
  Right = rect.GetWidth();
  Width = Left + Right;
  Height = Top + Bottom;
}


void Styles::GetSkinMetadata(const wxString& filePath, SkinMetadata& skinMetadata) {
  TiXmlDocument doc(filePath.mb_str());
  doc.LoadFile();

  TiXmlElement* root = doc.FirstChildElement("Skin");
  if (!root) {
    WLOG(_T("Styles::LoadSkinFile: Could not load XML. No Skin element found: %s"), filePath);
    return;
  }

  Styles::GetSkinMetadata(root, skinMetadata);
}


void Styles::GetSkinMetadata(TiXmlElement* skinDocumentRoot, SkinMetadata& skinMetadata) {
  skinMetadata.CompatibleVersion = wxString(skinDocumentRoot->Attribute("compatibleVersion"), wxConvUTF8);
  skinMetadata.Name = wxString(skinDocumentRoot->Attribute("name"), wxConvUTF8);
  skinMetadata.Author = wxString(skinDocumentRoot->Attribute("author"), wxConvUTF8);
}


void Styles::LoadSkinFile(const wxString& filePath) {
  TiXmlDocument doc(filePath.mb_str());
  doc.LoadFile();

  TiXmlElement* root = doc.FirstChildElement("Skin");
  if (!root) {
    WLOG(_T("Styles::LoadSkinFile: Could not load XML. No Skin element found: %s"), filePath);
    return;
  }

  SkinMetadata skinMetadata;
  Styles::GetSkinMetadata(root, skinMetadata);

  if (!Styles::IsSkinVersionCompatible(skinMetadata.CompatibleVersion)) {
    MessageBoxes::ShowError(wxString::Format(_("This skin is not compatible with the current version of %s."), APPLICATION_NAME));
    return;
  }

  // *****************************************************************
  // Set default values
  // *****************************************************************
  Styles::OptionButton.IconColor = wxColour(255,255,255);
  Styles::ArrowButton.ColorDown = wxNullColour;
  Styles::ArrowButton.ColorOver = wxNullColour;
  
  // *****************************************************************
  // Load the XML
  // *****************************************************************
  for (TiXmlElement* element = root->FirstChildElement(); element; element = element->NextSiblingElement()) {
    wxString elementName = wxString(element->Value(), wxConvUTF8);
    TiXmlHandle handle(element);
    wxRect resultRect;

    if (elementName == _T("ArrowButton")) {
      XmlUtil::ReadElementTextAsRect(handle, "Rectangle", Styles::ArrowButton.SourceRectangle);
      XmlUtil::ReadElementTextAsColor(handle, "ColorOver", Styles::ArrowButton.ColorOver);
      XmlUtil::ReadElementTextAsColor(handle, "ColorDown", Styles::ArrowButton.ColorDown);
    }

    if (elementName == _T("BrowseButton")) {
      XmlUtil::ReadElementTextAsColor(handle, "ColorOver", Styles::BrowseButton.ColorOver);
      XmlUtil::ReadElementTextAsColor(handle, "ColorDown", Styles::BrowseButton.ColorDown);
    }

    if (elementName == _T("OptionPanel")) {
      XmlUtil::ReadElementTextAsRect(handle, "Rectangle", Styles::OptionPanel.SourceRectangle);
      XmlUtil::ReadElementTextAsRect(handle, "Padding", resultRect);
      Styles::OptionPanel.Padding.FromRect(resultRect);
      Styles::OptionPanel.ButtonHGap = XmlUtil::ReadElementTextAsInt(handle, "HGap");
      Styles::OptionPanel.ButtonVGap = XmlUtil::ReadElementTextAsInt(handle, "VGap");
    }

    if (elementName == _T("MainPanel")) {
      XmlUtil::ReadElementTextAsRect(handle, "Rectangle", Styles::MainPanel.SourceRectangle);
    }

    if (elementName == _T("IconPanel")) {
      XmlUtil::ReadElementTextAsRect(handle, "Rectangle", Styles::InnerPanel.SourceRectangle);
      XmlUtil::ReadElementTextAsRect(handle, "Padding", resultRect);
      Styles::InnerPanel.Padding.FromRect(resultRect);
    }

    if (elementName == _T("Icon")) {
      XmlUtil::ReadElementTextAsRect(handle, "Padding", resultRect);
      Styles::Icon.Padding.FromRect(resultRect);
    }

    if (elementName == _T("OptionButton")) {
      XmlUtil::ReadElementTextAsColor(handle, "ColorOver", Styles::OptionButton.ColorOver);
      XmlUtil::ReadElementTextAsColor(handle, "ColorDown", Styles::OptionButton.ColorDown);
      XmlUtil::ReadElementTextAsPoint(handle, "DownIconOffset", Styles::OptionButton.DownIconOffset);
      XmlUtil::ReadElementTextAsColor(handle, "IconColor", Styles::OptionButton.IconColor);
    }
  }

}


bool Styles::IsSkinVersionCompatible(const wxString& skinVersion) {
  double dSkinVersion;
  skinVersion.ToDouble(&dSkinVersion);
  
  double dThisVersion;
  wxString fullVersion = VersionInfo::GetVersionString();
  wxArrayString splitted;
  StringUtil::Split(fullVersion, splitted, _T("."));
  wxString tdVersion = splitted[0] + _T(".") + splitted[1];
  tdVersion.ToDouble(&dThisVersion);

  if (dSkinVersion < 1.3 && dThisVersion >= 1.3) return false;

  return true;
}