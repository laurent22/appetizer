/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>
#include <Style.h>
#include <XmlUtil.h>
using namespace appetizer;

IconStyle Style::icon;
BackgroundStyle Style::background;


void PaddingStyle::fromRect(const QRect& rect) {
  left = rect.left();
  top = rect.top();
  bottom = rect.height();
  right = rect.width();
  width = left + right;
  height = top + bottom;
}


void Style::loadSkinFile(const QString& filePath) {
  TiXmlDocument doc(filePath.toUtf8());
  doc.LoadFile(TIXML_ENCODING_UTF8);

  TiXmlElement* root = doc.FirstChildElement("Skin");
  if (!root) {
    qWarning() << "Style::loadSkinFile: Could not load XML. No Skin element found: " + filePath;
    return;
  }

  //SkinMetadata skinMetadata;
  //Styles::GetSkinMetadata(root, skinMetadata);

  //if (!Styles::IsSkinVersionCompatible(skinMetadata.CompatibleVersion)) {
  //  MessageBoxes::ShowError(wxString::Format(_("This skin is not compatible with the current version of %s."), APPLICATION_NAME));
  //  return;
  //}

  // *****************************************************************
  // Set default values
  // *****************************************************************
  Style::icon.labelGap = 10;
  
  // *****************************************************************
  // Load the XML
  // *****************************************************************
  for (TiXmlElement* element = root->FirstChildElement(); element; element = element->NextSiblingElement()) {
    QString elementName = QString::fromUtf8(element->Value());
    TiXmlHandle handle(element);
    QRect resultRect;

    if (elementName == "Icon") {
      XmlUtil::readElementTextAsRect(handle, "Padding", resultRect);
      Style::icon.padding.fromRect(resultRect);
    }

    if (elementName == "Background") {
      XmlUtil::readElementTextAsRect(handle, "Padding", resultRect);
      Style::background.padding.fromRect(resultRect);
    }

  }

}