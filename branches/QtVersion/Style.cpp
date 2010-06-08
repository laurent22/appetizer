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
TabStyle Style::tab;


void RectangleStyle::fromRect(const QRect& rect) {
  left = rect.left();
  top = rect.top();
  bottom = rect.height();
  right = rect.width();
  width = left + right;
  height = top + bottom;
}


TextFormat::TextFormat() {
  family = "Arial";
  bold = false;
  size = 10;
  fontInitialized_ = false;
}


void TextFormat::fromXml(TiXmlHandle handle) {
  XmlUtil::readElementText(handle, "Family", "Arial");
  XmlUtil::readElementTextAsColor(handle, "Color", color);
  size = XmlUtil::readElementTextAsInt(handle, "Size", 10);
  bold = XmlUtil::readElementTextAsBool(handle, "Bold", false);
  fontInitialized_ = false;
}


QFont TextFormat::font() {
  if (fontInitialized_) return font_;

  font_ = QFont(family, size, bold ? QFont::Bold : QFont::Normal);
  fontInitialized_ = true;
  return font_;
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
    
    } else if (elementName == "Background") {
      
      XmlUtil::readElementTextAsRect(handle, "Padding", resultRect);
      Style::background.padding.fromRect(resultRect);
    
    } else if (elementName == "Tab") {

      XmlUtil::readElementTextAsRect(handle, "Padding", resultRect);
      Style::tab.padding.fromRect(resultRect);
      resultRect = QRect(0,0,0,0);
      XmlUtil::readElementTextAsRect(handle, "Margin", resultRect);
      Style::tab.margin.fromRect(resultRect);
      if (handle.Child("TextFormat", 0).ToElement()) Style::tab.textFormat.fromXml(handle.Child("TextFormat", 0));

    }

  }

}