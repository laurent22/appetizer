/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#include <Constants.h>
#include <Style.h>
#include <XmlUtil.h>

using namespace appetizer;

IconStyle Style::icon;
BackgroundStyle Style::background;
TabStyle Style::tab;
IconPanelStyle Style::iconPanel;
FloatingButtonStyle Style::floatingButton;


RectangleStyle::RectangleStyle() {
  left = 0;
  top = 0;
  bottom = 0;
  right = 0;
  width = 0;
  height = 0;
}


void RectangleStyle::fromRect(const QRect& rect) {
  left = rect.left();
  top = rect.top();
  bottom = rect.height();
  right = rect.width();
  width = left + right;
  height = top + bottom;
}

BackgroundStyle::BackgroundStyle() {
  shadow = NULL;
}

FloatingButtonStyle::FloatingButtonStyle() {
  shadow = NULL;
}

QRect BackgroundStyle::getContentRectangle(int width, int height) const { return Style::calculateContentRectangle(width, height, padding); };


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


QGraphicsDropShadowEffect* Style::parseShadowXml(TiXmlHandle handle) {
  QGraphicsDropShadowEffect* output = new QGraphicsDropShadowEffect();
  output->setXOffset(XmlUtil::readElementTextAsInt(handle, "XOffset", 4));
  output->setYOffset(XmlUtil::readElementTextAsInt(handle, "YOffset", 4));
  output->setBlurRadius(XmlUtil::readElementTextAsInt(handle, "BlurRadius", 2));
  output->setColor(XmlUtil::readElementTextAsColor(handle, "Color", QColor(0,0,0,127)));
  return output;
}


QGraphicsDropShadowEffect* Style::cloneShadow(QGraphicsDropShadowEffect* s) {
  QGraphicsDropShadowEffect* output = new QGraphicsDropShadowEffect();
  output->setXOffset(s->xOffset());
  output->setYOffset(s->yOffset());
  output->setBlurRadius(s->blurRadius());
  output->setColor(QColor(s->color().red(), s->color().green(), s->color().blue(), s->color().alpha()));
  return output;
}


SkinMetadata Style::getSkinMetadata(TiXmlElement* skinDocumentRoot) {
  SkinMetadata skinMetadata;
  skinMetadata.compatibleVersion = QString::fromUtf8(skinDocumentRoot->Attribute("compatibleVersion"));
  skinMetadata.name = QString::fromUtf8(skinDocumentRoot->Attribute("name"));
  skinMetadata.author = QString::fromUtf8(skinDocumentRoot->Attribute("author"));
  skinMetadata.isNull = false;
  return skinMetadata;
}


SkinMetadata Style::getSkinMetadata(const QString& filePath) {
  TiXmlDocument doc(filePath.toAscii());
  doc.LoadFile();

  TiXmlElement* root = doc.FirstChildElement("Skin");
  if (!root) {
    qWarning() << "Styles::LoadSkinFile: Could not load XML. No Skin element found:" << filePath;
    SkinMetadata output;
    output.isNull = true;
    return output;
  }

  return Style::getSkinMetadata(root);
}


bool Style::isSkinVersionCompatible(const QString& skinVersion) {
  QStringList splitted = skinVersion.split(".");
  if (splitted.length() <= 0) return true; // Try to load it anyway
  return splitted[0] == "2";
}


QRect Style::calculateContentRectangle(int width, int height, const RectangleStyle& padding) {
  return QRect(padding.left,
               padding.top,
               width - padding.width,
               height - padding.height);
}


void Style::loadSkinFile(const QString& filePath) {
  TiXmlDocument doc(filePath.toUtf8());
  doc.LoadFile(TIXML_ENCODING_UTF8);

  TiXmlElement* root = doc.FirstChildElement("Skin");
  if (!root) {
    qWarning() << "Style::loadSkinFile: Could not load XML. No Skin element found: " + filePath;
    return;
  }

  SkinMetadata skinMetadata = Style::getSkinMetadata(root);

  if (!Style::isSkinVersionCompatible(skinMetadata.compatibleVersion)) {
    MessageBoxes::error(StringUtil::convertFields(_("This skin is not compatible with the current version of %s.")).arg(APPLICATION_NAME));
    return;
  }

  // *****************************************************************
  // Set default values
  // *****************************************************************
  SAFE_DELETE(Style::background.shadow);
  SAFE_DELETE(Style::floatingButton.shadow);
  Style::icon.labelGap = 10;
  Style::floatingButton.hGap = 4;
  Style::floatingButton.vGap = 4;
  
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
      if (handle.Child("Shadow", 0).ToElement()) Style::background.shadow = Style::parseShadowXml(handle.Child("Shadow", 0));

    } else if (elementName == "FloatingButton") {

      Style::floatingButton.hGap = XmlUtil::readElementTextAsInt(handle, "HGap");
      Style::floatingButton.vGap = XmlUtil::readElementTextAsInt(handle, "VGap");
      if (handle.Child("Shadow", 0).ToElement()) Style::floatingButton.shadow = Style::parseShadowXml(handle.Child("Shadow", 0));

    } else if (elementName == "IconPanel") {
      
      XmlUtil::readElementTextAsRect(handle, "Padding", resultRect);
      Style::iconPanel.padding.fromRect(resultRect);
    
    } else if (elementName == "Tab") {

      XmlUtil::readElementTextAsRect(handle, "Padding", resultRect);
      Style::tab.padding.fromRect(resultRect);
      Style::tab.hGap = XmlUtil::readElementTextAsInt(handle, "HGap");
      if (handle.Child("TextFormat", 0).ToElement()) Style::tab.textFormat.fromXml(handle.Child("TextFormat", 0));

    }

  }

}