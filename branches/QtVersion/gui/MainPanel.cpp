/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#include <Application.h>
#include <MainPanel.h>
#include <Style.h>

using namespace appetizer;

MainPanel::MainPanel() {
  backgroundSprite_ = new NineSliceItem();
  backgroundSprite_->loadBackgroundImage("s:\\Docs\\PROGS\\C++\\Appetizer\\source\\branches\\QtVersion\\Data\\Skin\\Default\\Background.png");
  addItem(backgroundSprite_);

  iconPanel_ = new IconPanel();
  iconPanel_->loadFolderItems(Application::instance()->user().rootFolderItem()->id());
  addItem(iconPanel_);

  lastDrawnMaskSize_ = QSize(0, 0);
}


void MainPanel::drawMask(QPainter* painter, int x, int y, int width, int height) {
  if (lastDrawnMaskSize_.width() != width || lastDrawnMaskSize_.height() != height) {
    maskNineSlicePainter_.loadImage("s:\\Docs\\PROGS\\C++\\Appetizer\\source\\branches\\QtVersion\\Data\\Skin\\Default\\Background.png");
    maskPixmap_ = QPixmap(width, height);
    lastDrawnMaskSize_ = QSize(width, height);
  }
  
  maskNineSlicePainter_.drawImage(painter, x, y, width, height);
}


void MainPanel::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget) {
  GraphicsItem::paint(painter, option, widget);

  backgroundSprite_->setWidth(width());
  backgroundSprite_->setHeight(height());

  iconPanel_->setX(Style::background.padding.left);
  iconPanel_->setY(Style::background.padding.top);
  iconPanel_->setWidth(width() - Style::background.padding.width);
  iconPanel_->setHeight(height() - Style::background.padding.height);
}