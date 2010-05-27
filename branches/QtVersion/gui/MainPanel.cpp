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
  backgroundItem_ = new NineSliceItem();
  backgroundItem_->loadBackgroundImage("s:\\Docs\\PROGS\\C++\\Appetizer\\source\\branches\\QtVersion\\Data\\Skin\\Default\\Background.png");
  //addItem(backgroundItem_);

  iconPanel_ = new IconPanel();
  iconPanel_->loadFolderItems(Application::instance()->user().rootFolderItem()->id());
  addItem(iconPanel_);
}


QBitmap* MainPanel::composedMask() {
  QPixmap image(backgroundItem_->width(), backgroundItem_->height());
  image.fill(QColor(0,0,0));

  QPainter painter(&image);
  backgroundItem_->nineSlicePainter().drawImage(&painter, 0, 0, image.width(), image.height());
  int w = image.width();
  QBitmap* output = new QBitmap(image.createMaskFromColor(QColor(255,0,255).rgb()));
  return output;
}


void MainPanel::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget) {
  GraphicsItem::paint(painter, option, widget);  

  backgroundItem_->setWidth(width());
  backgroundItem_->setHeight(height());

  iconPanel_->setX(Style::background.padding.left);
  iconPanel_->setY(Style::background.padding.top);
  iconPanel_->setWidth(width() - Style::background.padding.width);
  iconPanel_->setHeight(height() - Style::background.padding.height);
}