/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>
#include <MainScene.h>
#include <Application.h>
#include <Style.h>

using namespace appetizer;

MainScene::MainScene() {
  backgroundItem_ = new NineSliceItem();
  backgroundItem_->loadBackgroundImage("s:\\Docs\\PROGS\\C++\\Appetizer\\source\\branches\\QtVersion\\Data\\Skin\\Default\\Background.png");
  addItem(backgroundItem_);

  iconPanel_ = new IconPanel();
  iconPanel_->loadFolderItems(Application::instance()->user().rootFolderItem()->id());
  addItem(iconPanel_);
}


void MainScene::drawBackground(QPainter* painter, const QRectF& rect) {
  QGraphicsScene::drawBackground(painter, rect);

  backgroundItem_->setWidth(width());
  backgroundItem_->setHeight(height());

  iconPanel_->setX(Style::background.padding.left);
  iconPanel_->setY(Style::background.padding.top);
  iconPanel_->setWidth(width() - Style::background.padding.width);
  iconPanel_->setHeight(height() - Style::background.padding.height);
}