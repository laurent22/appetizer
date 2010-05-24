/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>
#include <MainScene.h>
using namespace appetizer;

MainScene::MainScene() {
  backgroundItem_ = new NineSliceItem();
  backgroundItem_->loadBackgroundImage("c:\\Users\\Laurent_2\\Desktop\\200.PNG");
  addItem(backgroundItem_);

  iconPanel_ = new IconPanel();
  iconPanel_->loadFolderItems(user.rootFolderItem->id());
  addItem(iconPanel_);
}


void MainScene::drawBackground(QPainter* painter, const QRectF& rect) {
  QGraphicsScene::drawBackground(painter, rect);

  backgroundItem_->setWidth(width());
  backgroundItem_->setHeight(height());

  iconPanel_->setWidth(width());
  iconPanel_->setHeight(height());
}