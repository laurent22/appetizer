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
  mainPanel_ = new MainPanel();
  addItem(mainPanel_);
}


void MainScene::drawMask(QPainter* painter, int x, int y, int width, int height) {
  mainPanel_->drawMask(painter, x, y, width, height);
}


void MainScene::drawBackground(QPainter* painter, const QRectF& rect) {
  QGraphicsScene::drawBackground(painter, rect);

  mainPanel_->setWidth(width());
  mainPanel_->setHeight(height());
}