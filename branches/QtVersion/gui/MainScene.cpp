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

  resizeSprite_ = new GraphicsItem();
  resizeSprite_->setCursor(Qt::SizeFDiagCursor);
  resizeSprite_->setWidth(16);
  resizeSprite_->setHeight(16);
  addItem(resizeSprite_);

  QObject::connect(resizeSprite_, SIGNAL(mousePressed()),
                   this, SLOT(resizeSprite_mousePressed()));
  QObject::connect(resizeSprite_, SIGNAL(mouseMoved()),
                   this, SLOT(resizeSprite_mouseMoved()));
}


MainPanel* MainScene::mainPanel() const {
  return mainPanel_;
}


void MainScene::resizeSprite_mousePressed() {
  resizeDragData_.startMouse = QCursor::pos();
  resizeDragData_.startSize = QSize(this->width(), this->height());
}


void MainScene::resizeSprite_mouseMoved() {
  QPoint p = QCursor::pos();
  int dx = p.x() - resizeDragData_.startMouse.x();
  int dy = p.y() - resizeDragData_.startMouse.y();
  int newWidth = resizeDragData_.startSize.width() + dx;
  int newHeight = resizeDragData_.startSize.height() + dy;

  if (newWidth < MAIN_WINDOW_MIN_WIDTH) newWidth = MAIN_WINDOW_MIN_WIDTH;
  if (newHeight < MAIN_WINDOW_MIN_HEIGHT) newHeight = MAIN_WINDOW_MIN_HEIGHT;

  QRectF r = this->sceneRect();
  this->setSceneRect(r.x(), r.y(), newWidth, newHeight);
}


void MainScene::drawMask(QPainter* painter, int x, int y, int width, int height) {
  mainPanel_->drawMask(painter, x, y, width, height);
}


void MainScene::drawBackground(QPainter* painter, const QRectF& rect) {
  QGraphicsScene::drawBackground(painter, rect);

  mainPanel_->setWidth(width());
  mainPanel_->setHeight(height());

  resizeSprite_->setX(width() - resizeSprite_->width());
  resizeSprite_->setY(height() - resizeSprite_->height());
}