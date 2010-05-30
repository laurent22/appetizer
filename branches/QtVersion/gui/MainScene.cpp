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

  resizeSprite_ = new ResizeSprite();
  resizeSprite_->setCursor(Qt::SizeFDiagCursor);
  resizeSprite_->setWidth(16);
  resizeSprite_->setHeight(16);
  resizeSprite_->showDebugRectangle();
  addItem(resizeSprite_);
}


void MainScene::ResizeSprite::mousePressEvent(QGraphicsSceneMouseEvent* event) {
  MainScene* thisScene = (MainScene*)(this->scene());
  resizeDragData_.startMouse = QCursor::pos();
  resizeDragData_.startSize = QSize(thisScene->width(), thisScene->height());
}


void MainScene::ResizeSprite::mouseReleaseEvent(QGraphicsSceneMouseEvent* event) {
  
}


void MainScene::ResizeSprite::mouseMoveEvent(QGraphicsSceneMouseEvent* event) {
  MainScene* thisScene = (MainScene*)(this->scene());

  QPoint p = QCursor::pos();
  int dx = p.x() - resizeDragData_.startMouse.x();
  int dy = p.y() - resizeDragData_.startMouse.y();
  int newWidth = resizeDragData_.startSize.width() + dx;
  int newHeight = resizeDragData_.startSize.height() + dy;

  QRectF r = thisScene->sceneRect();
  thisScene->setSceneRect(r.x(), r.y(), newWidth, newHeight);
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