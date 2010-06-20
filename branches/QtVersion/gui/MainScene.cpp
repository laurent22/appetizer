/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#include <Application.h>
#include <MainScene.h>
#include <Style.h>

using namespace appetizer;

MainScene::MainScene(GraphicsWindow* parentWindow): GraphicsScene(parentWindow) {
  mainPanel_ = new MainPanel(this->parentWindow());
  addItem(mainPanel_);

  resizeSprite_ = new GraphicsItem(this->parentWindow());
  resizeSprite_->setCursor(Qt::SizeFDiagCursor);
  resizeSprite_->resize(16, 16);
  addItem(resizeSprite_);

  QObject::connect(resizeSprite_, SIGNAL(mousePressed()), this, SLOT(resizeSprite_mousePressed()));
  QObject::connect(resizeSprite_, SIGNAL(mouseMoved()), this, SLOT(resizeSprite_mouseMoved()));

  mainPanel_->loadFolderItems(Application::instance()->rootFolderItem()->id());
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


void MainScene::drawBackground(QPainter* painter, const QRectF& rect) {
  QGraphicsScene::drawBackground(painter, rect);

  mainPanel_->resize(width(), height());

  resizeSprite_->move(width() - Style::background.padding.right, height() - Style::background.padding.bottom);
}