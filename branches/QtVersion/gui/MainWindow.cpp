/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#include <MainWindow.h>

using namespace appetizer;

MainWindow::MainWindow(): GraphicsWindow() {
  windowPixmap_ = NULL;

  scene_ = new MainScene(this);

  view_ = new QGraphicsView(this);
  view_->setScene(scene_);
  view_->setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
  view_->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
  view_->show();

  QObject::connect(scene_, SIGNAL(sceneRectChanged(const QRectF&)),
                   this, SLOT(scene_sceneRectChanged(const QRectF&)));
  QObject::connect(scene_->mainPanel()->backgroundSprite(), SIGNAL(mousePressed()),
                   this, SLOT(backgroundSprite_mousePressed()));
  QObject::connect(scene_->mainPanel()->backgroundSprite(), SIGNAL(mouseMoved()),
                   this, SLOT(backgroundSprite_mouseMoved()));
}


MainWindow::~MainWindow() {
  SAFE_DELETE(windowPixmap_);
}


QPixmap* MainWindow::getWindowPixmap() {
  SAFE_DELETE(windowPixmap_);

  windowPixmap_ = new QPixmap(scene_->width(), scene_->height());
  windowPixmap_->fill(QColor(0,0,0,0));
  QPainter painter(windowPixmap_);
  painter.begin(this);
  scene_->render(&painter);

  return windowPixmap_;
}


void MainWindow::backgroundSprite_mousePressed() {
  moveDragData_.startMouse = QCursor::pos();
  moveDragData_.startPos = QPoint(x(), y());
}


void MainWindow::backgroundSprite_mouseMoved() {
  QPoint p = QCursor::pos();
  int dx = p.x() - moveDragData_.startMouse.x();
  int dy = p.y() - moveDragData_.startMouse.y();
  int newX = moveDragData_.startPos.x() + dx;
  int newY = moveDragData_.startPos.y() + dy;
  move(newX, newY);
}


void MainWindow::scene_sceneRectChanged(const QRectF& rect) {
  resize(rect.width(), rect.height());
}


void MainWindow::resizeEvent(QResizeEvent* event) {
  QWidget::resizeEvent(event);

  int newWidth = event->size().width();
  int newHeight = event->size().height();

  // The view has a 1 pixel gray border that goes on top of all its children,
  // so we make it bigger and move it in such a way that the border is off-screen.
  view_->move(-1, -1);
  view_->resize(newWidth + 2, newHeight + 2);

  scene_->setSceneRect(0, 0, newWidth, newHeight);
  scene_->invalidate();

  QPixmap pixmap(newWidth, newHeight);
  QPainter painter(&pixmap);
  scene_->drawMask(&painter, 0, 0, newWidth, newHeight);
  QBitmap b = pixmap.createMaskFromColor(QColor(255,0,255));
  setMask(b);
}