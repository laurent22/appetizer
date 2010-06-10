/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>
#include <MainWindow.h>

#include <NineSlicePainter.h>

using namespace appetizer;

MainWindow::MainWindow(): QWidget(NULL, Qt::FramelessWindowHint | Qt::WindowSystemMenuHint) {
  scene_ = new MainScene();

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

 setWindowFlags(Qt::FramelessWindowHint | Qt::Widget);
 resize(500,500);
 SetWindowLong(winId(), 
               GWL_EXSTYLE, 
               GetWindowLong(winId(), GWL_EXSTYLE) | WS_EX_LAYERED);	
}


void MainWindow::updateAlpha(QPixmap& widgetMask)
{	
 HBITMAP oldBitmap;
 HBITMAP hBitmap;	
 SIZE size;
 size.cx = widgetMask.width();
 size.cy = widgetMask.height();
 HDC screenDc = GetDC(NULL);
 POINT pointSource;
 pointSource.x = 0;
 pointSource.y = 0; 
 POINT topPos;
 topPos.x = x();
 topPos.y = y();	
 HDC memDc = CreateCompatibleDC(screenDc);
 BLENDFUNCTION blend;
 blend.BlendOp             = AC_SRC_OVER;
 blend.BlendFlags          = 0;
 blend.SourceConstantAlpha = 255;
 blend.AlphaFormat         = AC_SRC_ALPHA;
 hBitmap = widgetMask.toWinHBITMAP(QPixmap::PremultipliedAlpha); 
 oldBitmap = (HBITMAP)SelectObject(memDc, hBitmap);
 UpdateLayeredWindow(winId(), screenDc,  &topPos,  &size, memDc,  &pointSource, 0, &blend, ULW_ALPHA);
 ReleaseDC( NULL, screenDc);
 if (hBitmap!=NULL) {
   SelectObject(memDc, oldBitmap);
   DeleteObject(hBitmap); 
   DeleteObject(hBitmap);
 }
 DeleteDC(memDc); 
}


void MainWindow::updateAlphaWidget() {
  QPixmap pixmap(scene_->width(), scene_->height());
  pixmap.fill(QColor(0,0,0,0));
  QPainter painter(&pixmap);
  painter.begin(this);
  scene_->render(&painter);
  updateAlpha(pixmap);
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